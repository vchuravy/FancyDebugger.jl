module FancyDebugger

export @breakpoint, debug

# TODO: 
# - Doesn't deal with dynamic dispatch yet, we need to replace Expr(:call, ) with a call in AbsInt
# - Invalidations (e.g. turn breakpoints on/off, contract worldage )
#
# Notes:
# - JuliaInterpreter.jl ignores AbsstractInterpreter, this is a bug, but currently necessary for this to work.
#   otherwise we would need to switch back to the NativeInterpreter before executing the code we want to debug.
#
# - CompilerPlugin design
#
#  contextual(DebugPlugin()) do
#     f()
#  end
#
#  Needs a StackedMethodTable so that we can compose compiler plugins.  

import GPUCompiler: CodeCache, invalidate
const GLOBAL_CI_CACHE = CodeCache()

import Core: MethodMatch, MethodTable
import Core.Compiler: _methods_by_ftype, InferenceParams, get_world_counter, MethodInstance,
    specialize_method, InferenceResult, typeinf, InferenceState, NativeInterpreter,
    code_cache, AbstractInterpreter, OptimizationParams, WorldView, MethodTableView

import Debugger

struct DebugInterpreter <: AbstractInterpreter
    global_cache::CodeCache
    method_table::Union{Nothing,Core.MethodTable}

    # Cache of inference results for this particular interpreter
    local_cache::Vector{InferenceResult}
    # The world age we're working inside of
    world::UInt

    # Parameters for inference and optimization
    inf_params::InferenceParams
    opt_params::OptimizationParams


    function DebugInterpreter(cache::CodeCache, mt::Union{Nothing,Core.MethodTable}, world::UInt, ip::InferenceParams, op::OptimizationParams)
        @assert world <= Base.get_world_counter()

        return new(
            cache,
            mt,

            # Initially empty cache
            Vector{InferenceResult}(),

            # world age counter
            world,

            # parameters for inference and optimization
            ip,
            op
        )
    end
end

Core.Compiler.InferenceParams(interp::DebugInterpreter) = interp.inf_params
Core.Compiler.OptimizationParams(interp::DebugInterpreter) = interp.opt_params
Core.Compiler.get_world_counter(interp::DebugInterpreter) = interp.world
Core.Compiler.get_inference_cache(interp::DebugInterpreter) = interp.local_cache
Core.Compiler.code_cache(interp::DebugInterpreter) = WorldView(interp.global_cache, interp.world)

# No need to do any locking since we're not putting our results into the runtime cache
Core.Compiler.lock_mi_inference(interp::DebugInterpreter, mi::MethodInstance) = nothing
Core.Compiler.unlock_mi_inference(interp::DebugInterpreter, mi::MethodInstance) = nothing

Core.Compiler.may_optimize(interp::DebugInterpreter) = true
Core.Compiler.may_compress(interp::DebugInterpreter) = true
Core.Compiler.may_discard_trees(interp::DebugInterpreter) = true
Core.Compiler.verbose_stmt_info(interp::DebugInterpreter) = false

using Core.Compiler: OverlayMethodTable
Core.Compiler.method_table(interp::DebugInterpreter) =
    OverlayMethodTable(interp.world, interp.method_table)

Base.Experimental.@MethodTable(GLOBAL_METHOD_TABLE)

##
# MethodTable extensions 

function delete!(mt::Core.MethodTable, m::Method)
    ccall(:jl_method_table_disable, Cvoid, (Any, Any), mt, m)
end

function whichtt(mt::MethodTable, sig)
    mtv = OverlayMethodTable(Base.get_world_counter(), mt)
    whichtt(mtv, sig)
end

function whichtt(mtv::MethodTableView, sig)
    match, valid_worlds, overlayed = Core.Compiler.findsup(sig, mtv)
    match === nothing && return nothing
    return match.method
end

#
##

##
# Future: CompilerPlugins.jl

function debug(f, args...)
    @nospecialize f args
    interp = DebugInterpreter(GLOBAL_CI_CACHE, GLOBAL_METHOD_TABLE, get_world_counter(), InferenceParams(), OptimizationParams())
    oc = construct_oc_in_absint(f, interp, args...)
    oc(args...)
end

function construct_oc_in_absint(f, interp, args...)
    @nospecialize f args
    tt = Base.signature_type(f, Tuple{map(Core.Typeof, args)...})
    match, valid_worlds, overlayed = Core.Compiler.findsup(tt, Core.Compiler.method_table(interp))
    if match === nothing
        error(lazy"Unable to find matching $tt")
    end
    mi = specialize_method(match.method, match.spec_types, match.sparams)::MethodInstance
    code = Core.Compiler.get(code_cache(interp), mi, nothing)
    if code !== nothing
        inf = code.inferred::Vector{UInt8}
        ci = Base._uncompressed_ir(code, inf)
        return Core.OpaqueClosure(ci)
    end
    result = InferenceResult(mi)
    frame = InferenceState(result, #=cache=# :global, interp)
    typeinf(interp, frame)
    ci = frame.src
    return Core.OpaqueClosure(ci)
end

function get_single_method_match(@nospecialize(tt), lim, world)
    mms = _methods_by_ftype(tt, lim, world)
    isa(mms, Bool) && single_match_error(tt)
    local mm = nothing
    for i = 1:length(mms)
        mmᵢ = mms[i]::MethodMatch
        if tt === mmᵢ.spec_types
            mm === nothing || single_match_error(tt)
            mm = mmᵢ
        end
    end
    mm isa MethodMatch || single_match_error(tt)
    return mm
end

# End CompilerPlugins.jl
##

# TODO make this work for: g(x::T) where T
macro breakpoint(expr)
    def = deepcopy(expr)
    @assert def.head == :call
    def.args[1] = Expr(:overlay, GLOBAL_METHOD_TABLE, esc(def.args[1])) 
    quote
        $def = $Debugger.@enter $(esc(expr))
    end
end

end
