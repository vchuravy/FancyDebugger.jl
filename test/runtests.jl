using Test
using FancyDebugger

g() = 1
Base.Experimental.@overlay FancyDebugger.GLOBAL_METHOD_TABLE g() = 2

f() = g()
f_invoke() = invoke(g, Tuple{})
f_invokelatest() = invokelatest(g)

@testset "Overlay'd method execution" begin
    @test g() == 1
    @test FancyDebugger.debug(g) == 2

    @test f() == 1
    @test FancyDebugger.debug(f) == 2

    @test f_invoke() == 1
    @test FancyDebugger.debug(f_invoke) == 2

    @test f_invokelatest() == 1
    @test_broken FancyDebugger.debug(f_invokelatest) == 2
end

Base.Experimental.@overlay FancyDebugger.GLOBAL_METHOD_TABLE g() = 3

@testset "Invalidated Overlay'd method execution" begin
    @test g() == 1
    @test FancyDebugger.debug(g) == 3

    @test f() == 1
    @test FancyDebugger.debug(f) == 3

    @test f_invoke() == 1
    @test FancyDebugger.debug(f_invoke) == 3

    @test f_invokelatest() == 1
    @test_broken FancyDebugger.debug(f_invokelatest) == 3
end

let m = FancyDebugger.whichtt(FancyDebugger.GLOBAL_METHOD_TABLE, Base.signature_type(g, Tuple{}))
    FancyDebugger.delete!(FancyDebugger.GLOBAL_METHOD_TABLE, m)
end

@testset "Deleted Overlay'd method execution" begin
    @test g() == 1
    @test FancyDebugger.debug(g) == 1

    @test f() == 1
    @test FancyDebugger.debug(f) == 1

    @test f_invoke() == 1
    @test FancyDebugger.debug(f_invoke) == 1

    @test f_invokelatest() == 1
    @test FancyDebugger.debug(f_invokelatest) == 1
end

@noinline g_barrier() = Base.inferencebarrier(1)
Base.Experimental.@overlay FancyDebugger.GLOBAL_METHOD_TABLE g_barrier() = 2

f_barrier() = g_barrier()

@testset "Overlay'd method execution" begin
    @test g_barrier() == 1
    @test FancyDebugger.debug(g_barrier) == 2

    @test f_barrier() == 1
    @test FancyDebugger.debug(f_barrier) == 2
end

Base.Experimental.@overlay FancyDebugger.GLOBAL_METHOD_TABLE g() = 3

@testset "Invalidated Overlay'd method execution" begin
    @test g_barrier() == 1
    @test FancyDebugger.debug(g_barrier) == 3

    @test f_barrier() == 1
    @test FancyDebugger.debug(f_barrier) == 3
end

let m = FancyDebugger.whichtt(FancyDebugger.GLOBAL_METHOD_TABLE, Base.signature_type(g, Tuple{}))
    FancyDebugger.delete!(FancyDebugger.GLOBAL_METHOD_TABLE, m)
end

@testset "Deleted Overlay'd method execution" begin
    @test g_barrier() == 1
    @test FancyDebugger.debug(g_barrier) == 1

    @test f_barrier() == 1
    @test FancyDebugger.debug(f_barrier) == 1
end