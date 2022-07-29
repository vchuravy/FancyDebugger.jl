# Prototype for a CompilerPlugin based Debugger for Julia

```julia
g() = println("hello")
f() = g()

@breakpoint g()
debug(f)
```