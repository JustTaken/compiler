# Goal
The propouse of this compiler is to be light weight. Whats the meaning of light
weight?. It means that it does not do crazy optimizations as you going to see
in productions compilers - like the ones made with llvm - when they take out a
lot of code that is not necessary. For example, this one does not replace
binary operations that are known to be know at compile time.

```rust
let a = 10 + 10; // what the user types

let a = 20; // with production compilers
let a = 10 + 10; // with this compiler
```

Why? because this is a design choice.

# Status
This is made to compile a made up language, and the programs that runs without any
problems are just programs that use integers and do specific and restricted
binary operations with then. Like:

```txt
type i32 = 4;

proc main(): i32 {
    let a: i32 = 10 + 10;

    let c: i32 = {
        let b: i32 = a + 50;
        b
    };

    c
}
```
