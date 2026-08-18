# Fractum

Fractum is a source-to-source compiler for a statically typed JavaScript-like language featuring Hindley-Milner (HM) type inference, implemented in Haskell. It parses Fractum source code, performs static type checking, and lowers the typed abstract syntax tree (AST) into clean, readable JavaScript output.

## Motivation

JavaScript is expressive, but its dynamic typing can shift certain bugs from compile-time to runtime. Fractum explores an alternative design space:

*   **Familiar syntax**: A JavaScript-like syntax to lower the learning curve.
*   **Strong static typing**: Catches type-related errors before the code is executed.
*   **HM type inference**: Reduces the burden of explicit type annotations while maintaining strong guarantees.

Fractum is intended for experimentation, education, and as a foundation for advanced language tooling.

## Getting Started

### Prerequisites

*   GHC (Glasgow Haskell Compiler)
*   Cabal package manager
*   NodeJs (only for e2e tests)

### Building from Source

```bash
cabal build
```

### Installing the compiler

```bash 
# Change --installdir to your desired output directory
cabal install all --installdir=$HOME/.local/bin --overwrite-policy=always
```

### Running the Compiler

```bash
fractumc --help
Fractum - type-safe Javascript dialect

Usage: fractumc SOURCE_FILES... [-O|--opt] [-o|--output ARG] [--color ARG]

  Compile Fractum files to readable javascript

Available options:
  SOURCE_FILES...          Source file to process
  -O,--opt                 Enable compiler optimizations
  -o,--output ARG          Output file emited by Compiler[default out.js]
  --color ARG              Disable coloured output (--color=never)
  -h,--help                Show this help text
```

## Language Guide

### Variable Binding and Immutability

Bindings are immutable by default, compiling to `const` in JavaScript. To declare a mutable binding, use `let mut`, which compiles to `let`.

```typescript
let x = 42;        // Immutable (const)
let mut y = 0;     // Mutable (let)
y = y + 1;         // OK

// x = 10;         // TYPE ERROR: Cannot assign to immutable binding 'x'
```

Type annotations are optional but fully supported:

```typescript
let a: Int = 42;
let mut b: Int = 0;
```

### Numeric Types and Explicit Casting

`Int` and `Float` are distinct types with no implicit coercion between them:
an arithmetic or comparison operator requires both operands to already be the
same numeric type. Converting between them requires an explicit Rust-style
`as` cast.

```typescript
let x = 5.5;       // Float
let y = 7;          // Int

// let bad = x + y;  // TYPE ERROR: `+` expects both operands to be `Float`

let z = x + y as Float;  // `as` binds tighter than `+`, so this is `x + (y as Float)`
```

Casting `Float` to `Int` truncates towards zero, matching Rust's `as`:

```typescript
let n = 9.9 as Int;   // 9
let m = -9.9 as Int;  // -9
```

### Functions and Lambdas

Functions can be declared using standard function syntax or arrow functions (lambdas). Type inference handles missing annotations where possible.

```typescript
function add(x: Int, y: Int): Int {
  return x + y;
}

// Arrow functions
let inc = (n: Int): Int => n + 1;
```

### Polymorphism and Higher-Order Functions

Fractum supports let-polymorphism, enabling reusable generic functions.

```typescript
let id = (x) => x;
let apply = (f, x) => f(x);

let result1 = id(42);
let result2 = id(true);
let result3 = apply(id, 100);
```

### Complex Types: Arrays, Objects, and Typing

Arrays and objects have structural typing. Accessing fields is statically checked.

```typescript
let arr = [1, 2, 3];
let obj = { a: 1, b: 2 };
let val: Int = obj.a;
```

User-defined types and parametric type aliases allow mapping complex structures:

```typescript
type Pair<A, B> = { first: A, second: B };

let p: Pair<Int, String> = { first: 42, second: "hello" };
```

### Algebraic Data types and pattern matching

```ts 
enum Option<T> {
    Some(T),
    None,
}

let x = Option::Some(10);

let val = match (x) {
    Option::Some(v) => v + 1,
    Option::None => 0,
};

print(val); // prints 11
```

The compiler enforces exhaustive pattern matching. For example:

```ts
let val = match (x) {
    Option::Some(v) => v + 1,
};
```

The above code will throw the error: match on `Option` is not exhaustive, missing: `None`

It is possible to add a wild-card catch all case to fix this

```ts
let val = match (x) {
    Option::Some(v) => v + 1,
    _ => 0,
};
```

### Control Flow

Standard conditionals are type-checked to ensure consistency.

#### if expressions
```typescript
let v = if (true) 1 else 0;
```

#### if statements
```ts 
if (2 == 2) {
  print(2);
} else {
  print(3);
}
```

#### while loops 
```ts 
let mut i = 0;

while (i < 5) {
  print(i);
  i = i + ;
}
```

### Modules

Fractum supports a TypeScript-like named import/export system. `export` prefixes a top-level `let`, `function`, `type`, or `enum` declaration to make it visible to other files; `import` pulls named declarations in from another file by relative path (the `.fr` extension is optional).

```ts
// math.fr
export function add(x: Int, y: Int): Int {
  return x + y;
}
export let PI = 3;
```

```ts
// main.fr
import { add, PI as Pi } from "./math";

print(add(1, Pi));
```

Aliasing with `as` is supported for both value and type/enum imports. There is no bundler or module loader involved at runtime: the compiler resolves the whole import graph itself and emits a single, readable JavaScript file, renaming any identifiers that would otherwise collide across files.

### DOM Manipulation (The Elm Architecture)

Fractum ships a small standard library (`stdlib/`) for building browser UIs in the
style of Elm's Model-View-Update: a `Model` describes the state of the application,
`view` renders it as a `Html<Msg>` tree, and `update` folds incoming `Msg` values into
a new model. Rendering itself is handled by a virtual DOM diff/patch engine in the
JavaScript runtime (`runtime/fractum_runtime.js`), so `view` stays a pure function
with no direct DOM access.

```ts
import { Attribute, Html, div, button, span, text } from "./stdlib/Html";
import { Cmd, cmd_none } from "./stdlib/Cmd";

type Model = { count: Int };

enum Msg {
  Increment,
  Decrement,
}

function init(): { model: Model, cmd: Cmd<Msg> } {
  return { model: { count: 0 }, cmd: cmd_none() };
}

function update(msg: Msg, model: Model): { model: Model, cmd: Cmd<Msg> } {
  return match (msg) {
    Msg::Increment => { model: { count: model.count + 1 }, cmd: cmd_none() },
    Msg::Decrement => { model: { count: model.count - 1 }, cmd: cmd_none() },
  };
}

function view(model: Model): Html<Msg> {
  return div([], [
    button([Attribute::OnClick(Msg::Decrement)], [text("-")]),
    span([], [text(toString(model.count))]),
    button([Attribute::OnClick(Msg::Increment)], [text("+")]),
  ]);
}

app({
  init:   init,
  update: update,
  view:   view,
  root:   "app",
});
```

`app` is a compiler builtin that lowers directly to `TypedJS.app(...)`
in the emitted JavaScript: it mounts onto the DOM element with the given `root` id,
performs the first render, and thereafter drives a batched dispatch loop that
re-renders on every message via a keyed/unkeyed virtual DOM diff. No hand-written
JavaScript glue is needed after compiling.

The standard library also covers side effects and subscriptions without breaking
purity: `Cmd<Msg>` (`stdlib/Cmd.fr`) describes effects such as HTTP requests, timers,
and local storage access, which the runtime interprets and feeds back in as `Msg`
values; `Sub<Msg>` (`stdlib/Sub.fr`) describes standing subscriptions such as
animation frames, key presses, and window resizes. `Option<T>` and `Result<T, E>`
(`stdlib/Option.fr`, `stdlib/Result.fr`) round out the library and are used
throughout to keep `null`/`undefined` and thrown exceptions out of application code.

<!--
Two runnable applications ship with the compiler: `examples/counter/` is the
smallest complete Elm-architecture program, and `examples/taskboard/` is a
five-module task board that exercises the rest of the surface, including keyed
list reordering, every sanitized event handler, commands for storage, timers,
randomness and HTTP, and subscriptions that start and stop with the model.
-->

## Diagnostics and Error Reporting

Fractum features a detailed diagnostic engine. Below is a catalog of currently implemented error codes:

| Code  | Kind                | Example Message                                        |
| :---- | :------------------ | :----------------------------------------------------- |
| E0001 | Type mismatch       | expected 'Int', found 'Bool'                           |
| E0002 | Infinite type       | type variable 'a' occurs in 'a -> a'                   |
| E0003 | Unbound variable    | 'x' not found in this scope                            |
| E0004 | Missing field       | no field 'z' on this type                              |
| E0005 | Duplicate binding   | 'x' is already defined in this scope                   |
| E0006 | Immutable assign    | cannot assign to 'x'                                   |
| E0007 | Undefined type      | type 'Foo' is not defined                              |
| E0008 | Duplicate type      | type 'Point' is already defined                        |
| E0009 | Type arity          | 'Pair' expects 2 type argument(s) but 1 were given     |
| E0010 | Non Exhaustive Match | match on `Color` is not exhaustive, missing: `Blue` |
| E0011 | Unknown Variant      | variant `Bogus` does not exist on enum `Shape` |
| E0012 | Unknown Enum         | enum `Option` is not defined |
| E0012 | Variant Arity Mismatch | variant `Some` of enum `Option` expects 1 field(s) but 2 were given |
| E0014 | Module not found     | cannot find module `./missing`                         |
| E0015 | Unbound import       | module `./math` has no exported member `sub`           |
| E0016 | Circular import      | modules form a cycle: `a.fr -> b.fr -> a.fr`         |
| E0017 | Invalid cast         | cannot cast `Bool` to `Int` -- only conversions between `Int` and `Float` are supported |
| E0099 | Internal / General   | raw message                                            |
