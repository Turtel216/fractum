# Fractum

Fractum is a source-to-source compiler for a statically typed JavaScript-like language featuring Hindley-Milner (HM) type inference, implemented in Haskell. It parses Fractum source code, performs static type checking, and lowers the typed abstract syntax tree (AST) into clean, readable JavaScript output.

## Motivation

JavaScript is expressive, but its dynamic typing can shift certain bugs from compile-time to runtime. Fractum explores an alternative design space:

*   **Familiar syntax**: A JavaScript-like syntax to lower the learning curve.
*   **Strong static typing**: Catches type-related errors before the code is executed.
*   **HM type inference**: Reduces the burden of explicit type annotations while maintaining strong guarantees.
*   **Readable generation**: Outputs idiomatic JavaScript that is easy to debug and deploy.

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

## Example Compilation

### Input

```typescript
let id = (x) => x;
let a = id(1);

function add(x: Int, y: Int): Int {
  return x + y;
}

let mut b = add(a, 1);

if (b > 5) {
  print(b);
} else {
  b = 100;
  print(b);
}
```

### Output (out.js)

```javascript
const id = (x) => x;
const a = id(1);
function add(x, y) {
  return x + y;
}
let b = add(a, 1);
if (b > 5) {
  console.log(b);
} else {
  b = 100;
  console.log(b);
}
```

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
| E0099 | Internal / General   | raw message                                            |

## Roadmap and Current Limitations

**Currently Supported:**
*   Parser (Megaparsec) and Typed AST
*   HM-style type inference and checking
*   Precedence-aware JavaScript pretty-printer
*   Immutability by default and mutation checks
*   Polymorphic functions and type aliases
*   Algebraic data types (ADTs) and pattern matching

**Future Enhancements:**
*   Module and import system
*   Optimization pipeline
*   Flexible object typing while preserving static structural safety.
*   Language Server Protocol (LSP) integration and a dedicated code formatter.
