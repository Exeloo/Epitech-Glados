# glados part-2 languge documentation

## User Manual

### Introduction

This document is a user manual for the glados part-2 language. It provides a brief overview of the language and its features.

### Getting Started

#### Data Types

The glados part-2 language supports various data types, both simple and complex.

##### Simple Data Types

- **Integer**: Represents whole numbers.
  ```glados
  let num = 42;
  ```
- **Float**: Represents numbers with decimal points.
  ```glados
  let pi = 3.14;
  ```
- **Boolean**: Represents true or false values.
  ```glados
  let isTrue = true;
  let isFalse = false;
  ```
- **String**: Represents sequences of characters.
  ```glados
  let greeting = "Hello, World!";
  ```

##### Complex Data Types

- **Array**: Represents a collection of values.
  ```glados
  let numbers = [1, 2, 3, 4, 5];
  ```
- **Object**: Represents a collection of key-value pairs.
  ```glados
  let person = {
      name: "Alice",
      age: 30
  };
  ```

#### Variables

Variables can be declared and assigned values using the let keyword.

```glados
let x = 10;
let y = 20;
```

#### Arithmetic Operations

The language supports basic arithmetic operations such as addition(+), subtraction(-), multiplication(*), division(/) and modulus(%). The result of an operation can be stored in a variable.

```glados
let sum = x + y;
let difference = x - y;
let product = x * y;
let quotient = x / y;
let remainder = x % y;
```

#### Comparison Operators

The language supports comparison operators such as less than(<) and equal to(==). The result of a comparison can be used in conditional statements.

```glados
let isLess = x < y;
let isEqual = x == y;
```

#### Conditional Statements

The language supports if conditional statements.

```glados
if x > y {
    print("x is greater than y");
}
```

#### Loops

The language supports while and for loops.

```glados
let i = 0;
while i < 10 {
    // body
}
```

```glados
for (let j = 0; j < 10; j = j + 1) {
  // body
}
```

#### Functions

Functions can be defined using the function keyword.

```glados
function add(a, b) {
    return a + b;
}

let result = add(10, 20);
```

## Grammar

The grammar (using EBNF) for the glados part-2 language is defined as follows:

```
program         ::= { statement }

statement       ::= variable-declaration | expression | if-statement | while-statement | for-statement | function-declaration

variable-declaration ::= "let" identifier "=" expression

expression      ::= term { operator term }

term            ::= identifier | number | function-call

operator        ::= "+" | "-" | "*" | "/" | "%" | "<" | "=="

if-statement    ::= "if" "(" expression ")" "{" program "}"

while-statement ::= "while" "(" expression ")" "{" program "}"

for-statement   ::= "for" "(" variable-declaration ";" expression ";" expression ")" "{" program "}"

function-declaration ::= "function" identifier "(" [ parameters ] ")" "{" program "}"

parameters      ::= identifier { "," identifier }

function-call   ::= identifier "(" [ arguments ] ")"

arguments       ::= expression { "," expression }

identifier      ::= letter { letter | digit | "_" }

number          ::= digit { digit }

letter          ::= "a" | ... | "z" | "A" | ... | "Z"

digit           ::= "0" | ... | "9"
```

## Compilation Process

#### Parsing

The first step in the compilation process is parsing the source code to generate an S-expression tree (sExpr). The parser reads the source code character by character and constructs the tree based on the grammar rules.

#### S-Expression to AST Conversion

The S-expression tree is then converted to an Abstract Syntax Tree (AST) by recursively traversing the tree and creating AST nodes for each expression.

#### Binary Conversion

The AST is then converted to a asembly like code by recursively traversing the tree and generating the corresponding assembly instructions, this code is then written to a file in binary format.

## Security

#### Input Validation

To ensure the integrity and security of the compilation process, the parser validates the source code by reading it character by character and constructing an S-expression tree based on the predefined grammar rules. This prevents any bad code from being executed during the compilation process.

#### Error Handling

The compiler has built-in error handling mechanisms to detect and report any syntax errors in the source code. If an error is detected, the compiler will display an error message indicating the type of error and the part of the code where it occurred.

#### Testing

unit tests validate the correctness and security of the compilation process. These tests cover various scenarios, ensuring that the compiler behaves as expected under different conditions.
