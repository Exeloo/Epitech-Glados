# JavadosScript documentation

## User Manual

### Introduction

This document is a user manual for the glados part-2 language, JavadosScript. It provides a brief overview of the language and its features.

JavadosScript is imperative and supports basic arithmetic operations, conditional statements, loops, functions, and data types.

JavadosScript is compiled to a custom assembly-like code that can be executed on a virtual machine.

The grammar of JavadosScript is inspired by javascript.

All instructions are written in the form of a statement, which is a sequence of characters that ends with a semicolon.

### Getting Started

#### Data Types

The glados part-2 language supports various data types, both simple and complex.

##### Simple Data Types

- **Integer**: Represents whole numbers.
  ```glados
  num = 42;
  ```
- **Float**: Represents numbers with decimal points.
  ```glados
  pi = 3.14;
  ```
- **Boolean**: Represents true or false values.
  ```glados
  isTrue = true;
  isFalse = false;
  ```
- **String**: Represents sequences of characters.
  ```glados
  greeting = "Hello, World!";
  ```

##### Complex Data Types

- **Array**: Represents a collection of values.
  ```glados
  numbers = [1, 2, 3, 4, 5];
  ```
- **Object**: Represents a collection of key-value pairs.
  ```glados
  person = {
      name: "Alice",
      age: 30
  };
  ```

#### Variables

Variables can be declared and assigned values using the "=" sign.

```glados
x = 10;
y = 20;
```

#### Arrays

Arrays can be declared and initialized with values, then accessed using an index and modified.

```glados
numbers = [1, 2, 3, 4, 5];

first = numbers[0];

numbers[1] = 10;
```

#### Arithmetic Operations

The language supports basic arithmetic operations such as addition(+), subtraction(-), multiplication(*), division(/) and modulus(%). The result of an operation can be stored in a variable.

```glados
sum = x + y;
difference = x - y;
product = x * y;
quotient = x / y;
remainder = x % y;
```

#### Comparison Operators

The language supports comparison operators such as less than(<), more than(>), equal to(==), not equal to(!=), less than or equal to(<=), and more than or equal to(>=).
The result of a comparison can be used in conditional statements.

```glados
isLess = x < y;
isEqual = x == y;
isNotEqual = x != y;
isLessOrEqual = x <= y;
isMoreOrEqual = x >= y;
```

#### Logical Operators

The language supports logical operators such as not(!), or(||), and(&&). The result of a logical operation can be used in conditional statements.

```glados
isNot = !x;
isOr = x || y;
isAnd = x && y;
```

#### Statements

Statements can be used to perform actions such as printing to the console, returning a value, breaking out of a loop, and continuing to the next iteration of a loop.

```glados
print("Hello, World!");

return 42;

break;

continue;
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
i = 0;
while (i < 10) {
  i = i + 1;
}
```

```glados
for (j = 0; j < 10; j = j + 1) {
  print(j);
}
```

#### Functions

Functions can be defined using the function keyword.

```glados
function add(a, b) {
    return a + b;
}

result = add(10, 20);
```

## Grammar

The grammar (using EBNF) for the glados part-2 language is defined as follows:

```
program              = { statement } ;

statement            = variable declaration | expression | if statement | while statement | for statement | function declaration | statement loop ;

variable declaration = identifier , "=" , expression ;

expression           = [ statement arguments ] , term , [ operator , term ] ;

term                 = identifier | number | function call ;

operator             = "+" | "-" | "*" | "/" | "%" | "<" | "==" | "!=" | "<=" | ">=" | ">" | "&&" | "||" ;

statement arguments  = "!" | "return" | "print" ;

statement loop       = "break" | "continue" ;

if statement         = "if" , "(" , expression , ")" , "{" , program , "}" ;

while statement      = "while" , "(" , expression , ")" , "{" , program , "}" ;

for statement        = "for" , "(" , [ variable declaration ] , ";" , expression , ";" , [ expression ] , ")" , "{" , program , "}" ;

function declaration = "function" , identifier , "(" , [ parameters ] , ")" , "{" , program , "}" ;

parameters           = identifier , { "," , identifier } ;

function call        = identifier , "(" , [ arguments ] , ")" ;

arguments            = expression , { "," , expression } ;

identifier           = letter , { letter | digit | "_" } ;

number               = [ "-" ], digit , { digit } ;

letter               = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;

digit                = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
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

Unit tests validate the correctness and security of the compilation process. These tests cover various scenarios, ensuring that the compiler behaves as expected under different conditions.
