# peLLang documentation (by Peltorator)

![Image description](logo.png)

This is the first part of the official documentation of the peLLang programming language. You can become my patron on Patreon if you want this language to be released ASAP.

## Spaces

It's allowed to use `space` or `\n`. There is no difference between them at all.

I received some critical reviews in which people complained that there are no `\t` symbols. Let me explain it.
Tabs are sometimes displayed as 4 spaces and sometimes as 8 spaces. If accidentally someone uses them mixed, it's a nightmare.
Just set a tab button on your computer as 4 spaces.

## Numbers

Any sequence of digits is a number. For example:

- `0`
- `1`
- `9`
- `12`
- `0000`
- `00001010`

Some examples of incorrect numbers:

- `-12` (it's an expression)
- `1e5` (we don't use this stuff, just print 5 zeros)

## Variables

The name of a variable can be any non-empty sequence of lower and upper case letters, digits and underscores (`_`) which can't start with a digit.

Some examples:

- `peltorator`
- `Peltorator`
- `imp`
- `drozd`
- `_______p_E_l_t_o_r_101_a_t_o_r_____`
- `a`
- `_`

Some incorrect exmples:

- `300iq`
- `rock'n'roll`


## Expressions

Basic expression is a number or a variable. Expressions are constructed from basic expressions using brackets, functions calls (we'll talk about it later) and the following operations:

Operator | Example       | Associativity | Priority | Comment
---------|---------------|---------------|----------|--------
 `\|\|`  | `x \|\| y`    | Right         | 1        |
 `&&`    | `x && y`      | Right         | 1        | As you can see, `\|\|` and `&&` have equal priority. It's because I think that mixed use of them makes it difficult to read someone's code.
 `==`    | `x == y`      | No            | 2        | We don't have bools, so the result is `0` or `1`.
 `!=`    | `x != y`      | No            | 2        | We don't have bools, so the result is `0` or `1`.
 `<`     | `x < y`       | No            | 2        | We don't have bools, so the result is `0` or `1`.
 `<=`    | `x <= y`      | No            | 2        | We don't have bools, so the result is `0` or `1`.
 `>`     | `x > y`       | No            | 2        | We don't have bools, so the result is `0` or `1`.
 `>=`    | `x >= y`      | No            | 2        | We don't have bools, so the result is `0` or `1`.
 `+`     | `x + y`       | Left          | 3        |
 `-`     | `x - y`       | Left          | 3        |
 `*`     | `x * y`       | Left          | 4        |
 `/`     | `x / y`       | Left          | 4        | Rounding to zero.
 `^`     | `x ^ y`       | Left          | 5        |
 `-`     | `-x`          | Right         | 6        | It's unary minus. As you can see, it's priority is greater than `^`, so `-3^2 = 9`.

Some examples:

- `1`
- `-2^2`
- `----4`
- `(((-peltorator * (42 - 010)) || (2 == isaf27)) && x && (((y))))`
- `f(2, x) + 12 == 0 || g()`

Some incorrect examples:
- `+5` (Unary plus is forbidden)
- `(10 a + 7)`

## Instructions

In `if` and `while` instructions `0` is `false` and all other numbers are `true`.

Instruction     | Example                                       | Notes
----------------|-----------------------------------------------|--------------------------------------------------------------
assign          | `assign var (expr)`                           | You should put at least one space symbol after `asign` word.
if              | `if (expr) { instruct } else { instruct }`    |
`{}`            | `{ instruct_1; instruct_2; ... instruct_i; }` |
while           | `while (expr) instruct`                       |
read            | `read var`                                    | You should put at least one space symbol after `read` word.
print           | `print (expr)`                                |

## Functions

Function definition is something of this kind:

`func f(x1, x2, ..., xn) bd return (exp);`

Where `func` is a keyword, `f, x1, ..., xn` are correct variable names (`n >= 0`), `bd` is something of kind `{}`, `return` is a keyword and `exp` is expression. You can think that `return` is inside `bd` and is just the last instruction. If you don't really want to return anything, just return `0`. If you don't need any function body, you can just use an empty body: `{}`.

Note that you can use any variables from `bd` that are assigned at the highest level in `return` expression.

Note that variables in different functions that have equal names are different variables.

Note that `f(x)` and `f(x, y)` are different functions.

If you defined one function twice, the compiler chooses an implementation at his discretion.

If you call a function that doesn't exist, empty function that returns `0` is called instead.

Some examples:

- `func _() { } return (0);`
- `func f(x, y, z) { print(x + y); assign t (x * z); } return (t + y);`

Some incorrect examples:
- `func f(x) `
- `func f(x) { }`
- `func f(x) return (0);`

## Code

The correct code should be constructed from these instructions recursively.
Note that if you didn't assign a variable, then it's value is equal to zero.

Correct code is something of this kind:

`f1 f2 f3 ... fn body`

Where `f1, f2, ..., fn` are function definitions (`n >= 0`) and `body` is a construction of type `{}`.

Some examples:

- `{ print (505); }`
- `{read x; if (17 + 2 == x) { print (1); } else { print (2); }; read y; read x; }`
- `func fib(x) { print (x); } return (0); func fact(y, z) { } return (y * z); { fib(2); print (fact(3, 4) + 14); }`
- `func f() { } return (0); { print (f()); }`
- `{ }`

Some incorrect examples:

- `print (505);`
- `func f() { } return (42);`
- ` `


If you have any questions you can text me on Telegram: @peltorator.

