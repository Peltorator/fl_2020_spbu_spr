# peLLang documentation (by Peltorator)

This is the first part of the official documentation of the peLLang programming language. At this moment there is no compiler or interpreter. You can compile it with your eyes. Stay tuned! You can become my patron on Patreon if you want this language to be released ASAP.

## Spaces

It's allowed to use `space` or `\n`. There is no difference between them at all.

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

A name of a variable can be any non-empty sequence of lower and upper case letters, digits and underscores (`_`) which can't start with a digit.
Also a name of a variable can't be one of keywords of peLLang (we'll discuss them later).

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

Basic expression is a number or a variable. Expressions are constructed from basic expressions using brackets and the following operations:

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

Some incorrect examples:
- `+5` (Unary plus is forbidden)
- `(10 a + 7)`

## Instructions

In `if` and `while` instructions `0` is `false` and all other numbers are `true`.

Instruction     | Example
----------------|---------
assign          | `assign var (expr)`
if              | `if (expr) { instruct } else { instruct }`
`{}`            | `{ instruct_1; instruct_2; ... instruct_i; }`
while           | `while (expr) instruct`
read            | `read var`
print           | `print (expr)`

Correct code should be constructed from theese instructions recursively. Also `{}` must be present at the top level.
Note that you must assign a variable before using it.

Some examples:

- `{ write (505); }`
- `{read x; if (17 + 2 == x) { print (1); } else { print (2); }; read y; read x; }`

Some incorrect examples:

- `write (505)`
- `{ if (17 + 2 == x) { print (1); } else { print (2); }; }`


If you have any questions you can text me on Telegram: @peltorator.
