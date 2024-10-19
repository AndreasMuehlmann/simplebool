# simplebool

A program that parses a boolean expressions and tries to simplify it showing how it simplifies the expression.

## Quickstart

- `cabal` has to be installed.
- Clone the repository with: `git clone https://github.com/AndreasMuehlmann/simplebool.git`.
- Run the program with `cabal run simplebool -- "<boolean expression>"`
- Or build an executable with `cabal build`

## Specifying boolean expressions

Example:

```
(a and not a ) or not (not c or b) and 1 or 0
```

A variable name should only contain letters

## Interpreting the output

The program first prints the given boolean expression.
Then it goes on to print the rule it used to modify the experssion and what the expression looks like after applying the modification.

Example:

```
simplebool "(a and not a ) or not (not c or b) and 1 or 0"
(((a and not a) or not (not c or b)) and 1) or 0
identity -> ((a and not a) or not (not c or b)) and 1
identity -> (a and not a) or not (not c or b)
complement -> 0 or not (not c or b)
identity -> not (not c or b)
de-morgan -> not not c or not b
double-negation -> c or not b
```
