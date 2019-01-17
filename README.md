# husband

Husband (n): A boyfriend that constrains. 

# Overview

Husband is a brainfuck with pointer constraints. The symbols `^` and `v` constrain the interpreter to only perform incrementing or decrementing operations (respectively) for the tape cell in which they are declared.

Brainfuck with these constraints is substantially more pleasant than without.

Additionally, Husband imports the function semantics of [Boyfriend](https://github.com/rgscherf/boyfriend), unlocking code reuse in a Brainfuck environment.

## Using constraints

For example, starting from cell 0, the sequence `^++` constrains cell 0 to only allow incrementing operations, and then increments the counter at that cell twice. Conversely, `v++` constrains cell 0 to only allow decrementing operations, and then immediately returns with an error message when the increment is called. 

It is an error to mark an opposing constraint on a cell after it has been constrained, as in `^v`.

Constraints work across loops as you would expect, maintaining a global scope for which tape cells are constrained.

## Using functions

Per the [Boyfriend](https://github.com/rgscherf/boyfriend) docs, you may enclose any expression(s) in `{}` to save them as a function. The function is *keyed to the value of the tape at the point where you defined that function*. Later, use `;` to call the function keyed to the value at the tape's current position.

For example, `+{+++}` saves the function "increment the current tape cell three times" to the key "1". Later, call `;` on a cell whose value is "1" to call that function starting on the current cell.

## Usage

From the Haskell repl:

```
:l Husband

eval "++>++."
```

You may also use the function `strToExprs` with a string expression to see the AST for that expression.
