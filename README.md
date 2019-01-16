# husband

# Overview

Husband is a brainfuck with pointer constraints. The symbols `^` and `v` constrain the interpreter to only perform incrementing or decrementing operations (respectively) for the tape cell in which they are declared.

Brainfuck with these constraints is substantially more pleasant than without.


## Using constraints

For example, starting from cell 0, the sequence `^++` constrains cell 0 to only allow incrementing operations, and then increments the counter at that cell twice. Conversely, `v++` constrains cell 0 to only allow decrementing operations, and then immediately returns with an error message when the increment is called. 

It is an error to mark an opposing constraint on a cell after it has been constrained, as in `^v`.

Constraints work across loops as you would expect, maintaining a global scope for which tape cells are constrained.