# Designing bf-piet: A Brainfuck to Piet transpiler


## Introduction to Brainfuck and Piet

### Notation and Terminology

This list is simply here for reference (skip on first reading);
the terms are introduced and explained in the text as needed.

* `=` = equality, `<>` = inequality,
  `==` = equivalence, `:=` = definition/assignment.
* CP = cell (data) pointer, *CP = value at the address of the cell pointer.
* IP = instruction pointer, *IP = instruction at the instruction pointer.
* move-op = an operation from {`<`, `>`},
  use-op = an operation from {`+`, `-`, `[`, `]`, `,`, `.`}.
* CC = codel chooser, DP = direction pointer
* IR = Intermediate Representation,
  IR block = a 2D array of codels implementing Piet IR.
* Often the entry and exit of an IR block can be described by a six character
  string like "TL>BRv" which means that code enters from the top left (TL)
  in the right direction (>) and exits from the bottom right (BR) in the
  downwards direction (v). Other direction strings are ^ and <.
* canonical (adj.) = an IR block that takes input from the top left
  (or top right) and outputs to the top right (top left).
  Both entry and exit are rightwards (leftwards).
  More concisely, canonical = ("TL>TR>" or "TR<TL<").

### Brainfuck

[Brainfuck reference]: https://en.wikipedia.org/wiki/Brainfuck

### Piet

[Piet reference]: http://www.dangermouse.net/esoteric/piet.html

### Piet IR

Directly translating Brainfuck to Piet commands is possible but inconvenient,
as the number of Piet commands increases very quickly.
Introducing an intermediate representation (IR) allows us to think and reason
about our code more easily.

Our Piet IR instruction set is defined below.
Most of these can be represented using Piet codels arranged in a straight line,
so we describe each IR instruction as a sequence of Piet commands,
which can in turn be translated into a sequence of colour blocks.
Since the representation in terms of Piet commands is not unique,
only one possible command sequence has been listed.

_Note:_ In the following table, all Piet operations append in the direction
of the DP (initially rightwards) unless mentioned otherwise.
Single letters like `a` denote integers,
`$(·)` denotes evaluation
(so `$(ir_instr)` will expand as a sequence `pt_cmd_1, pt_cmd_2, ...`), and
`size` denotes a variable storing the size of the current colour block.

Piet IR instruction | Piet command/codel sequence
--------------------|----------------------
`input`             | `in, $(mod 256)`
`output`            | `dup, out`
`white`             | Appends a white codel.
`random`            | Appends a random coloured codel.
`cp A` (`A ≥ 0`)    | Appends `A` copies of the current codel.
`grow A` (`A > 0`)  | If `size ≤ A`, then `$(cp A-size)`, else `$(white), $(random), $(cp A-1)`.
`push a` (`a > 0`)  | `$(grow a), push`
`push a` (`a = 0`)  | `$(push 1), not`
`push a` (`a < 0`)  | `$(push 1), $(push a+1), subtract`
`add a`             | `$(push a), add`
`subtract a`        | `$(push a), subtract`
`multiply a`        | `$(push a), multiply`
`mod a`             | `$(push a), mod`
`roll a b`          | `$(push b), $(push a), roll`
`loop_until_zero $(ir)` | No generic straight-line representation exists. See [Control flow](#Control-flow-operands--and-).
`eop`               | No generic straight-line representation exists. See [End of program](#End-of-progra).


## Modularity

The transpiler is conceptually broken up into the following passes:

* **Parse** - Converts Brainfuck input to instructions and syntax errors.

  As far as I know, the only requirement is that brackets ought to be matched.

* **Polish** (or Optimise) - Takes parsed input and produces more efficient
  Brainfuck output.

  Warns if simple optimisations such as stripping `++><--` are possible.

* **Publish** (or Translate) - Translates Brainfuck code to Piet IR.

  Depending on command-line flags, the pass makes different optimisations.
  Say we want to optimise for known Brainfuck [functions][Brainfuck algorithms]
  that can be translated to faster Piet IR.
  In such a case, simple loops like `[>+<-]` should be replaced with an
  appropriate `add`-like Piet command sequence
  (the command sequence will be longer than just `add` if the two cell
  values are not at the top of the stack, more on this later).

* **Paint** - Takes a sequence of Piet IR and outputs a 2D array of
  colours representing the final program.

  Like an actual painter, the Paint pass has many degrees of freedom:
  - colouring the program in various ways,
  - changing the shapes of different colour blocks
  - deciding the layout of codels, and
  - inserting dummy code (comments?) around the "actual" code.

* **Print** - Takes a 2D array of colours and creates an image file.

[Brainfuck algorithms]: https://esolangs.org/wiki/Brainfuck_algorithms


## The tape and the stack

Brainfuck code talks about a tape with at least 30,000 cells.
Piet code talks about an ever-changing stack.
Perhaps the two aren't a match made in heaven...

### Programs with loops
Here is an approach that will work _if_ one assumes that the initial size
of the Piet stack is bigger than the maximum value of CP during the life
of the program.

The basic idea is to treat the stack as a circular tape, where the `roll`
operation is used to simulate move-ops.

* `>` increments *CP by 1 - Piet IR: `roll -1 tape_size`.
* `<` decrements *CP by 1 - Piet IR: `roll 1 tape_size`.

(The actual signs are irrelevant, only the relative -1 is important.)

Technically, we would be 100% correct if we set the stack size to
30,000 and called it a day as the errors for "going around" the tape are
implementation defined.

### Programs without loops

(This approach will result in fewer instructions overall but is limited because
it translated Brainfuck's relative movement scheme to an absolute one in Piet.)
<strike>
Roughly speaking, a Brainfuck programs falls in one of these two categories:

* It uses a large number of distinct cells during its lifetime.
* It sticks to using a few number of cells.

It is expected that most "useful" Brainfuck code falls into the latter group.
Therefore, in order to minimise the amount of generated Piet code, by default,
we only record used cells in the Piet stack.
</strike>

The Translate pass maintains a vector of values containing cell positions.
The Piet stack is read _backwards_ from the end of the vector.
```
                          bf-piet
vector  0 --------------> 6  ──┼──╖
 celln  7 23 10  8  1  4 13  ──┘  ║ ──┐
 value A4 9B 04 30 F1 C6 75  ──┐  ║ ──┴╴Brainfuck
 stack  6 <-------------- 0  ──┤ ─╜
                           Piet
```

#### Translating move-ops

The Translate pass has a function `c2v: cell_pos -> Maybe vector_pos` which
searches for a `cell_pos` in the vector (`type vector_pos := unsigned int`).
It also maintains a variable `cp` (type `cell_pos`)
representing the cell pointer as it steps through the code.
If a move-op is encountered, only the `cp` is changed.

If the next operation is a use-op, there are two possibilities:
`U0 := c2v(cp) = None` (key is missing) and
`U1 := c2v(cp) = Some x` (key is present).

_Note:_ For convenience, we refer to a maximal contiguous sequence of use-ops
more simply as a "use-op sequence".

The operation for `U0` is simple:
append `cp` to the vector,
generate Piet: `push 1, not == push 0` and
perform the use-op sequence.

For `U1`, the value to be operated on may not be at the top of stack
(`x <> vector.len()-1`).
We should somehow get it to the top of the stack ("bubbling")
and perform all the use-ops before we come across the next move-op
(i.e., perform the next use-op sequence).

So how exactly do we generate the Piet code for handling `U1`?
Here are two slightly different approaches one could take:

* Bubbling only -
  Bubble `cp` to the end of the vector (which represents the top of the stack).
  We then bubble the value on the Piet stack using `bubble`.
  Let `y := vector.len() - x`.
  Piet IR: `roll -1 y`.
  ```
  Example:
                   x            bubble cp
  vector  0  1  2  3  4  5  6  ----------->  vector  0  1  2  3  4  5  6
  celln   7 23 10  8  1  4 13                celln   7 23 10  1  4 13  8
  ```
  Now perform the next use-op sequence.

* Bubbling and burying -
  The vector will be left unaltered;
  the bubbling and burying will only be done on the stack.
  Let `y := vector.len() - x`.
  Piet IR: `roll -1 y`.
  Now perform the next use-op sequence.
  Piet IR: `roll +1 y`.
  The value is buried at the same place it was originally at,
  which ensures that the vector cell order is consistent with the stack order.

## Translating use-ops

The preceding section described the translation of the move-ops `>` and `<`.
Recall that the translation of move-ops ensures that *CP is present at the top
of the stack immediately before a use-op is called.

Here, we discuss how the use-ops are converted to Piet code.

### Numbers and Number-modifying operands: `+`, `-`, `,`

First, let us look at the representation of numbers.
Brainfuck cells are byte-sized, so the numbers represented are 0 .. 255 (inclusive).
On the other hand, Piet allows arbitrary sized integers (overflow is a runtime error),
which makes life simple: each Brainfuck cell can be represented by a single element.
Three use-ops out of six manipulate the value in a given cell:

####

* `+` increments *CP by 1 - Piet IR: `add 1, mod 256`.
* `-` decrements *CP by 1 - Piet IR: `add 255, mod 256`.
* `,` takes one byte of input and sets *CP to that value -
  Piet IR: `input == in, mod 256`.

### Output operand: `.`

`.` writes *CP to `stdout` - Piet IR: `output == dup, out`.

### Control flow operands: `[` and `]`

For looping, the actual 2D layout of codels will be important.

For simplicity, we just discuss one possible code generation for `[code]`
when all IR blocks are rectangular and canonical. A more general treatment
is described in [IR block borders and layout](ir-block.md).

So far, all the IR instructions discussed expand as a linear sequence of codels,
so their IR blocks are trivially rectangular and canonical.
If we can show that there is always a canonical rectangular IR block for `[code]`,
assuming that there is a canonical rectangular IR block for `code`
(note: `code` is in Piet IR, not Brainfuck),
we will be done (proof by induction).

`[code]` keeps executing `code` until *CP is not zero - Piet IR:
  ```
  loop_until_zero $(code)
    == loop (dup, not, not, pointer (pass=exit, fail=$(code))) # fake IR
    ==
     in  | white | ..... | green |  red  | lgreen| dblue |lyellow|  out  |
   Mblack| white | Mblack| Mblack| Mblack| Mblack| Mblack| white | Mblack|
   Mblack|codeout|  ???  |  ???  |  ???  |  ???  | codein| white | Mblack|
   Mblack|  ???  |  ???  |  ???  |  ???  |  ???  |  ???  | black | Mblack|
         .       .       .       .       .       .       .       .       .
         .       .       .       .       .       .       .       .       .
   Mblack|codeend|  ???  |  ???  |  ???  |  ???  |codeend| Mblack| Mblack|
  ```

* `Mblack` (may be black) denotes codels which might need to be black
  to perform some combination of
  * preventing control flow from leaking into the outer IR block
  (`in` → `out` ↓ `Mblack`),
  - preventing control flow from leaking out of the inner IR block
  (`codeout` → `codein` ↓ `codeend`), and
  * turning control flow correctly before entering and after exiting the
  inner IR block.
* `code*` and `???` codels represent the `code` IR block.
* `.....` indicates white codels that might be needed to accommodate the
  width of the `code` IR block.
* `in` and `out` blocks are white.

P.S. The vague resemblance of Mblack with Mbappé is purely coincidental.

### End of program

Again, there are a lot of combinations of no-ops possible here,
so we just look at simple example. Piet IR:
```
eop ==
   in  | white | white | black | random|
 Mblack| black | white | black | black |
 black | white | white | white | black |
 Mblack| black | black | black | random|
```
`in` is white but may be of other colours so long as there the effective result
of `eop` is simply terminating the program (the stack is unchanged at the end).

## On beauty

How do we make the generated Piet code beautiful?
One possibility is to mix substitutions (see below) with probabilities for variety.
When an optimal flag (say `--opt`) is passed, only the shortest substitution is used.
Another possibility is that one give the transpiler certain rules to optimise aesthetics.
Replacing large numbers with smaller ones should make the code attractive, for example.
Depending on the value of an aesthetics flag, we can restrict the kinds of
transformations that the transpiler can perform.

A list of possible "first-order" variations could be like:
```
push 256 == push 2, dup, multiply (2), dup, multiply (4), dup, multiply (16)
         == push 1, dup, add (1), dup, multiply (2), dup, multiply (4), dup,
            multiply (16)
         == push 3, dup, multiply (3), dup, multiply (9), push 3, multiply (3),
            push 3, dup, push 1, add (1), multiply (4), add (12), push 1, add (1)
```

One could have "higher-order" variations where relations between consecutive
IR statements are exploited. For example, we use `dup` here instead of
simplifying `add 255, mod 256` to `push 255, add, push 256, mod`.
```
add 255, mod 256 == push 256, dup, push 3, push 1, roll, push 1, subtract (1),
                    add (256), push 2, push 1, roll, mod (256)
```
