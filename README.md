# A haskell Sudoku Solver using MiniSAT

This is a simple sudoku solver implementing in Haskell by calling MiniSAT.

#### How to build

Please install the build tool **Stack**: (https://docs.haskellstack.org) and make sure you have the MiniSAT binary in you PATH.

```bash
~.../sudoku$ stack build
~.../sudoku$ stack install
~.../sudoku$ sdks -h
```

#### What is designed

I would like to talk about some important modules which build the CNF.

 * **Proposition.Core** ---- *data structures and basic operation on fomulas*
 * **Proposition.NormalForm** ---- *NNF, CNF and DIMACS transformation*
 * **Proposition.Utils**  ---- *conversion of polish notation to infix noation*
 * **Proposition.Parser** ---- *ascii form formula parser*
 * **proposition.MSatPort** ----  *callee for minisat as child process* 
 * **proposition.MsatPaeser** ---- *output from minisat parser*

The overall constraints generations is in the file Solver.hs.
We have to get five parts of constraints, as a example, let's take a look at row constraints.

Given 4x4 sudoku problem:

x11|x12|x13|x14
-|-|-|-
x21|x22|x23|x24
x31|x32|x33|x34
x41|x42|x43|x44

we use x111 to express when the x11=1,
similarly, x424 means x42=4.
For the first line, to express there must have one 1 in this line we have:

(¬ x111 ∨ ¬x121) ∧ (¬ x111 ∨ ¬x131) ∧ (¬ x111 ∨ ¬x141)  

∧ (¬ x121 ∨ ¬x131)  ∧ (¬ x121 ∨ ¬x141)

∧ (¬ x141 ∨ ¬x151)

and the other constraints is similar to this.

### What about some tests
In this section, we show some tests. It should be mentioned that the form of input file must be like: 

```test
(
0 1 3 0
2 0 0 0
0 0 0 3
0 2 1 0
)
```
which means a 4x4 sudoku problem and 0 means this block is to be filled.

#### 4x4 blocks 

The input file is 4_1.txt and the result is: 
```test
(r1,c1):4
(r1,c2):1
(r1,c3):3
(r1,c4):2
(r2,c1):2
(r2,c2):3
(r2,c3):4
(r2,c4):1
(r3,c1):1
(r3,c2):4
(r3,c3):2
(r3,c4):3
(r4,c1):3
(r4,c2):2
(r4,c3):1
(r4,c4):4
```
which (r1,c1) means row 1 column 1.

#### 9x9 blocks
The input file is 9_1.txt and the result is:

```text
(r1,c1):3
(r1,c2):9
(r1,c3):7
(r1,c4):1
(r1,c5):8
(r1,c6):4
(r1,c7):5
(r1,c8):2
(r1,c9):6
(r2,c1):4
(r2,c2):8
(r2,c3):1
(r2,c4):6
(r2,c5):2
(r2,c6):5
(r2,c7):7
(r2,c8):9
(r2,c9):3
(r3,c1):2
(r3,c2):5
(r3,c3):6
(r3,c4):9
(r3,c5):7
(r3,c6):3
(r3,c7):8
(r3,c8):4
(r3,c9):1
(r4,c1):6
(r4,c2):7
(r4,c3):4
(r4,c4):5
(r4,c5):3
(r4,c6):2
(r4,c7):9
(r4,c8):1
(r4,c9):8
(r5,c1):1
(r5,c2):2
(r5,c3):9
(r5,c4):8
(r5,c5):4
(r5,c6):6
(r5,c7):3
(r5,c8):5
(r5,c9):7
(r6,c1):8
(r6,c2):3
(r6,c3):5
(r6,c4):7
(r6,c5):9
(r6,c6):1
(r6,c7):2
(r6,c8):6
(r6,c9):4
(r7,c1):5
(r7,c2):4
(r7,c3):2
(r7,c4):3
(r7,c5):1
(r7,c6):8
(r7,c7):6
(r7,c8):7
(r7,c9):9
(r8,c1):7
(r8,c2):6
(r8,c3):3
(r8,c4):4
(r8,c5):5
(r8,c6):9
(r8,c7):1
(r8,c8):8
(r8,c9):2
(r9,c1):9
(r9,c2):1
(r9,c3):8
(r9,c4):2
(r9,c5):6
(r9,c6):7
(r9,c7):4
(r9,c8):3
(r9,c9):5
```
which (r1,c1) means row 1 column 1.

### Note
* The input file must end with unix-like EOF '\n'.
* You can prepare your own problem set like the form in the example directory to check.
* I compiled a Windows version of minist (NOT under cygwin), so feel free to use.
