# A haskell Sudoku Solver using MiniSAT

This is a simple sudoku solver implementing in Haskell by calling MiniSAT.

#### How to build

Please install the build tool **Stack**: (https://docs.haskellstack.org) and make sure you have the MiniSAT binary in you PATH.

```bash
~.../sudoku$ stack build
~.../sudoku$ stack install
~.../sudoku$ sudoku -h
```

#### What is designed

I would like to talk about some important modules which build the CNF.

 * **Proposition.Core** ---- *data structures and basic operation on formulas*
 * **Proposition.NormalForm** ---- *NNF, CNF and DIMACS transformation*
 * **Proposition.Utils**  ---- *conversion of polish notation to infix notation*
 * **Proposition.Parser** ---- *ascii form formula parser*
 * **Proposition.MSatPort** ----  *callee for minisat as child process* 
 * **Proposition.MsatParser** ---- *output from minisat parser*

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
4 1 3 2
2 3 4 1
1 4 2 3
3 2 1 4
```
which (r1,c1) means row 1 column 1.

#### 9x9 blocks
The input file is 9_1.txt and the result is:

```text
3 9 7 1 8 4 5 2 6
4 8 1 6 2 5 7 9 3
2 5 6 9 7 3 8 4 1
6 7 4 5 3 2 9 1 8
1 2 9 8 4 6 3 5 7
8 3 5 7 9 1 2 6 4
5 4 2 3 1 8 6 7 9
7 6 3 4 5 9 1 8 2
9 1 8 2 6 7 4 3 5
```

### Note
* The input file must end with unix-like EOF '\n'.
* You can prepare your own problem set like the form in the example directory to check.
* I compiled a Windows version of minisat (NOT under cygwin), so feel free to use.
