about TINY
==========

More details in the book [Compiler Construction](http://www.cs.sjsu.edu/~louden/cmptext/).
An simple implementation in C was provided by the author.

### Lexical

```
Reserved Words: if, then, else, end, repeat, until, read, write
Sepcial Symbols: +, -, *, /, =, <, (, ), ;, :=
number: 1 or more digits
identifier: 1 or more letters
```
- The code is free format.
- Comments are enclosed in curly brackets.

### Syntax

BNF grammar:

```
program -> stmt-sequence
stmt-sequence -> stmt-sequence ; statement | statement
statement -> if-stmt | repeat-stmt | assign-stmt | read-stmt | write-stmt

if-stmt -> if exp then stmt-sequence end
         | if exp then stmt-sequence else stmt-sequence end
repeat-stmt -> repeat stmt-sequence until exp
assign-stmt -> identifier := exp
read-stmt -> read identifier
write-stmt -> write exp

exp -> simple-exp comparison-op simple-exp | simple-exp
comparison-op -> < | =
simple-exp -> simple-exp addop term | trem
addop -> + | -
term -> trem mulop factor | factor
mulop -> * | /
factor -> ( exp ) | number | identifier
```

### Semantic

- No procedures or functions.
- No explicitd declarations.
- Only two simple types: integer and boolean.
- All variables are integer variables.

### Virtual Machine

- Tiny Machine has some of the properties of RISC.
- TM has two basic instruction formats: register-only, or RO instructions, and register-memory, or RM instructions.
- Code form: three-address.
- Address mode: register + offset.

RO Instructions:

```
format: opcode r, s, t

opcode  effect

HALT    stop execution (operands ignored)
IN      reg[r] <- integer value read from the standard input (s and t ignored)
OUT     reg[r] <- the standard output (s and t ignored)
ADD     reg[r] = reg[s] + reg[t]
SUB     reg[r] = reg[s] - reg[t]
MUL     reg[r] = reg[s] * reg[t]
DIV     reg[r] = reg[s] / reg[t] (may generate ZERO_DIV)
```

RM Instructions:

```
format: opcode r, d(s)

(a = d + reg[s]; any reference to dMem[a] generates DMEM_ERR if a < 0 or a >= DADDR_SIZE)

opcode  effect

LD      reg[r] = dMem[a] (load r with memory value at a)
LDA     reg[r] = a       (load address a directly into r)
LDC     reg[r] = d       (load constant d directly into r - s is ignored)
ST      dMem[a] = reg[r] (store value in r to memory location a)
JLT     if (reg[r] <  0) reg[PC_REG] = a
        (jump to instruction a if r is negative, similarly for the following)
JLE     if (reg[r] <= 0) reg[PC_REG] = a
JGE     if (reg[r] >= 0) reg[PC_REG] = a
JGT     if (reg[r] >  0) reg[PC_REG] = a
JEQ     if (reg[r] == 0) reg[PC_REG] = a
JNE     if (reg[r] != 0) reg[PC_REG] = a
```

- TM consists of a read-only instruction memory, a data memory, and a set of eight general-purpose registers.
- TM has build-in IO facilities for integers.
- TM detects three possible runtime errors: IMEM_ERR, DMEM_ERR and ZERO_DIV.
- TM reads the assembly code directly from a source file.

Simulator commands:

```
s(tep) <n>      Execute n (default 1) TM instructions
g(o)            Execute TM instructions until HALT
r(egs)          Print the contents of the registers
i(Mem) <b <n>>  Print n iMem locations starting at b
d(Mem) <b <n>>  Print n dMem locations starting at b
t(race)         Toggle instruction trace
p(rint)         Toggle print of total instructions executed ('go' only)
c(lear)         Reset simulator for new execution of program
h(elp)          Cause this list of commands to be printed
q(uit)          Terminate the simulation
```

### Sample Program

```
{ Sample program
  in TINY language -
  computes factorial
}
read x; { input an integer }
if 0 < x then { don't compute if x <= 0 }
  fact := 1;
  repeat
    fact := fact * x;
    x := x - 1
  until x = 0;
  write fact  { output factorial of x }
end
```

