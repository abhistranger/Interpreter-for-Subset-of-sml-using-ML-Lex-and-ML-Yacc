# Lexer-and-Parser-for-Boolean-Algebra-using-ML-Lex-and-ML-Yacc
To run the program:
1. Put all files in one directory
2. open terminal in the current directory
# To Compile
```bash
make
```
or
```bash
make all
```
It will open the sml interactive terminal
# For lexing and persing of file or string
```bash
parseFile <file_name>;
```
Note : file name should be written as a string. for example if the file is input.txt then the command will be parseFile "input.txt"

If you want to run for a string then use this:
```bash
parseString <string>;
```
# For type checking:
```bash
open Typing;
```
```bash
parseFile <file_name>;
```
```bash
getType (it,[]);
```
# For evaluating:
```bash
open Evaluator;
```
```bash
parseFile <file_name>;
```
```bash
getType (it,[]);
```
