#!/bin/bash
gcc -o linked_list linked_list.h
gcc -o symbol_table symbol_table.h
gcc -o code_generation code_generation.h
yacc -d -v ada1.y
lex ada.l
/usr/bin/gcc lex.yy.c y.tab.c -o ada -ll -ly
rm -f lex.yy.c y.tab.c linked_list symbol_table code_generation y.output y.tab.h
