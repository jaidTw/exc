all:
	lex exc.l
	yacc -Wall -d --debug exc.y
	g++ -Wall -g -O3 *.cpp *.c -lfl -ly -o exc
	rm y.tab.c y.tab.h lex.yy.c
