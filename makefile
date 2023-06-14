source: lex.yy.o y.tab.o
	gcc -o source lex.yy.o y.tab.o -ll -lm

lex.yy.o: lex.yy.c y.tab.h
	gcc -c -g lex.yy.c

y.tab.o: y.tab.c y.tab.h
	gcc -c -g y.tab.c

lex.yy.c: source.l y.tab.h
	flex source.l

y.tab.h y.tab.c: source.y 
	bison -y -d source.y

clean:
	rm source *.o lex.yy.c y.tab.c

