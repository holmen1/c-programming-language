calculator: getch.o getop.o stack.o main.o
	gcc -std=c90 -Wall -o calculator getch.o getop.o stack.o main.o

getch.o: getch.c
	gcc -std=c90 -Wall -c getch.c -o getch.o

getop.o: getop.c
	gcc -std=c90 -Wall -c getop.c -o getop.o

stack.o: stack.c
	gcc -std=c90 -Wall -c stack.c -o stack.o

main.o: main.c
	gcc -std=c90 -Wall -c main.c -o main.o

clean:
	rm -f *.o calculator

.PHONY: clean