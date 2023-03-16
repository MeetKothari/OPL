CC = g++
CFLAGS = -Wall -std=c++11 -pedantic -g
OBJS = test.o

test: $(OBJS)
	$(CC) $(CFLAGS) -o test $(OBJS)
test.o: test.cpp
	$(CC) $(CFLAGS) -c test.cpp -o test.o
clean:
	rm -f test $(OBJS)