.PHONY: all clean

all: ../ebin	
	erlc -I ../include -o ../ebin ./*.erl

../ebin:
	mkdir ../ebin

clean:
	rm ../ebin/*.beam
