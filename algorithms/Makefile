FLAGS= -Wall -g
ALVOS= proj5
all: ${ALVOS}
%: %.c %.h
	gcc -o $@ $? ${FLAGS}
clean:
	rm -f *~ ${ALVOS}
