CC=gcc
LIBS=
CFLAGS=-O2 -Wall -std=gnu99

push-proposal: push-proposal.hs
	ghc push-proposal.hs

install: push-proposal
	install -m 4755 -o root push-proposal /usr/bin/

.o: $*.c
	$(CC) $(LIBS) $(CFLAGS) $< -o $%
