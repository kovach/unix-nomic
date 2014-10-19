CC=gcc
LIBS=
CFLAGS=-O2 -Wall -std=gnu99

push-proposal: push-proposal.hs
	ghc push-proposal.hs

propose: propose.hs
	ghc propose.hs

install: push-proposal propose
	install -m 4755 -o root push-proposal /usr/bin/
	install -m 755 -o root propose /usr/bin/

.o: $*.c
	$(CC) $(LIBS) $(CFLAGS) $< -o $%
