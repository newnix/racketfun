.POSIX:

include ../Makefile

TARGETS += ch2 ch2-bin ch2-profile ch2-xtime
BINDIR = ./bin

ch2: ch2${SOURCEXT}
	@racket $?

ch2-bin: ch2${SOURCEXT}
	@echo "Turning $? into a binary"
	@$(CC) exe -o ${BINDIR}/ch2 $?

ch2-profile: ch2${SOURCEXT}
	@$(CC) profile $?

ch2-xtime: ch2-bin
	@time racket ch2${SOURCEXT}
	@echo "Now the binary version"
	@time ${BINDIR}/ch2
