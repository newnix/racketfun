.POSIX:

include ../Makefile

CH2 = ch2 ch2-bin ch2-profile ch2-xtime
CH3 = ch3 ch3-bin ch3-profile ch3-xtime

TARGETS += ${CH2} ${CH3}
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

ch3: ch3${SOURCEXT}
	@racket $?

ch3-bin: ch3${SOURCEXT}
	@echo "Turning $? into a binary"
	@$(CC) exe -o ${BINDIR}/ch3 $?

ch3-profile: ch3${SOURCEXT}
	@$(CC) profile $?

ch3-xtime: ch3-bin
	@time racket ch3${SOURCEXT}
	@echo "Now the binary version"
	@time ${BINDIR}/ch3
