.POSIX:

## This is probably not going to be entirely useful, but 
## at the very least will allow simplified DVCS invocations

.SUFFIXES: .rkt
DVCS = fossil
REPONAME = racketfun
## Technically not accurate, but close enough for anything I
## might want to compile to a binary
CC = raco
RACKET = racket

SOURCEXT = .rkt

.rkt:
	@$(RACKET) $<

## Make targets
TARGETS = commit status push diff help

commit:
	@${DVCS} commit

status:
	@${DVCS} status

push:
	@gitsync -r ${REPONAME} -n master

diff:
	@${DVCS} diff

help:
	@echo "Valid targets; ${TARGETS}"
	@echo -e "\thelp: This help message"
	@echo -e "\tcommit: Run ${DVCS} commit"
	@echo -e "\tstatus: Run ${DVCS} status"
	@echo -e "\tpush: Run gitsync -r ${REPONAME} -n master"
	@echo -e "\tdiff: Run ${DVCS} diff"

%:
