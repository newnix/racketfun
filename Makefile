.POSIX:

## This is probably not going to be entirely useful, but 
## at the very least will allow simplified DVCS invocations

DVCS = fossil
REPONAME = racketfun
## Technically not accurate, but close enough for anything I
## might want to compile to a binary
CC = raco

commit:
	@${DVCS} commit

status:
	@${DVCS} status

push:
	@gitsync -r ${REPONAME} -n master

diff:
	@${DVCS} diff
