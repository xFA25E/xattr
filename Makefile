CC = gcc
LD = gcc
CFLAGS = -Wall -Werror
LDFLAGS =

all: xattr-core.so

clean:
	[ -e xattr-core.so ] && rm xattr-core.so || true

test: xattr-test.el xattr.el xattr-core.so
	emacs -Q -module-assertions -batch -L . -l xattr-test.el -f ert-run-tests-batch-and-exit

%.so: %.o
	$(LD) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -I$(EMACS_SRC) -fPIC -c $<
