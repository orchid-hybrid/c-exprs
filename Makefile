all:
	echo "nothing to do"

test:
	raco test testing/testing.rkt
	raco test languages/test-c.rkt
