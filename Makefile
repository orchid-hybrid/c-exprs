all:
	echo "nothing to do"

test:
	raco test testing/testing.rkt
	raco test tools/test-pattern-matcher.rkt
	raco test languages/c/test-c.rkt
