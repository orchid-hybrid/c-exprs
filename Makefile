all: cexpr

cexpr:
	csc -prologue c-expr.scm run-c-expr.scm -o cexpr
