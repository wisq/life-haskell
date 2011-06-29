all: test

test:
	echo main | ghci test.hs | grep -A9999 'ghci>'
