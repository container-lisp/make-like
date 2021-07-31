make-like: Makefile template.tar.gz
	buildapp --output make-like \
		--asdf-path `pwd`/.. \
		--asdf-tree ~/quicklisp/dists/quicklisp/software \
		--load-system make-like \
		--compress-core \
		--entry "make-like:main"

template.tar.gz: clean
	tar cvfz template.tar.gz template

clean:
	-rm -f make-like
	-find ./ -name *~ | xargs -n1 rm
