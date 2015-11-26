EXECS=file-modules extract-dependencies stack-run-auto

all: $(EXECS)

$(EXECS): FORCE
	cd $@ && stack build && cp `stack path --dist-dir`/build/$@/$@ ../dist/$@

FORCE:
