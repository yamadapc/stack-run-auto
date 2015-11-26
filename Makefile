EXECS=file-modules extract-dependencies stack-run-auto

all: $(EXECS)
	cd stack-run-auto && cp `stack path --dist-dir`/build/module-package/module-package ../dist/module-package

$(EXECS): FORCE
	cd $@ && stack build && cp `stack path --dist-dir`/build/$@/$@ ../dist/$@

FORCE:
