.PHONY: clean test eastwood cljfmt cloverage

VERSION ?= 1.9

test:
	lein with-profile +$(VERSION) test

# Eastwood can't handle orchard.java.parser at the moment, because
# tools.jar isn't in the classpath when Eastwood runs.

eastwood:
	lein with-profile +$(VERSION),+eastwood eastwood \
	     "{:exclude-namespaces [orchard.java.parser]}"

cljfmt:
	lein with-profile +$(VERSION),+cljfmt cljfmt check

# Cloverage can't handle some of the code in this project.  For now we
# must filter problematic namespaces (`-e`) and tests (`-t`) from
# instrumentation. Note: this means for now coverage reporting isn't
# exact. See issue cider-nrepl/#457 for background.

cloverage:
	lein with-profile +$(VERSION),+cloverage cloverage --codecov \
	     -e "orchard.java.parser"

clean:
	lein clean
	rm .source-deps
