.PHONY: clean test eastwood cljfmt cloverage

VERSION ?= 1.9

# Some tests need to be filtered based on JVM version.  This selector
# will be mapped to a function in project.clj, and that function
# determines which `deftest` to run based on their metadata.
TEST_SELECTOR = :java$(shell lein version | cut -d " " -f 5 | cut -d "." -f 1-2)

test:
	lein with-profile +$(VERSION) test $(TEST_SELECTOR)

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
