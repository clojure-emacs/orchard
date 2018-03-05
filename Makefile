.PHONY: test docs eastwood cljfmt cloverage release deploy clean

VERSION ?= 1.9

# Some tests need to be filtered based on JVM version.  This selector
# will be mapped to a function in project.clj, and that function
# determines which `deftest` to run based on their metadata.
JAVA_VERSION := $(shell lein with-profile +sysutils \
                        sysutils :java-version-simple | cut -d " " -f 2)
TEST_SELECTOR := :java$(JAVA_VERSION)

test:
	lein with-profile +$(VERSION) test $(TEST_SELECTOR)

# Documentation management via autodoc (https://github.com/plexus/autodoc)
# Pin a specific commit in that repo to prevent accidental changes in
# that upstream project breaking our CI. Periodically update the URL
# below as needed.

autodoc.sh:
	curl -L https://raw.githubusercontent.com/plexus/autodoc/ffc2c72c/autodoc.sh -o $@
	chmod +x $@

docs: autodoc.sh
	@if [[ "$(TRAVIS)" == "true" && "$(TRAVIS_PULL_REQUEST)" == "false" ]]; then \
	    git remote set-url --push \
	        origin https://$(GH_USER):$(GH_PASSWORD)@github.com/clojure-emacs/orchard.git; \
	fi
	AUTODOC_SUBDIR=`git rev-parse --abbrev-ref HEAD` \
	AUTODOC_CMD="lein with-profile +$(VERSION),+codox codox" ./autodoc.sh

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

# When releasing, the BUMP variable controls which field in the
# version string will be incremented in the *next* snapshot
# version. Typically this is either "major", "minor", or "patch".

BUMP ?= patch

release:
	lein with-profile +$(VERSION) release $(BUMP)

# Deploying requires the caller to set environment variables as
# specified in project.clj to provide a login and password to the
# artifact repository.

deploy:
	lein with-profile +$(VERSION) deploy clojars

clean:
	lein clean
	rm -rf gh-pages autodoc.sh
