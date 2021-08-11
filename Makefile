.PHONY: test test-watch docs eastwood cljfmt release deploy clean

VERSION ?= 1.10

TEST_PROFILES ?= +test

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

test:
	lein with-profile +$(VERSION),$(TEST_PROFILES) test

test-watch: test-resources/clojuredocs/export.edn
	lein with-profile +$(VERSION),$(TEST_PROFILES) test-refresh

eastwood:
	lein with-profile +$(VERSION),+eastwood,$(TEST_PROFILES) eastwood

cljfmt:
	lein with-profile +$(VERSION),+cljfmt cljfmt check

kondo:
	lein with-profile -dev,+clj-kondo run -m clj-kondo.main --lint src test

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
