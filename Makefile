.PHONY: test test-watch docs eastwood cljfmt release deploy clean

VERSION ?= 1.10

TEST_PROFILES ?= +test

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

test:
	lein with-profile -user,-dev,+$(VERSION),$(TEST_PROFILES) test

test-watch: test-resources/clojuredocs/export.edn
	lein with-profile +$(VERSION),$(TEST_PROFILES) test-refresh

eastwood:
	lein with-profile -user,-dev,+$(VERSION),+eastwood,$(TEST_PROFILES) eastwood

cljfmt:
	lein with-profile -user,-dev,+$(VERSION),+cljfmt cljfmt check

kondo:
	lein with-profile -user,-dev,+clj-kondo run -m clj-kondo.main --lint src test src-jdk8 src-newer-jdks

# When releasing, the BUMP variable controls which field in the
# version string will be incremented in the *next* snapshot
# version. Typically this is either "major", "minor", or "patch".

BUMP ?= patch

release: clean
	lein with-profile -user,-dev,+$(VERSION) release $(BUMP)

# Deploying requires the caller to set environment variables as
# specified in project.clj to provide a login and password to the
# artifact repository.

deploy: clean
	lein with-profile -user,-dev,+$(VERSION) deploy clojars

install: clean
	lein with-profile -user,-dev,+$(VERSION) install

clean:
	lein with-profile -user,-dev clean
