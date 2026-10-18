.PHONY: test test-with-cljs eastwood cljfmt kondo install deploy clean lint download-jdk-src javac javac-test check-env
.DEFAULT_GOAL := install

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Ee

CLOJURE ?= clojure
CLOJURE_VERSION ?= 1.12
# The version local installs get. Releases take theirs from the git tag.
PROJECT_VERSION ?= 99.99

resources/clojuredocs/export.edn:
	curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# We need Java sources to test Java parsing functionality, but the Docker images
# we use on CircleCI doesn't include src.zip. So we have to download them from
# Github and repackage in a form that is resemblant to src.zip from normal
# distributions.

download-jdk-src:
	$(CLOJURE) -T:build download-jdk-src

javac:
	$(CLOJURE) -T:build javac

javac-test:
	$(CLOJURE) -T:build javac :with-tests true

test: download-jdk-src javac-test
	$(CLOJURE) -X:$(CLOJURE_VERSION):dev:test

# Having Clojurescript on classpath enables extra tests.
test-with-cljs: download-jdk-src javac-test
	$(CLOJURE) -X:$(CLOJURE_VERSION):dev:test:+cljs

eastwood: javac-test
	$(CLOJURE) -M:eastwood

cljfmt:
	$(CLOJURE) -M:cljfmt check

kondo:
	$(CLOJURE) -M:kondo

lint: kondo cljfmt eastwood

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env
	@if ! echo "$(CIRCLE_TAG)" | grep -q "^v"; then \
		echo "[Error] CIRCLE_TAG $(CIRCLE_TAG) must start with 'v'."; \
		exit 1; \
	fi
	# Clean is performed inside deploy task, no need to clean in Make
	export PROJECT_VERSION=$$(echo "$(CIRCLE_TAG)" | sed 's/^v//'); \
	$(CLOJURE) -T:build deploy :version "\"$$PROJECT_VERSION\""

# Usage: make install (or PROJECT_VERSION=1.2.3 make install)
install:
	$(CLOJURE) -T:build install :version '"$(PROJECT_VERSION)"'

clean:
	$(CLOJURE) -T:build clean

check-env:
ifndef CLOJARS_USERNAME
	$(error CLOJARS_USERNAME is undefined)
endif
ifndef CLOJARS_PASSWORD
	$(error CLOJARS_PASSWORD is undefined)
endif
ifndef CIRCLE_TAG
	$(error CIRCLE_TAG is undefined. Please only perform deployments by publishing git tags. CI will do the rest.)
endif
