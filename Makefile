.PHONY: test eastwood cljfmt kondo install deploy clean lint copy-sources-to-jdk javac javac-test
.DEFAULT_GOAL := install

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Ee

HOME=$(shell echo $$HOME)
CLOJURE_VERSION ?= 1.12

resources/clojuredocs/export.edn:
	curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# We need Java sources to test Java parsing functionality, but the Docker images
# we use on CircleCI doesn't include src.zip. So we have to download them from
# Github and repackage in a form that is resemblant to src.zip from normal
# distributions.

download-jdk-src:
	clojure -T:build download-jdk-src

javac:
	clojure -T:build javac

javac-test:
	clojure -T:build javac :with-tests true

test: download-jdk-src javac-test
	clojure -X:$(CLOJURE_VERSION):dev:test

# Having Clojurescript on classpath enables extra tests.
test-with-cljs: download-jdk-src javac-test
	clojure -X:$(CLOJURE_VERSION):dev:test:+cljs

eastwood: clean javac-test
	clojure -M:eastwood

cljfmt:
	clojure -M:cljfmt check

kondo:
	clojure -M:kondo

lint: kondo cljfmt eastwood

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env
	@if ! echo "$(CIRCLE_TAG)" | grep -q "^v"; then \
		echo "[Error] CIRCLE_TAG $(CIRCLE_TAG) must start with 'v'."; \
		exit 1; \
	fi
	export PROJECT_VERSION=$$(echo "$(CIRCLE_TAG)" | sed 's/^v//'); \
	# Clean is performed inside deploy task, no need to clean in Make
	clojure -T:build deploy :version "$PROJECT_VERSION"

# Usage: PROJECT_VERSION=99.99 make install
install: check-install-env clean
	clojure -T:build install :version '"$(PROJECT_VERSION)"'

clean:
	clojure -T:build clean

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

check-install-env:
ifndef PROJECT_VERSION
	$(error Please set PROJECT_VERSION as an env var beforehand.)
endif
