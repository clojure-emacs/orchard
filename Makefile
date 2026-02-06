.PHONY: test eastwood cljfmt kondo install deploy clean lint copy-sources-to-jdk
.DEFAULT_GOAL := install

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Ee

HOME=$(shell echo $$HOME)
CLOJURE_VERSION ?= 1.12
TEST_PROFILES ?= "-user,-dev,+test"

resources/clojuredocs/export.edn:
curl -o $@ https://github.com/clojure-emacs/clojuredocs-export-edn/raw/master/exports/export.compact.edn

# We need Java sources to test Java parsing functionality, but the Docker images
# we use on CircleCI doesn't include src.zip. So we have to download them from
# Github and repackage in a form that is resemblant to src.zip from normal
# distributions.

base-src-jdk8.zip:
	# echo 'Placeholder. We dont parse sources on JDK8.'
	touch $@

base-src-jdk11.zip:
	bash .circleci/download-jdk-sources.sh https://github.com/adoptium/jdk11u/archive/refs/tags/jdk-11.0.28+0.zip jdk11 $@

base-src-jdk17.zip:
	bash .circleci/download-jdk-sources.sh https://github.com/adoptium/jdk17u/archive/refs/tags/jdk-17.0.15+5.zip jdk17 $@

base-src-jdk21.zip:
	bash .circleci/download-jdk-sources.sh https://github.com/adoptium/jdk21u/archive/refs/tags/jdk-21.0.7+5.zip jdk21 $@

base-src-jdk25.zip:
	bash .circleci/download-jdk-sources.sh https://github.com/adoptium/jdk/archive/refs/tags/jdk-25+36.zip jdk25 $@

copy-sources-to-jdk: base-src-$(JDK_SRC_VERSION).zip
	mkdir -p $(JAVA_HOME)/lib && cp base-src-$(JDK_SRC_VERSION).zip $(JAVA_HOME)/lib/src.zip

# Placeholder job for When JDK_SRC_VERSION is unset.
base-src-.zip:
	echo 'JDK_SRC_VERSION is unset.'

test: copy-sources-to-jdk clean
	lein with-profile $(TEST_PROFILES),+$(CLOJURE_VERSION) test

# Having Clojurescript on classpath enables extra tests.
test-with-cljs: copy-sources-to-jdk
	lein with-profile $(TEST_PROFILES),+$(CLOJURE_VERSION),+cljs test

eastwood:
	lein with-profile $(TEST_PROFILES),+$(CLOJURE_VERSION),+eastwood eastwood

cljfmt:
	lein with-profile -user,-dev,+$(CLOJURE_VERSION),+cljfmt cljfmt check

kondo: clean
	lein with-profile -dev,+test,+clj-kondo clj-kondo

lint: kondo cljfmt eastwood

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env clean
	@if ! echo "$(CIRCLE_TAG)" | grep -q "^v"; then \
		echo "[Error] CIRCLE_TAG $(CIRCLE_TAG) must start with 'v'."; \
		exit 1; \
	fi
	export PROJECT_VERSION=$$(echo "$(CIRCLE_TAG)" | sed 's/^v//'); \
	lein with-profile -user,-dev,+$(CLOJURE_VERSION),-provided deploy clojars

# Usage: PROJECT_VERSION=99.99 make install
install: clean check-install-env
	lein with-profile -user,-dev,+$(CLOJURE_VERSION),-provided install

clean:
	lein with-profile -user,-dev clean

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
