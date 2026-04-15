.PHONY: test eastwood cljfmt kondo install deploy clean lint copy-sources-to-jdk download-sources compile-java compile-test-java
.DEFAULT_GOAL := install

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Ee

HOME=$(shell echo $$HOME)
CLOJURE_VERSION ?= 1.12

# Map short Clojure versions to full versions (for sources JAR lookup)
CLOJURE_VERSION_FULL_1.10 = 1.10.3
CLOJURE_VERSION_FULL_1.11 = 1.11.4
CLOJURE_VERSION_FULL_1.12 = 1.12.4
CLOJURE_VERSION_FULL = $(CLOJURE_VERSION_FULL_$(CLOJURE_VERSION))

# The Clojure sources JAR is needed on the classpath for certain tests.
# tools.deps doesn't support Maven classifiers, so we download it manually.
SOURCES_JAR = $(HOME)/.m2/repository/org/clojure/clojure/$(CLOJURE_VERSION_FULL)/clojure-$(CLOJURE_VERSION_FULL)-sources.jar

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

# Placeholder job for when JDK_SRC_VERSION is unset.
base-src-.zip:
	echo 'JDK_SRC_VERSION is unset.'

download-sources:
	@if [ ! -f "$(SOURCES_JAR)" ]; then \
		mkdir -p "$$(dirname "$(SOURCES_JAR)")"; \
		curl -sL -o "$(SOURCES_JAR)" \
		  "https://repo1.maven.org/maven2/org/clojure/clojure/$(CLOJURE_VERSION_FULL)/clojure-$(CLOJURE_VERSION_FULL)-sources.jar"; \
	fi

compile-java:
	clojure -T:build compile-java

compile-test-java:
	clojure -T:build compile-test-java

test: copy-sources-to-jdk clean compile-java compile-test-java download-sources
	clojure -Sdeps '{:deps {org.clojure/clojure-sources {:local/root "$(SOURCES_JAR)"}}}' \
	  -M:test:$(CLOJURE_VERSION)

# Having Clojurescript on classpath enables extra tests.
test-with-cljs: copy-sources-to-jdk compile-java compile-test-java download-sources
	clojure -Sdeps '{:deps {org.clojure/clojure-sources {:local/root "$(SOURCES_JAR)"}}}' \
	  -M:test:cljs:$(CLOJURE_VERSION)

eastwood: compile-java compile-test-java
	clojure -M:test:$(CLOJURE_VERSION):eastwood

cljfmt:
	clojure -M:$(CLOJURE_VERSION):cljfmt check

kondo: clean compile-java
	clojure -M:test:clj-kondo

lint: kondo cljfmt eastwood

# Deployment is performed via CI by creating a git tag prefixed with "v".
# Please do not deploy locally as it skips various measures.
deploy: check-env clean
	@if ! echo "$(CIRCLE_TAG)" | grep -q "^v"; then \
		echo "[Error] CIRCLE_TAG $(CIRCLE_TAG) must start with 'v'."; \
		exit 1; \
	fi
	export PROJECT_VERSION=$$(echo "$(CIRCLE_TAG)" | sed 's/^v//'); \
	clojure -T:build deploy

# Usage: PROJECT_VERSION=99.99 make install
install: clean check-install-env
	clojure -T:build install

clean:
	rm -rf target

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
