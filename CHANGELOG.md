# Changelog

## master (unreleased)

* [#38](https://github.com/clojure-emacs/orchard/pull/38): [Inspector] Add eval
  and def features to inspector.

## 0.3.4 (2018-12-29)

### Changes

* [#35](https://github.com/clojure-emacs/orchard/pull/35): [Inspector] Render Java's lists, maps, and arrays as collections.
* [#35](https://github.com/clojure-emacs/orchard/pull/35): [Inspector] Truncate long inline values.
* Ensure all classpath entries have absolute paths.

## 0.3.3 (2018-10-20)

### Bugs fixed

* [#33](https://github.com/clojure-emacs/orchard/pull/33): Address a NPE in `class-info` when dealing with classes created via `reify`.

## 0.3.2 (2018-10-14)

### Bugs fixed

* Figure out the package of classes defined with `deftype` and `defrecord` on Java 8.

## 0.3.1 (2018-09-15)

### Bugs fixed

* [#28](https://github.com/clojure-emacs/orchard/pull/28): Fix inspector dropping items after nil value.

### 0.3.0 (2018-06-16)

### New features

* Extracted info and eldoc functionality from `cider-nrepl` into `orchard.info` and `orchard.eldoc`.

## 0.2.0 (2018-05-07)

### New features

* Added a generic API for querying namespaces and vars (`orchard.query`).

## 0.1.0 (2018-05-04)

### New features

* Initial extraction of nREPL-agnostic functionality from `cider-nrepl`.

### Changes

* [#24](https://github.com/clojure-emacs/orchard/pull/24): [Inspector] Separately clickable keys and values when inspecting maps.
* [#24](https://github.com/clojure-emacs/orchard/pull/24): [Inspector] Remember page numbers for each nesting level.
