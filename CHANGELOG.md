# Changelog

## master (unreleased)

* The _class info cache_ is now initialized silently by default. This results in less confusing output.
  * You can now `@orchard.java/cache-initializer` for deterministically waiting for this cache workload to complete.
  * You can control its verbosity by setting `"-Dorchard.initialize-cache.silent=false"` (or `[...]=true`).

## 0.6.5 (2021-02-13)

### Bugs Fixed

* [#106](https://github.com/clojure-emacs/orchard/issues/106): Handle properly scoped cljs macros in info.
* Further improve dynamic classpath modification robustness.

## 0.6.4 (2021-01-25)

### Bugs Fixed

* Make dynamic classpath modification robust to compiler loader binding (for some users `@Compiler/LOADER` was not bound for some reason).

## 0.6.3 (2021-01-21)

### Bugs Fixed

* [#103](https://github.com/clojure-emacs/orchard/issues/103): Ensure JDK sources is visible in any invoking classloader context.

## 0.6.2 (2021-01-04)

### Bugs Fixed

* [#100](https://github.com/clojure-emacs/orchard/issues/100): Fix compatibility with Java 15.

### Changes

* Updated the ClojureDocs EDN export URL (see <https://github.com/clojure-emacs/clojuredocs-export-edn/issues/3>).

## 0.6.1 (2020-10-07)

* Remove reflection warnings.

## 0.6.0 (2020-07-14)

### Changes

* [#96](https://github.com/clojure-emacs/orchard/pull/96): Bundle exported ClojureDocs data and make fetching updates explicit (it used to happen automatically).

## 0.5.11 (2020-07-10)

### Bugs Fixed

* [#95](https://github.com/clojure-emacs/orchard/pull/95): Fix large number of open files from java parser.
* [#91](https://github.com/clojure-emacs/orchard/issues/91): Don't use composite profiles for provided deps. (turned out that's not supported by Leiningen)

## 0.5.10 (2020-06-03)

### Bugs fixed

* [#91](https://github.com/clojure-emacs/orchard/issues/91): Fix broken release caused by some bug in Lein 2.9.3.

## 0.5.9 (2020-05-30)

### Bugs fixed

* [#86](https://github.com/clojure-emacs/orchard/issues/86): Fix resolution order in `info`.

## 0.5.8 (2020-04-14)

### Bugs fixed

* [#84](https://github.com/clojure-emacs/orchard/pull/84): Fix javadoc urls.

## 0.5.7 (2020-03-03)

### Bugs fixed

* [#83](https://github.com/clojure-emacs/orchard/pull/83): Ignore non file URLs when checking for directory or file extensions.

## 0.5.6 (2020-02-14)

### Bugs fixed

* [#82](https://github.com/clojure-emacs/orchard/pull/82/): Correctly parse Java version strings that contain $OPT segments after the version numbers

## 0.5.5 (2019-12-30)

### Bugs fixed

* [#81](https://github.com/clojure-emacs/orchard/issues/81): [Inspector] Handle InaccessibleObjectException on Java 9+.
* [#80](https://github.com/clojure-emacs/orchard/issues/80): [Inspector] Render nils in data structures.

## 0.5.4 (2019-11-05)

### Bugs fixed

* [#75](https://github.com/clojure-emacs/orchard/issues/75): Fix bug with no `:file` for `info` on namespaces without any `def`s.

## 0.5.3 (2019-10-08)

### Bugs fixed

* [#74](https://github.com/clojure-emacs/orchard/pull/74): Don't check for internet connectivity for local ClojureDocs cache URLs.

## 0.5.2 (2019-10-02)

### Bugs fixed

* [#72](https://github.com/clojure-emacs/orchard/pull/72): Check for internet connectivity before downloading ClojureDocs exported file.

## 0.5.1 (2019-09-11)

### Bugs fixed

* [#69](https://github.com/clojure-emacs/orchard/pull/69): Include specs in `var-meta`.

## 0.5.0 (2019-08-29)

### New features

* [#42](https://github.com/clojure-emacs/orchard/pull/42): Add namespace alias support for `spec-list` and `spec-form`.
* [#46](https://github.com/clojure-emacs/orchard/pull/46): [Inspector] Separate static from non-static fields when rendering raw objects.
* [#46](https://github.com/clojure-emacs/orchard/pull/46): [Inspector] Show fields inherited from superclasses when rendering raw objects.
* [#47](https://github.com/clojure-emacs/orchard/pull/46): [Java] Cache class-info for editable Java classes.
* [#51](https://github.com/clojure-emacs/orchard/issues/51): Add basic xref functionality in `orchard.xref`.
* [#64](https://github.com/clojure-emacs/orchard/issues/64): Port `cache-dir` from [soc/directories-jvm](https://github.com/soc/directories-jvm) to `orchard.util.os`.
* [#64](https://github.com/clojure-emacs/orchard/issues/64): Add finding docs functionality from ClojureDocs in `orchard.clojuredocs`.
* Extract `cider-nrepl`'s resource lookup functionality into `orchard.java.resource`.

### Changes

* Update JavaDoc URL paths for Java 10+. Remove support for Java 7 JavaDoc URL paths.
* Remove dependency on `clojure.tools.namespace` (replaced by `orchard.namespace`).
* Remove dependency on `java.classpath` (replaced by `orchard.java.classpath`).
* Add java parser for JDK9+; rename legacy JDK8 parser.
* Make search for JDK directory resources compatible with JDK9+.
* Move `transform-value` to `cider-nrepl`.

### Bugs fixed

* [#59](https://github.com/clojure-emacs/orchard/pull/59): Correct inlined-dep prefix for removing namespaces
* [#57](https://github.com/clojure-emacs/orchard/pull/57): Fix parsing of Java versions for Java 11+.

## 0.4.0 (2019-01-14)

### New features

* [#38](https://github.com/clojure-emacs/orchard/pull/38): [Inspector] Add eval
  and def features to inspector.

### Bugs fixed

* [#39](https://github.com/clojure-emacs/orchard/pull/39): Catch `LinkageError` when calling `Class/forName`.

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
