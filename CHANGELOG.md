# Changelog

## master (unreleased)

### New features

* `orchard.inspect`: offer new `previous-sibling` and `next-sibling` functions.
  * These help navigate sequential collections one item at a time, in a streamlined fashion.

### Changes

* `orchard.inspect`: don't render keyword/symbol/number values as strings.
* `orchard.inspect`: don't use `pr-str` over the main `Value: ` being inspected. 
  * All values are already formatted as strings, so this `pr-str` was redundant.
* `orchard.inspect`: render non-accessible fields better.
  * If a given field cannot be inspected (because it's private and the JDK module system prevents opening it), we return the fixed symbol `<non-inspectable value>` for representing its value, clients being free to elide its rendering.
  * Now, for a given inspected Object (except Class objects), we return these sections, if present: `Instance fields`, `Static fields` ,`Private instance fields`, `Private static fields`.
  * For Class objects, we keep grouping the fields under a single `Fields` section.
* `orchard.inspect`: render field names as symbols, not strings.

## 0.16.1 (2023-10-05)

* Make some internals safer.

## 0.16.0 (2023-10-05)

* `class-info` and `member-info`: include `:annotated-arglists`.
  * `:annotated-arglists` represents each arglist as a string with return and param type info, rendered as metadata.

## 0.15.2 (2023-10-05)

### Bugs fixed

* Correctly infer indentation for `->`-like macros.
* Don't infer indentation for `def`-like macros.

## 0.15.1 (2023-09-21)

## Changes

* `:doc-block-tags-fragments`: exclude tags other than `Returns`, `Throws` and `Param`.
  * This helps keeping the rendered docstrings concise, and predictably formatted.

## 0.15.0 (2023-09-20)

### Changes

* [#189](https://github.com/clojure-emacs/orchard/issues/179): `info` for Java: return extra `:doc`-related attributes, as reflected in the new `orchard.java.parser-next` namespace, allowing clients to render HTML and non-HTML fragments with precision.
  * This namespace needs the `--add-opens=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED` JVM flag to be present, otherwise a fallback will be used.
  * ([enrich-classpath](https://github.com/clojure-emacs/enrich-classpath) adds that flag when suitable)

### Bugs fixed

* [#182](https://github.com/clojure-emacs/orchard/issues/182): `info` on ClojureScript: don't mistakenly prioritize special form names over var names.

## 0.14.2 (2023-08-09)

### Changes

- [#176](https://github.com/clojure-emacs/orchard/issues/176): `orchard.xref`: include info for test vars.
- `orchard.xref`: avoid duplicate vars that might appear following REPL re-evaluation.

## 0.14.1 (2023-08-05)

### Changes

- [#173](https://github.com/clojure-emacs/orchard/issues/173): Parallelize `orchard.xref/fn-refs`. 

## 0.14.0 (2023-08-03)

- [cider-nrepl #788](https://github.com/clojure-emacs/cider-nrepl/issues/788): Infer var metadata for 'indirect' vars.
- [#170](https://github.com/clojure-emacs/orchard/pull/170): Fix exception thrown when inspecting short Eduction.

## 0.13.0 (2023-07-21)

- [cider-nrepl #777](https://github.com/clojure-emacs/cider-nrepl/issues/777): Introduce `orchard.indent` ns / functionality.

## 0.12.0 (2023-06-24)

### New features

- [#167](https://github.com/clojure-emacs/orchard/pull/167): Added new inspector function `tap-current-value`.

### Changes

- [#165](https://github.com/clojure-emacs/orchard/issues/165): Mark special forms and macros accordingly for ElDoc (before they were all marked as functions).

## 0.11.0 (2022-10-25)

### New features

- [#163](https://github.com/clojure-emacs/orchard/pull/163): Support Clojure Spec 2 in the Spec browser.

## 0.10.0 (2022-09-04)

### Bugs fixed

* [#158](https://github.com/clojure-emacs/orchard/issues/158): Make `classpath-namespaces` resilient to faulty ns declarations.

### Changes

* [#161](https://github.com/clojure-emacs/orchard/pull/161): Add Datafy section to inspector and align section headers
  * Add a `Datafy` section to the inspector. For more details, take a
    look at the
    [Datafiable](https://github.com/clojure-emacs/orchard/blob/master/doc/inspector.org#datafiable)
    and
    [Navigable](https://github.com/clojure-emacs/orchard/blob/master/doc/inspector.org#navigable)
    sections of the Orchard inspector
    [docs](https://github.com/clojure-emacs/orchard/blob/master/doc/inspector.org).
  * Align all section headers to start with `---`.

## 0.9.2 (2022-02-22)

* Guard against OOMs in `orchard.java/member-info`.

## 0.9.1 (2022-01-17)

### Bugs fixed

* Fix a NullPointerException in `orchard.cljs.analysis`.
* Fix a `Cannot open <nil> as a Reader`.

## 0.9.0 (2022-01-10)

### Changes

* [#51](https://github.com/clojure-emacs/orchard/issues/51): Extend `find-usages`:
  * `orchard.xref/fn-deps` now also finds anonymous function dependencies.
  * New: `orchard.xref/fn-deps-class` as a lower level API so you can still get the main functions deps only.
  * New: `orchard.xref/fn-transitive-deps`.
* [#65](https://github.com/clojure-emacs/orchard/issues/65): Make ClojureScript dependency `:provided`.
  * If you were using Orchard for ClojureScript functionality, it might be a good idea to make sure you have an explicit `org.clojure/clojurescript` dependency.

### Bugs fixed

* [#142](https://github.com/clojure-emacs/orchard/issues/142): Make `read-namespace` handle read conditionals gracefully.

## 0.8.0 (2021-12-15)

### Changes

* Remove `dynapath` dependency
  * With it, defns related with mutable classloader are now deprecated and are no-ops
  * `-Dorchard.use-dynapath=false` has no effect now either.
* Accomodate [`enrich-classpath`](https://github.com/clojure-emacs/enrich-classpath)
  * Now, if you intend to use Orchard for its Java functionality, it is expected that you use enrich-classpath also.
  * If not present, Java-related features won't work (but at least won't throw a compile-time error).

### Bugs Fixed

* [#135](https://github.com/clojure-emacs/orchard/issues/135): Fix problematic double var lookup in `orchard.xref/fn-refs`.

### Changes

* [#124](https://github.com/clojure-emacs/orchard/pull/124): Remove costly `io/resource` lookup.

## 0.7.3 (2021-10-02)

### Changes

* [#133](https://github.com/clojure-emacs/orchard/issues/133): `info:` don't fall back to `clojure.core` for fully-qualified symbols.

## 0.7.2 (2021-09-30)

### Changes

* `orchard.namespace` functionality is now parallelized when possible.

### Bugs Fixed

* [#123](https://github.com/clojure-emacs/orchard/pull/123): Fix info lookups from namespaces that don't yet exist
* [#125](https://github.com/clojure-emacs/orchard/issues/125): Don't fail if the classpath references a non-existing .jar
* [#128](https://github.com/clojure-emacs/orchard/issues/128): Strengthen `apropos`

## 0.7.1 (2021-04-18)

### Bugs Fixed

* [#116](https://github.com/clojure-emacs/orchard/pull/116): Directories in the classpath having a file extension (such as `.jar`) will not confuse Orchard anymore, which had the potential to cause errors.

## 0.7.0 (2021-04-13)

### New features

* [#111](https://github.com/clojure-emacs/orchard/pull/111): [Inspector] Configure truncation limits.

### Changes

* [#113](https://github.com/clojure-emacs/orchard/issues/113): Add ability to skip functionality that works by altering the classpath.
  * You an opt in to this choice by setting `"-Dorchard.use-dynapath=false"`.
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
