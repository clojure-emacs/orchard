[![CircleCI](https://img.shields.io/circleci/build/github/clojure-emacs/orchard/master.svg)](https://circleci.com/gh/clojure-emacs/orchard/tree/master)
[![Coverage](https://codecov.io/gh/clojure-emacs/orchard/branch/master/graph/badge.svg)](https://codecov.io/gh/clojure-emacs/orchard/)
[![Clojars Project](https://img.shields.io/clojars/v/cider/orchard.svg)](https://clojars.org/cider/orchard)
[![cljdoc badge](https://cljdoc.org/badge/cider/orchard)](https://cljdoc.org/d/cider/orchard/CURRENT)
[![downloads badge](https://versions.deps.co/cider/orchard/downloads.svg)](https://clojars.org/cider/orchard)

# Orchard

A Clojure library designed to provide common functionality for Clojure
development tools (e.g. Clojure editor plugins and IDEs).

Orchard provides the building blocks that Clojure editors and IDEs need:

### Code Navigation

Jump to definitions and trace how code connects.

```clojure
(require '[orchard.info :as info])
(info/info 'clojure.core 'map)
;; => {:ns clojure.core, :name map, :file "clojure/core.clj", :arglists ([f] [f coll] ...), ...}

(require '[orchard.xref :as xref])
(xref/fn-deps 'my.app/handler)
;; => #{#'ring.util.response/response #'my.app/render-page ...}
```

- Var info and metadata lookup
- Find function dependencies and usages
- Namespace utilities and classpath access

### Documentation

Surface the right docs at the right time.

```clojure
(require '[orchard.eldoc :as eldoc])
(-> (info/info 'clojure.core '+) eldoc/eldoc)
;; => {:ns "clojure.core", :name "+", :type "function", :eldoc [[] ["x"] ["x" "y"] ["x" "y" "&" "more"]], ...}

(require '[orchard.clojuredocs :as clojuredocs])
(clojuredocs/find-doc "clojure.core" "map")
;; => {:examples [...], :see-also [...], :notes [...], ...}
```

- Eldoc (function signature) display
- ClojureDocs integration
- Spec lookups (supports both `clojure.spec.alpha` and `clojure.alpha.spec`)

### Exploration

Discover and understand code and data.

```clojure
(require '[orchard.apropos :as apropos])
(apropos/find-symbols {:var-query {:search #"^print"}})
;; => [{:name clojure.core/print, :doc "Prints the object(s)...", :type "function"} ...]

(require '[orchard.inspect :as inspect])
(inspect/start {:page-size 32} (range 100))
;; => {:rendered ("Class: ..." "--- Contents:" ...), :counter 1, :value (0 1 2 ...), ...}
```

- Enhanced apropos (regex search across namespaces)
- Namespace and var querying
- Value [inspector](https://github.com/clojure-emacs/orchard/blob/master/doc/inspector.org)

### Debugging & Profiling

Make sense of what went wrong and where time is spent.

```clojure
(require '[orchard.stacktrace :as stacktrace])
(try (/ 1 0) (catch Exception e (stacktrace/analyze e)))
;; => [{:class "java.lang.ArithmeticException", :message "Divide by zero", :stacktrace [...], ...}]

(require '[orchard.trace :as trace])
(trace/trace-var* #'my.app/process)
;; All calls to my.app/process will now be traced
```

- Stacktrace analysis
- Function tracing (alternative for `tools.trace`)
- Simple function profiler

### Java Interop

First-class support for navigating and documenting Java code.

```clojure
(require '[orchard.java :as java])
(java/member-info 'java.util.Map 'size)
;; => {:name size, :type java.util.Map, :argtypes [], :returns int, ...}
```

- Class and member info via reflection
- Java source navigation and Javadoc extraction
- Classpath utilities (alternative for `java.classpath`)

### Editor Support

Small but essential pieces for a polished editing experience.

```clojure
(require '[orchard.pp :as pp])
(pp/pprint-str {:a 1 :b {:c 2 :d [3 4 5]}} {:max-width 30})

(require '[orchard.indent :as indent])
(indent/infer-style-indent (meta #'clojure.core/let))
;; => {:name let, ..., :style/indent [[:block 1]]}
```

- Indentation inference
- Pretty printing (alternative for `clojure.pprint`)

## Why?

Much of the tooling code required to build Clojure editors and smart REPLs
is tool-agnostic and *should* be reused between tools, instead of copied
and altered in each and every tool.

Having a common tooling foundation typically means:

- Better foundation (e.g. more functionality, good documentation, etc) with more contributors
- Less work for tool authors as they don't have to reinvent the wheel for every tool
- Happier end users

## Design

Orchard is meant to be used to build programmer tooling relying on inspecting the state of a running REPL.
REPL-powered tooling has been a core Lisp idea for many years and there are many Clojure libraries
in that space (e.g. `compliment`, `tools.trace`, `sayid`, etc).

One thing to keep in mind is that Orchard relies (mostly) on runtime information, not the source code itself.
In simple terms - only code that's loaded (evaluated) will be taken under consideration. That's pretty different
from the static analysis approach taken by tools for most programming languages where it's not possible to
easily inspect the state of running program.

Some other design goals are listed below.

### No Runtime Dependencies

Orchard is meant to run alongside your application and we can't have a
dev tools library interfere with your app right? Dependency collisions are nasty problems which are best solved
by making sure there can't be any shared libraries to cause the conflict.

### API Optimized for Editors

Code editors can't know what symbols resolve to without consulting a REPL that's why they would typically
send a combination of a symbol name and some ns (e.g. the current namespace), so they can be resolved to
some var on which an operation would be invoked.

That's why the majority of the functions in Orchard take a combination of a ns and a symbol instead of a var.
Probably down the road we'll provide var-friendly versions of most functions as well.

### REPL Agnostic

No matter whether you're using nREPL, a socket REPL, or prepl, Orchard has your back. nREPL clients might
opt to wrap some of the Orchard functionality in middleware for convenience (as `cider-nrepl` does), but they
can just eval away if they please.

## API Documentation

Documentation for the master branch as well as tagged releases are available
[here](https://cljdoc.org/d/cider/orchard).

## Usage

**orchard requires Clojure 1.10+ and Java 8+.**

> [!NOTE]
>
> Java 8 is soft-deprecated in Orchard since version 0.29. Core Orchard functionality continues to work on JDK 8, but these following features don't:
>
> - Java sources parsing

Just add `orchard` as a dependency and start hacking.

```clojure
[cider/orchard "0.41.0"]
```

Consult the [API documentation](https://cljdoc.org/d/cider/orchard/CURRENT) to get a better idea about the
functionality that's provided.

## Dealing with Java sources

Orchard can locate Java source files (on the classpath, in the JDK's `src.zip`,
or in Maven `-sources.jar` artifacts), parse them for Javadoc and argument
names, and even download missing source JARs automatically. See the
[Java interop guide](https://cljdoc.org/d/cider/orchard/CURRENT/doc/java-interop-support)
for details.

## Development

Having JDK sources archive (`$JAVA_HOME/lib/src.zip`) is important for
development of Java-related features in Orchard. Certain features parse those
Java sources as a source of information. The archive doesn't need to be on the
classpath, it just need to exist in the distribution.

You can install Orchard locally like this:

```shell
PROJECT_VERSION=99.99 make install
```

For releasing to [Clojars](https://clojars.org/):

```shell
git tag -a vX.Y.Z -m "Release X.Y.Z"
git push --tags
git push
```

### Tests and formatting

To run the CI tasks locally use:

``` shell
make test cljfmt kondo eastwood
```

## Caveats and Known Issues

### `xref/fn-deps` and `xref/fn-refs` limitations

These functions use a Clojure compiler implementation detail to find references to other function var dependencies.

You can find a more in-depth explanation in this [post](https://lukas-domagala.de/blog/clojure-analysis-and-introspection.html).

The important implications from this are:

- very fast
- functions marked with meta `:inline` will not be found (`inc`, `+`, ...)
- redefining function vars that include lambdas will still return the dependencies of the old plus the new ones
- ([explanation](https://lukas-domagala.de/blog/clojure-compiler-class-cache.html))
- does not work on AoT compiled functions

### Java 8 support

As noted earlier Java 8 is soft-deprecated in Orchard since version 0.29. Core
Orchard functionality continues to work on JDK 8, but the following features
don't:

- Java sources parsing

We are aware that some people are stuck using Java 8 and we'll keep supporting
for as long as we can, but it's no longer a priority for us that every feature
works with Java 8.

## History

Originally [SLIME][] was the most
popular way to program in Clojure with Emacs and a lot of useful
functionality was created for it to support things like code
completion, value inspection, finding references, apropos and so
on. This functionality was implemented as a swank adapter written in
Clojure and lived in the
[swank-clojure][] project.

Subsequently [CIDER][] and
[cider-nrepl][] replaced
SLIME and swank, and much code was moved from `swank-clojure` to
`cider-nrepl` and continued to evolve there.

> [!TIP]
>
> You can watch the presentation [The Evolution of the Emacs tooling for
> Clojure](https://www.youtube.com/watch?v=4X-1fJm25Ww&list=PLZdCLR02grLoc322bYirANEso3mmzvCiI&index=6)
> to learn more about all of this.

This project is an effort to prevent repeating the mistakes of the
past - `cider-nrepl` was split into two libraries, so that non-nREPL
clients can make of use of the general functionality contained in
`cider-nrepl` (e.g. things like `apropos`, `inspect`, etc).

## License

Copyright © 2018-2026 Bozhidar Batsov & contributors

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

[SLIME]: https://github.com/slime/slime
[swank-clojure]: https://github.com/technomancy/swank-clojure
[CIDER]: https://github.com/clojure-emacs/cider
[cider-nrepl]:https://github.com/clojure-emacs/cider-nrepl
