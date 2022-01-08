[![CircleCI](https://circleci.com/gh/clojure-emacs/orchard/tree/master.svg?style=svg)](https://circleci.com/gh/clojure-emacs/orchard/tree/master)
[![Coverage](https://codecov.io/gh/clojure-emacs/orchard/branch/master/graph/badge.svg)](https://codecov.io/gh/clojure-emacs/orchard/)
[![Clojars Project](https://img.shields.io/clojars/v/cider/orchard.svg)](https://clojars.org/cider/orchard)
[![cljdoc badge](https://cljdoc.org/badge/cider/orchard)](https://cljdoc.org/d/cider/orchard/CURRENT)
[![downloads badge](https://versions.deps.co/cider/orchard/downloads.svg)](https://clojars.org/cider/orchard)

# Orchard

A Clojure library designed to provide common functionality for Clojure
development tools (e.g. Clojure editor plugins and IDEs).

Right now `orchard` provides functionality like:

* enhanced apropos
* classpath utils (alternative for `java.classpath`)
* value inspector
* Java class handling utilities
* Utilities for dealing with metadata
* Namespace utilities
* Fetching ClojureDocs documentation
* Finding function dependencies (other functions invoked by a function) and usages

## Why?

Much of the tooling code required to build Clojure editors and smart REPLs
is tool-agnostic and *should* be reused between tools, instead of copied
and altered in each and every tool.

Having a common tooling foundation typically means:

* Better foundation (e.g. more functionality, good documentation, etc) with more contributors
* Less work for tool authors as they don't have to reinvent the wheel for every tool
* Happier end users

## Design

Orchard is meant to be used to build programmer tooling relying on inspecting the state of a running REPL.
REPL-powered tooling has been a core Lisp idea for many years and there are many Clojure libraries
in that space (e.g. `compliment`, `tools.trace`, `sayid`, etc).

One thing to keep in mind is that Orchard relies (mostly) on runtime information, not the source code itself.
In simple terms - only code that's loaded (evaluated) will be taken under consideration. That's pretty different
from the static analysis approach taken by tools for most programming languages where it's not possible to
easily inspect the state of running program.

Some other design goals are listed bellow.

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

No matter whether you're using nREPL, a socket REPL, unrepl or prepl, Orchard has your back. nREPL clients might
opt to wrap some of the Orchard functionality in middleware for convenience (as `cider-nrepl` does), but they
can just eval away if they please.

## API Documentation

Documentation for the master branch as well as tagged releases are available
[here](https://cljdoc.org/d/cider/orchard).

## Usage

**orchard requires Clojure 1.8+ and Java 8+.**

Just add `orchard` as a dependency and start hacking.

```clojure
[cider/orchard "0.8.0"]
```

Consult the [API documentation](https://cljdoc.org/d/cider/orchard/CURRENT) to get a better idea about the
functionality that's provided.

### Using `enrich-classpath` for best results

There are features that Orchard intends to provide (especially, those related to Java interaction) which need to assume a pre-existing initial classpath that already has various desirable items, such as the JDK sources, third-party sources, special jars such as `tools` (for JDK8), a given project's own Java sources... all that is a domain in itself, which is why our [enrich-classpath](https://github.com/clojure-emacs/enrich-classpath) project does it.

For getting the most out of Orchard, it is therefore recommended/necessary to use `enrich-classpath`. Please refer to its installation/usage instructions.

### xref/fn-deps and xref/fn-refs limitations

These functions use a Clojure compiler implementation detail to find references to other function var dependencies.

You can find a more in-depth explanation in this [post](https://lukas-domagala.de/blog/clojure-analysis-and-introspection.html).

The important implications from this are:

* very fast
* functions marked with meta :inline will not be found (inc, +, ...)
* redefining function vars that include lambdas will still return the dependencies of the old plus the new ones
([explanation](https://lukas-domagala.de/blog/clojure-compiler-class-cache.html))

## Configuration options

So far, Orchard follows these options, which can be specified as Java system properties
(which means that end users can choose to set them globally without fiddling with tooling internals):

* `"-Dorchard.initialize-cache.silent=true"` (default: `true`)
  * if `false`, the _class info cache_ initialization may print warnings (possibly spurious ones).

## Tests and formatting

To run the CI tasks locally use:

`make test cljfmt kondo eastwood`

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

You can watch the presentation [The Evolution of the Emacs tooling for
  Clojure](https://www.youtube.com/watch?v=4X-1fJm25Ww&list=PLZdCLR02grLoc322bYirANEso3mmzvCiI&index=6)
  to learn more about all of this.

This project is an effort to prevent repeating the mistakes of the
past - `cider-nrepl` was split into two libraries, so that non-nREPL
clients can make of use of the general functionality contained in
`cider-nrepl` (e.g. things like `apropos`, `inspect`, etc).

## License

Copyright © 2018-2021 Bozhidar Batsov & contributors

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

[SLIME]: https://github.com/slime/slime
[swank-clojure]: https://github.com/technomancy/swank-clojure
[CIDER]: https://github.com/clojure-emacs/cider
[cider-nrepl]:https://github.com/clojure-emacs/cider-nrepl
