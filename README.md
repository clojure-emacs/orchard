[![Build Status](https://travis-ci.org/clojure-emacs/orchard.png?branch=master)](https://travis-ci.org/clojure-emacs/orchard)
[![Dependencies Status](https://versions.deps.co/clojure-emacs/orchard/status.svg)](https://versions.deps.co/clojure-emacs/orchard)
[![Coverage](https://codecov.io/gh/clojure-emacs/orchard/branch/master/graph/badge.svg)](https://codecov.io/gh/clojure-emacs/orchard/)


# orchard (alpha)

A Clojure library designed to provide common functionality for Clojure development tools (e.g. CIDER).

## History

Originally [SLIME](https://github.com/slime/slime) was the most
popular way to program in Clojure with Emacs and a lot of useful
functionality was created for it to support things like code
completion, value inspection, finding references, apropos and so
on. This functionality was implemented as a swank adapter written in
Clojure and lived in the
[swank-clojure](https://github.com/technomancy/swank-clojure) project.

Subsequently [CIDER](https://github.com/clojure-emacs/cider) and
[cider-nrepl](https://github.com/clojure-emacs/cider-nrepl) replaced
SLIME and swank, and much code was moved from `swank-clojure` to
`cider-nrepl` and continued to evolve there.

You can watch the presentation [The Evolution of the Emacs tooling for
  Clojure](https://www.youtube.com/watch?v=4X-1fJm25Ww&list=PLZdCLR02grLoc322bYirANEso3mmzvCiI&index=6)
  to learn more about all of this.

This project is an effort to prevent repeating the mistakes of the
past - `cider-nrepl` was split into two libraries, so that non-nREPL
clients can make of use of the general functionality contained in
`cider-nrepl` (e.g. things like `apropos`, `inspect`, etc).

Much of the tooling code required to build Clojure editors and smart REPLs
is tool-agnostic and *should* be reused between tools, instead of copied
and altered in each and every tool.

## Usage

Coming soon...

## License

Copyright © 2018 Bozhidar Batsov & contributors

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
