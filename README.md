# Relude temporary workaround
* Relude needs to be pinned until it can be published to Opam
* Relude has its own pinned items (bastet) that won't be installed correctly via just `make install`, which will fail with something like this:
```
<><> Processing actions <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
⬇ retrieved bisect_ppx.2.8.3  (cached)
⬇ retrieved bastet.2.0.0  (cached)
⬇ retrieved melange-jest.0.1.0  (cached)
∗ installed bisect_ppx.2.8.3
∗ installed melange-jest.0.1.0
⬇ retrieved relude.dev  (no changes)
∗ installed bastet.2.0.0
[ERROR] The compilation of relude.dev failed at "dune build --debug-dependency-path -p relude -j 11
        @install".

#=== ERROR while compiling relude.dev =========================================#
# context     2.1.3 | linux/x86_64 | ocaml-base-compiler.5.1.0 | pinned(git+https://github.com/reazen/relude.git#v2#5a76fd6b)
# path        ~/git/melange-aoc/_opam/.opam-switch/build/relude.dev
# command     ~/.opam/opam-init/hooks/sandbox.sh build dune build --debug-dependency-path -p relude -j 11 @install
# exit-code   1
# env-file    ~/.opam/log/relude-23699-94589f.env
# output-file ~/.opam/log/relude-23699-94589f.out
### output ###
# [...]
# -> required by alias install
# (cd _build/default && /home/trite/git/melange-aoc/_opam/bin/melc -w -40 -g -bin-annot -I src/.relude.objs/melange -I /home/trite/git/melange-aoc/_opam/lib/bastet/melange -I /home/trite/git/melange-aoc/_opam/lib/melange/js/melange -I /home/trite/git/melange-aoc/_opam/lib/melange/node/melange --bs-stop-after-cmj --bs-package-output . --bs-module-name Relude_Interface --bs-package-name relude -n[...]
# File "src/Relude_Interface.re", line 1, characters 5-21:
# 1 | open Bastet.Interface;
#          ^^^^^^^^^^^^^^^^
# Error: Unbound module Bastet
# -> required by
#    _build/default/src/.relude.objs/melange/relude__Relude_Interface.cmi
# -> required by
#    _build/install/default/lib/relude/melange/relude__Relude_Interface.cmi
# -> required by _build/default/relude.install
# -> required by alias install



<><> Error report <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
┌─ The following actions failed
│ λ build relude dev
└─
┌─ The following changes have been performed
│ ∗ install bastet       2.0.0
│ ∗ install bisect_ppx   2.8.3
│ ∗ install melange-jest 0.1.0
└─

The former state can be restored with:
    /usr/local/bin/opam switch import
"/home/trite/git/melange-aoc/_opam/.opam-switch/backup/state-20231121225736.export"
make: *** [Makefile:25: install] Error 31
```
* Fix it by running this:
```
opam pin add -y relude.dev "https://github.com/reazen/relude.git#v2"
```
* The reason this works is that transitive pinned dependencies can't be installed automatically, but CAN be installed when running an `opam pin` manually


# melange-aoc

A simple project template using [Melange](https://github.com/melange-re/melange)
with [opam](https://opam.ocaml.org/).

If you are looking for a template with esy, check [melange-esy-template](https://github.com/melange-re/melange-esy-template).

## Quick Start

```shell
make init

# In separate terminals:
make watch
make serve
```

When running `make init`, you may encounter an error like this:

```
[ERROR] Could not determine which packages to install for this switch:
  * Missing dependency:
    - melange >= 1.0.0
    no matching version
```

It means `opam` doesn't have the latest packages definitions from the opam-repository. To address this, first run `opam update`, then rerun `make init`.

### React

React support is provided by
[`reason-react`](https://github.com/reasonml/reason-react/). The entry
point of the sample React app is [`src/ReactApp.re`](src/ReactApp.re).

## Commands

In opam / dune projects, our personal preference is to group commonly used commands in a Makefile. This is completely optional.

You can see all available commands by running `make help` or just `make`. Here
are a few of the most useful ones:

- `make init`: set up opam local switch and download OCaml, Melange and
JavaScript dependencies
- `make install`: install OCaml, Melange and JavaScript dependencies
- `make watch`: watch for the filesystem and have Melange rebuild on every
change
- `make serve`: serve the application with a local HTTP server

## JavaScript output

Since Melange compiles source files into JavaScript files, it can be used
for projects on any JavaScript platform - not just the browser.

The template includes two `melange.emit` stanza for two separate apps. This
stanza tells Dune to generate JavaScript files using Melange, and specifies in
which folder the JavaScript files should be placed, by leveraging the `target`
field:
- The React app JavaScript files will be placed in `_build/default/src/output/*`.
- The NodeJS app JavaScript files will be placed in `_build/default/src/node/*`.

So for example, [`src/Hello.ml`](src/Hello.ml) (using OCaml syntax) can be run with
`node`:

```bash
node _build/default/src/node/src/Hello.js
```

Similarly, `_build/default/src/output/src/ReactApp.js` can be passed as entry to a bundler
like Webpack:

```bash
webpack --mode production --entry ./_build/default/src/output/src/ReactApp.js
```
