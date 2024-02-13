# Relude temporary workaround
If you run into issues around Relude/Bastet that look like the errors below, then here's some info from a while ago. As of the time of writing this, `make install` should handle both libraries correctly.

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


# melange-aoc


## Just command runner
This repo uses the [Just command runner](https://github.com/casey/just) for command execution. Common commands:
* Initialize the repository: `just init`
* Run after updating dependencies: `just install`
* Build once: `just build`
* Build and watch for changes: `just watch`
* Serve the site (works best in a 2nd terminal, with `just watch` running in the first): `just serve`
* Remove opam switch, build artifacts, and node modules, requiring re-initialization: `just hard-reset`
* Everything has gone horribly wrong, [fix it fix it fix it](https://www.youtube.com/watch?v=8ZCysBT5Kec): `just hard-reset init`
