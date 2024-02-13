@_default:
  just --list

dune := "opam exec -- dune"

# Remove all generated files
hard-reset:
  rm -f melange-aoc.opam
  rm -rf _build
  rm -rf _opam
  rm -rf node_modules

# Create Opam switch
create-switch:
  opam switch create . 5.1.0 -y --deps-only

# Install deps and run opam-check-npm-deps
install:
  npm install
  opam update
  opam install dune
  {{ dune }} build melange-aoc.opam # Generate the opam file
  opam install -y . --deps-only --with-test
  opam exec opam-check-npm-deps

# Initialize the project
init: create-switch install

# Watch for changes and rebuild
watch:
  {{ dune }} build -w

# Build
build:
  {{ dune }} build

# Serve the app
serve:
  npx webpack serve --open --mode development --entry ./_build/default/src/output/src/ReactApp.js --history-api-fallback

# Bundle
bundle:
  npx webpack --mode production --entry ./_build/default/src/output/src/ReactApp.js