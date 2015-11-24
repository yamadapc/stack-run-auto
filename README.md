stack-run-auto
==============
Automatically finds dependencies and runs a Haskell script (no cabal manifest,
no stack.yaml, no project). Currently a proof-of-concept using scripts.

Implemented as a collection of bash scripts. Depends on:
- `curl` (for making HTTP requests to the Hackage repository and the Hayoo
  search-engine)
- `stack` (for running scripts with `runghc` after installing resolved
- `bash`
  dependencies)
- `grep` (needs GNU grep)
- `jq` (for processing Hayoo responses)
- `node` (for processing Hackage responses)
- `tail`
- `head`
- `sort`
- `uniq`

## Installation
Assuming you have all the binaries listed above, run `npm install` at the
package root and symlink all these binaries onto your `$PATH`. Honestly this
isn't ready for public use, I just want to expose how it's done.

## Usage
```
stack-run-auto Test.hs
```

This will:
- Run the `file-modules` script on the input file. This prints the imported
  modules from this Haskell file.
- Use the `module-package` script to find the package containing each of these
  modules. This uses the Hayoo search-engine to query for the module trying to
  find from which package it came from. `jq` post-processes the JSON output to
  find the package matches
- We need all packages that are needed to run the file to be specified, so we
  also need to query for the dependencies of each of the imported packages we
  found in the last step. This is done with the `hackage-dependencies` script.
  It hits the Hackage `/package/:packagename/dependencies` page, then passes the
  page onto `extract-dependencies-simple`, which is a `node.js` script using
  `cheerio` to parse simple dependencies out (it's not a `grep` because we don't
  want optional dependencies, so we need the HTML structure - see the
  [text page](http://hackage.haskell.org/package/text/dependencies) as an
  example)
- Once we have all the dependencies, we pass them onto
  `stack runghc File.hs --package package1 --package package2 ...`

## Example output
```
$ ./stack-run-auto Main.hs
Parsing modules...
Finding package for Control.Concurrent...
----> base
Finding package for Control.Distributed.Process...
----> distributed-process
Finding package for Control.Distributed.Process.Node...
----> distributed-process
Finding package for Network.Transport...
----> network-transport
Finding package for Network.Transport.TCP...
----> network-transport-tcp
Finding dependencies for base...
Finding dependencies for distributed-process...
----> distributed-process binary bytestring data-accessor distributed-static ghc-prim hashable mtl network-transport random rank1dynamic stm syb transformers
Finding dependencies for network-transport...
----> network-transport binary bytestring deepseq hashable transformers
Finding dependencies for network-transport-tcp...
----> network-transport-tcp bytestring containers data-accessor network network-transport
Run from outside a project, using implicit global project config
Using resolver: lts-3.5 from implicit global project's config file: /home/yamadapc/.stack/global/stack.yaml
network-transport-0.4.2.0: download
data-accessor-0.2.2.6: download
network-transport-0.4.2.0: configure
# ... Stack downloads and builds
Completed all 6 actions.
# ... Starts to run
```

- - -

It'd be nice to have this polished. A couple of things:
- The Hayoo API is super useful. Editor plugins could really benefit from
  automatically resolving the package name from a module name. You can drop-in
  `module-package` onto an Elisp script for example and have it automatically
  add dependencies for you.
- The Hackage dependencies bit is the worst part. There should be an easier way
  to do this (a better API, a command-line tool or something like that). I'd
  rather not have `extract-dependencies-simple`, since that introduces the
  Node.js dependency, which is ugly.
- `file-modules` is useful, but fragile. I'm sure someone has an actual parser
  that can do it perfectly. I choose the quick-and-dirty route.

## License
This code is licensed under the MIT license. For more information please refer
to the [LICENSE](/LICENSE) file.
