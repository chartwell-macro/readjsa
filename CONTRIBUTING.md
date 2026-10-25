# Contributing to readjsa

Please use the repository's [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com)
for a reproducible bug report, a documentation improvement, or a focused
feature proposal. Pull requests should explain the user-facing problem and
include tests or documentation for the change.

Before opening a pull request, run the offline source-layout test suite from
the package root:

```sh
Rscript -e 'testthat::test_local(".", reporter = "summary")'
```

For a package-level check, also run:

```sh
R CMD build .
R CMD check --as-cran readjsa_*.tar.gz
```

The routine tests and examples must remain deterministic and must not require
the live Jobs and Skills Australia or Department of Employment and Workplace
Relations sites. Use the packaged fixtures for tests and examples. The
opt-in scripts under `scripts/` are for maintainer validation and performance
work; they are not part of the ordinary CRAN test path.

Please preserve the existing public function signatures, returned columns and
classes, provenance fields, cache semantics, and offline behavior unless a
change to that contract is explicitly proposed and documented. Keep changes
small and reviewable, and do not commit downloaded source files, credentials,
local caches, package checks, or rendered site output.

Documentation changes should keep the README, function reference, vignettes,
and `NEWS.md` consistent. Regenerate generated Roxygen outputs when their
source comments change, and include the generated files in the same pull
request.
