## Test environments

- Local: macOS Sequoia 15.6 (arm64), R 4.5.1, source tarball checked with
  `R CMD check --as-cran --timings`, including examples, tests, rebuilt
  vignettes, and PDF/HTML manuals.
- The effective package maintainer is Chartwell Macro. The required maintainer contact
  is supplied only in `DESCRIPTION`; public citation and documentation
  surfaces omit email addresses.
- GitHub Actions for the chart package-content commit
  `pre-publication baseline`: Ubuntu with R release,
  oldrel-1, and devel; macOS with R release; and Windows with R release.
  Every `R CMD check --as-cran` job returned `Status: OK` in run
  [32732066055](https://github.com/chartwell-macro/readjsa/actions/runs/32732066055).
  The source-layout tests passed in the same run. Offline coverage passed in
  run [32732066042](https://github.com/chartwell-macro/readjsa/actions/runs/32732066042),
  and the pkgdown site build and preview artifact passed in run
  [32732065992](https://github.com/chartwell-macro/readjsa/actions/runs/32732065992).
- GitHub Actions for package-content commits after
  `pre-publication baseline` are currently rejected before
  any runner step begins because the account payment or spending limit
  requires attention. The affected ownership-amendment
  [R CMD check](https://github.com/chartwell-macro/readjsa/actions/runs/32741189612),
  [coverage](https://github.com/chartwell-macro/readjsa/actions/runs/32741189721),
  and [pkgdown](https://github.com/chartwell-macro/readjsa/actions/runs/32741190020)
  runs therefore provide no package result. The local results below cover the
  current package payload. This comments file is excluded from the source
  tarball, so evidence-only updates do not change the checked payload.

## R CMD check results

0 errors | 0 warnings | 1 note

The clean installed-tarball test run passed 208 expectations with no failures,
warnings, or skips. The source tarball is 729,130 bytes with SHA-256
`aa01f0f2b19a3f363e550a94a8b9c6265f413d2912168628df71a43cf47b841b`.
Fresh source, tarball, installed-package, rendered-vignette, and pkgdown-site
scans found no personal maintainer address. The required non-personal contact
appears only in built and installed package metadata; citations and public
documentation are email-free.

The NOTE identifies this as a new submission and reports that the GitHub
repository, issue tracker, and intended pkgdown URL return HTTP 404 to an
anonymous checker. The repository is currently private and GitHub Pages is
disabled. The pkgdown build itself passes; only publication is unavailable.
This is a pre-submission blocker: do not submit the package until those URLs
are publicly reachable and a fresh incoming check clears the URL findings.

## Maintainer identity

`Chartwell Macro` is the maintainer's explicit public package and citation identity and
the copyright holder for the current fork's package-code contributions. It
appears first in `Authors@R` with
`aut`, `cre` and `cph` roles; the effective built `Author` and `Maintainer`
fields, installed citation, package topic,
README and pkgdown author and licence pages were checked against that choice.
Matt Cowgill remains an author and Angus Hughes and Amee McMillan remain
contributors. The exact upstream 2024 MIT notice remains preserved verbatim
and applicable to inherited portions; official data remain subject to their
publishers' rights and terms. The abbreviated identity is a reviewed metadata
choice, not a CRAN blocker.

## Downstream checks

This is an initial CRAN release and therefore has no CRAN reverse dependencies.
The known Chartwell downstream provider pins the reviewed pre-release commit.
Its fixture-backed package/provider contract test was rerun read-only against
an isolated installation of this candidate, with only the expected identity
field shimmed to the existing downstream pin; the production downstream
repository and data were not modified.
