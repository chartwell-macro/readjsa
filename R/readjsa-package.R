#' readjsa: Reproducible access to Jobs and Skills Australia data
#'
#' `readjsa` discovers, downloads, verifies, caches, and parses selected
#' official Jobs and Skills Australia (JSA) and Department of Employment and
#' Workplace Relations (DEWR) releases.  It currently covers the Internet
#' Vacancy Index (IVI), Recruitment Experiences and Outlook Survey (REOS), and
#' Small Area Labour Markets (SALM) publication families.
#'
#' The high-level [read_ivi()], [read_reos()], and [read_salm()] functions
#' combine official-release discovery with HTTPS transport that applies
#' timeout, retry, and received-size checks, plus a user-scoped raw cache.  The
#' corresponding `*_file()` functions are local,
#' deterministic parsers for packaged or previously downloaded files.
#' [jsa_release_manifest()] records the supported family IDs and
#' [fetch_jsa_release()] exposes the verified cache boundary.  High-level
#' readers attach discovered release-vintage and raw-file provenance to every
#' observation.  Local readers attach provenance when it is supplied and
#' otherwise retain the same columns with unavailable release fields.  Source-
#' specific metadata, units, classifications, and publication statuses remain
#' explicit.
#'
#' The package does not require credentials and does not contact a source when
#' a local parser is used.  Live discovery and fetching require network access
#' to the official HTTPS JSA/DEWR hosts; tests and examples use small packaged
#' fixtures and injected transports.  See the individual function topics for
#' exact schemas, failure classes, cache behaviour, and supported coverage.
#'
#' @section Provenance and licensing:
#' The current fork's package-code contributions are distributed under the MIT
#' license and are copyright Chartwell Macro.  Inherited portions retain the exact
#' upstream notice preserved in `inst/UPSTREAM.md` and
#' `inst/UPSTREAM_LICENSE.md`.  Source terms and attribution are retained in
#' returned data; official data and publications remain subject to their
#' publishers' rights and stated terms, not the package copyright.
#'
#' @author Chartwell Macro
#'
#' @docType package
#' @name readjsa
#' @aliases readjsa-package
#' @keywords internal
"_PACKAGE"
