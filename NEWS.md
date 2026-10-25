# readjsa 0.1.0

- Hardens Matt Cowgill's `readjsa` 0.0.1 client while preserving the public
  `read_ivi()`, `read_reos()` and `read_salm()` function families.
- Adds official release discovery for six Internet Vacancy Index families,
  including GCCSA/SA4 and JSA Remoteness/Northern Australia.
- Reads every table listed in the current Recruitment Experiences and Outlook
  Survey workbook and preserves titles, definitions, footnotes and units.
- Reads official smoothed SA2 and LGA Small Area Labour Markets releases with
  quarter-end dates and explicit ASGS vintages.
- Adds bounded, locked transport, exact SHA-256 cache verification,
  release-specific identity, writer locks, retries and deterministic pruning.
- Preserves source URLs, validated final URLs, release/reference vintage,
  classifications, revisions, attribution, parser versions and raw hashes.
- Canonicalises REOS percentages to decimal proportions and preserves
  publisher-defined SALM unavailable values explicitly.
- Adds fully offline fixtures, official-file replay, independent cell
  cross-checks and per-family performance evidence.
- Certifies the personal workflow for explicit one-family reads and documents
  the resource envelope for local batch use.
- Adds precise, runnable documentation for every exported function, including
  output schemas, errors, side effects, network use and cache behaviour.
- Adds a task-oriented README, four deterministic offline vignettes, a grouped
  pkgdown reference site, package citation and contribution guidance.
- Adds an end-to-end IVI time-series and `ggplot2` chart walkthrough, rendered
  offline from an exact, provenance-documented June 2026 official snapshot,
  with a compact plotting example based directly on returned columns.
- Standardises the primary package author, maintainer and current-fork
  copyright holder as Chartwell Macro across package, citation, licence, README and
  site metadata, while preserving upstream and official-source rights.
- Limits maintainer email metadata to the technically required package field;
  citations and public documentation identify Chartwell Macro without an email address.
- Adds R release checks on Linux, macOS and Windows, plus Linux oldrel and
  devel checks, offline coverage, and pkgdown preview/deployment workflows.
- Makes official English release-date parsing locale independent and makes
  forced cache replacement portable when destination files already exist on
  Windows, without changing public signatures, defaults, schemas or cache keys.
- Excludes repository-only validation and performance tools from the source
  package while retaining their source-layout CI coverage.
