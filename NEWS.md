# readjsa 0.1.0

- Hardens Matt Cowgill's `readjsa` 0.0.1 client while preserving the public
  `read_ivi()`, `read_reos()` and `read_salm()` function families.
- Adds official release discovery for six Internet Vacancy Index families,
  including GCCSA/SA4 and JSA Remoteness/Northern Australia.
- Reads every table listed in the current Recruitment Experiences and Outlook
  Survey workbook and preserves titles, definitions, footnotes and units.
- Reads official smoothed SA2 and LGA Small Area Labour Markets releases with
  quarter-end dates and explicit ASGS vintages.
- Adds bounded atomic transport, exact SHA-256 cache verification,
  release-specific identity, writer locks, retries and deterministic pruning.
- Preserves source URLs, validated final URLs, release/reference vintage,
  classifications, revisions, attribution, parser versions and raw hashes.
- Canonicalises REOS percentages to decimal proportions and preserves
  publisher-defined SALM unavailable values explicitly.
- Adds fully offline fixtures, official-file replay, independent cell
  cross-checks and per-family performance evidence.
- Certifies the personal workflow for explicit one-family reads and documents
  the resource envelope for local batch use.
- Documents private GitHub installation and the complete supported data
  surface in a conventional package README.
