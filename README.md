# readjsa

`readjsa` provides reproducible access to official Jobs and Skills Australia
and Department of Employment and Workplace Relations labour-market data. It
discovers current releases, verifies and caches exact source files, parses
audited workbook and CSV families, and retains release, classification,
revision and source-hash provenance.

This repository is a hardened derivative of Matt Cowgill's experimental
`readjsa` 0.0.1 client.

## Installation

Install the private GitHub repository with an authenticated GitHub session:

```r
pak::pak("chartwell-macro/readjsa")
```

Or build and install from a local clone:

```sh
R CMD build .
R CMD INSTALL readjsa_0.1.0.tar.gz
```

## Data covered

| Dataset | Package surface |
|---|---|
| Internet Vacancy Index | Six current families covering skill level, ANZSCO2/ANZSCO4, states and territories, IVI regions, GCCSA/SA4, and JSA Remoteness/Northern Australia |
| Recruitment Experiences and Outlook Survey | Every table listed in the official workbook Information sheet, with titles, definitions, footnotes, frequency and units |
| Small Area Labour Markets | Official smoothed SA2 and LGA CSV releases with quarter-end dates and explicit ASGS vintages |

The current replay evidence contains 2,531,244 IVI observations, 6,075 REOS
observations and 535,680 SALM observations.

## Use

```r
library(readjsa)

catalogue <- discover_jsa_releases("ivi")

ivi <- read_ivi("skill_states")
reos <- read_reos(tables = c("1.1", "3.3"))
salm <- read_salm("sa2")
```

The six IVI family names are:

```r
c(
  "skill_states",
  "anzsco4_states",
  "anzsco2_states",
  "anzsco2_ivi_regions",
  "anzsco2_gccsa_sa4",
  "anzsco4_remoteness_northern"
)
```

The personal-use workflow reads one explicit family at a time. Local batch
workflows can use `tables = "all"` and materialise more than 2.5 million
current IVI observations on a suitably provisioned machine.

Local official files use the same audited parsers:

```r
ivi_local <- read_ivi_file("internet-vacancies.xlsx")
reos_local <- read_reos_file("recruitment-insights.xlsx")
salm_local <- read_salm_file("salm-smoothed-sa2.csv")
```

## Data contract

Every observation records the source URL and file, byte count, SHA-256,
validated final transport URL, retrieval time, release/reference vintage,
parser and schema versions, classification, revision policy and attribution.

REOS percentages are returned as canonical decimal proportions. SALM quarter
labels become quarter-end dates, and publisher-defined unavailable values
remain explicit missing observations.

## Reproducibility

The raw cache verifies every hit, binds entries to full release identity,
serialises writers and applies configurable file-size, age, count and
aggregate-byte bounds. Persisting the release catalogue row and `raw_sha256`
provides an exact replay identity.

The default test suite is fully offline. Opt-in live validation and retained
performance evidence are documented under `inst/validation/`.

Installed documentation is available through:

```r
help(package = "readjsa")
```

Upstream authorship and the pinned MIT licence are documented under
`inst/UPSTREAM.md` and `inst/UPSTREAM_LICENSE.md`.
