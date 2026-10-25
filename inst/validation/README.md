# readjsa stage-3 validation — 27 July 2026

## Personal-use certification

The functional and source-evidence gate is certified: the package suite and
source check pass, all nine current official files completed
download-then-SHA-256 replay, exact output parity is retained, the independent
workbook-cell checks matched, and both committed evidence charts passed review.
The certified personal workflow reads one explicit family at a time. Local
batch callers can also materialise every family with `tables = "all"` on a
suitably provisioned machine. The retained performance matrix records the
resource envelope for future hosted deployment work.

This evidence was generated from the current official JSA and DEWR files.
All nine files were first downloaded and then replayed as SHA-256-verified
cache hits. Current effective URLs remained on the original official hosts.

## Replay result

| Dataset | Observations | Source files | Parsed period |
|---|---:|---:|---|
| IVI | 2,531,244 | 6 | 2006-01-01 to 2026-06-01 |
| REOS | 6,075 | 1 | 2020-04-01 to 2026-06-01 |
| SALM | 535,680 | 2 | 2010-12-31 to 2026-03-31 |

REOS values are canonical proportions in `[0, 1]`; the observed range is
`0` to `0.86`.

## Performance diagnosis and current block

The earlier quiet run measured the old all-nine-family validator as one batch:
the cold run took 29.734 seconds and peaked at 3,772.53 MiB; warm runs took
25.510–28.905 seconds and peaked as high as 4,046.59 MiB. That harness retained
IVI (2,531,244 rows), REOS (6,075 rows), and SALM (535,680 rows) together and
then re-sourced the validator in the same R process. It correctly exposed an
unsafe batch, but it does not represent the app's independent family routes
and cannot provide the required per-route cold/warm tables.

The validator now releases each family before reading the next. The measurement
harness is serialized and requires at least three independent empty-cache cold
runs and three verified-cache warm runs for every family. Every run uses a
fresh R process and records latency, worker baseline RSS, absolute peak RSS,
peak increment, and a projection from the measured 416 MiB deployed-app idle
baseline.

### Compliant quiet-slot matrix

The independently validated quiet-slot run contains 54 successful rows:
exactly nine families × cold/warm × three repetitions. Each complete
nine-family pass returns 3,072,999 observations. Every cold row records exactly
one raw cache release, zero hits, and one miss (`1/0/1`); every warm row
records exactly one release, one hit, and zero misses (`1/1/0`).

The worst single measurement took `4.369` seconds. Absolute process RSS peaked
at `1438.3125 MiB`; the largest read increment was `1349.046875 MiB`, giving a
projected deployed peak of `416 + 1349.046875 = 1765.046875 MiB`.

Against the stage-3 limits of p95 latency no greater than 1.5 seconds and
projected deployed peak no greater than 1024 MiB, 10 of the 18 family/mode
groups fail at least one limit:

- latency fails for both cold and warm groups of `anzsco2_gccsa_sa4`,
  `anzsco2_ivi_regions`, `anzsco2_states`, `anzsco4_states`, and `salm_sa2`;
- memory fails for both cold and warm groups of `anzsco2_ivi_regions`,
  `anzsco2_states`, `anzsco4_states`, and `salm_sa2`.

The exact durable evidence is `quiet-slot-performance-raw.csv` and
`quiet-slot-performance-summary.csv`. The summary is byte-identical to the
source-run summary. The raw evidence retains the source-run UTC timestamps and
changes only `log_path`, from a private absolute run path to
`logs/<basename>`.

- original source-run raw SHA-256:
  `cff01b2b0709330b034ba74cbbd2a5f91d0bfb34423e3c00fc3c0a3bfe4dfb8c`;
- durable sanitized raw SHA-256:
  `cc302deb1d75dd0ec2312f9306804750bbb247931630f3bb3db1cf499ba29a40`;
- byte-identical summary SHA-256:
  `f0d8e3752d2c4d1e89b7be8ec961ecb1567af8ee3fe9a5e2065fb586ce9ce32e`.

The following evidence is a single-sample retained-cache diagnostic, not a
benchmark and not a performance PASS:

| Family | Rows | Returned MiB | Before peak MiB | After peak MiB | Projected deployed peak MiB |
|---|---:|---:|---:|---:|---:|
| `skill_states` | 53,136 | 18.66 | 303.05 | 210.83 | 539.09 |
| `anzsco4_states` | 788,364 | 252.12 | 1,088.50 | 959.73 | 1,289.61 |
| `anzsco2_states` | 504,792 | 173.81 | 649.05 | 571.94 | 901.38 |
| `anzsco2_ivi_regions` | 818,292 | 259.19 | 1,115.81 | 1,031.12 | 1,355.88 |
| `anzsco2_gccsa_sa4` | 256,500 | 80.54 | 482.44 | 447.09 | 772.80 |
| `anzsco4_remoteness_northern` | 110,160 | 34.87 | 307.84 | 267.12 | 593.33 |
| `reos_tables` | 6,075 | 1.79 | 139.89 | 140.34 | 468.95 |
| `salm_sa2` | 434,496 | 117.65 | 543.69 | 510.88 | 838.72 |
| `salm_lga` | 101,184 | 28.24 | 290.81 | 272.48 | 598.28 |

Rows, columns, returned object sizes, raw SHA-256 values, and date bounds are
identical before and after the parity-safe reshape/duplicate-check changes.
The exact evidence is in `performance-diagnostic-before.csv` and
`performance-diagnostic-after.csv`.

The remaining limit is architectural, not a harness artefact. A direct
`readxl` read of only `A1:IM100` from the 34 MiB uncompressed
`anzsco4_states` worksheet XML peaked at 490.75 MiB before the 252.12 MiB
returned observation object was built. Phase profiling of the full route
reached 636 MiB after sheet extraction, 781 MiB while binding, 868 MiB during
duplicate validation, and 894 MiB after provenance attachment. The final
quiet-slot matrix reaches a projected `1765.046875 MiB`. Remediation must
provide streaming or another bounded equivalent for all four memory-failing
families: `anzsco4_states`, `anzsco2_states`, `anzsco2_ivi_regions`, and
`salm_sa2`. It must also bring all five latency-failing families below the p95
limit, including `anzsco2_gccsa_sa4`. The architecture finding remains valid,
but the personal-use disposition is
`ACCEPTED_PERSONAL_SOURCE_SPECIFIC_WITH_MEMORY_WARNING`. This acceptance does
not relabel the latency or memory measurements as passes.

## Independent workbook-cell cross-checks

- IVI `skill_states`, Australia trend, June 2026: direct workbook `205057.541`; parsed `205057.541`.
- REOS table 1.1, all employers, June 2026: direct workbook `0.51`; parsed `0.51` (51%).

The REOS page states only a page-modified date (2026-07-21), so
`release_date` and `vintage_date` remain missing. IVI records the explicit
2026-07-22 release date; SALM records 2026-06-30.

## Exact official snapshots

| Family | Bytes | SHA-256 | Final URL |
|---|---:|---|---|
| `skill_states` |   440,452 | `5bacad1cf2fbef22b1f802e3fc3d8c5051038c31ff5839a2af7e8db10e8185fd` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco_skill_level_states_and_territories_-_june_2026.xlsx) |
| `anzsco4_states` | 5,185,708 | `93d9cf223a91fccc691a34e857b7eb1c898b71dd59df173be6ca4cb5d9b51031` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco4_occupations_states_and_territories_-_june_2026.xlsx) |
| `anzsco4_remoteness_northern` |   586,327 | `c8478d72b3cb1010cc2d1665acd83de3145485015a6be2f824c96d94ba69d8f7` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco4_occupations_jsa_remoteness_and_northern_australia_classification_-_june_2026.xlsx) |
| `anzsco2_states` | 3,898,024 | `6b33e489e0fe7910230cd271c0513a9c9f7444eb883b39cb9f8a5fec37c3b80d` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco2_occupations_states_and_territories_-_june_2026.xlsx) |
| `anzsco2_ivi_regions` | 5,199,449 | `96e2c38e4843e793b027153f416d6d71c81e81c256432ca52324de50fe5e5f60` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco2_occupations_ivi_regions_-_june_2026.xlsx) |
| `anzsco2_gccsa_sa4` | 1,360,701 | `946f934f0f8f71d9ee284ea65a0dc146b86574aba9b4e747826148772b3ea53a` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/internet_vacancies_anzsco2_occupations_gccsa_and_sa4_regions_-_june_2026.xlsx) |
| `reos_tables` |   193,638 | `e1f8d3f80461f4b430843ca5066c7a5f9d27cfe85d81880091ee0c7aa82b57f5` | [official file](https://www.jobsandskills.gov.au/sites/default/files/2026-07/recruitment_insights_report_-_june_2026_data_file.xlsx) |
| `salm_sa2` | 2,438,006 | `e05f45d9cf5e38abcc55c8925afbb988697374ffb473d5dcb20ef57573c6c517` | [official file](https://www.dewr.gov.au/download/17068/salm-smoothed-sa2-datafiles-asgs-2021-march-quarter-2026/43117/salm-smoothed-sa2-datafiles-asgs-2021-march-quarter-2026/csv) |
| `salm_lga` |   600,309 | `eb09876cde89ab2413b981bfc661a6cd0276ab46f5b1274972ee8f3c22228420` | [official file](https://www.dewr.gov.au/download/17069/salm-smoothed-lga-datafiles-asgs-2025-march-quarter-2026/43119/salm-smoothed-lga-datafiles-asgs-2025-march-quarter-2026/csv) |

## Upstream and terms

The audited upstream pin is
`94f063c92c7634c35e09e58269900064ce09f186`; the public upstream `main`
ref resolved to that exact commit during this validation. The preserved
upstream licence SHA-256 is
`0df6b8b14adafc47bd12f1b19effb217fb0ef236bda41a7dedd40d145362ecad`.

Official data attribution: © Commonwealth of Australia. See each returned
`terms_url`; website material is recorded as CC BY 4.0 subject to stated
exclusions.

See `performance-status.csv` for the machine-readable gate classification.
