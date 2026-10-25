# Offline parser fixtures

These are compact synthetic workbooks and CSV/HTML files that reproduce the
publisher-owned schemas observed in the June 2026 IVI and REOS releases and the
March-quarter 2026 SALM release. They contain no copied production observations.

The SALM fixtures reproduce the publisher's dash-availability/ASGS preamble
contract. The REOS fixture includes synthetic workbook notes, semantic table
title/definition rows, and the official mixed storage convention where ordinary
proportions are numeric decimals and flagged proportions are text percentages.
`reos_bad_scale.xlsx` is the fail-closed out-of-range companion.

`salm_lga_asgs2016.csv` reproduces the historical classification-header drift
that the parser must preserve instead of relabelling as the current ASGS
vintage. Run `generate-fixtures.R` from this directory to regenerate the XLSX
files with `writexl`.
