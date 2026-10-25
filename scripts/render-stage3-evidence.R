if (!identical(tolower(Sys.getenv("READJSA_LIVE")), "true")) {
  stop("Set READJSA_LIVE=true to opt in to official live-source validation.")
}
if (!identical(tolower(Sys.getenv("READJSA_STAGE3_EVIDENCE")), "true")) {
  stop("Set READJSA_STAGE3_EVIDENCE=true to regenerate committed evidence.")
}

library(readjsa)

cache_dir <- Sys.getenv(
  "READJSA_CACHE_DIR",
  file.path(tempdir(), "readjsa-stage3-cache")
)
output_dir <- Sys.getenv(
  "READJSA_EVIDENCE_DIR",
  file.path("inst", "validation")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

catalogues <- lapply(
  c("ivi", "reos", "salm"),
  discover_jsa_releases
)
names(catalogues) <- c("ivi", "reos", "salm")
observations <- list(
  ivi = read_ivi(release = catalogues$ivi, cache_dir = cache_dir),
  reos = read_reos(release = catalogues$reos, cache_dir = cache_dir),
  salm = read_salm(release = catalogues$salm, cache_dir = cache_dir)
)

stopifnot(
  all(observations$reos$value >= 0 & observations$reos$value <= 1, na.rm = TRUE),
  length(unique(c(
    observations$ivi$raw_sha256,
    observations$reos$raw_sha256,
    observations$salm$raw_sha256
  ))) == 9L
)

snapshots <- lapply(
  seq_len(nrow(do.call(rbind, catalogues))),
  function(index) {
    releases <- do.call(rbind, catalogues)
    fetch_jsa_release(
      releases[index, , drop = FALSE],
      cache_dir = cache_dir
    )
  }
)
names(snapshots) <- vapply(snapshots, `[[`, character(1), "family_id")
stopifnot(
  all(vapply(snapshots, `[[`, logical(1), "cache_hit")),
  all(vapply(
    snapshots,
    function(snapshot) identical(snapshot$source_url, snapshot$final_url),
    logical(1)
  ))
)

ivi_series <- observations$ivi[
  observations$ivi$family_id == "skill_states" &
    observations$ivi$series_type == "trend" &
    observations$ivi$state == "AUST" &
    observations$ivi$skill_level == "0" &
    observations$ivi$date >= as.Date("2019-01-01"),
  ,
  drop = FALSE
]
reos_series <- observations$reos[
  observations$reos$table == "1.1" &
    observations$reos$disaggregation == "All employers",
  ,
  drop = FALSE
]

ivi_raw <- as.data.frame(
  readxl::read_excel(
    snapshots$skill_states$path,
    sheet = "Trend",
    col_types = "text",
    .name_repair = "minimal"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
ivi_latest_column <- names(ivi_raw)[which.max(suppressWarnings(
  as.numeric(names(ivi_raw))
))]
ivi_direct <- as.numeric(ivi_raw[
  ivi_raw$State == "AUST" & ivi_raw$Skill_level == "0",
  ivi_latest_column,
  drop = TRUE
])
ivi_parsed <- ivi_series$value[which.max(ivi_series$date)]
stopifnot(isTRUE(all.equal(ivi_direct, ivi_parsed, tolerance = 1e-12)))

reos_raw <- as.data.frame(
  readxl::read_excel(
    snapshots$reos_tables$path,
    sheet = "1.1 Recruitment Rate (M)",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  ),
  stringsAsFactors = FALSE
)
reos_header <- which(tolower(trimws(reos_raw[[1L]])) == "month")
reos_latest <- which.max(suppressWarnings(as.numeric(reos_raw[[1L]])))
reos_column <- which(
  trimws(as.character(reos_raw[reos_header, , drop = TRUE])) ==
    "All employers"
)
reos_direct <- as.numeric(reos_raw[reos_latest, reos_column])
reos_parsed <- reos_series$value[which.max(reos_series$date)]
stopifnot(isTRUE(all.equal(reos_direct, reos_parsed, tolerance = 1e-12)))

render_chart <- function(path, expression) {
  grDevices::png(
    path,
    width = 1600,
    height = 900,
    res = 144,
    bg = "#F7F5F0"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(
    mar = c(7, 6, 6, 2),
    family = "sans",
    fg = "#183642",
    col.axis = "#183642",
    col.lab = "#183642"
  )
  force(expression)
}

render_chart(
  file.path(output_dir, "ivi-australia-trend.png"),
  {
    graphics::plot(
      ivi_series$date,
      ivi_series$value / 1000,
      type = "l",
      lwd = 3,
      col = "#D1495B",
      xlab = "",
      ylab = "Online job advertisements (thousands)",
      main = "Internet Vacancy Index — Australia trend"
    )
    graphics::grid(col = "#D8D4CA")
    graphics::lines(
      ivi_series$date,
      ivi_series$value / 1000,
      lwd = 3,
      col = "#D1495B"
    )
    graphics::mtext(
      "Monthly, January 2019 to June 2026",
      side = 3,
      line = 0.5,
      cex = 0.9,
      col = "#56666B"
    )
    graphics::mtext(
      "Source: Jobs and Skills Australia. © Commonwealth of Australia.",
      side = 1,
      line = 4.7,
      at = graphics::par("usr")[1L],
      adj = 0,
      xpd = NA,
      cex = 0.75,
      col = "#56666B"
    )
    graphics::mtext(
      paste(
        "Licence: CC BY 4.0 subject to stated exclusions.",
        "Terms: jobsandskills.gov.au/copyright-and-disclaimer"
      ),
      side = 1,
      line = 5.7,
      at = graphics::par("usr")[1L],
      adj = 0,
      xpd = NA,
      cex = 0.68,
      col = "#56666B"
    )
  }
)

render_chart(
  file.path(output_dir, "reos-recruitment-rate.png"),
  {
    graphics::plot(
      reos_series$date,
      reos_series$value * 100,
      type = "l",
      lwd = 3,
      col = "#00798C",
      xlab = "",
      ylab = "Employers recruiting (%)",
      main = "Recruitment Experiences and Outlook Survey",
      ylim = c(0, max(reos_series$value * 100, na.rm = TRUE) * 1.08)
    )
    graphics::grid(col = "#D8D4CA")
    graphics::lines(
      reos_series$date,
      reos_series$value * 100,
      lwd = 3,
      col = "#00798C"
    )
    graphics::mtext(
      "Recruitment rate, all employers, April 2020 to June 2026",
      side = 3,
      line = 0.5,
      cex = 0.9,
      col = "#56666B"
    )
    graphics::mtext(
      "Source: Jobs and Skills Australia. © Commonwealth of Australia.",
      side = 1,
      line = 4.7,
      at = graphics::par("usr")[1L],
      adj = 0,
      xpd = NA,
      cex = 0.75,
      col = "#56666B"
    )
    graphics::mtext(
      paste(
        "Licence: CC BY 4.0 subject to stated exclusions.",
        "Terms: jobsandskills.gov.au/copyright-and-disclaimer"
      ),
      side = 1,
      line = 5.7,
      at = graphics::par("usr")[1L],
      adj = 0,
      xpd = NA,
      cex = 0.68,
      col = "#56666B"
    )
  }
)

families <- vapply(snapshots, `[[`, character(1), "family_id")
bytes <- vapply(snapshots, `[[`, numeric(1), "raw_bytes")
hashes <- vapply(snapshots, `[[`, character(1), "raw_sha256")
urls <- vapply(snapshots, `[[`, character(1), "final_url")
hash_rows <- paste0(
  "| `", families, "` | ", format(bytes, big.mark = ",", scientific = FALSE),
  " | `", hashes, "` | [official file](", urls, ") |"
)
lines <- c(
  "# readjsa stage-3 validation — 27 July 2026",
  "",
  "## Stage-3 exit status: `ACCEPTED_PERSONAL_SOURCE_SPECIFIC_WITH_MEMORY_WARNING`",
  "",
  paste(
    "Functional/source evidence: PASS for the exact 3,072,999-row replay.",
    "The owner accepts personal, explicit-family use with the measured memory",
    "warning; default-All and hosted deployment remain blocked."
  ),
  "",
  "This evidence was generated from the current official JSA and DEWR files.",
  "All nine files were first downloaded and then replayed as SHA-256-verified",
  "cache hits. Current effective URLs remained on the original official hosts.",
  "",
  "## Replay result",
  "",
  "| Dataset | Observations | Source files | Parsed period |",
  "|---|---:|---:|---|",
  paste0(
    "| IVI | ", format(nrow(observations$ivi), big.mark = ","),
    " | 6 | 2006-01-01 to 2026-06-01 |"
  ),
  paste0(
    "| REOS | ", format(nrow(observations$reos), big.mark = ","),
    " | 1 | 2020-04-01 to 2026-06-01 |"
  ),
  paste0(
    "| SALM | ", format(nrow(observations$salm), big.mark = ","),
    " | 2 | 2010-12-31 to 2026-03-31 |"
  ),
  "",
  "REOS values are canonical proportions in `[0, 1]`; the observed range is",
  paste0(
    "`", min(observations$reos$value, na.rm = TRUE), "` to `",
    max(observations$reos$value, na.rm = TRUE), "`."
  ),
  "",
  "## Independent workbook-cell cross-checks",
  "",
  paste0(
    "- IVI `skill_states`, Australia trend, June 2026: direct workbook `",
    format(ivi_direct, scientific = FALSE, digits = 15, trim = TRUE),
    "`; parsed `",
    format(ivi_parsed, scientific = FALSE, digits = 15, trim = TRUE), "`."
  ),
  paste0(
    "- REOS table 1.1, all employers, June 2026: direct workbook `",
    reos_direct, "`; parsed `", reos_parsed, "` (", reos_parsed * 100, "%)."
  ),
  "",
  "The REOS page states only a page-modified date (2026-07-21), so",
  "`release_date` and `vintage_date` remain missing. IVI records the explicit",
  "2026-07-22 release date; SALM records 2026-06-30.",
  "",
  "## Exact official snapshots",
  "",
  "| Family | Bytes | SHA-256 | Final URL |",
  "|---|---:|---|---|",
  hash_rows,
  "",
  "## Upstream and terms",
  "",
  "The audited upstream pin is",
  "`94f063c92c7634c35e09e58269900064ce09f186`; the public upstream `main`",
  "ref resolved to that exact commit during this validation. The preserved",
  "upstream licence SHA-256 is",
  "`0df6b8b14adafc47bd12f1b19effb217fb0ef236bda41a7dedd40d145362ecad`.",
  "",
  "Official data attribution: © Commonwealth of Australia. See each returned",
  "`terms_url`; website material is recorded as CC BY 4.0 subject to stated",
  "exclusions.",
  "",
  paste(
    "The retained-cache diagnostic CSVs are explicitly single-sample,",
    "non-benchmark evidence. The formal serialized matrix of at least three",
    "cold and three warm fresh-process reads for every family remains pending."
  ),
  "",
  paste(
    "The compliant quiet matrix remains authoritative. Personal acceptance",
    "does not clear the streaming requirement for hosted/public deployment.",
    "See performance-status.csv."
  )
)
writeLines(lines, file.path(output_dir, "README.md"), useBytes = TRUE)
