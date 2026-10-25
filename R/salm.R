.quarter_end <- function(x) {
  start <- .excel_date(x)
  as.Date(vapply(start, function(date) {
    if (is.na(date)) return(NA_real_)
    as.numeric(seq(date, by = "1 month", length.out = 2L)[2L] - 1)
  }, numeric(1)), origin = "1970-01-01")
}

.salm_preamble <- function(path, geography) {
  lines <- readLines(path, n = 2L, warn = FALSE, encoding = "UTF-8")
  if (length(lines) != 2L) {
    .abort(
      "SALM file is missing its two-line publication preamble.",
      class = "readjsa_schema_error"
    )
  }
  note <- sub("^\ufeff?\"", "", lines[[1L]])
  note <- sub("\",,*\\r?$", "", note)
  expected_geography <- if (identical(geography, "sa2")) "SA2" else "LGA"
  if (
    !grepl(
      "Cells containing a dash \\(-\\) indicate that data are unavailable",
      note
    ) ||
      !grepl(expected_geography, note, fixed = TRUE) ||
      !grepl("ASGS", note, fixed = TRUE) ||
      nzchar(gsub("[,[:space:]\"]", "", lines[[2L]]))
  ) {
    .abort(
      "SALM ", toupper(geography),
      " preamble no longer defines dash availability and ASGS breaks.",
      class = "readjsa_schema_error"
    )
  }
  note
}

#' Parse an official Small Area Labour Markets CSV
#'
#' `read_salm_file()` reads one local SALM CSV and converts its wide quarterly
#' observations to long form.  The requested geography and ASGS vintage are
#' validated against the release headers, and the publisher's two-line
#' dash-availability/ASGS preamble is retained.  Historical ASGS vintages are
#' reported as published rather than silently relabelled.  The parser is
#' offline and does not write files or contact DEWR.
#'
#' @param path One existing local `.csv` path.  It may be a packaged fixture or
#'   a downloaded file; no network request is made.
#' @param geography One of `"sa2"` or `"lga"`; the default expression
#'   `c("sa2", "lga")` selects `"sa2"` through `match.arg()`.
#' @param provenance Optional named list of scalar provenance fields.  Allowed
#'   fields are `provider_id`, `dataset_id`, `family_id`, `release_id`,
#'   `reference_period`, `release_date`, `vintage_date`,
#'   `page_modified_date`, `retrieved_at`, `source_url`, `final_url`,
#'   `source_filename`, `page_url`, `raw_bytes`, `raw_sha256`,
#'   `parser_version`, `schema_version`, `revision_policy`, `terms_url`,
#'   `licence`, and `attribution`.  Parser identity, schema, provider, terms,
#'   licence, and attribution cannot be overridden.  The exact local byte
#'   count and SHA-256 are verified; observation fields cannot be replaced.
#'   The default `list()` supplies local-file defaults.
#' @return A base [`data.frame`][base::data.frame] in deterministic long-form
#'   order.  The observation columns, in order, are `date` (`Date`),
#'   `geography_type`, `geography_code`, `geography_name`, `data_item`, `unit`,
#'   `unit_multiplier` (numeric), `frequency`, `seasonal_adjustment`, `value`
#'   (numeric), `observation_status`, and `raw_value`, followed by
#'   `classification_version` and `file_notes`.  The final provenance columns
#'   are `provider_id`, `dataset_id`, `family_id`, `release_id`,
#'   `reference_period`, `release_date`, `vintage_date`,
#'   `page_modified_date`, `retrieved_at`, `source_url`, `final_url`,
#'   `source_filename`, `page_url`, `raw_bytes`, `raw_sha256`,
#'   `parser_version`, `schema_version`, `revision_policy`, `terms_url`,
#'   `licence`, and `attribution`.  Date fields are `Date`, `retrieved_at` is
#'   UTC `POSIXct`, `raw_bytes` is numeric, and all other metadata fields are
#'   character.  The three published data items are retained; dash values are
#'   represented by `observation_status = "unavailable"` and `NA` `value`, while
#'   not-published, not-applicable, missing, flagged, and published tokens remain
#'   distinguishable.  Original tokens are retained in `raw_value`.  The return
#'   value has no custom attributes.  Invalid files raise `readjsa_file_error`,
#'   `readjsa_schema_error`, or `readjsa_duplicate_error`; invalid provenance
#'   raises `readjsa_provenance_error` or `readjsa_url_error`.
#'
#' @examples
#' path <- system.file("extdata", "salm_sa2.csv", package = "readjsa")
#' observations <- read_salm_file(path, geography = "sa2")
#' stopifnot(is.data.frame(observations), nrow(observations) > 0L)
#' unique(observations$classification_version)
#'
#' @seealso [read_salm()], [jsa_release_manifest()], [fetch_jsa_release()]
#' @family SALM-parsers
#' @export
read_salm_file <- function(path, geography = c("sa2", "lga"),
                           provenance = list()) {
  .assert_file(path, "csv")
  geography <- match.arg(geography)
  file_notes <- .salm_preamble(path, geography)
  parsed_data <- suppressMessages(readr::read_csv(
    path,
    skip = 2L,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  ))
  parse_problems <- readr::problems(parsed_data)
  if (nrow(parse_problems)) {
    .abort(
      "SALM CSV contains malformed or truncated rows.",
      class = "readjsa_schema_error"
    )
  }
  data <- as.data.frame(parsed_data, stringsAsFactors = FALSE)
  original_names <- names(data)
  clean <- .clean_names(original_names)
  item_column <- which(clean == "data_item")
  name_pattern <- if (geography == "sa2") {
    "statistical_area_level_2.*sa2"
  } else {
    "local_government_area.*lga"
  }
  code_pattern <- if (geography == "sa2") "^sa2_code" else "^lga_code"
  name_column <- grep(name_pattern, clean)
  code_column <- grep(code_pattern, clean)
  if (length(item_column) != 1L || length(name_column) != 1L ||
    length(code_column) != 1L) {
    .abort(
      "SALM ", toupper(geography),
      " headers do not match the published three-identifier schema.",
      class = "readjsa_schema_error"
    )
  }
  header_text <- paste(original_names[c(name_column, code_column)],
    collapse = " "
  )
  vintage <- regmatches(
    header_text, regexpr("20\\d{2}\\s+ASGS", header_text, ignore.case = TRUE)
  )
  if (length(vintage) != 1L || !nzchar(vintage)) {
    .abort(
      "SALM geography header does not state an ASGS vintage.",
      class = "readjsa_schema_error"
    )
  }
  id_columns <- c(item_column, name_column, code_column)
  date_columns <- setdiff(seq_along(data), id_columns)
  dates <- .quarter_end(original_names[date_columns])
  if (!length(date_columns) || any(is.na(dates))) {
    .abort(
      "SALM contains unexpected non-quarter observation columns.",
      class = "readjsa_schema_error"
    )
  }
  expected_items <- c(
    "Smoothed unemployment (persons)",
    "Smoothed labour force (persons)",
    "Smoothed unemployment rate (%)"
  )
  data[[item_column]] <- trimws(as.character(data[[item_column]]))
  observed_items <- unique(data[[item_column]])
  if (!setequal(observed_items, expected_items)) {
    .abort(
      "SALM must contain exactly the three supported data items.",
      class = "readjsa_schema_error"
    )
  }
  geography_code <- trimws(as.character(data[[code_column]]))
  geography_name <- trimws(as.character(data[[name_column]]))
  if (
    anyNA(geography_code) || any(!nzchar(geography_code)) ||
      anyNA(geography_name) || any(!nzchar(geography_name))
  ) {
    .abort(
      "SALM geography codes and names must be complete.",
      class = "readjsa_schema_error"
    )
  }
  item_key <- paste(geography_code, data[[item_column]], sep = "\r")
  geography_map <- unique(data.frame(
    geography_code = geography_code,
    geography_name = geography_name,
    stringsAsFactors = FALSE
  ))
  if (
    anyDuplicated(item_key) ||
      anyDuplicated(geography_map$geography_code) ||
      any(table(geography_code) != length(expected_items))
  ) {
    .abort(
      "SALM must contain one complete three-item set per geography.",
      class = "readjsa_schema_error"
    )
  }
  pieces <- lapply(seq_along(date_columns), function(index) {
    parsed <- .parse_number_status(
      data[[date_columns[index]]],
      dash_status = "unavailable"
    )
    item <- data[[item_column]]
    data.frame(
      date = rep(dates[index], nrow(data)),
      geography_type = geography,
      geography_code = trimws(as.character(data[[code_column]])),
      geography_name = trimws(as.character(data[[name_column]])),
      data_item = item,
      unit = ifelse(grepl("rate", item, ignore.case = TRUE),
        "percent", "persons"
      ),
      unit_multiplier = 1,
      frequency = "Quarterly",
      seasonal_adjustment = "seasonal_adjustment_not_stated",
      value = parsed$value,
      observation_status = parsed$observation_status,
      raw_value = parsed$raw_value,
      stringsAsFactors = FALSE
    )
  })
  out <- .bind_rows(pieces)
  out$classification_version <- paste(
    toupper(geography), toupper(vintage)
  )
  out$file_notes <- file_notes
  family <- paste0("salm_", geography)
  provenance <- .local_file_provenance(path, "salm", family, provenance)
  .validate_reference_period(
    out, provenance, paste("SALM", toupper(geography))
  )
  out <- .attach_provenance(out, provenance)
  .check_duplicates(
    out,
    c("date", "geography_type", "geography_code", "data_item"),
    paste("SALM", toupper(geography))
  )
  out
}

#' Download and read Small Area Labour Markets releases
#'
#' `read_salm()` discovers or accepts SA2/LGA release rows, fetches and
#' verifies each CSV through the bounded raw cache, and delegates to
#' [read_salm_file()].  Pass a release catalogue and injected `transport` for
#' a deterministic offline workflow.  The default path performs HTTPS I/O only
#' to official DEWR hosts, writes cache files under `cache_dir`, and never reads
#' or stores credentials.
#'
#' @param geography `"all"` (the default), `"sa2"`, or `"lga"`.  A character
#'   vector of `"sa2"`/`"lga"` is also accepted; selected families are returned
#'   in the requested order.
#' @param release Optional release catalogue data frame, normally from
#'   [discover_jsa_releases()] with `dataset = "salm"`.  It must contain exactly one valid row
#'   for each selected geography.  If `NULL`, the current official catalogue is
#'   discovered online.
#' @param cache_dir One non-empty cache path, created if absent; defaults to
#'   [jsa_cache_dir()].
#' @param force One non-missing logical value.  If `TRUE`, ignore valid cache
#'   entries and fetch selected releases again.
#' @param transport A `(url, destination, timeout, max_bytes)` function for
#'   deterministic tests/offline fixtures; see [fetch_jsa_release()].
#' @return A base [`data.frame`][base::data.frame] formed by row-binding the
#'   selected [read_salm_file()] results.  It has the same 35 columns, classes,
#'   ASGS/vintage and observation-status invariants, provenance fields, and row
#'   ordering as the file parser.  If
#'   `READJSA_MEASUREMENT_CACHE_AUDIT=true`, it additionally has a
#'   `readjsa_raw_cache_hits` logical attribute with one value per fetched
#'   geography; otherwise it has no custom attributes.  Discovery, transport,
#'   cache, schema, duplicate, and provenance failures are propagated with the
#'   condition classes documented by [fetch_jsa_release()] and
#'   [read_salm_file()].
#'
#' @examples
#' fixture <- system.file("extdata", "salm_sa2.csv", package = "readjsa")
#' release <- data.frame(
#'   provider_id = "dewr", dataset_id = "salm", family_id = "salm_sa2",
#'   release_id = "salm/salm_sa2/example",
#'   reference_period = as.Date("2026-03-31"),
#'   release_date = as.Date("2026-07-22"),
#'   vintage_date = as.Date("2026-07-22"),
#'   page_modified_date = as.Date("2026-07-22"),
#'   source_url = "https://www.dewr.gov.au/downloads/salm_sa2.csv",
#'   source_filename = "salm_sa2.csv",
#'   page_url = "https://www.dewr.gov.au/employment-research/small-area-labour-markets",
#'   terms_url = "https://www.dewr.gov.au/copyright",
#'   licence = "CC BY 4.0 subject to stated exclusions",
#'   attribution = "© Commonwealth of Australia",
#'   stringsAsFactors = FALSE
#' )
#' transport <- function(url, destination, timeout, max_bytes) {
#'   stopifnot(file.copy(fixture, destination, overwrite = TRUE))
#'   invisible(destination)
#' }
#' observations <- read_salm(
#'   geography = "sa2", release = release,
#'   cache_dir = tempfile("readjsa-salm-example-"), transport = transport
#' )
#' stopifnot(nrow(observations) > 0L, all(observations$family_id == "salm_sa2"))
#'
#' @seealso [read_salm_file()], [discover_jsa_releases()], [fetch_jsa_release()]
#' @family SALM-readers
#' @export
read_salm <- function(
    geography = "all",
    release = NULL,
    cache_dir = jsa_cache_dir(),
    force = FALSE,
    transport = .default_transport) {
  geographies <- if (identical(geography, "all")) c("sa2", "lga") else {
    match.arg(geography, c("sa2", "lga"), several.ok = TRUE)
  }
  families <- paste0("salm_", geographies)
  if (is.null(release)) release <- discover_jsa_releases("salm")
  release <- .select_release_rows(release, "salm", families)
  records <- lapply(seq_len(nrow(release)), function(index) {
    snapshot <- fetch_jsa_release(
      release[index, , drop = FALSE],
      cache_dir = cache_dir,
      force = force,
      transport = transport
    )
    provenance <- .snapshot_provenance(snapshot)
    list(
      observations = read_salm_file(
        snapshot$path, sub("^salm_", "", snapshot$family_id),
        provenance = provenance
      ),
      provenance = provenance
    )
  })
  .attach_raw_cache_audit(
    .bind_rows(lapply(records, function(record) record$observations)),
    lapply(records, function(record) record$provenance)
  )
}
