.reos_information <- function(path) {
  raw <- as.data.frame(
    .read_excel_text(path, "Information", col_names = FALSE),
    stringsAsFactors = FALSE
  )
  normalized <- matrix(
    .clean_names(unlist(raw, use.names = FALSE)),
    nrow = nrow(raw), ncol = ncol(raw)
  )
  header_hits <- matrix(
    normalized %in% c("series", "frequency", "disaggregations", "table"),
    nrow = nrow(normalized), ncol = ncol(normalized)
  )
  header_row <- which(rowSums(header_hits) >= 4L)
  if (length(header_row) != 1L) {
    .abort(
      "REOS Information sheet must contain exactly one metadata header row.",
      class = "readjsa_schema_error"
    )
  }
  header <- as.character(raw[header_row, ])
  names_map <- .clean_names(header)
  selected <- match(
    c("series", "frequency", "disaggregations", "table"), names_map
  )
  data <- raw[(header_row + 1L):nrow(raw), selected, drop = FALSE]
  names(data) <- c("series", "frequency", "disaggregations", "table")
  data <- data[!is.na(data$table) & trimws(data$table) != "", , drop = FALSE]
  data$table <- sub("^Table\\s+", "", trimws(data$table), ignore.case = TRUE)
  if (!nrow(data) || anyDuplicated(data$table)) {
    .abort(
      "REOS metadata table identifiers are empty or duplicated.",
      class = "readjsa_schema_error"
    )
  }
  notes <- trimws(as.character(unlist(
    raw[seq_len(header_row - 1L), , drop = FALSE],
    use.names = FALSE
  )))
  notes <- unique(notes[!is.na(notes) & nzchar(notes)])
  attr(data, "workbook_notes") <- paste(notes, collapse = "\n")
  data
}

.reos_table <- function(path, sheet, metadata) {
  raw <- as.data.frame(
    .read_excel_text(path, sheet, col_names = FALSE),
    stringsAsFactors = FALSE
  )
  first <- trimws(as.character(raw[[1L]]))
  header_row <- which(tolower(first) %in% c("month", "quarter"))
  if (length(header_row) != 1L) {
    .abort(
      "REOS sheet `", sheet,
      "` must contain exactly one Month or Quarter data header.",
      class = "readjsa_schema_error"
    )
  }
  expected_frequency <- tolower(metadata$frequency)
  actual_frequency <- tolower(first[header_row])
  if (!startsWith(expected_frequency, actual_frequency)) {
    .abort(
      "REOS sheet `", sheet, "` frequency conflicts with Information metadata.",
      class = "readjsa_schema_error"
    )
  }
  preamble <- first[seq_len(header_row - 1L)]
  preamble <- preamble[!is.na(preamble) & nzchar(preamble)]
  preamble <- preamble[
    !grepl("^Recruitment Experiences and Outlook Survey", preamble)
  ]
  if (length(preamble) < 2L) {
    .abort(
      "REOS sheet `", sheet, "` is missing its title or definition.",
      class = "readjsa_schema_error"
    )
  }
  table_title <- preamble[[1L]]
  table_definition <- paste(preamble[-1L], collapse = " ")
  labels <- trimws(as.character(raw[header_row, -1L, drop = TRUE]))
  keep <- !is.na(labels) & labels != ""
  if (!any(keep) || anyDuplicated(labels[keep])) {
    .abort(
      "REOS sheet `", sheet,
      "` has empty or duplicated observation labels.",
      class = "readjsa_schema_error"
    )
  }
  columns <- which(keep) + 1L
  groups <- if (header_row > 1L) {
    trimws(as.character(raw[header_row - 1L, columns, drop = TRUE]))
  } else {
    rep(NA_character_, length(columns))
  }
  current <- "overall"
  for (index in seq_along(groups)) {
    if (!is.na(groups[index]) && groups[index] != "") current <- groups[index]
    groups[index] <- current
  }
  dates <- .excel_date(raw[(header_row + 1L):nrow(raw), 1L, drop = TRUE])
  date_raw <- trimws(as.character(
    raw[(header_row + 1L):nrow(raw), 1L, drop = TRUE]
  ))
  rows <- !is.na(dates)
  if (!any(rows)) {
    .abort(
      "REOS sheet `", sheet, "` has no observation rows.",
      class = "readjsa_schema_error"
    )
  }
  unexplained <- !rows & !is.na(date_raw) & date_raw != ""
  last_observation <- if (any(rows)) max(which(rows)) else 0L
  embedded <- unexplained & seq_along(unexplained) <= last_observation
  if (any(embedded)) {
    .abort(
      "REOS sheet `", sheet,
      "` contains an unrecognised token inside the observation date range.",
      class = "readjsa_schema_error"
    )
  }
  table_notes <- paste(date_raw[unexplained], collapse = " | ")
  if (!nzchar(table_notes)) table_notes <- NA_character_
  dates <- dates[rows]
  pieces <- lapply(seq_along(columns), function(index) {
    parsed <- .parse_number_status(
      raw[(header_row + 1L):nrow(raw), columns[index], drop = TRUE][rows],
      percent_as_proportion = TRUE
    )
    if (any(parsed$value < 0 | parsed$value > 1, na.rm = TRUE)) {
      .abort(
        "REOS sheet `", sheet,
        "` contains a value outside the canonical proportion range [0, 1].",
        class = "readjsa_schema_error"
      )
    }
    data.frame(
      date = dates,
      table = metadata$table,
      series = metadata$series,
      frequency = metadata$frequency,
      unit = "proportion",
      unit_multiplier = 1,
      seasonal_adjustment = "not_seasonally_adjusted",
      disaggregation_group = groups[index],
      disaggregation = labels[keep][index],
      table_title = table_title,
      table_definition = table_definition,
      table_notes = table_notes,
      value = parsed$value,
      observation_status = parsed$observation_status,
      raw_value = parsed$raw_value,
      stringsAsFactors = FALSE
    )
  })
  .bind_rows(pieces)
}

#' Parse a Recruitment Experiences and Outlook Survey workbook
#'
#' `read_reos_file()` reads one local official REOS workbook and discovers its
#' metadata and data header rows from semantic labels.  It preserves workbook
#' interpretation notes, table title/definition, disaggregation groups and
#' labels, frequency, seasonal basis, and footnotes while converting each
#' selected table to long observations.  The parser is offline and does not
#' write files or contact the publisher.  It validates the Information-sheet
#' contract, table/sheet identity, observation dates, proportion bounds, and
#' duplicate keys before returning.
#'
#' @param path One existing local `.xlsx` path.  It may be a packaged fixture
#'   or a downloaded file; no network request is made.
#' @param tables `"all"` (the default) or one or more table identifiers exactly
#'   as listed in the workbook Information sheet, for example `"1.1"`.  An
#'   empty selection and unknown identifiers are rejected.
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
#'   order.  The observation columns, in order, are `date` (`Date`), `table`,
#'   `series`, `frequency`, `unit`, `unit_multiplier` (numeric),
#'   `seasonal_adjustment`, `disaggregation_group`, `disaggregation`,
#'   `table_title`, `table_definition`, `table_notes`, `value` (numeric),
#'   `observation_status`, and `raw_value`, followed by
#'   `classification_version` and `workbook_notes`.  The final provenance
#'   columns are `provider_id`, `dataset_id`, `family_id`, `release_id`,
#'   `reference_period`, `release_date`, `vintage_date`,
#'   `page_modified_date`, `retrieved_at`, `source_url`, `final_url`,
#'   `source_filename`, `page_url`, `raw_bytes`, `raw_sha256`,
#'   `parser_version`, `schema_version`, `revision_policy`, `terms_url`,
#'   `licence`, and `attribution`.  Date fields are `Date`, `retrieved_at` is
#'   UTC `POSIXct`, `raw_bytes` is numeric, and all other metadata fields are
#'   character.  Percent values are normalised to proportions in `[0, 1]`;
#'   published, suppressed, not-published, not-applicable, missing, and flagged
#'   states remain distinguishable in `observation_status`, with original tokens
#'   retained in `raw_value`.  The return value has no custom attributes.
#'   Invalid files raise
#'   `readjsa_file_error`, `readjsa_schema_error`, or `readjsa_duplicate_error`;
#'   invalid provenance raises `readjsa_provenance_error` or
#'   `readjsa_url_error`.
#'
#' @examples
#' path <- system.file("extdata", "reos_current.xlsx", package = "readjsa")
#' observations <- read_reos_file(path, tables = "1.1")
#' stopifnot(is.data.frame(observations), nrow(observations) > 0L)
#' unique(observations$unit)
#'
#' @seealso [read_reos()], [discover_jsa_releases()], [fetch_jsa_release()]
#' @family REOS-parsers
#' @export
read_reos_file <- function(path, tables = "all", provenance = list()) {
  .assert_file(path, "xlsx")
  sheets <- .excel_sheets(path)
  if (!"Information" %in% sheets) {
    .abort("REOS workbook has no Information sheet.",
      class = "readjsa_schema_error"
    )
  }
  metadata <- .reos_information(path)
  workbook_notes <- attr(metadata, "workbook_notes")
  if (
    length(workbook_notes) != 1L ||
      is.na(workbook_notes) ||
      !grepl("not seasonally adjusted", workbook_notes, ignore.case = TRUE)
  ) {
    .abort(
      "REOS Information sheet no longer states the seasonal-adjustment basis.",
      class = "readjsa_schema_error"
    )
  }
  data_sheets <- setdiff(sheets, "Information")
  sheet_codes <- sub("\\s.*$", "", data_sheets)
  if (anyDuplicated(sheet_codes)) {
    .abort("REOS workbook has duplicate table sheet identifiers.",
      class = "readjsa_schema_error"
    )
  }
  missing_sheets <- setdiff(metadata$table, sheet_codes)
  unknown_sheets <- setdiff(sheet_codes, metadata$table)
  if (length(missing_sheets) || length(unknown_sheets)) {
    .abort(
      "REOS Information/sheet mismatch. Missing: ",
      paste(missing_sheets, collapse = ", "), "; unknown: ",
      paste(unknown_sheets, collapse = ", "),
      class = "readjsa_schema_error"
    )
  }
  if (!identical(tables, "all")) {
    if (
      !is.character(tables) ||
        !length(tables) ||
        anyNA(tables) ||
        any(!nzchar(trimws(tables)))
    ) {
      .abort("`tables` must select at least one named REOS table.")
    }
    invalid <- setdiff(tables, metadata$table)
    if (length(invalid)) .abort("Unknown REOS table(s): ",
      paste(invalid, collapse = ", ")
    )
    metadata <- metadata[metadata$table %in% tables, , drop = FALSE]
  }
  out <- .bind_rows(lapply(seq_len(nrow(metadata)), function(index) {
    code <- metadata$table[index]
    sheet <- data_sheets[match(code, sheet_codes)]
    .reos_table(path, sheet, metadata[index, , drop = FALSE])
  }))
  out$classification_version <-
    "REOS release-vintage workbook disaggregation labels"
  out$workbook_notes <- workbook_notes
  provenance <- .local_file_provenance(
    path, "reos", "reos_tables", provenance
  )
  .validate_reference_period(out, provenance, "REOS")
  out <- .attach_provenance(out, provenance)
  .check_duplicates(
    out,
    c("date", "table", "disaggregation_group", "disaggregation"),
    "REOS"
  )
  out
}

#' Download and read a Recruitment Experiences and Outlook Survey release
#'
#' `read_reos()` discovers or accepts one REOS release, fetches and verifies it
#' through the bounded raw cache, then delegates to [read_reos_file()].  Pass a
#' release row and injected `transport` for a deterministic offline workflow.
#' The default path performs HTTPS I/O only to official JSA hosts, writes cache
#' files under `cache_dir`, and never reads or stores credentials.
#'
#' @param tables `"all"` (the default) or one or more REOS table identifiers
#'   such as `"1.1"`; identifiers must be present in the workbook.
#' @param release Optional one-row release catalogue data frame, normally from
#'   [discover_jsa_releases()] with `dataset = "reos"`.  If `NULL`, the current official REOS
#'   catalogue is discovered online.
#' @param cache_dir One non-empty cache path, created if absent; defaults to
#'   [jsa_cache_dir()].
#' @param force One non-missing logical value.  If `TRUE`, ignore a valid cache
#'   entry and fetch the release again.
#' @param transport A `(url, destination, timeout, max_bytes)` function for
#'   deterministic tests/offline fixtures; see [fetch_jsa_release()].
#' @return A base [`data.frame`][base::data.frame] with the same 38 columns,
#'   classes, proportion/status invariants, provenance fields, and row ordering
#'   as [read_reos_file()].  If `READJSA_MEASUREMENT_CACHE_AUDIT=true`, it also
#'   has a `readjsa_raw_cache_hits` logical attribute of length one; otherwise
#'   it has no custom attributes.  Discovery, transport, cache, schema,
#'   duplicate, and provenance failures are propagated with the condition
#'   classes documented by [fetch_jsa_release()] and [read_reos_file()].
#'
#' @examples
#' fixture <- system.file("extdata", "reos_current.xlsx", package = "readjsa")
#' release <- data.frame(
#'   provider_id = "jobs_skills_australia", dataset_id = "reos",
#'   family_id = "reos_tables", release_id = "reos/reos_tables/example",
#'   reference_period = as.Date("2026-06-01"),
#'   release_date = as.Date("2026-07-22"),
#'   vintage_date = as.Date("2026-07-22"),
#'   page_modified_date = as.Date("2026-07-22"),
#'   source_url = "https://www.jobsandskills.gov.au/downloads/reos_current.xlsx",
#'   source_filename = "reos_current.xlsx",
#'   page_url = "https://www.jobsandskills.gov.au/data/recruitment-experiences-and-outlook-survey",
#'   terms_url = "https://www.jobsandskills.gov.au/copyright-and-disclaimer",
#'   licence = "CC BY 4.0 subject to stated exclusions",
#'   attribution = "© Commonwealth of Australia",
#'   stringsAsFactors = FALSE
#' )
#' transport <- function(url, destination, timeout, max_bytes) {
#'   stopifnot(file.copy(fixture, destination, overwrite = TRUE))
#'   invisible(destination)
#' }
#' observations <- read_reos(
#'   tables = "1.1", release = release,
#'   cache_dir = tempfile("readjsa-reos-example-"), transport = transport
#' )
#' stopifnot(nrow(observations) > 0L, all(observations$table == "1.1"))
#'
#' @seealso [read_reos_file()], [discover_jsa_releases()], [fetch_jsa_release()]
#' @family REOS-readers
#' @export
read_reos <- function(
    tables = "all",
    release = NULL,
    cache_dir = jsa_cache_dir(),
    force = FALSE,
    transport = .default_transport) {
  if (is.null(release)) release <- discover_jsa_releases("reos")
  release <- .select_release_rows(release, "reos", "reos_tables")
  snapshot <- fetch_jsa_release(
    release, cache_dir = cache_dir, force = force, transport = transport
  )
  provenance <- .snapshot_provenance(snapshot)
  .attach_raw_cache_audit(
    read_reos_file(
      snapshot$path, tables = tables,
      provenance = provenance
    ),
    list(provenance)
  )
}
