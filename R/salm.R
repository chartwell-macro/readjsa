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
#' The geography and ASGS vintage are validated against release headers. The
#' publisher's dash-availability/ASGS preamble is validated and retained. The
#' parser supports historical ASGS vintages without silently relabelling them.
#'
#' @param path Local `.csv` path.
#' @param geography `"sa2"` or `"lga"`.
#' @param provenance Optional release provenance fields.
#' @return Long-form SALM observations with ASGS and revision provenance.
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
#' @inheritParams read_ivi
#' @param geography `"all"`, `"sa2"`, or `"lga"`.
#' @return Long-form SALM observations.
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
