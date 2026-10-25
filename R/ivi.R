.local_file_provenance <- function(path, dataset, family, provenance) {
  local <- list(
    raw_bytes = unname(file.info(path)$size),
    raw_sha256 = digest::digest(
      path, algo = "sha256", file = TRUE, serialize = FALSE
    )
  )
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance)))) {
    .abort("`provenance` must be a named list.")
  }
  for (name in c("raw_bytes", "raw_sha256")) {
    if (
      name %in% names(provenance) &&
        !identical(as.character(provenance[[name]]), as.character(local[[name]]))
    ) {
      .abort(
        "Supplied ", name, " does not match the exact local file.",
        class = "readjsa_provenance_error"
      )
    }
  }
  if (!"source_filename" %in% names(provenance)) {
    provenance$source_filename <- basename(path)
  }
  provenance$raw_bytes <- local$raw_bytes
  provenance$raw_sha256 <- local$raw_sha256
  .provenance_defaults(
    dataset, family, provenance
  )
}

.ivi_sheet_spec <- function(family, sheets) {
  candidates <- switch(family,
    skill_states = data.frame(
      sheet_pattern = c(
        "^Trend$", "^Trend Index$", "^Seasonally Adjusted$",
        "^Seasonally Adjusted Index$"
      ),
      series_type = c(
        "trend", "trend_index", "seasonally_adjusted",
        "seasonally_adjusted_index"
      )
    ),
    anzsco2_states = data.frame(
      sheet_pattern = c(
        "^Trend$", "^Trend Index$", "^Seasonally Adjusted$",
        "^Seasonally Adjusted Index$"
      ),
      series_type = c(
        "trend", "trend_index", "seasonally_adjusted",
        "seasonally_adjusted_index"
      )
    ),
    anzsco4_states = data.frame(
      sheet_pattern = "^4 digit (?:3|three) month average$",
      series_type = "three_month_average"
    ),
    anzsco2_ivi_regions = data.frame(
      sheet_pattern = c("^Averaged$", "^Indexed$"),
      series_type = c("three_month_average", "index")
    ),
    anzsco2_gccsa_sa4 = data.frame(
      sheet_pattern = "^Averaged$",
      series_type = "three_month_average"
    ),
    anzsco4_remoteness_northern = data.frame(
      sheet_pattern = c("^JSA Remoteness$", "^JSA Northern Australia$"),
      series_type = c("three_month_average", "three_month_average")
    ),
    .abort("Unsupported IVI family: ", family)
  )
  candidates$sheet <- vapply(candidates$sheet_pattern, function(pattern) {
    hit <- grep(pattern, sheets, ignore.case = TRUE, value = TRUE)
    if (length(hit) != 1L) {
      .abort(
        "IVI ", family, " workbook expected exactly one sheet matching `",
        pattern, "`; found ", length(hit), ".",
        class = "readjsa_schema_error"
      )
    }
    hit
  }, character(1))
  candidates
}

.ivi_file_notes <- function(path, sheets) {
  if (!"Notes" %in% sheets) return(NA_character_)
  raw <- as.data.frame(
    .read_excel_text(path, "Notes", col_names = FALSE),
    stringsAsFactors = FALSE
  )
  values <- trimws(as.character(unlist(raw, use.names = FALSE)))
  values <- unique(values[!is.na(values) & nzchar(values)])
  if (!length(values)) {
    .abort("IVI Notes sheet is empty.", class = "readjsa_schema_error")
  }
  paste(values, collapse = "\n")
}

.wide_observations <- function(data, id_columns, context) {
  names(data) <- .clean_names(names(data))
  .require_columns(data, id_columns, context)
  if (!nrow(data)) {
    .abort(context, " has no observation rows.",
      class = "readjsa_schema_error"
    )
  }
  other <- setdiff(names(data), id_columns)
  parsed_dates <- .excel_date(other)
  if (any(is.na(parsed_dates))) {
    .abort(
      context, " has unexpected non-date column(s): ",
      paste(other[is.na(parsed_dates)], collapse = ", "),
      class = "readjsa_schema_error"
    )
  }
  if (!length(other)) {
    .abort(context, " has no observation columns.",
      class = "readjsa_schema_error"
    )
  }
  row_count <- nrow(data)
  column_count <- length(other)
  observation_count <- row_count * column_count
  out <- lapply(id_columns, function(column) {
    rep(data[[column]], times = column_count)
  })
  names(out) <- id_columns
  out$date <- rep(parsed_dates, each = row_count)
  out$value <- numeric(observation_count)
  out$observation_status <- character(observation_count)
  out$raw_value <- character(observation_count)
  for (index in seq_along(other)) {
    parsed <- .parse_number_status(data[[other[index]]])
    rows <- seq.int(
      from = (index - 1L) * row_count + 1L,
      length.out = row_count
    )
    out$value[rows] <- parsed$value
    out$observation_status[rows] <- parsed$observation_status
    out$raw_value[rows] <- parsed$raw_value
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

.ivi_sheet <- function(path, family, sheet, series_type) {
  data <- as.data.frame(.read_excel_text(path, sheet), stringsAsFactors = FALSE)
  names(data) <- .clean_names(names(data))
  id_columns <- switch(family,
    skill_states = c("level", "title", "state", "skill_level"),
    anzsco4_states = c("anzsco_code", "anzsco_title", "state"),
    anzsco2_states = c("level", "anzsco_code", "title", "state"),
    anzsco2_ivi_regions =
      c("level", "state", "region", "anzsco_code", "anzsco_title"),
    anzsco2_gccsa_sa4 = c(
      "level", "state", "region_name", "region_code",
      "region_level", "anzsco_code", "anzsco_title"
    ),
    anzsco4_remoteness_northern = {
      geography_column <- if (grepl("Northern", sheet, ignore.case = TRUE)) {
        "northern_australia"
      } else {
        "jsa_remoteness"
      }
      c("level", "anzsco_code", "anzsco_title", geography_column)
    }
  )
  out <- .wide_observations(data, id_columns, paste("IVI", family, sheet))
  out$series_type <- series_type
  out$measure <- if (grepl("index$", series_type)) "index" else
    "online_job_advertisements"
  out$unit <- if (out$measure[[1L]] == "index") {
    "index"
  } else {
    "online job advertisements"
  }
  out$unit_multiplier <- 1
  out$frequency <- "Monthly"
  out$seasonal_adjustment <- if (grepl("^trend", series_type)) {
    "trend"
  } else if (grepl("^seasonally_adjusted", series_type)) {
    "seasonally_adjusted"
  } else if (identical(family, "anzsco4_states")) {
    "not_seasonally_adjusted"
  } else {
    "seasonal_adjustment_not_stated"
  }
  if (family == "skill_states") {
    out$occupation_level <- out$level
    out$occupation_code <- NA_character_
    out$occupation_title <- out$title
    out$geography_type <- "state_or_territory"
    out$geography_code <- out$state
    out$geography_name <- out$state
  } else {
    out$occupation_level <- out$level %||% NA_character_
    out$occupation_code <- out$anzsco_code
    out$occupation_code[out$occupation_code == "."] <- NA_character_
    out$occupation_title <- out$anzsco_title %||% out$title
    if (family %in% c("anzsco4_states", "anzsco2_states")) {
      out$geography_type <- "state_or_territory"
      out$geography_code <- out$state
      out$geography_name <- out$state
    } else if (family == "anzsco2_ivi_regions") {
      out$geography_type <- "jsa_ivi_region"
      out$geography_code <- NA_character_
      out$geography_name <- out$region
    } else if (family == "anzsco2_gccsa_sa4") {
      out$geography_type <- tolower(out$region_level)
      out$geography_code <- out$region_code
      out$geography_name <- out$region_name
    } else {
      geography_column <- if ("northern_australia" %in% names(out)) {
        "northern_australia"
      } else {
        "jsa_remoteness"
      }
      out$geography_type <- if (geography_column == "northern_australia") {
        "jsa_northern_australia"
      } else {
        "jsa_remoteness"
      }
      out$geography_code <- NA_character_
      out$geography_name <- out[[geography_column]]
    }
  }
  if (!"skill_level" %in% names(out)) out$skill_level <- NA_character_
  if (!"state" %in% names(out)) out$state <- NA_character_
  keep <- c(
    "date", "value", "observation_status", "raw_value", "series_type",
    "measure", "unit", "unit_multiplier", "frequency",
    "seasonal_adjustment", "occupation_level", "occupation_code",
    "occupation_title", "skill_level", "state", "geography_type",
    "geography_code", "geography_name"
  )
  out[keep]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Parse an official Internet Vacancy Index workbook
#'
#' `read_ivi_file()` reads one local IVI `.xlsx` release and normalises its
#' supported wide sheets to long observations.  Notes, publication units,
#' frequency, seasonal basis, occupation classification, geography, and exact
#' file provenance are retained.  The parser is offline and does not write
#' files or contact the publisher.  It validates the workbook sheet layout,
#' observation dates, duplicate keys, and (when supplied) the release
#' reference period before returning.
#'
#' @param path One existing local `.xlsx` path.  It may be a packaged fixture
#'   or a downloaded file; no network request is made.
#' @param family One of the six IVI family IDs in [jsa_release_manifest()]:
#'   `"skill_states"`, `"anzsco4_states"`, `"anzsco2_states"`,
#'   `"anzsco2_ivi_regions"`, `"anzsco2_gccsa_sa4"`, or
#'   `"anzsco4_remoteness_northern"`.
#' @param provenance Optional named list of scalar provenance fields.  Allowed
#'   fields are `provider_id`, `dataset_id`, `family_id`, `release_id`,
#'   `reference_period`, `release_date`, `vintage_date`,
#'   `page_modified_date`, `retrieved_at`, `source_url`, `final_url`,
#'   `source_filename`, `page_url`, `raw_bytes`, `raw_sha256`,
#'   `parser_version`, `schema_version`, `revision_policy`, `terms_url`,
#'   `licence`, and `attribution`.  Parser identity, schema, provider, terms,
#'   licence, and attribution cannot be overridden.  `raw_bytes` and
#'   `raw_sha256` are checked against the exact local file; observation fields
#'   cannot be replaced.  The default `list()` supplies local-file defaults.
#' @return A base [`data.frame`][base::data.frame] in deterministic long-form
#'   order.  The observation columns, in order, are `date` (`Date`), `value`
#'   (numeric), `observation_status` (character), `raw_value` (character),
#'   `series_type`, `measure`, `unit`, `unit_multiplier` (numeric),
#'   `frequency`, `seasonal_adjustment`, `occupation_level`,
#'   `occupation_code`, `occupation_title`, `skill_level`, `state`,
#'   `geography_type`, `geography_code`, and `geography_name` (character),
#'   followed by `classification_version` and `file_notes`.  The final
#'   provenance columns are `provider_id`, `dataset_id`, `family_id`,
#'   `release_id`, `reference_period`, `release_date`, `vintage_date`,
#'   `page_modified_date`, `retrieved_at`, `source_url`, `final_url`,
#'   `source_filename`, `page_url`, `raw_bytes`, `raw_sha256`,
#'   `parser_version`, `schema_version`, `revision_policy`, `terms_url`,
#'   `licence`, and `attribution`.  Date fields are `Date`, `retrieved_at` is
#'   UTC `POSIXct`, `raw_bytes` is numeric, and all other metadata fields are
#'   character.  Published, suppressed, not-published, not-applicable, missing,
#'   and flagged values remain distinguishable through `observation_status`;
#'   raw tokens remain in `raw_value`.  The return value has no custom
#'   attributes.
#'   Invalid files raise `readjsa_file_error`, `readjsa_schema_error`, or
#'   `readjsa_duplicate_error`; invalid provenance raises
#'   `readjsa_provenance_error` or `readjsa_url_error`.
#'
#' @examples
#' path <- system.file("extdata", "ivi_skill_states.xlsx", package = "readjsa")
#' observations <- read_ivi_file(path, family = "skill_states")
#' stopifnot(is.data.frame(observations), nrow(observations) > 0L)
#' c("date", "value", "occupation_title", "geography_name") %in% names(observations)
#'
#' @seealso [read_ivi()], [jsa_release_manifest()], [fetch_jsa_release()]
#' @family IVI-parsers
#' @export
read_ivi_file <- function(path, family, provenance = list()) {
  .assert_file(path, "xlsx")
  family <- match.arg(family, .ivi_families$family_id)
  sheets <- .excel_sheets(path)
  spec <- .ivi_sheet_spec(family, sheets)
  file_notes <- .ivi_file_notes(path, sheets)
  out <- .bind_rows(Map(
    function(sheet, series_type) {
      .ivi_sheet(path, family, sheet, series_type)
    },
    spec$sheet, spec$series_type
  ))
  manifest <- .ivi_families[
    match(family, .ivi_families$family_id), , drop = FALSE
  ]
  out$classification_version <- manifest$classification_version
  out$file_notes <- file_notes
  provenance <- .local_file_provenance(path, "ivi", family, provenance)
  .validate_reference_period(out, provenance, paste("IVI", family))
  .check_duplicates(
    out,
    c(
      "date", "series_type", "occupation_level", "occupation_code",
      "occupation_title", "skill_level", "state", "geography_type",
      "geography_code", "geography_name"
    ),
    paste("IVI", family)
  )
  .attach_provenance(out, provenance)
}

.normalize_ivi_families <- function(tables) {
  if (identical(tables, "all")) return(.ivi_families$family_id)
  aliases <- stats::setNames(
    .ivi_families$family_id, .ivi_families$upstream_alias
  )
  aliases <- aliases[!is.na(names(aliases))]
  tables <- ifelse(tables %in% names(aliases), aliases[tables], tables)
  invalid <- setdiff(tables, .ivi_families$family_id)
  if (length(invalid)) .abort("Unsupported IVI table(s): ", paste(invalid,
    collapse = ", "
  ))
  unique(unname(tables))
}

#' Download and read Internet Vacancy Index releases
#'
#' `read_ivi()` is the high-level IVI reader: it selects one or more supported
#' families, discovers a current official release when `release` is omitted,
#' fetches/verifies it through the cache, and combines the corresponding
#' [read_ivi_file()] outputs.  Pass a release catalogue and injected
#' `transport` for a deterministic offline workflow.  With the default
#' transport, this function performs HTTPS I/O only to official JSA hosts and
#' writes bounded raw-release cache files under `cache_dir`; it never reads or
#' stores credentials.
#'
#' @param tables Either `"all"`, one or more family IDs from
#'   [jsa_release_manifest()], or the upstream aliases `"skill"`, `"4dig"`,
#'   `"2dig_states"`, and `"2dig_regions"`.  Duplicates are removed while
#'   preserving selection order.
#' @param release Optional release catalogue data frame, normally from
#'   [discover_jsa_releases()].  It must contain exactly one valid row for each
#'   selected family.  If `NULL`, the current catalogue is discovered online.
#' @param cache_dir One non-empty cache path, created if absent; defaults to
#'   [jsa_cache_dir()].
#' @param force One non-missing logical value.  If `TRUE`, ignore valid cache
#'   entries and fetch selected releases again.
#' @param transport A `(url, destination, timeout, max_bytes)` function for
#'   deterministic tests/offline fixtures; see [fetch_jsa_release()].
#' @return A base [`data.frame`][base::data.frame] formed by row-binding the
#'   selected [read_ivi_file()] results.  It has the same 41 columns, classes,
#'   observation-status invariants, provenance fields, and row ordering as the
#'   file parser.  If `READJSA_MEASUREMENT_CACHE_AUDIT=true`, the data frame
#'   additionally has a `readjsa_raw_cache_hits` logical attribute with one
#'   value per fetched family; otherwise it has no custom attributes.  Missing,
#'   suppressed, not-published, not-applicable, and flagged observations are not
#'   silently dropped.  Discovery,
#'   transport, cache, schema, duplicate, and provenance failures are propagated
#'   with the condition classes documented by [fetch_jsa_release()] and
#'   [read_ivi_file()].
#'
#' @examples
#' fixture <- system.file("extdata", "ivi_skill_states.xlsx", package = "readjsa")
#' release <- data.frame(
#'   provider_id = "jobs_skills_australia", dataset_id = "ivi",
#'   family_id = "skill_states", release_id = "ivi/skill_states/example",
#'   reference_period = as.Date("2026-06-01"),
#'   release_date = as.Date("2026-07-22"),
#'   vintage_date = as.Date("2026-07-22"),
#'   page_modified_date = as.Date("2026-07-22"),
#'   source_url = "https://www.jobsandskills.gov.au/downloads/ivi_skill_states.xlsx",
#'   source_filename = "ivi_skill_states.xlsx",
#'   page_url = "https://www.jobsandskills.gov.au/data/internet-vacancy-index",
#'   terms_url = "https://www.jobsandskills.gov.au/copyright-and-disclaimer",
#'   licence = "CC BY 4.0 subject to stated exclusions",
#'   attribution = "© Commonwealth of Australia",
#'   stringsAsFactors = FALSE
#' )
#' transport <- function(url, destination, timeout, max_bytes) {
#'   stopifnot(file.copy(fixture, destination, overwrite = TRUE))
#'   invisible(destination)
#' }
#' observations <- read_ivi(
#'   tables = "skill", release = release,
#'   cache_dir = tempfile("readjsa-ivi-example-"), transport = transport
#' )
#' stopifnot(nrow(observations) > 0L, all(observations$family_id == "skill_states"))
#'
#' @seealso [read_ivi_file()], [discover_jsa_releases()], [fetch_jsa_release()]
#' @family IVI-readers
#' @export
read_ivi <- function(
    tables = "all",
    release = NULL,
    cache_dir = jsa_cache_dir(),
    force = FALSE,
    transport = .default_transport) {
  families <- .normalize_ivi_families(tables)
  if (!length(families) || anyNA(families)) {
    .abort("At least one supported IVI table must be selected.")
  }
  if (is.null(release)) release <- discover_jsa_releases("ivi")
  release <- .select_release_rows(release, "ivi", families)
  records <- lapply(seq_len(nrow(release)), function(index) {
    snapshot <- fetch_jsa_release(
      release[index, , drop = FALSE],
      cache_dir = cache_dir,
      force = force,
      transport = transport
    )
    provenance <- .snapshot_provenance(snapshot)
    list(
      observations = read_ivi_file(
        snapshot$path, snapshot$family_id,
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
