.abort <- function(..., class = "readjsa_error") {
  condition <- structure(
    list(message = paste0(...), call = NULL),
    class = c(class, "error", "condition")
  )
  stop(condition)
}

.provenance_fields <- c(
  "provider_id", "dataset_id", "family_id", "release_id",
  "reference_period", "release_date", "vintage_date", "page_modified_date",
  "retrieved_at", "source_url", "final_url", "source_filename", "page_url",
  "raw_bytes", "raw_sha256", "parser_version", "schema_version",
  "revision_policy", "terms_url", "licence", "attribution"
)

.canonical_date <- function(value, name, allow_missing = TRUE) {
  missing <- length(value) == 1L && is.na(value)
  if (missing) {
    out <- as.Date(NA)
  } else if (inherits(value, "Date")) {
    out <- as.Date(value)
  } else if (
    is.character(value) &&
      length(value) == 1L &&
      grepl("^\\d{4}-\\d{2}-\\d{2}$", value)
  ) {
    out <- suppressWarnings(tryCatch(
      as.Date(value),
      error = function(error) as.Date(NA)
    ))
  } else {
    .abort(
      "Provenance date `", name, "` must be an ISO date or missing.",
      class = "readjsa_provenance_error"
    )
  }
  if (is.na(out) && (!missing || !allow_missing)) {
    .abort(
      "Provenance date `", name, "` cannot be missing or invalid.",
      class = "readjsa_provenance_error"
    )
  }
  out
}

.assert_file <- function(path, extensions) {
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    .abort("Source file does not exist: ", path, class = "readjsa_file_error")
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% extensions) {
    .abort(
      "Expected a ", paste(extensions, collapse = " or "), " file, got .", ext,
      class = "readjsa_file_error"
    )
  }
  invisible(path)
}

.clean_names <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

.english_month_tokens <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

.english_month_number <- function(month) {
  month <- tolower(trimws(as.character(month)))
  out <- match(month, tolower(.english_month_tokens))
  abbreviated <- !is.na(out) & out > 12L
  out[abbreviated] <- out[abbreviated] - 12L
  out
}

.english_date <- function(day, month, year) {
  day <- suppressWarnings(as.integer(day))
  year <- suppressWarnings(as.integer(year))
  month_number <- .english_month_number(month)
  if (
    length(day) != 1L || length(year) != 1L || length(month_number) != 1L ||
      is.na(day) || is.na(year) || is.na(month_number) ||
      day < 1L || year < 0L
  ) {
    return(as.Date(NA))
  }
  suppressWarnings(as.Date(sprintf(
    "%04d-%02d-%02d", year, month_number, day
  ), format = "%Y-%m-%d"))
}

.english_month_year_date <- function(value) {
  if (length(value) != 1L || is.na(value)) return(as.Date(NA))
  value <- trimws(as.character(value))
  captures <- regmatches(
    value,
    regexec("^([A-Za-z]+)[[:space:]]+([0-9]{4})$", value, perl = TRUE)
  )[[1L]]
  if (length(captures) != 3L) {
    captures <- regmatches(
      value,
      regexec("^([A-Za-z]+)-([0-9]{2})$", value, perl = TRUE)
    )[[1L]]
    if (length(captures) != 3L) return(as.Date(NA))
    year <- suppressWarnings(as.integer(captures[[3L]]))
    year <- if (is.na(year)) NA_integer_ else if (year <= 68L) {
      year + 2000L
    } else {
      year + 1900L
    }
    return(.english_date(1L, captures[[2L]], year))
  }
  .english_date(1L, captures[[2L]], captures[[3L]])
}

.english_day_month_year_date <- function(value) {
  if (length(value) != 1L || is.na(value)) return(as.Date(NA))
  value <- trimws(as.character(value))
  captures <- regmatches(
    value,
    regexec(
      "^([0-9]{1,2})[[:space:]]+([A-Za-z]+)[[:space:]]+([0-9]{4})$",
      value,
      perl = TRUE
    )
  )[[1L]]
  if (length(captures) != 4L) return(as.Date(NA))
  .english_date(captures[[2L]], captures[[3L]], captures[[4L]])
}

.excel_date <- function(x) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))
  serial <- suppressWarnings(as.numeric(x))
  ok_serial <- !is.na(serial) & serial > 20000 & serial < 100000
  out[ok_serial] <- as.Date(serial[ok_serial], origin = "1899-12-30")
  missing <- is.na(out) & !is.na(x)
  iso <- missing & grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", x)
  if (any(iso)) {
    out[iso] <- suppressWarnings(as.Date(x[iso], format = "%Y-%m-%d"))
  }
  missing <- is.na(out) & !is.na(x)
  if (any(missing)) {
    parsed <- vapply(
      x[missing],
      function(value) as.numeric(.english_month_year_date(value)),
      numeric(1)
    )
    out[missing] <- as.Date(parsed, origin = "1970-01-01")
  }
  out
}

.parse_number_status <- function(
    x, dash_status = "suppressed", percent_as_proportion = FALSE) {
  if (
    length(dash_status) != 1L ||
      is.na(dash_status) ||
      !dash_status %in% c("suppressed", "unavailable")
  ) {
    .abort("`dash_status` must be `suppressed` or `unavailable`.")
  }
  if (
    length(percent_as_proportion) != 1L ||
      is.na(percent_as_proportion) ||
      !is.logical(percent_as_proportion)
  ) {
    .abort("`percent_as_proportion` must be one non-missing logical value.")
  }
  raw <- trimws(as.character(x))
  missing <- is.na(raw) | raw == ""
  suppressed <- grepl(
    "^(\\.|n\\.p\\.|n\\.a\\.|-|\\.\\.)$",
    raw, ignore.case = TRUE
  )
  flagged <- grepl("\\*$", raw)
  percent <- grepl("%\\*?$", raw)
  numeric_token <- grepl(
    paste0(
      "^[+-]?(?:\\d{1,3}(?:,\\d{3})+|\\d+|\\d*\\.\\d+)",
      "(?:[eE][+-]?\\d+)?%?\\*?$"
    ),
    raw,
    perl = TRUE
  )
  cleaned <- gsub("[,*%]", "", raw)
  value <- suppressWarnings(as.numeric(cleaned))
  if (percent_as_proportion) {
    convert <- percent & !suppressed & !missing
    value[convert] <- value[convert] / 100
  }
  status <- ifelse(
    missing, "missing",
    ifelse(
      suppressed,
      ifelse(grepl("n\\.p", raw, ignore.case = TRUE), "not_published",
        ifelse(grepl("n\\.a", raw, ignore.case = TRUE), "not_applicable",
          ifelse(raw == "-", dash_status, "suppressed")
        )
      ),
      ifelse(flagged, "flagged", "published")
    )
  )
  bad <- (!numeric_token | is.na(value)) & !missing & !suppressed
  if (any(bad)) {
    .abort(
      "Unrecognised observation token(s): ",
      paste(unique(raw[bad]), collapse = ", "),
      class = "readjsa_schema_error"
    )
  }
  list(value = value, observation_status = status, raw_value = raw)
}

.require_columns <- function(data, expected, context) {
  missing <- setdiff(expected, names(data))
  if (length(missing)) {
    .abort(
      context, " schema is missing required column(s): ",
      paste(missing, collapse = ", "),
      class = "readjsa_schema_error"
    )
  }
  invisible(data)
}

.bind_rows <- function(xs) {
  xs <- xs[lengths(xs) > 0L]
  if (!length(xs)) return(data.frame())
  columns <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[columns]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

.select_release_rows <- function(release, dataset_id, families) {
  if (!is.data.frame(release)) {
    .abort("`release` must be a release catalogue.")
  }
  required <- c("dataset_id", "family_id")
  .require_columns(release, required, "Release catalogue")
  selected <- release[
    release$dataset_id == dataset_id & release$family_id %in% families,
    ,
    drop = FALSE
  ]
  counts <- table(factor(selected$family_id, levels = families))
  invalid <- families[counts != 1L]
  if (length(invalid)) {
    .abort(
      "Release catalogue must contain exactly one selected row for: ",
      paste(invalid, collapse = ", "), ".",
      class = "readjsa_discovery_error"
    )
  }
  selected[match(families, selected$family_id), , drop = FALSE]
}

.provenance_defaults <- function(dataset_id, family_id, provenance = list()) {
  if (!is.list(provenance) || (length(provenance) && is.null(names(provenance)))) {
    .abort("`provenance` must be a named list.")
  }
  if (anyDuplicated(names(provenance))) {
    .abort("`provenance` field names must be unique.")
  }
  unknown <- setdiff(names(provenance), .provenance_fields)
  if (length(unknown)) {
    .abort(
      "Unsupported provenance field(s): ",
      paste(unknown, collapse = ", "),
      class = "readjsa_provenance_error"
    )
  }
  provider_id <- if (identical(dataset_id, "salm")) "dewr" else
    "jobs_skills_australia"
  defaults <- list(
    provider_id = provider_id,
    dataset_id = dataset_id,
    family_id = family_id,
    release_id = NA_character_,
    reference_period = as.Date(NA),
    release_date = as.Date(NA),
    vintage_date = as.Date(NA),
    page_modified_date = as.Date(NA),
    retrieved_at = as.POSIXct(NA, tz = "UTC"),
    source_url = NA_character_,
    final_url = NA_character_,
    source_filename = NA_character_,
    page_url = unname(.jsa_pages[dataset_id]),
    raw_bytes = NA_real_,
    raw_sha256 = NA_character_,
    parser_version = .readjsa_version,
    schema_version = .readjsa_schema_version,
    revision_policy = if (identical(dataset_id, "salm")) {
      paste(
        "Latest release revises the historical series when labour-force",
        "benchmarks or geography classifications change."
      )
    } else {
      "Published release retained as an immutable vintage."
    },
    terms_url = unname(.jsa_terms[provider_id]),
    licence = "CC BY 4.0 subject to stated exclusions",
    attribution = "\u00a9 Commonwealth of Australia"
  )
  out <- utils::modifyList(defaults, provenance)
  expected <- list(
    provider_id = provider_id,
    dataset_id = dataset_id,
    family_id = family_id,
    terms_url = unname(.jsa_terms[provider_id]),
    licence = "CC BY 4.0 subject to stated exclusions",
    attribution = "\u00a9 Commonwealth of Australia",
    parser_version = .readjsa_version,
    schema_version = .readjsa_schema_version
  )
  for (name in names(expected)) {
    if (!identical(as.character(out[[name]]), as.character(expected[[name]]))) {
      .abort(
        "Provenance field `", name, "` conflicts with the parser contract.",
        class = "readjsa_provenance_error"
      )
    }
  }
  scalar <- vapply(out, length, integer(1)) == 1L
  if (!all(scalar)) {
    .abort(
      "Every provenance field must be scalar.",
      class = "readjsa_provenance_error"
    )
  }
  for (name in c(
    "reference_period", "release_date", "vintage_date",
    "page_modified_date"
  )) {
    out[[name]] <- .canonical_date(out[[name]], name)
  }
  if (
    inherits(out$retrieved_at, "POSIXt")
  ) {
    out$retrieved_at <- as.POSIXct(out$retrieved_at, tz = "UTC")
  } else if (is.na(out$retrieved_at)) {
    out$retrieved_at <- as.POSIXct(NA, tz = "UTC")
  } else {
    .abort(
      "Provenance `retrieved_at` must be a date-time or missing.",
      class = "readjsa_provenance_error"
    )
  }
  expected_revision_policy <- defaults$revision_policy
  if (!identical(out$revision_policy, expected_revision_policy)) {
    .abort(
      "Provenance field `revision_policy` conflicts with the parser contract.",
      class = "readjsa_provenance_error"
    )
  }
  if (
    !is.na(out$source_url) &&
      nzchar(as.character(out$source_url))
  ) {
    .validate_official_url(as.character(out$source_url))
  }
  if (!is.na(out$final_url) && nzchar(as.character(out$final_url))) {
    .validate_official_url(as.character(out$final_url), "Final source URL")
  }
  if (!is.na(out$page_url) && nzchar(as.character(out$page_url))) {
    .validate_official_url(as.character(out$page_url), "Publication page URL")
  }
  out
}

.attach_provenance <- function(data, provenance) {
  overlap <- intersect(names(data), names(provenance))
  if (length(overlap)) {
    .abort(
      "Provenance cannot overwrite observation field(s): ",
      paste(overlap, collapse = ", "),
      class = "readjsa_provenance_error"
    )
  }
  n <- nrow(data)
  for (name in names(provenance)) {
    value <- provenance[[name]]
    if (length(value) != 1L) {
      .abort("Provenance field '", name, "' must be scalar.")
    }
    data[[name]] <- rep(value, n)
  }
  data
}

.check_duplicates <- function(data, keys, context) {
  .require_columns(data, keys, context)
  if (nrow(data) < 2L) return(invisible(data))
  order_arguments <- c(
    lapply(data[keys], function(value) {
      if (inherits(value, "Date")) as.numeric(value) else value
    }),
    list(na.last = TRUE, method = "radix")
  )
  ordered <- do.call(order, order_arguments)
  previous <- ordered[-length(ordered)]
  current <- ordered[-1L]
  same <- rep(TRUE, length(current))
  for (key in keys) {
    left <- data[[key]][previous]
    right <- data[[key]][current]
    equal <- left == right
    equal[is.na(equal)] <- is.na(left[is.na(equal)]) &
      is.na(right[is.na(equal)])
    same <- same & equal
    if (!any(same)) break
  }
  if (any(same)) {
    .abort(
      context, " contains duplicate observation keys (",
      paste(keys, collapse = ", "), ").",
      class = "readjsa_duplicate_error"
    )
  }
  invisible(data)
}

.read_excel_text <- function(path, sheet, col_names = TRUE, skip = 0L) {
  tryCatch(
    suppressMessages(readxl::read_excel(
      path,
      sheet = sheet,
      col_names = col_names,
      skip = skip,
      col_types = "text",
      .name_repair = "minimal"
    )),
    error = function(error) {
      .abort(
        "Could not read workbook sheet `", sheet, "`: ",
        conditionMessage(error),
        class = "readjsa_schema_error"
      )
    }
  )
}

.excel_sheets <- function(path) {
  tryCatch(
    readxl::excel_sheets(path),
    error = function(error) {
      .abort(
        "Could not read workbook structure: ", conditionMessage(error),
        class = "readjsa_schema_error"
      )
    }
  )
}

.validate_reference_period <- function(data, provenance, context) {
  reference_period <- provenance$reference_period
  if (length(reference_period) != 1L || is.na(reference_period)) {
    return(invisible(data))
  }
  .require_columns(data, "date", context)
  actual <- suppressWarnings(max(as.Date(data$date), na.rm = TRUE))
  expected <- .canonical_date(
    reference_period, "reference_period", allow_missing = FALSE
  )
  if (!identical(as.character(actual), as.character(expected))) {
    .abort(
      context, " latest observation date ", as.character(actual),
      " conflicts with release reference period ", as.character(expected),
      ". The file may be truncated or the release metadata may be stale.",
      class = "readjsa_provenance_error"
    )
  }
  invisible(data)
}
