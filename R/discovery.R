.read_page <- function(
    page_url, max_bytes = 5 * 1024^2, timeout = 30, retries = 2L) {
  .validate_official_url(page_url, "Publication page URL")
  last_error <- NULL
  for (attempt in seq_len(retries + 1L)) {
    last_error <- tryCatch({
      handle <- curl::new_handle()
      curl::handle_setopt(
        handle,
        timeout = timeout,
        connecttimeout = min(timeout, 10),
        maxfilesize_large = as.numeric(max_bytes),
        followlocation = TRUE,
        protocols_str = "https",
        redir_protocols_str = "https",
        useragent = "readjsa/0.1.0 (+official public data client)"
      )
      response <- curl::curl_fetch_memory(page_url, handle = handle)
      if (response$status_code < 200L || response$status_code >= 300L) {
        .abort(
          "Release page returned HTTP ", response$status_code, ": ", page_url,
          class = "readjsa_transport_error"
        )
      }
      .validate_official_url(response$url, "Publication page redirect target")
      if (length(response$content) > max_bytes) {
        .abort(
          "Release page exceeded byte limit.",
          class = "readjsa_size_error"
        )
      }
      NULL
    }, error = function(error) error)
    if (is.null(last_error)) return(rawToChar(response$content))
    if (attempt <= retries) {
      Sys.sleep(min(0.1 * (2 ^ (attempt - 1L)), 0.5))
    }
  }
  stop(last_error)
}

.read_html_input <- function(html, page_url) {
  .validate_official_url(page_url, "Publication page URL")
  if (is.null(html)) html <- .read_page(page_url)
  if (length(html) != 1L) .abort("`html` must be a path or scalar HTML text.")
  xml2::read_html(html)
}

.date_from_text <- function(text, quarter_end = FALSE) {
  text <- gsub("[_-]+", " ", text)
  month_pattern <- paste(.english_month_tokens, collapse = "|")
  match <- regexpr(
    paste0("(?i)(", month_pattern, ")\\s+(?:quarter\\s+)?(20\\d{2})"),
    text,
    perl = TRUE
  )
  if (match[[1]] < 0) return(as.Date(NA))
  token <- regmatches(text, match)
  token <- gsub("(?i)\\s+quarter\\s+", " ", token, perl = TRUE)
  date <- .english_month_year_date(token)
  if (quarter_end && !is.na(date)) {
    date <- seq(date, by = "1 month", length.out = 2L)[2L] - 1
  }
  date
}

.release_date_from_page <- function(text) {
  text <- gsub("\u00a0", " ", text, fixed = TRUE)
  month_pattern <- paste(.english_month_tokens, collapse = "|")
  patterns <- c(
    paste0(
      "(?i)released.{0,60}?(\\d{1,2}\\s+(",
      month_pattern, ")\\s+20\\d{2})"
    ),
    paste0(
      "(?i)release(?:d)?\\s+date\\D{0,10}(\\d{1,2}\\s+(",
      month_pattern, ")\\s+20\\d{2})"
    )
  )
  for (pattern in patterns) {
    match <- regexec(pattern, text, perl = TRUE)
    captures <- regmatches(text, match)[[1]]
    if (length(captures) >= 2L) {
      out <- .english_day_month_year_date(captures[[2]])
      if (!is.na(out)) return(out)
    }
  }
  as.Date(NA)
}

.page_modified_date_from_page <- function(text) {
  text <- gsub("\u00a0", " ", text, fixed = TRUE)
  month_pattern <- paste(.english_month_tokens, collapse = "|")
  match <- regexec(
    paste0(
      "(?i)(?:page\\s+)?last modified.{0,40}?(\\d{1,2}\\s+(",
      month_pattern, ")\\s+20\\d{2})"
    ),
    text,
    perl = TRUE
  )
  captures <- regmatches(text, match)[[1]]
  if (length(captures) < 2L) return(as.Date(NA))
  .english_day_month_year_date(captures[[2]])
}

.source_filename_from_url <- function(url) {
  vapply(url, function(one_url) {
    path <- sub("[?#].*$", "", one_url)
    parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    filename <- utils::tail(parts, 1L)
    if (
      filename %in% c("csv", "xlsx") &&
        length(parts) >= 2L
    ) {
      filename <- paste0(parts[[length(parts) - 1L]], ".", filename)
    }
    filename
  }, character(1), USE.NAMES = FALSE)
}

.ivi_family_from_text <- function(text) {
  normalized <- .clean_names(text)
  hits <- vapply(
    .ivi_families$pattern,
    function(pattern) grepl(pattern, normalized, perl = TRUE),
    logical(1)
  )
  if (sum(hits) != 1L) return(NA_character_)
  .ivi_families$family_id[hits]
}

.release_frame <- function(
    dataset, family, url, label, page_text, page_url) {
  quarter <- identical(dataset, "salm")
  reference_period <- .date_from_text(paste(label, url), quarter_end = quarter)
  release_date <- .release_date_from_page(page_text)
  page_modified_date <- .page_modified_date_from_page(page_text)
  provider <- if (identical(dataset, "salm")) "dewr" else
    "jobs_skills_australia"
  data.frame(
    provider_id = provider,
    dataset_id = dataset,
    family_id = family,
    release_id = paste(
      dataset, family,
      ifelse(is.na(reference_period), "unknown", format(reference_period)),
      substr(digest::digest(url, algo = "sha256", serialize = FALSE), 1L, 12L),
      sep = "/"
    ),
    reference_period = reference_period,
    release_date = release_date,
    vintage_date = release_date,
    page_modified_date = page_modified_date,
    source_url = url,
    source_filename = .source_filename_from_url(url),
    page_url = page_url,
    terms_url = unname(.jsa_terms[provider]),
    licence = "CC BY 4.0 subject to stated exclusions",
    attribution = "\u00a9 Commonwealth of Australia",
    stringsAsFactors = FALSE
  )
}

#' Discover official JSA and DEWR releases
#'
#' `discover_jsa_releases()` parses the publisher's current HTML page rather
#' than guessing dated file URLs.  It classifies supported IVI, REOS, and SALM
#' links, extracts release and page-modification dates, and records the exact
#' source URL needed for reproducible fetching.  Supplying `html` (and, for
#' SALM, `resource_html`) makes discovery deterministic and offline-testable.
#'
#' With `strict = TRUE` (the default), discovery refuses to return an
#' incomplete, ambiguous, unclassified, or period-less catalogue: exactly one
#' release must be found for each family in [jsa_release_manifest()].  Set
#' `strict = FALSE` only when inspecting a changing publisher page; the result
#' may then be partial or contain older releases.  When HTML is not supplied,
#' the function fetches a validated publication URL over HTTPS with a timeout,
#' bounded retries, and a received-response-size check.  The final response URL
#' must also be on the official host allowlist.  The HTTPS transport follows
#' redirects; intermediate redirect hosts are not independently validated.
#'
#' @param dataset One of `"ivi"`, `"reos"`, or `"salm"`.  `"ivi"` searches
#'   six IVI families, `"reos"` searches the REOS workbook family, and
#'   `"salm"` searches SA2 and LGA CSV families.
#' @param page_url One non-empty HTTPS URL on the official publication host.
#'   If `NULL`, the package's current official page for `dataset` is used.
#' @param html Optional scalar HTML text or path to a local HTML file.  If
#'   `NULL`, the official page is fetched with a five-megabyte response bound,
#'   30-second timeout, and bounded retries.  A local path is read without a
#'   network request.
#' @param resource_html Optional named list of scalar HTML text or local paths
#'   for SALM resource pages, with names `salm_sa2` and/or `salm_lga`.  It is
#'   used only when the landing page links to a resource page instead of a
#'   direct CSV.  Live discovery follows the resource pages automatically.
#' @param strict One non-missing logical value.  If `TRUE`, require exactly one
#'   classified release with a parseable reference period for every supported
#'   family.  If `FALSE`, return every classified link that can be represented.
#' @return A base [`data.frame`][base::data.frame], ordered by `family_id` and
#'   decreasing `reference_period`, with these columns in order:
#'   `provider_id`, `dataset_id`, `family_id`, `release_id`,
#'   `reference_period`, `release_date`, `vintage_date`,
#'   `page_modified_date`, `source_url`, `source_filename`, `page_url`,
#'   `terms_url`, `licence`, and `attribution`.  Date columns are `Date`; all
#'   other columns are character.  `release_id` is a stable identity derived
#'   from dataset/family/reference period/source URL.  The data frame has no
#'   custom attributes.  Failure to find or classify supported links raises
#'   `readjsa_discovery_error`; malformed or non-official URLs raise
#'   `readjsa_url_error`; transport failures raise
#'   `readjsa_transport_error` or `readjsa_size_error`.
#'
#' @examples
#' page <- system.file("extdata", "ivi.html", package = "readjsa")
#' catalogue <- discover_jsa_releases(
#'   "ivi",
#'   page_url = "https://www.jobsandskills.gov.au/data/internet-vacancy-index",
#'   html = page
#' )
#' stopifnot(is.data.frame(catalogue), all(c("family_id", "source_url") %in% names(catalogue)))
#'
#' @seealso [jsa_release_manifest()], [fetch_jsa_release()], [read_ivi()]
#' @family release-discovery
#' @export
discover_jsa_releases <- function(
    dataset = c("ivi", "reos", "salm"),
    page_url = NULL,
    html = NULL,
    resource_html = NULL,
    strict = TRUE) {
  dataset <- match.arg(dataset)
  if (length(strict) != 1L || is.na(strict) || !is.logical(strict)) {
    .abort("`strict` must be one non-missing logical value.")
  }
  if (is.null(page_url)) page_url <- unname(.jsa_pages[dataset])
  document <- .read_html_input(html, page_url)
  page_text <- paste(xml2::xml_text(document), collapse = " ")
  nodes <- xml2::xml_find_all(document, ".//a[@href]")
  href <- xml2::xml_attr(nodes, "href")
  label <- trimws(xml2::xml_text(nodes))
  url <- xml2::url_absolute(href, page_url)
  if (dataset == "salm") {
    landing_url <- url
    landing_label <- label
    direct <- grepl("/csv(?:$|[?#])|\\.csv(?:$|[?#])",
      landing_url, ignore.case = TRUE
    )
    combined_landing <- paste(landing_label, landing_url)
    resource_family <- ifelse(
      grepl(
        "salm.*(?:smoothed)?.*sa2|statistical.area.level.2",
        combined_landing, ignore.case = TRUE
      ),
      "salm_sa2",
      ifelse(grepl(
        "salm.*(?:smoothed)?.*lga|local.government",
        combined_landing, ignore.case = TRUE
      ), "salm_lga", NA_character_)
    )
    missing_direct <- setdiff(
      c("salm_sa2", "salm_lga"),
      ifelse(direct, resource_family, NA_character_)
    )
    for (family_id in missing_direct) {
      candidate <- which(
        resource_family == family_id &
          !direct &
          grepl("/resources/", landing_url, fixed = TRUE)
      )
      if (length(candidate) != 1L) {
        next
      }
      supplied <- if (is.list(resource_html) &&
        family_id %in% names(resource_html)) {
        resource_html[[family_id]]
      } else {
        NULL
      }
      resource_document <- .read_html_input(supplied, landing_url[candidate])
      resource_nodes <- xml2::xml_find_all(
        resource_document, ".//a[@href]"
      )
      resource_urls <- xml2::url_absolute(
        xml2::xml_attr(resource_nodes, "href"), landing_url[candidate]
      )
      csv <- grepl("/csv(?:$|[?#])|\\.csv(?:$|[?#])",
        resource_urls, ignore.case = TRUE
      )
      if (sum(csv) != 1L) {
        .abort(
          "SALM resource page for ", family_id,
          " must expose exactly one CSV release.",
          class = "readjsa_discovery_error"
        )
      }
      url <- c(url, resource_urls[csv])
      label <- c(label, paste(
        landing_label[candidate],
        trimws(xml2::xml_text(resource_nodes[csv]))
      ))
    }
  }
  keep <- if (dataset == "salm") {
    grepl("/csv(?:$|[?#])|\\.csv(?:$|[?#])", url, ignore.case = TRUE)
  } else {
    grepl("\\.xlsx(?:$|[?#])", url, ignore.case = TRUE)
  }
  url <- url[keep]
  label <- label[keep]
  if (!length(url)) {
    .abort(
      "No supported release links were found on ", page_url, ".",
      class = "readjsa_discovery_error"
    )
  }
  combined <- paste(label, basename(sub("[?#].*$", "", url)))
  family <- if (dataset == "ivi") {
    vapply(combined, .ivi_family_from_text, character(1))
  } else if (dataset == "reos") {
    ifelse(
      grepl("recruitment.*(?:data|file)|reos.*(?:data|file)",
        combined, ignore.case = TRUE
      ),
      "reos_tables", NA_character_
    )
  } else {
    ifelse(
      grepl("sa2|statistical.area.level.2", combined, ignore.case = TRUE),
      "salm_sa2",
      ifelse(grepl("lga|local.government", combined, ignore.case = TRUE),
        "salm_lga", NA_character_
      )
    )
  }
  if (strict && anyNA(family)) {
    .abort(
      "Official ", dataset,
      " page contains unclassified data release link(s): ",
      paste(.source_filename_from_url(url[is.na(family)]), collapse = ", "),
      ". Refusing to claim a complete catalogue.",
      class = "readjsa_discovery_error"
    )
  }
  supported <- !is.na(family)
  url <- url[supported]
  label <- label[supported]
  family <- family[supported]
  if (!length(url)) {
    .abort(
      "Links were present, but none matched a supported ", dataset,
      " family.", class = "readjsa_discovery_error"
    )
  }
  out <- .bind_rows(Map(
    function(family, url, label) {
      .release_frame(dataset, family, url, label, page_text, page_url)
    },
    family, url, label
  ))
  out <- out[!duplicated(out[c("family_id", "source_url")]), , drop = FALSE]
  expected <- jsa_release_manifest()
  expected <- expected$family_id[expected$dataset_id == dataset]
  if (strict) {
    counts <- table(factor(out$family_id, levels = expected))
    invalid_counts <- expected[counts != 1L]
    if (length(invalid_counts)) {
      .abort(
        "Official ", dataset,
        " release page must expose exactly one current link for family(s): ",
        paste(invalid_counts, collapse = ", "),
        ". Publication structure may have changed; refusing an ambiguous ",
        "or partial catalogue.",
        class = "readjsa_discovery_error"
      )
    }
    if (anyNA(out$reference_period)) {
      .abort(
        "Official ", dataset,
        " release period could not be parsed; refusing an unreplayable ",
        "catalogue.",
        class = "readjsa_discovery_error"
      )
    }
  }
  out[order(out$family_id, out$reference_period, decreasing = TRUE), ,
    drop = FALSE
  ]
}
