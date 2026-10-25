test_that("IVI discovery finds all six official workbook families", {
  releases <- discover_jsa_releases("ivi", html = fixture("ivi.html"))
  expect_equal(nrow(releases), 6L)
  expect_setequal(
    releases$family_id,
    c(
      "skill_states", "anzsco4_states", "anzsco2_states",
      "anzsco2_ivi_regions", "anzsco2_gccsa_sa4",
      "anzsco4_remoteness_northern"
    )
  )
  expect_equal(unique(releases$reference_period), as.Date("2026-06-01"))
  expect_equal(unique(releases$release_date), as.Date("2026-07-22"))
  expect_true(all(grepl("^https://www.jobsandskills.gov.au/", releases$source_url)))
})

test_that("REOS and SALM discovery retain release and terms metadata", {
  reos <- discover_jsa_releases("reos", html = fixture("reos.html"))
  expect_equal(reos$family_id, "reos_tables")
  expect_true(is.na(reos$release_date))
  expect_true(is.na(reos$vintage_date))
  expect_equal(reos$page_modified_date, as.Date("2026-07-21"))

  salm <- discover_jsa_releases(
    "salm",
    html = fixture("salm.html"),
    resource_html = list(
      salm_sa2 = fixture("salm_sa2_resource.html"),
      salm_lga = fixture("salm_lga_resource.html")
    )
  )
  expect_setequal(salm$family_id, c("salm_sa2", "salm_lga"))
  expect_true(all(salm$reference_period == as.Date("2026-03-31")))
  expect_true(all(salm$release_date == as.Date("2026-06-30")))
  expect_true(all(grepl("/csv$", salm$source_url)))
  expect_true(all(grepl("march-quarter-2026\\.csv$", salm$source_filename)))
  expect_true(all(salm$terms_url == "https://www.dewr.gov.au/copyright"))
})

test_that("strict discovery rejects duplicate, unknown, and unparsed releases", {
  html <- paste(readLines(fixture("ivi.html")), collapse = "\n")
  duplicate <- sub(
    "</body>",
    paste0(
      "<a href=\"/sites/default/files/2026-07/",
      "internet_vacancies_anzsco_skill_level_states_and_territories_",
      "-_june_2026_duplicate.xlsx\">Duplicate skill release June 2026</a>",
      "</body>"
    ),
    html,
    fixed = TRUE
  )
  expect_error(
    discover_jsa_releases("ivi", html = duplicate),
    class = "readjsa_discovery_error"
  )

  unknown <- sub(
    "</body>",
    paste0(
      "<a href=\"/sites/default/files/2026-07/",
      "internet_vacancies_new_classification_-_june_2026.xlsx\">",
      "Internet Vacancies New Classification - June 2026</a></body>"
    ),
    html,
    fixed = TRUE
  )
  expect_error(
    discover_jsa_releases("ivi", html = unknown),
    class = "readjsa_discovery_error"
  )

  no_period <- gsub("June 2026|june_2026", "current", html)
  expect_error(
    discover_jsa_releases("ivi", html = no_period),
    class = "readjsa_discovery_error"
  )
  expect_equal(
    readjsa:::.date_from_text("Jun 2026"),
    as.Date("2026-06-01")
  )
})

test_that("strict discovery refuses incomplete publication catalogues", {
  incomplete <- sub(
    "<a href=\"/sites/default/files/2026-07/internet_vacancies_anzsco4_occupations_jsa_remoteness[^<]+</a>",
    "", paste(readLines(fixture("ivi.html")), collapse = "\n")
  )
  expect_error(
    discover_jsa_releases("ivi", html = incomplete),
    class = "readjsa_discovery_error"
  )
  expect_error(
    discover_jsa_releases("ivi", html = fixture("ivi.html"), strict = NA),
    "`strict` must be"
  )
})
