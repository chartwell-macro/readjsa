test_that("the exact pinned upstream MIT notice is preserved", {
  licence_path <- system.file("UPSTREAM_LICENSE.md", package = "readjsa")
  licence <- readLines(licence_path, warn = FALSE)
  upstream <- readLines(
    system.file("UPSTREAM.md", package = "readjsa"),
    warn = FALSE
  )

  expect_true("Copyright (c) 2024 readjsa authors" %in% licence)
  expect_equal(
    digest::digest(
      licence_path, file = TRUE, algo = "sha256", serialize = FALSE
    ),
    "0df6b8b14adafc47bd12f1b19effb217fb0ef236bda41a7dedd40d145362ecad"
  )
  expect_match(
    paste(upstream, collapse = "\n"),
    "94f063c92c7634c35e09e58269900064ce09f186",
    fixed = TRUE
  )
  expect_match(
    paste(upstream, collapse = "\n"),
    "version 0.0.1",
    fixed = TRUE
  )
})
