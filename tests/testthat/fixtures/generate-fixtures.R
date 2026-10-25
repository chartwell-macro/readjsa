if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("The writexl package is required to regenerate fixtures.")
}

serial <- as.character(as.numeric(
  as.Date(c("2026-05-01", "2026-06-01")) - as.Date("1899-12-30")
))

frame <- function(values, columns) {
  out <- as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- c(columns, serial)
  out
}

state_2 <- frame(
  list(
    c("1", "3"), c("0", "11"), c("AUSTRALIA TOTAL", "Managers"),
    c("AUST", "NSW"), c("100", "10"), c("101", "n.p.")
  ),
  c("Level", "ANZSCO_CODE", "Title", "State")
)

state_4 <- frame(
  list(
    c("0", "1111"), c("Australia Total", "Chief Executives"),
    c("AUST", "NSW"), c("100", "2"), c("101", ".")
  ),
  c("ANZSCO_CODE", "ANZSCO_TITLE", "state")
)

skill <- frame(
  list(
    c("0", "2"), c("AUSTRALIA TOTAL", "Skill Level 1"),
    c("AUST", "NSW"), c("0", "1"), c("100", "25"), c("101", "26")
  ),
  c("Level", "Title", "State", "Skill_level")
)

regions <- frame(
  list(
    c("1", "3"), c("NSW", "NSW"), c("Fixture Region", "Fixture Region"),
    c("0", "11"), c("Fixture Region Total", "Managers"),
    c("80", "8"), c("82", "9")
  ),
  c("Level", "State", "region", "ANZSCO_CODE", "ANZSCO_TITLE")
)

gccsa <- frame(
  list(
    c("1", "3"), c("NSW", "NSW"),
    c("Greater Fixture", "Greater Fixture"), c("1GFIX", "1GFIX"),
    c("GCCSA", "GCCSA"), c("0", "11"),
    c("Greater Fixture Total", "Managers"), c("70", "7"), c("72", "8")
  ),
  c(
    "Level", "State", "region_name", "region_code", "region_level",
    "ANZSCO_CODE", "ANZSCO_TITLE"
  )
)

remote <- frame(
  list(
    c("0", "1"), c("0", "11"), c("Total", "Managers"),
    c("Regional", "Regional"), c("30", "3"), c("31", "4")
  ),
  c("Level", "ANZSCO_CODE", "ANZSCO_TITLE", "JSA Remoteness")
)

northern <- frame(
  list(
    c("0", "1"), c("0", "11"), c("Total", "Managers"),
    c("Northern Australia", "Northern Australia"),
    c("20", "2"), c("21", "3")
  ),
  c("Level", "ANZSCO_CODE", "ANZSCO_TITLE", "Northern Australia")
)

writexl::write_xlsx(
  list(
    Notes = data.frame(Note = "Synthetic current-shape fixture"),
    Trend = skill,
    `Trend Index` = skill,
    `Seasonally Adjusted` = skill,
    `Seasonally Adjusted Index` = skill
  ),
  "ivi_skill_states.xlsx"
)
writexl::write_xlsx(
  list(
    Trend = state_2,
    `Trend Index` = state_2,
    `Seasonally Adjusted` = state_2,
    `Seasonally Adjusted Index` = state_2
  ),
  "ivi_anzsco2_states.xlsx"
)
writexl::write_xlsx(
  list(
    Notes = data.frame(Note = "Synthetic current-shape fixture"),
    `4 digit 3 month average` = state_4
  ),
  "ivi_anzsco4_states.xlsx"
)
duplicate_state_4 <- rbind(state_4[1, , drop = FALSE], state_4[1, , drop = FALSE])
writexl::write_xlsx(
  list(
    Notes = data.frame(Note = "Intentional duplicate-key fixture"),
    `4 digit 3 month average` = duplicate_state_4
  ),
  "ivi_anzsco4_duplicate.xlsx"
)
writexl::write_xlsx(
  list(
    Notes = data.frame(Note = "Synthetic current-shape fixture"),
    Averaged = regions,
    Indexed = regions
  ),
  "ivi_anzsco2_ivi_regions.xlsx"
)
writexl::write_xlsx(
  list(
    Notes = data.frame(Note = "Synthetic current-shape fixture"),
    Averaged = gccsa
  ),
  "ivi_anzsco2_gccsa_sa4.xlsx"
)
writexl::write_xlsx(
  list(
    Notes = data.frame(Note = "Synthetic current-shape fixture"),
    `JSA Remoteness` = remote,
    `JSA Northern Australia` = northern,
    Concordance = data.frame(Note = "Not consumed")
  ),
  "ivi_anzsco4_remoteness_northern.xlsx"
)

table_codes <- c(
  "1.1", "1.2", "1.3", "1.4", "2.1", "2.2", "3.1",
  "3.2", "3.3", "4.1", "4.2", "4.3", "4.4"
)
frequencies <- ifelse(grepl("\\.(2|4)$", table_codes), "Quarterly", "Monthly")
series <- paste("Fixture series", table_codes)

information <- matrix(NA_character_, nrow = 16 + length(table_codes), ncol = 5)
information[4, 1] <- paste(
  "The REOS results are not seasonally adjusted and may therefore be",
  "affected by seasonal factors."
)
information[5, 1] <- paste(
  "Indicators with small sample sizes are marked with an asterisk or n.p."
)
information[16, 2:5] <- c("Series", "Frequency", "Disaggregations", "Table")
for (index in seq_along(table_codes)) {
  information[16 + index, 2:5] <- c(
    series[index], frequencies[index], "Region type", paste("Table", table_codes[index])
  )
}
information <- as.data.frame(information,
  stringsAsFactors = FALSE, check.names = FALSE
)
names(information) <- c(
  "Recruitment Experiences and Outlook Survey - June 2026",
  "", "", "", ""
)

reos_sheet <- function(code, frequency) {
  matrix <- matrix(NA_character_, nrow = 7, ncol = 3)
  matrix[1, 1] <- paste("Fixture series", code)
  matrix[2, 1] <- paste("Explicit definition for fixture table", code)
  matrix[4, 2] <- "Region type"
  matrix[5, ] <- c(if (frequency == "Quarterly") "Quarter" else "Month",
    "All employers", "Capital Cities"
  )
  matrix[6, ] <- c(serial[1], "0.50", "n.p.")
  matrix[7, ] <- c(serial[2], "0.51", "49%*")
  out <- as.data.frame(matrix,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  names(out) <- c(
    "Recruitment Experiences and Outlook Survey - June 2026",
    "", ""
  )
  out
}

reos <- list(Information = information)
for (index in seq_along(table_codes)) {
  name <- paste(table_codes[index], "Fixture")
  reos[[name]] <- reos_sheet(table_codes[index], frequencies[index])
}
writexl::write_xlsx(reos, "reos_current.xlsx")

bad_scale <- reos
bad_scale[["1.1 Fixture"]][7, 3] <- "160%*"
writexl::write_xlsx(bad_scale, "reos_bad_scale.xlsx")
