test_that("ex_to_PKNCAdose returns a PKNCAdose object", {
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001"),
    EXTRT   = c("DRUG A", "DRUG A"),
    EXDOSE  = c(100, 100),
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = c("2024-01-01T08:00", "2024-01-08T08:00"),
    EXENDTC = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  expect_s3_class(result, "PKNCAdose")
})

test_that("ex_to_PKNCAdose handles oral doses with NA EXENDTC", {
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001"),
    EXTRT   = c("DRUG A", "DRUG A"),
    EXDOSE  = c(100, 100),
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = c("2024-01-01T08:00", "2024-01-08T08:00"),
    EXENDTC = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  # Duration should be 0 for oral doses with NA EXENDTC
  expect_true(all(result$data$EXDUR == 0))
})

test_that("ex_to_PKNCAdose derives duration from EXENDTC for IV infusions", {
  ex <- data.frame(
    USUBJID = "SUBJ-001",
    EXTRT   = "DRUG B",
    EXDOSE  = 50,
    EXDOSU  = "mg",
    EXROUTE = "INTRAVENOUS INFUSION",
    EXSTDTC = "2024-01-01T09:00:00",
    EXENDTC = "2024-01-01T10:00:00",
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  expect_equal(result$data$EXDUR, 1)
})

test_that("ex_to_PKNCAdose maps routes correctly", {
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003", "SUBJ-004"),
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = c("ORAL", "INTRAVENOUS INFUSION", "INTRAVENOUS BOLUS", "SUBCUTANEOUS"),
    EXSTDTC = "2024-01-01T08:00",
    EXENDTC = c(NA, "2024-01-01T09:00", "2024-01-01T08:00", NA),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  route_col <- result$columns$route
  # Oral and subcutaneous are extravascular; IV infusion and bolus are intravascular
  expect_equal(
    result$data[[route_col]],
    c("extravascular", "intravascular", "intravascular", "extravascular")
  )
})

test_that("ex_to_PKNCAdose computes relative time from first dose per subject", {
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-002", "SUBJ-002"),
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = c("2024-01-01T08:00", "2024-01-02T08:00",
                "2024-01-01T10:00", "2024-01-02T10:00"),
    EXENDTC = c(NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  # First dose for each subject should be time 0, second dose at 24h
  expect_equal(result$data$AFRLT, c(0, 24, 0, 24))
})

test_that("ex_to_PKNCAdose handles mixed datetime precision", {
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001", "SUBJ-001"),
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = c("2024-01-01T08:00:00", "2024-01-01T10:30", "2024-01-02"),
    EXENDTC = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  expect_s3_class(result, "PKNCAdose")
  # All dates should be parsed (no NAs in AFRLT)
  expect_false(any(is.na(result$data$AFRLT)))
})

test_that("ex_to_PKNCAdose derives EXELTM from EXRFTDTC when available", {
  ex <- data.frame(
    USUBJID  = c("SUBJ-001", "SUBJ-001"),
    EXTRT    = "DRUG A",
    EXDOSE   = 100,
    EXDOSU   = "mg",
    EXROUTE  = "ORAL",
    EXSTDTC  = c("2024-01-01T08:00", "2024-01-02T08:00"),
    EXENDTC  = c(NA, NA),
    EXRFTDTC = c("2024-01-01T08:00", "2024-01-01T08:00"),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  # EXELTM should be 0 for first dose, 24 for second
  expect_equal(result$data$EXELTM, c(0, 24))
})


test_that("ex_to_PKNCAdose uses pre-existing EXDUR without deriving", {
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001"),
    EXTRT   = "DRUG B",
    EXDOSE  = c(50, 50),
    EXDOSU  = "mg",
    EXROUTE = "INTRAVENOUS INFUSION",
    EXSTDTC = c("2024-01-01T09:00:00", "2024-01-08T09:00:00"),
    EXENDTC = c("2024-01-01T10:00:00", "2024-01-08T10:00:00"),
    # Pre-existing EXDUR that differs from EXENDTC - EXSTDTC (1h)
    # e.g. actual infusion was 0.75h, recorded separately from collection times
    EXDUR   = c(0.75, 0.5),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  # Should use the provided EXDUR, not derive from EXENDTC - EXSTDTC
  expect_equal(result$data$EXDUR, c(0.75, 0.5))
})

test_that("ex_to_PKNCAdose uses pre-existing EXELTM without deriving", {
  ex <- data.frame(
    USUBJID  = c("SUBJ-001", "SUBJ-001"),
    EXTRT    = "DRUG A",
    EXDOSE   = c(100, 100),
    EXDOSU   = "mg",
    EXROUTE  = "ORAL",
    EXSTDTC  = c("2024-01-01T08:00", "2024-01-02T08:00"),
    EXENDTC  = c(NA, NA),
    EXRFTDTC = c("2024-01-01T08:00", "2024-01-01T08:00"),
    # Pre-existing EXELTM that differs from EXSTDTC - EXRFTDTC (0h, 24h)
    # e.g. protocol-defined nominal elapsed times
    EXELTM   = c(0, 168),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  # Should use the provided EXELTM, not derive from EXSTDTC - EXRFTDTC
  expect_equal(result$data$EXELTM, c(0, 168))
})

test_that("ex_to_PKNCAdose works without optional columns", {
  # Minimal dataset: no EXENDTC, no EXRFTDTC, no EXROUTE, no EXDOSU
  ex <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-001"),
    EXTRT   = "DRUG A",
    EXDOSE  = c(100, 100),
    EXSTDTC = c("2024-01-01T08:00", "2024-01-08T08:00"),
    stringsAsFactors = FALSE
  )
  result <- ex_to_PKNCAdose(ex)
  expect_s3_class(result, "PKNCAdose")
})

# --- Tests for shared helpers ------------------------------------------------

test_that("parse_iso8601_duration handles standard durations", {
  expect_equal(parse_iso8601_duration("PT2H"), 2)
  expect_equal(parse_iso8601_duration("PT30M"), 0.5)
  expect_equal(parse_iso8601_duration("PT3600S"), 1)
  expect_equal(parse_iso8601_duration("PT1H30M"), 1.5)
  expect_equal(parse_iso8601_duration("PT0H"), 0)
  expect_true(is.na(parse_iso8601_duration(NA_character_)))
  expect_true(is.na(parse_iso8601_duration("not_a_duration")))
})

test_that("parse_iso8601_duration handles negative elapsed times", {
  # PCELTM can encode pre-dose times as PT-0.083H
  expect_equal(parse_iso8601_duration("PT-0.083H"), -0.083)
})

test_that("parse_iso8601_duration handles vectors", {
  result <- parse_iso8601_duration(c("PT1H", "PT2H", NA, "PT30M"))
  expect_equal(result, c(1, 2, NA, 0.5))
})

test_that("route_cdisc_to_pknca maps routes correctly", {
  expect_equal(route_cdisc_to_pknca("ORAL"), "extravascular")
  expect_equal(route_cdisc_to_pknca("INTRAVENOUS INFUSION"), "intravascular")
  expect_equal(route_cdisc_to_pknca("INTRAVENOUS BOLUS"), "intravascular")
  expect_equal(route_cdisc_to_pknca("INTRAVENOUS DRIP"), "intravascular")
  expect_equal(route_cdisc_to_pknca("SUBCUTANEOUS"), "extravascular")
  expect_equal(route_cdisc_to_pknca("INTRAMUSCULAR"), "extravascular")
  # Case insensitive
  expect_equal(route_cdisc_to_pknca("oral"), "extravascular")
  expect_equal(route_cdisc_to_pknca("intravenous infusion"), "intravascular")
})

test_that("std_dtc_to_rdate parses mixed precision datetimes", {
  result <- std_dtc_to_rdate(c(
    "2024-01-01T08:00:00",
    "2024-01-01T10:30",
    "2024-01-02"
  ))
  expect_length(result, 3)
  expect_false(any(is.na(result)))
  expect_s3_class(result, "POSIXct")
})

# --- Tests for derive_pcrftdtc -----------------------------------------------

test_that("derive_pcrftdtc assigns most recent dose as reference", {
  pc <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S1"),
    PCDTC   = c(
      "2024-01-01T07:55:00",  # pre-dose
      "2024-01-01T09:00:00",  # 1h post dose 1
      "2024-01-02T07:55:00",  # pre-dose 2
      "2024-01-02T09:00:00"   # 1h post dose 2
    ),
    PCSTRESN = c(0, 5.0, 0.1, 4.8),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = c("S1", "S1"),
    EXSTDTC = c("2024-01-01T08:00:00", "2024-01-02T08:00:00"),
    stringsAsFactors = FALSE
  )

  result <- derive_pcrftdtc(pc, ex)
  expect_true("PCRFTDTC" %in% names(result))
  # Pre-dose sample before first dose -> earliest dose used
  expect_equal(result$PCRFTDTC[1], "2024-01-01T08:00:00")
  # Post dose 1
  expect_equal(result$PCRFTDTC[2], "2024-01-01T08:00:00")
  # Pre-dose 2 (before dose 2, after dose 1) -> dose 1
  expect_equal(result$PCRFTDTC[3], "2024-01-01T08:00:00")
  # Post dose 2
  expect_equal(result$PCRFTDTC[4], "2024-01-02T08:00:00")
})

test_that("derive_pcrftdtc handles multiple subjects independently", {
  pc <- data.frame(
    USUBJID = c("S1", "S2"),
    PCDTC   = c("2024-01-01T09:00:00", "2024-01-01T11:00:00"),
    PCSTRESN = c(5.0, 3.0),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = c("S1", "S2"),
    EXSTDTC = c("2024-01-01T08:00:00", "2024-01-01T10:00:00"),
    stringsAsFactors = FALSE
  )

  result <- derive_pcrftdtc(pc, ex)
  expect_equal(result$PCRFTDTC[1], "2024-01-01T08:00:00")
  expect_equal(result$PCRFTDTC[2], "2024-01-01T10:00:00")
})

test_that("derive_pcrftdtc warns when PCRFTDTC already exists", {
  pc <- data.frame(
    USUBJID  = "S1",
    PCDTC    = "2024-01-01T09:00:00",
    PCRFTDTC = "old_value",
    PCSTRESN = 5.0,
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = "S1",
    EXSTDTC = "2024-01-01T08:00:00",
    stringsAsFactors = FALSE
  )

  expect_warning(
    derive_pcrftdtc(pc, ex),
    regexp = "already exists.*overwritten",
    class = "pknca_pcrftdtc_overwrite"
  )
})

test_that("derive_pcrftdtc errors on missing columns", {
  pc <- data.frame(USUBJID = "S1", stringsAsFactors = FALSE)
  ex <- data.frame(USUBJID = "S1", EXSTDTC = "2024-01-01T08:00:00",
                   stringsAsFactors = FALSE)
  expect_error(derive_pcrftdtc(pc, ex), regexp = "PCDTC.*not found")

  pc2 <- data.frame(USUBJID = "S1", PCDTC = "2024-01-01T09:00:00",
                    stringsAsFactors = FALSE)
  ex2 <- data.frame(USUBJID = "S1", stringsAsFactors = FALSE)
  expect_error(derive_pcrftdtc(pc2, ex2), regexp = "EXSTDTC.*not found")
})

# --- Tests for pc_to_PKNCAconc ------------------------------------------------

test_that("pc_to_PKNCAconc returns a PKNCAconc object", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0, 2.1),
    PCORRES  = c("0.05", "5.0", "2.1"),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T07:55:00", "2024-01-01T09:00:00",
                 "2024-01-01T11:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00",
                 "2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  expect_s3_class(result, "PKNCAconc")
})

test_that("pc_to_PKNCAconc derives time from PCRFTDTC", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0, 2.1),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00",
                 "2024-01-01T11:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00",
                 "2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  expect_equal(result$data$AFRLT, c(0, 1, 3))
})

test_that("pc_to_PKNCAconc uses PCELTM when available", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0, 2.1),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00",
                 "2024-01-01T11:00:00"),
    PCELTM   = c("PT0H", "PT1H", "PT3H"),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  expect_s3_class(result, "PKNCAconc")
  # Nominal time should be parsed PCELTM
  expect_equal(result$data$PCELTM_hours, c(0, 1, 3))
})

test_that("pc_to_PKNCAconc prefers PCRFTDTC for AFRLT when both PCELTM and PCRFTDTC exist", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:30:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00"),
    PCELTM   = c("PT0H", "PT1.5H"),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  # AFRLT derived from PCRFTDTC (actual times)
  expect_equal(result$data$AFRLT, c(0, 1.5))
  # PCELTM_hours used as nominal time
  expect_equal(result$data$PCELTM_hours, c(0, 1.5))
})

test_that("pc_to_PKNCAconc handles BLQ correctly", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(NA, 5.0, NA, NA),
    PCORRES  = c("0.05", "5.0", NA, "BLQ"),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00",
                 "2024-01-01T11:00:00", "2024-01-01T14:00:00"),
    PCRFTDTC = rep("2024-01-01T08:00:00", 4),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  conc <- result$data$PCSTRESN
  # Row 1: PCSTRESN=NA, PCORRES="0.05" (numeric) -> BLQ -> 0
  expect_equal(conc[1], 0)
  # Row 2: PCSTRESN=5.0 -> unchanged
  expect_equal(conc[2], 5.0)
  # Row 3: PCSTRESN=NA, PCORRES=NA -> stays NA (not done)
  expect_true(is.na(conc[3]))
  # Row 4: PCSTRESN=NA, PCORRES="BLQ" (non-numeric) -> stays NA
  expect_true(is.na(conc[4]))
})

test_that("pc_to_PKNCAconc builds correct formula with PCSPEC and PCTEST", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = rep("2024-01-01T08:00:00", 2),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  f <- formula(result)
  # Should be: PCSTRESN ~ AFRLT | PCSPEC + USUBJID / PCTEST
  expect_equal(deparse(f), "PCSTRESN ~ AFRLT | PCSPEC + USUBJID/PCTEST")
})

test_that("pc_to_PKNCAconc works without PCSPEC", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1"),
    PCTEST   = "DrugA",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = rep("2024-01-01T08:00:00", 2),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  f <- formula(result)
  expect_equal(deparse(f), "PCSTRESN ~ AFRLT | USUBJID/PCTEST")
})

test_that("pc_to_PKNCAconc works without PCTEST or PCSPEC", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1"),
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = rep("2024-01-01T08:00:00", 2),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  f <- formula(result)
  expect_equal(deparse(f), "PCSTRESN ~ AFRLT | USUBJID")
})

test_that("pc_to_PKNCAconc errors when no time derivation is possible", {
  pc <- data.frame(
    USUBJID  = "S1",
    PCSTRESN = 5.0,
    PCDTC    = "2024-01-01T09:00:00",
    stringsAsFactors = FALSE
  )
  expect_error(
    pc_to_PKNCAconc(pc),
    regexp = "Cannot derive time"
  )
})

test_that("pc_to_PKNCAconc errors on missing required columns", {
  pc <- data.frame(
    USUBJID = "S1",
    PCDTC   = "2024-01-01T09:00:00",
    stringsAsFactors = FALSE
  )
  expect_error(
    pc_to_PKNCAconc(pc),
    regexp = "Required column.*PCSTRESN"
  )
})

test_that("pc_to_PKNCAconc handles multiple subjects", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S2", "S2"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0, 0, 3.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00",
                 "2024-01-01T10:00:00", "2024-01-01T11:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00",
                 "2024-01-01T10:00:00", "2024-01-01T10:00:00"),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  expect_s3_class(result, "PKNCAconc")
  # S1: 0h, 1h; S2: 0h, 1h
  expect_equal(result$data$AFRLT, c(0, 1, 0, 1))
})

test_that("pc_to_PKNCAconc sets concu from PCSTRESU", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1"),
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ng/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = rep("2024-01-01T08:00:00", 2),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  # concu should be set from PCSTRESU column
  expect_true("concu" %in% names(result$columns))
})

test_that("pc_to_PKNCAconc works with PCELTM only (no PCRFTDTC)", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S1"),
    PCTEST   = "DrugA",
    PCSTRESN = c(0, 5.0, 2.1),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00",
                 "2024-01-01T11:00:00"),
    PCELTM   = c("PT0H", "PT1H", "PT3H"),
    stringsAsFactors = FALSE
  )
  result <- pc_to_PKNCAconc(pc)
  expect_s3_class(result, "PKNCAconc")
  # AFRLT should come from PCELTM when PCRFTDTC is absent
  expect_equal(result$data$AFRLT, c(0, 1, 3))
})

# --- Integration: derive_pcrftdtc + pc_to_PKNCAconc --------------------------

test_that("derive_pcrftdtc output feeds into pc_to_PKNCAconc", {
  pc <- data.frame(
    USUBJID  = c("S1", "S1", "S1"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0, 2.1),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00",
                 "2024-01-01T11:00:00"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = c("S1"),
    EXSTDTC = c("2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )

  pc_with_ref <- derive_pcrftdtc(pc, ex)
  result <- pc_to_PKNCAconc(pc_with_ref)
  expect_s3_class(result, "PKNCAconc")
  expect_equal(result$data$AFRLT, c(0, 1, 3))
})

# --- Tests for sdtm_join -----------------------------------------------------

test_that("sdtm_join merges DM columns onto target by shared keys", {
  target <- data.frame(
    STUDYID = "S1",
    USUBJID = c("S1-01", "S1-01", "S1-02"),
    DOMAIN  = "PC",
    VALUE   = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    STUDYID = "S1",
    USUBJID = c("S1-01", "S1-02"),
    DOMAIN  = "DM",
    AGE     = c(25, 30),
    SEX     = c("M", "F"),
    stringsAsFactors = FALSE
  )
  result <- sdtm_join(target, dm)
  expect_true("AGE" %in% names(result))
  expect_true("SEX" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$AGE, c(25, 25, 30))
  # DOMAIN from dm should NOT appear (target keeps its own)
  expect_equal(result$DOMAIN, c("PC", "PC", "PC"))
})

test_that("sdtm_join does not duplicate existing columns", {
  target <- data.frame(
    USUBJID = "S1-01",
    AGE     = 99,
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    USUBJID = "S1-01",
    AGE     = 25,
    SEX     = "M",
    stringsAsFactors = FALSE
  )
  result <- sdtm_join(target, dm)
  # AGE should remain from target (99), not overwritten
  expect_equal(result$AGE, 99)
  # SEX is new, should be added
  expect_equal(result$SEX, "M")
})

test_that("sdtm_join warns when no shared columns", {
  target <- data.frame(X = 1, stringsAsFactors = FALSE)
  src <- data.frame(Y = 2, stringsAsFactors = FALSE)
  expect_warning(
    sdtm_join(target, src),
    class = "pknca_sdtm_no_shared_cols"
  )
})

test_that("sdtm_join skips NULL or empty sources", {
  target <- data.frame(USUBJID = "S1", VALUE = 1, stringsAsFactors = FALSE)
  result <- sdtm_join(target, NULL, data.frame())
  expect_equal(result, target)
})

test_that("sdtm_join chains multiple sources", {
  target <- data.frame(
    USUBJID = c("S1", "S2"),
    VALUE   = c(1, 2),
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    USUBJID = c("S1", "S2"),
    AGE     = c(25, 30),
    stringsAsFactors = FALSE
  )
  vs_wide <- data.frame(
    USUBJID = c("S1", "S2"),
    WEIGHT  = c(70, 80),
    stringsAsFactors = FALSE
  )
  result <- sdtm_join(target, dm, vs_wide)
  expect_true(all(c("AGE", "WEIGHT") %in% names(result)))
  expect_equal(result$AGE, c(25, 30))
  expect_equal(result$WEIGHT, c(70, 80))
})

# --- Tests for vs_to_baseline ------------------------------------------------

test_that("vs_to_baseline filters by VSBLFL and pivots to wide", {
  vs <- data.frame(
    STUDYID  = "S1",
    USUBJID  = c("S1-01", "S1-01", "S1-01", "S1-01"),
    VSTESTCD = c("WEIGHT", "HEIGHT", "WEIGHT", "HEIGHT"),
    VSSTRESN = c(70, 175, 72, 176),
    VSBLFL   = c("Y", "Y", NA, NA),
    VISITNUM = c(1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  result <- vs_to_baseline(vs)
  expect_equal(nrow(result), 1)
  expect_true("WEIGHT" %in% names(result))
  expect_true("HEIGHT" %in% names(result))
  expect_equal(result$WEIGHT, 70)
  expect_equal(result$HEIGHT, 175)
})

test_that("vs_to_baseline falls back to first record when VSBLFL absent", {
  vs <- data.frame(
    USUBJID  = c("S1-01", "S1-01"),
    VSTESTCD = c("WEIGHT", "WEIGHT"),
    VSSTRESN = c(70, 72),
    VISITNUM = c(1, 2),
    stringsAsFactors = FALSE
  )
  result <- vs_to_baseline(vs)
  expect_equal(nrow(result), 1)
  expect_equal(result$WEIGHT, 70)
})

test_that("vs_to_baseline handles multiple subjects", {
  vs <- data.frame(
    USUBJID  = c("S1-01", "S1-01", "S1-02", "S1-02"),
    VSTESTCD = c("WEIGHT", "HEIGHT", "WEIGHT", "HEIGHT"),
    VSSTRESN = c(70, 175, 85, 180),
    VSBLFL   = "Y",
    stringsAsFactors = FALSE
  )
  result <- vs_to_baseline(vs)
  expect_equal(nrow(result), 2)
  expect_equal(result$WEIGHT, c(70, 85))
  expect_equal(result$HEIGHT, c(175, 180))
})

test_that("vs_to_baseline warns on no baseline records", {
  vs <- data.frame(
    USUBJID  = "S1-01",
    VSTESTCD = "WEIGHT",
    VSSTRESN = 70,
    VSBLFL   = NA,
    stringsAsFactors = FALSE
  )
  expect_warning(
    vs_to_baseline(vs),
    class = "pknca_vs_no_baseline"
  )
})

test_that("vs_to_baseline errors on missing required columns", {
  vs <- data.frame(USUBJID = "S1", stringsAsFactors = FALSE)
  expect_error(vs_to_baseline(vs), regexp = "VSTESTCD.*not found")
})

# --- Tests for sdtm_to_PKNCAdata ---------------------------------------------

test_that("sdtm_to_PKNCAdata returns a PKNCAdata object with PC and EX", {
  pc <- data.frame(
    STUDYID  = "S1",
    USUBJID  = c("S1-01", "S1-01"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    STUDYID = "S1",
    USUBJID = "S1-01",
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = "2024-01-01T08:00:00",
    EXENDTC = NA,
    stringsAsFactors = FALSE
  )
  result <- sdtm_to_PKNCAdata(pc, ex)
  expect_s3_class(result, "PKNCAdata")
  expect_s3_class(result$conc, "PKNCAconc")
  expect_s3_class(result$dose, "PKNCAdose")
})

test_that("sdtm_to_PKNCAdata enriches data with DM columns", {
  pc <- data.frame(
    STUDYID  = "S1",
    USUBJID  = c("S1-01", "S1-01"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    STUDYID = "S1",
    USUBJID = "S1-01",
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = "2024-01-01T08:00:00",
    EXENDTC = NA,
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    STUDYID = "S1",
    USUBJID = "S1-01",
    AGE     = 25,
    SEX     = "M",
    stringsAsFactors = FALSE
  )
  result <- sdtm_to_PKNCAdata(pc, ex, dm = dm)
  # DM columns should be present in both conc and dose data
  expect_true("AGE" %in% names(result$conc$data))
  expect_true("SEX" %in% names(result$conc$data))
  expect_true("AGE" %in% names(result$dose$data))
})

test_that("sdtm_to_PKNCAdata enriches data with baseline VS", {
  pc <- data.frame(
    STUDYID  = "S1",
    USUBJID  = c("S1-01", "S1-01"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    STUDYID = "S1",
    USUBJID = "S1-01",
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = "2024-01-01T08:00:00",
    EXENDTC = NA,
    stringsAsFactors = FALSE
  )
  vs <- data.frame(
    STUDYID  = "S1",
    USUBJID  = "S1-01",
    VSTESTCD = c("WEIGHT", "HEIGHT"),
    VSSTRESN = c(70, 175),
    VSBLFL   = "Y",
    stringsAsFactors = FALSE
  )
  result <- sdtm_to_PKNCAdata(pc, ex, vs = vs)
  expect_true("WEIGHT" %in% names(result$conc$data))
  expect_true("HEIGHT" %in% names(result$conc$data))
  expect_equal(unique(result$conc$data$WEIGHT), 70)
})

test_that("sdtm_to_PKNCAdata derives PCRFTDTC from EX when missing", {
  pc <- data.frame(
    STUDYID  = "S1",
    USUBJID  = c("S1-01", "S1-01"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    STUDYID = "S1",
    USUBJID = "S1-01",
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = "2024-01-01T08:00:00",
    EXENDTC = NA,
    stringsAsFactors = FALSE
  )
  # PC has no PCRFTDTC or PCELTM — should auto-derive from EX
  result <- sdtm_to_PKNCAdata(pc, ex)
  expect_s3_class(result, "PKNCAdata")
  expect_equal(result$conc$data$AFRLT, c(0, 1))
})

test_that("sdtm_to_PKNCAdata passes pc_args and ex_args through", {
  pc <- data.frame(
    STUDYID  = "S1",
    SUBJ     = c("S1-01", "S1-01"),
    PCTEST   = "DrugA",
    PCSPEC   = "SERUM",
    PCSTRESN = c(0, 5.0),
    PCSTRESU = "ug/mL",
    PCDTC    = c("2024-01-01T08:00:00", "2024-01-01T09:00:00"),
    PCRFTDTC = c("2024-01-01T08:00:00", "2024-01-01T08:00:00"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    STUDYID = "S1",
    SUBJ    = "S1-01",
    EXTRT   = "DRUG A",
    EXDOSE  = 100,
    EXDOSU  = "mg",
    EXROUTE = "ORAL",
    EXSTDTC = "2024-01-01T08:00:00",
    EXENDTC = NA,
    stringsAsFactors = FALSE
  )
  # Use custom USUBJID column name
  result <- sdtm_to_PKNCAdata(
    pc, ex,
    pc_args = list(USUBJID = "SUBJ"),
    ex_args = list(USUBJID = "SUBJ")
  )
  expect_s3_class(result, "PKNCAdata")
})

