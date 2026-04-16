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

