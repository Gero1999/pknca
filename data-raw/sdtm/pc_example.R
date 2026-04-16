# Example SDTM PC (Pharmacokinetic Concentrations) domain
#
# Simulates a Phase I PK study with:
# - 4 subjects across 2 treatment arms (Drug A oral, Drug B IV infusion)
# - Single analyte (DrugA) measured in serum
# - Multiple dosing occasions (Day 1 and Day 2, 24h apart)
# - Pre-dose, post-dose, and trough time points
# - BLQ samples (PCSTRESN = NA with numeric PCORRES)
# - NOT DONE samples (both PCSTRESN and PCORRES = NA)
# - PCRFTDTC and PCELTM provided for time derivation
#
# Modeled after pharmaverse/aNCA sdtm_example.R structure

pc_example <- data.frame(
  STUDYID  = "PKS-001",
  DOMAIN   = "PC",
  USUBJID  = c(
    # Subject 1 (Drug A oral): 8 samples across 2 doses
    rep("PKS-001-001", 8),
    # Subject 2 (Drug A oral): 8 samples across 2 doses
    rep("PKS-001-002", 8),
    # Subject 3 (Drug B IV): 8 samples across 2 doses
    rep("PKS-001-003", 8),
    # Subject 4 (Drug B IV): 6 samples (includes NOT DONE)
    rep("PKS-001-004", 6)
  ),
  PCTEST   = "DrugA",
  PCTESTCD = "DRUGA",
  PCSPEC   = "SERUM",
  PCORRES  = c(
    # Subject 1: Day 1 pre-dose BLQ, then measurable, Day 2 pre-dose BLQ
    "0.05", "1.2", "5.8", "2.1", "0.05", "1.5", "6.2", "2.4",
    # Subject 2: similar pattern, one NOT DONE
    "0.05", "0.9", "4.5", "1.8", NA, "1.1", "5.0", "2.0",
    # Subject 3: IV, higher concentrations
    "0.1", "8.5", "15.2", "6.3", "0.1", "9.0", "16.1", "7.0",
    # Subject 4: IV, some samples
    "0.1", "7.2", "13.8", "5.5", "0.1", "7.8"
  ),
  PCSTRESN = c(
    # Subject 1: BLQ at pre-dose (NA), rest measurable
    NA, 1.2, 5.8, 2.1, NA, 1.5, 6.2, 2.4,
    # Subject 2: BLQ at pre-dose, NOT DONE at Day 2 pre-dose
    NA, 0.9, 4.5, 1.8, NA, 1.1, 5.0, 2.0,
    # Subject 3
    NA, 8.5, 15.2, 6.3, NA, 9.0, 16.1, 7.0,
    # Subject 4
    NA, 7.2, 13.8, 5.5, NA, 7.8
  ),
  PCSTRESU = "ug/mL",
  PCDTC    = c(
    # Subject 1: Day 1 (08:00 dose), Day 2 (08:00 dose)
    "2024-03-01T07:55:00", "2024-03-01T09:00:00",
    "2024-03-01T11:00:00", "2024-03-01T14:00:00",
    "2024-03-02T07:55:00", "2024-03-02T09:00:00",
    "2024-03-02T11:00:00", "2024-03-02T14:00:00",
    # Subject 2: Day 1 (08:15 dose), Day 2 (08:15 dose)
    "2024-03-01T08:10:00", "2024-03-01T09:15:00",
    "2024-03-01T11:15:00", "2024-03-01T14:15:00",
    "2024-03-02T08:10:00", "2024-03-02T09:15:00",
    "2024-03-02T11:15:00", "2024-03-02T14:15:00",
    # Subject 3: Day 1 (09:00 infusion start), Day 2 (09:00 infusion start)
    "2024-03-01T08:55:00", "2024-03-01T10:00:00",
    "2024-03-01T12:00:00", "2024-03-01T15:00:00",
    "2024-03-02T08:55:00", "2024-03-02T10:00:00",
    "2024-03-02T12:00:00", "2024-03-02T15:00:00",
    # Subject 4: Day 1 (09:05 infusion start), Day 2
    "2024-03-01T09:00:00", "2024-03-01T10:05:00",
    "2024-03-01T12:05:00", "2024-03-01T15:05:00",
    "2024-03-02T09:00:00", "2024-03-02T10:05:00"
  ),
  PCRFTDTC = c(
    # Subject 1: reference = dose time for each period
    rep("2024-03-01T08:00:00", 4), rep("2024-03-02T08:00:00", 4),
    # Subject 2
    rep("2024-03-01T08:15:00", 4), rep("2024-03-02T08:15:00", 4),
    # Subject 3
    rep("2024-03-01T09:00:00", 4), rep("2024-03-02T09:00:00", 4),
    # Subject 4
    rep("2024-03-01T09:05:00", 4), rep("2024-03-02T09:05:00", 2)
  ),
  PCELTM   = c(
    # Subject 1: nominal times relative to dose
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    # Subject 2
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    # Subject 3
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    # Subject 4
    "PT-0.083H", "PT1H", "PT3H", "PT6H",
    "PT-0.083H", "PT1H"
  ),
  stringsAsFactors = FALSE
)

# Corresponding EX data for derive_pcrftdtc example
ex_for_pc_example <- data.frame(
  STUDYID  = "PKS-001",
  DOMAIN   = "EX",
  USUBJID  = c(
    "PKS-001-001", "PKS-001-001",
    "PKS-001-002", "PKS-001-002",
    "PKS-001-003", "PKS-001-003",
    "PKS-001-004", "PKS-001-004"
  ),
  EXTRT    = c(rep("DRUG A", 4), rep("DRUG B", 4)),
  EXDOSE   = c(100, 100, 200, 200, 50, 50, 50, 50),
  EXDOSU   = "mg",
  EXROUTE  = c(rep("ORAL", 4), rep("INTRAVENOUS INFUSION", 4)),
  EXSTDTC  = c(
    "2024-03-01T08:00:00", "2024-03-02T08:00:00",
    "2024-03-01T08:15:00", "2024-03-02T08:15:00",
    "2024-03-01T09:00:00", "2024-03-02T09:00:00",
    "2024-03-01T09:05:00", "2024-03-02T09:05:00"
  ),
  EXENDTC  = c(
    NA, NA, NA, NA,
    "2024-03-01T10:00:00", "2024-03-02T10:00:00",
    "2024-03-01T10:05:00", "2024-03-02T10:05:00"
  ),
  stringsAsFactors = FALSE
)
