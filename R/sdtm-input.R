ex <- pharmaversesdtm::ex_vaccine
ex_to_PKNCAdose <- function(
  ex,
  USUBJID = "USUBJID",
  EXTRT = "EXTRT",


  # Time variables to determine dose
  EXSTDTC = "EXSTDTC",
  EXDUR = "EXDUR",
  # In case EXDUR is not derived
  EXENDTC = "EXENDTC",

  # Nominal time variables
  EXELTM = "EXELTM",
  # In case EXELTM is not derived
  EXTPTNUM = "EXTPTNUM",
  EXRFTDTC = "EXRFTDTC",

  EXDOSE = "EXDOSE",
  EXDOSU = "EXDOSU",
  EXROUTE = "EXROUTE"
) {

  std_dtc_to_rdate <- function(dtc) {
    formats <- c(
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%dT%H:%M",
      "%Y-%m-%d%T%H"
    )
    dtc_to_dt <- list()
    for (fmt in formats) {
      dtc_to_dt[[fmt]] <-
        as.POSIXct(dtc, format=fmt, tz="UTC")
    }
    # Combine the results, preferring the most complete formats first
    coalesce(dtc_to_dt[[formats[1]]], dtc_to_dt[[formats[2]]], dtc_to_dt[[formats[3]]])
  }

  route_cdisc_to_pknca <- function(route) {
    intravascular_pattern <- paste0(
    "(INFUS|DRIP|IV|INTRAVEN|IVADMIN|BOLUS|INTRAVASCULAR|INTRA-?ARTERIAL|",
    "INTRACARDIAC|INTRACORONARY)"
    )
    ifelse(
        grepl(intravascular_pattern, gsub("[^[:alnum:]]", "", route)),
        "intravascular",
        "extravascular"
      )
  }

  ex2 <- ex %>%

    # Standardise all dates to R date-time format
    mutate(
      !!sym(EXSTDTC) := if (!!EXSTDTC %in% names(ex)) {
        std_dtc_to_rdate(!!sym(EXSTDTC))
      } else {
        as.POSIXct(NA)
      },
      !!sym(EXENDTC) := if (!!EXENDTC %in% names(ex)) {
        std_dtc_to_rdate(!!sym(EXENDTC))
      } else {
        as.POSIXct(NA)
      },
      !!sym(EXRFTDTC) := if (!!EXRFTDTC %in% names(ex)) {
        std_dtc_to_rdate(!!sym(EXRFTDTC))
      } else {
        NULL
      }
    ) %>%
    # Derive EXDUR if missing
    mutate(
      !!sym(EXDUR) := if (!!EXDUR %in% names(ex)) {
        !!sym(EXDUR)
      } else {
        as.numeric(difftime(
          !!sym(EXENDTC),
          !!sym(EXSTDTC),
          units="hours"
        ))
      }
    ) %>%
    # Derive EXELTM if missing
    mutate(
      !!sym(EXELTM) := if (!!EXELTM %in% names(ex)) {
        !!sym(EXELTM)
      } else if (!!EXRFTDTC %in% names(ex)) {
        as.numeric(difftime(
          !!sym(EXSTDTC),
          !!sym(EXRFTDTC),
          units="hours"
        ))
      } else {
        NULL
      }
    ) %>%
    # Determine for each subject the reference (first) dose date-time
    group_by(!!sym(USUBJID)) %>%
    mutate(
      EX_reference = min(!!sym(EXSTDTC), na.rm=TRUE)
    ) %>%
    ungroup() %>%
    # Determine dose time in hours from reference
    mutate(
      AFRLT = as.numeric(difftime(
        EXSTDTC,
        EX_reference,
        units="hours"
      )),
    )
  PKNCAdose_args <- list(
    data = ex2,
    formula = as.formula(
      paste(EXDOSE, "~", "AFRLT", "|", paste(c(EXTRT, USUBJID), collapse="+"))
    ),
    route = if(EXROUTE %in% names(ex)) route_cdisc_to_pknca(EXROUTE) else NULL,
    time.nominal = if (EXELTM %in% names(ex2)) EXELTM else NULL,
    duration = if (EXDUR %in% names(ex2)) EXDUR else NULL,
    doseu = if (EXDOSU %in% names(ex2)) EXDOSU else NULL
  )
  # Remove NULL entries
  PKNCAdose_args <- PKNCAdose_args[!sapply(PKNCAdose_args, is.null)]
  do.call(PKNCA::PKNCAdose, PKNCAdose_args)
}
o_dose <- ex_to_PKNCAdose(ex)
