#' Convert an EX (Exposure) SDTM domain to a PKNCAdose object
#'
#' Transforms a CDISC SDTM EX domain data frame into a \code{PKNCAdose} object
#' suitable for NCA analysis with PKNCA. Handles date-time parsing, duration
#' derivation, elapsed time derivation, route mapping, and relative time
#' computation.
#'
#' @param ex A data.frame containing the EX (Exposure) SDTM domain
#' @param USUBJID Column name for the unique subject identifier
#' @param EXTRT Column name for the treatment name
#' @param EXSTDTC Column name for the start date/time of treatment (ISO 8601)
#' @param EXDUR Column name for the duration of treatment. If the column is
#'   absent, it is derived from \code{EXSTDTC} and \code{EXENDTC}.
#' @param EXENDTC Column name for the end date/time of treatment (ISO 8601).
#'   Used to derive \code{EXDUR} when not available.
#' @param EXELTM Column name for the planned elapsed time since first dose.
#'   If absent, derived from \code{EXSTDTC} and \code{EXRFTDTC}.
#' @param EXTPTNUM Column name for the planned time point number
#' @param EXRFTDTC Column name for the reference date/time (ISO 8601).
#'   Used to derive \code{EXELTM} when not available.
#' @param EXDOSE Column name for the dose per administration
#' @param EXDOSU Column name for the dose units
#' @param EXROUTE Column name for the route of administration
#' @return A \code{PKNCAdose} object
#' @importFrom dplyr mutate group_by ungroup coalesce
#' @importFrom rlang sym
#' @export
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
      "%Y-%m-%dT%H",
      "%Y-%m-%d"
    )
    dtc_to_dt <- list()
    for (fmt in formats) {
      dtc_to_dt[[fmt]] <-
        as.POSIXct(dtc, format=fmt, tz="UTC")
    }
    # Combine the results, preferring the most complete formats first
    dplyr::coalesce(dtc_to_dt[[formats[1]]], dtc_to_dt[[formats[2]]],
                    dtc_to_dt[[formats[3]]], dtc_to_dt[[formats[4]]])
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
        dur <- as.numeric(difftime(
          !!sym(EXENDTC),
          !!sym(EXSTDTC),
          units="hours"
        ))
        # When EXENDTC is NA (e.g. oral/instantaneous doses), duration defaults to 0
        ifelse(is.na(dur), 0, dur)
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
    route = if(EXROUTE %in% names(ex)) route_cdisc_to_pknca(ex2[[EXROUTE]]) else NULL,
    time.nominal = if (EXELTM %in% names(ex2)) EXELTM else NULL,
    duration = if (EXDUR %in% names(ex2)) EXDUR else NULL,
    doseu = if (EXDOSU %in% names(ex2)) EXDOSU else NULL
  )
  # Remove NULL entries
  PKNCAdose_args <- PKNCAdose_args[!sapply(PKNCAdose_args, is.null)]
  do.call(PKNCA::PKNCAdose, PKNCAdose_args)
}

