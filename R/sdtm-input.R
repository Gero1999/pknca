# --- Shared SDTM helpers -----------------------------------------------------

#' Parse ISO 8601 date-time strings with mixed precision
#'
#' Handles full datetime, datetime without seconds, datetime with hour only,
#' and date-only formats. Returns POSIXct in UTC.
#'
#' @param dtc Character vector of ISO 8601 date-time strings
#' @return POSIXct vector in UTC
#' @keywords internal
std_dtc_to_rdate <- function(dtc) {
  formats <- c(
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%dT%H",
    "%Y-%m-%d"
  )
  dtc_to_dt <- list()
  for (fmt in formats) {
    dtc_to_dt[[fmt]] <- as.POSIXct(dtc, format = fmt, tz = "UTC")
  }
  dplyr::coalesce(
    dtc_to_dt[[formats[1]]], dtc_to_dt[[formats[2]]],
    dtc_to_dt[[formats[3]]], dtc_to_dt[[formats[4]]]
  )
}

#' Parse an ISO 8601 duration string to numeric hours
#'
#' Supports durations in the form \code{PT<number>H}, \code{PT<number>M},
#' \code{PT<number>S}, or combinations like \code{PT1H30M}. Returns the total
#' duration in hours as a numeric value.
#'
#' @param x Character vector of ISO 8601 duration strings (e.g. \code{"PT2H"},
#'   \code{"PT1H30M"}, \code{"PT90M"})
#' @return Numeric vector of durations in hours
#' @keywords internal
parse_iso8601_duration <- function(x) {
  vapply(x, function(val) {
    if (is.na(val) || !grepl("^PT", val)) return(NA_real_)
    hours <- 0
    h_match <- regmatches(val, regexpr("[0-9.]+(?=H)", val, perl = TRUE))
    if (length(h_match) == 1) hours <- hours + as.numeric(h_match)
    m_match <- regmatches(val, regexpr("[0-9.]+(?=M)", val, perl = TRUE))
    if (length(m_match) == 1) hours <- hours + as.numeric(m_match) / 60
    s_match <- regmatches(val, regexpr("[0-9.]+(?=S)", val, perl = TRUE))
    if (length(s_match) == 1) hours <- hours + as.numeric(s_match) / 3600
    hours
  }, numeric(1), USE.NAMES = FALSE)
}

#' Map CDISC route of administration to PKNCA route
#'
#' @param route Character vector of CDISC route values
#' @return Character vector of \code{"intravascular"} or
#'   \code{"extravascular"}
#' @keywords internal
route_cdisc_to_pknca <- function(route) {
  intravascular_pattern <- paste0(
    "(INFUS|DRIP|IV|INTRAVEN|IVADMIN|BOLUS|INTRAVASCULAR|INTRA-?ARTERIAL|",
    "INTRACARDIAC|INTRACORONARY)"
  )
  ifelse(
    grepl(intravascular_pattern, gsub("[^[:alnum:]]", "", toupper(route))),
    "intravascular",
    "extravascular"
  )
}

# --- EX to PKNCAdose ---------------------------------------------------------

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

# --- Derive PCRFTDTC from EX ------------------------------------------------

#' Derive PCRFTDTC (reference datetime) for PC data from EX dosing records
#'
#' For each PC record, assigns the most recent dose start datetime
#' (\code{EXSTDTC}) that is at or before the sample collection time
#' (\code{PCDTC}) for the same subject. This is the standard SDTM derivation
#' of the reference datetime used to compute elapsed time relative to the most
#' recent dose.
#'
#' @param pc A data.frame containing the PC (Pharmacokinetic Concentrations)
#'   SDTM domain. Must contain at least \code{USUBJID} and \code{PCDTC}
#'   columns (or the names specified via parameters).
#' @param ex A data.frame containing the EX (Exposure) SDTM domain. Must
#'   contain at least \code{USUBJID} and \code{EXSTDTC} columns (or the names
#'   specified via parameters).
#' @param USUBJID Column name for the unique subject identifier (must match
#'   in both \code{pc} and \code{ex})
#' @param PCDTC Column name for the PC collection date/time (ISO 8601)
#' @param EXSTDTC Column name for the EX start date/time (ISO 8601)
#' @param PCRFTDTC Column name to assign for the derived reference date/time.
#'   If this column already exists in \code{pc}, it is overwritten with a
#'   warning.
#' @return The \code{pc} data.frame with an added (or replaced) \code{PCRFTDTC}
#'   column containing ISO 8601 datetime strings.
#' @export
derive_pcrftdtc <- function(
  pc,
  ex,
  USUBJID = "USUBJID",
  PCDTC = "PCDTC",
  EXSTDTC = "EXSTDTC",
  PCRFTDTC = "PCRFTDTC"
) {
  checkmate::assert_data_frame(pc, min.rows = 1)
  checkmate::assert_data_frame(ex, min.rows = 1)
  checkmate::assert_string(USUBJID)
  checkmate::assert_string(PCDTC)
  checkmate::assert_string(EXSTDTC)
  checkmate::assert_string(PCRFTDTC)

  if (!USUBJID %in% names(pc)) {
    stop("Column '", USUBJID, "' not found in pc data")
  }
  if (!PCDTC %in% names(pc)) {
    stop("Column '", PCDTC, "' not found in pc data")
  }
  if (!USUBJID %in% names(ex)) {
    stop("Column '", USUBJID, "' not found in ex data")
  }
  if (!EXSTDTC %in% names(ex)) {
    stop("Column '", EXSTDTC, "' not found in ex data")
  }

  if (PCRFTDTC %in% names(pc)) {
    rlang::warn(
      paste0("Column '", PCRFTDTC, "' already exists in pc and will be overwritten"),
      class = "pknca_pcrftdtc_overwrite"
    )
  }

  # Parse datetimes
  pc_dt <- std_dtc_to_rdate(pc[[PCDTC]])
  ex_subj <- ex[[USUBJID]]
  ex_dt <- std_dtc_to_rdate(ex[[EXSTDTC]])

  # For each PC record, find the most recent dose at or before collection time
  pcrftdtc <- vapply(seq_len(nrow(pc)), function(i) {
    subj <- pc[[USUBJID]][i]
    sample_time <- pc_dt[i]
    if (is.na(sample_time)) return(NA_character_)

    # All dose times for this subject
    mask <- ex_subj == subj & !is.na(ex_dt)
    dose_times <- ex_dt[mask]

    if (length(dose_times) == 0) return(NA_character_)

    # Most recent dose at or before sample time
    eligible <- dose_times[dose_times <= sample_time]
    if (length(eligible) == 0) {
      # Pre-dose sample: use the earliest dose for this subject
      ref <- min(dose_times)
    } else {
      ref <- max(eligible)
    }
    format(ref, "%Y-%m-%dT%H:%M:%S")
  }, character(1), USE.NAMES = FALSE)

  pc[[PCRFTDTC]] <- pcrftdtc
  pc
}

# --- PC to PKNCAconc ---------------------------------------------------------

#' Convert a PC (Pharmacokinetic Concentrations) SDTM domain to a PKNCAconc
#' object
#'
#' Transforms a CDISC SDTM PC domain data frame into a \code{PKNCAconc} object
#' suitable for NCA analysis with PKNCA.
#'
#' @section Time derivation:
#' The function computes a relative time variable (\code{AFRLT}, actual time
#' from reference in hours) using the following priority:
#' \enumerate{
#'   \item \code{PCELTM}: If the column exists in \code{pc}, its ISO 8601
#'     duration values (e.g. \code{"PT2H"}) are parsed to numeric hours.
#'   \item \code{PCRFTDTC}: If \code{PCELTM} is absent but \code{PCRFTDTC}
#'     exists, elapsed time is derived as \code{PCDTC - PCRFTDTC} in hours.
#'     Use \code{\link{derive_pcrftdtc}} to populate this column from EX data
#'     before calling this function.
#'   \item If neither is available, the function stops with an error.
#' }
#'
#' @section BLQ handling:
#' Below the limit of quantification (BLQ) concentrations are identified by
#' comparing \code{PCSTRESN} and \code{PCORRES}:
#' \itemize{
#'   \item When \code{PCSTRESN} is \code{NA} but \code{PCORRES} contains a
#'     parseable numeric value, the record is treated as BLQ and the
#'     concentration is set to \code{0}.
#'   \item When both \code{PCSTRESN} and \code{PCORRES} are \code{NA} (or
#'     \code{PCORRES} is non-numeric), the concentration remains \code{NA}
#'     (not done / missing sample).
#' }
#' PKNCA's \code{clean.conc.blq} options control how BLQ values (zeros) are
#' handled during NCA calculations.
#'
#' @param pc A data.frame containing the PC SDTM domain
#' @param USUBJID Column name for the unique subject identifier
#' @param PCTEST Column name for the analyte/test name. Used as the analyte
#'   grouping variable in the PKNCAconc formula (after \code{/}).
#' @param PCSPEC Column name for the specimen type (e.g. \code{"SERUM"},
#'   \code{"PLASMA"}). Used as an additional grouping variable.
#' @param PCSTRESN Column name for the standardized numeric result
#'   (concentration). This is the primary concentration value.
#' @param PCORRES Column name for the original (reported) result. Used to
#'   detect BLQ records: when \code{PCSTRESN} is \code{NA} but \code{PCORRES}
#'   has a numeric value, the concentration is set to \code{0} (BLQ).
#' @param PCSTRESU Column name for the standardized result units
#' @param PCDTC Column name for the collection date/time (ISO 8601)
#' @param PCELTM Column name for the planned elapsed time (ISO 8601 duration,
#'   e.g. \code{"PT2H"}). If present, used as the nominal time variable.
#' @param PCRFTDTC Column name for the reference date/time (ISO 8601). Used
#'   to derive elapsed time when \code{PCELTM} is absent. See
#'   \code{\link{derive_pcrftdtc}} to populate this from EX data.
#' @return A \code{PKNCAconc} object with:
#' \itemize{
#'   \item Concentration from \code{PCSTRESN} (BLQ set to 0)
#'   \item Time as \code{AFRLT} (hours from reference)
#'   \item Groups: \code{PCSPEC + USUBJID / PCTEST}
#'   \item Units from \code{PCSTRESU} (if available)
#'   \item Nominal time from \code{PCELTM} (if available, parsed to hours)
#' }
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @export
pc_to_PKNCAconc <- function(
  pc,
  USUBJID = "USUBJID",
  PCTEST = "PCTEST",
  PCSPEC = "PCSPEC",

  # Concentration columns
  PCSTRESN = "PCSTRESN",
  PCORRES = "PCORRES",
  PCSTRESU = "PCSTRESU",

  # Time columns
  PCDTC = "PCDTC",
  PCELTM = "PCELTM",
  PCRFTDTC = "PCRFTDTC"
) {
  checkmate::assert_data_frame(pc, min.rows = 1)

  # --- Validate required columns ---
  required <- c(USUBJID, PCSTRESN, PCDTC)
  missing_req <- setdiff(required, names(pc))
  if (length(missing_req) > 0) {
    stop(
      "Required column(s) missing from pc: ",
      paste(missing_req, collapse = ", ")
    )
  }

  # --- BLQ handling ---
  # When PCSTRESN is NA but PCORRES has a numeric value, treat as BLQ (set to 0)
  conc <- pc[[PCSTRESN]]
  if (PCORRES %in% names(pc)) {
    pcorres_numeric <- suppressWarnings(as.numeric(pc[[PCORRES]]))
    blq_mask <- is.na(conc) & !is.na(pcorres_numeric)
    conc[blq_mask] <- 0
  }
  pc[[PCSTRESN]] <- conc

  # --- Time derivation ---
  has_pceltm <- PCELTM %in% names(pc)
  has_pcrftdtc <- PCRFTDTC %in% names(pc)
  has_pcdtc <- PCDTC %in% names(pc)

  if (has_pceltm) {
    # Parse ISO 8601 duration to numeric hours
    pceltm_hours <- parse_iso8601_duration(pc[[PCELTM]])
    pc[["PCELTM_hours"]] <- pceltm_hours
  }

  if (!has_pcdtc) {
    stop("Column '", PCDTC, "' is required in pc data")
  }

  if (has_pcrftdtc) {
    # Derive AFRLT = PCDTC - PCRFTDTC in hours
    pc_dt <- std_dtc_to_rdate(pc[[PCDTC]])
    ref_dt <- std_dtc_to_rdate(pc[[PCRFTDTC]])
    pc[["AFRLT"]] <- as.numeric(difftime(pc_dt, ref_dt, units = "hours"))
  } else if (has_pceltm) {
    # Use parsed PCELTM as the time axis
    pc[["AFRLT"]] <- pc[["PCELTM_hours"]]
  } else {
    stop(
      "Cannot derive time: neither '", PCELTM, "' nor '", PCRFTDTC,
      "' found in pc data. ",
      "Use derive_pcrftdtc() to add a reference datetime from EX data."
    )
  }

  # --- Build formula ---
  # Groups: PCSPEC + USUBJID / PCTEST (analyte)
  # PCTEST is the analyte, placed after / so PKNCAconc treats USUBJID as subject
  has_pcspec <- PCSPEC %in% names(pc)
  has_pctest <- PCTEST %in% names(pc)

  if (has_pcspec && has_pctest) {
    group_str <- paste0(PCSPEC, "+", USUBJID, "/", PCTEST)
  } else if (has_pctest) {
    group_str <- paste0(USUBJID, "/", PCTEST)
  } else if (has_pcspec) {
    group_str <- paste0(PCSPEC, "+", USUBJID)
  } else {
    group_str <- USUBJID
  }

  formula_str <- paste(PCSTRESN, "~", "AFRLT", "|", group_str)

  # --- Build PKNCAconc arguments ---
  PKNCAconc_args <- list(
    data = pc,
    formula = as.formula(formula_str)
  )

  # Nominal time from PCELTM (parsed to hours)
  if (has_pceltm) {
    PKNCAconc_args$time.nominal <- "PCELTM_hours"
  }

  # Concentration units
  if (PCSTRESU %in% names(pc)) {
    PKNCAconc_args$concu <- PCSTRESU
  }

  # Time units are always hours (from our derivation)
  PKNCAconc_args$timeu <- "hr"

  do.call(PKNCA::PKNCAconc, PKNCAconc_args)
}
