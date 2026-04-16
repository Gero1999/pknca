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

# --- Enrich SDTM domains with subject-level data ----------------------------

#' Join subject-level SDTM domains onto a target domain
#'
#' Merges columns from one or more subject-level SDTM data frames (e.g. DM, VS)
#' onto a target data frame using the columns that are common between them as
#' join keys. This follows the CDISC convention where \code{STUDYID} and
#' \code{USUBJID} are the standard cross-domain identifiers.
#'
#' Columns that already exist in \code{target} are not duplicated from the
#' source domain (the target's version is kept). The \code{DOMAIN} column, if
#' present, is always excluded from the join keys and from the merged result
#' because it is domain-specific by definition.
#'
#' @param target A data.frame to enrich (e.g. PC or EX domain)
#' @param ... One or more data.frames to merge onto \code{target}. Each is
#'   joined sequentially via \code{dplyr::left_join} using the intersection of
#'   column names (excluding \code{DOMAIN}) as keys.
#' @return The \code{target} data.frame with additional columns from each
#'   source domain.
#' @keywords internal
sdtm_join <- function(target, ...) {
  sources <- list(...)
  for (src in sources) {
    if (is.null(src) || !is.data.frame(src) || nrow(src) == 0) next

    # Exclude DOMAIN from key detection — it is domain-specific
    target_names <- setdiff(names(target), "DOMAIN")
    src_names <- setdiff(names(src), "DOMAIN")

    join_keys <- intersect(target_names, src_names)
    if (length(join_keys) == 0) {
      rlang::warn(
        "No shared columns found between target and source domain; skipping join",
        class = "pknca_sdtm_no_shared_cols"
      )
      next
    }

    # Only bring in columns that don't already exist in target (plus the keys)
    new_cols <- setdiff(src_names, target_names)
    if (length(new_cols) == 0) next

    src_subset <- src[, c(join_keys, new_cols), drop = FALSE]
    target <- dplyr::left_join(target, src_subset, by = join_keys)
  }
  target
}

#' Prepare baseline vital signs as a one-row-per-subject data frame
#'
#' Filters a VS (Vital Signs) SDTM domain to baseline records and pivots the
#' result to wide format so that each test code becomes its own column (e.g.
#' \code{WEIGHT}, \code{HEIGHT}). The resulting data frame has one row per
#' subject and is ready to be joined onto other domains.
#'
#' @param vs A data.frame containing the VS SDTM domain
#' @param USUBJID Column name for the unique subject identifier
#' @param VSTESTCD Column name for the vital sign test code
#' @param VSSTRESN Column name for the standardized numeric result
#' @param VSBLFL Column name for the baseline flag. Records with
#'   \code{VSBLFL == "Y"} are selected. If the column is absent, the earliest
#'   record per subject per test is used instead.
#' @param STUDYID Column name for the study identifier (included in output
#'   if present)
#' @return A data.frame with one row per subject and one column per vital sign
#'   test code (e.g. \code{WEIGHT}, \code{HEIGHT}).
#' @keywords internal
vs_to_baseline <- function(
  vs,
  USUBJID = "USUBJID",
  VSTESTCD = "VSTESTCD",
  VSSTRESN = "VSSTRESN",
  VSBLFL = "VSBLFL",
  STUDYID = "STUDYID"
) {
  checkmate::assert_data_frame(vs, min.rows = 1)

  if (!USUBJID %in% names(vs)) {
    stop("Column '", USUBJID, "' not found in vs data")
  }
  if (!VSTESTCD %in% names(vs)) {
    stop("Column '", VSTESTCD, "' not found in vs data")
  }
  if (!VSSTRESN %in% names(vs)) {
    stop("Column '", VSSTRESN, "' not found in vs data")
  }

  # Filter to baseline records
  if (VSBLFL %in% names(vs)) {
    vs_bl <- vs[!is.na(vs[[VSBLFL]]) & vs[[VSBLFL]] == "Y", , drop = FALSE]
  } else {
    # Fallback: take first record per subject per test
    vs <- vs[order(vs[[USUBJID]], vs[[VSTESTCD]]), , drop = FALSE]
    vs_bl <- vs[!duplicated(vs[, c(USUBJID, VSTESTCD)]), , drop = FALSE]
  }

  if (nrow(vs_bl) == 0) {
    rlang::warn(
      "No baseline VS records found; returning empty data frame",
      class = "pknca_vs_no_baseline"
    )
    # Return minimal structure
    id_cols <- intersect(c(STUDYID, USUBJID), names(vs))
    return(vs[0, id_cols, drop = FALSE])
  }

  # Select only the columns needed for pivoting
  id_cols <- intersect(c(STUDYID, USUBJID), names(vs_bl))
  pivot_data <- vs_bl[, c(id_cols, VSTESTCD, VSSTRESN), drop = FALSE]

  # Pivot to wide: one column per VSTESTCD
  tidyr::pivot_wider(
    pivot_data,
    names_from = VSTESTCD,
    values_from = VSSTRESN
  )
}

# --- SDTM to PKNCAdata -------------------------------------------------------

#' Convert SDTM domains (PC, EX, DM, VS) to a PKNCAdata object
#'
#' Orchestrates the full conversion from CDISC SDTM domain data frames into a
#' \code{PKNCAdata} object ready for NCA analysis. Internally calls
#' \code{\link{pc_to_PKNCAconc}} and \code{\link{ex_to_PKNCAdose}}, and
#' optionally enriches both with subject-level data from DM and baseline VS.
#'
#' @section Domain enrichment:
#' When \code{dm} and/or \code{vs} are provided, their columns are merged onto
#' both the PC and EX data \emph{before} creating the PKNCAconc and PKNCAdose
#' objects. This means subject-level covariates (e.g. \code{AGE}, \code{SEX},
#' \code{WEIGHT}) are available in the resulting objects for downstream use
#' (e.g. dose normalization by weight via \code{normalize_by_col}).
#'
#' Joins use the intersection of column names between domains as merge keys,
#' following the CDISC convention where \code{STUDYID} and \code{USUBJID} are
#' the standard cross-domain identifiers. The \code{DOMAIN} column is always
#' excluded from join keys.
#'
#' VS data is first reduced to baseline records (one row per subject) via
#' \code{\link{vs_to_baseline}} before joining.
#'
#' @param pc A data.frame containing the PC (Pharmacokinetic Concentrations)
#'   SDTM domain
#' @param ex A data.frame containing the EX (Exposure) SDTM domain
#' @param dm An optional data.frame containing the DM (Demographics) SDTM
#'   domain. If provided, subject-level columns are merged onto PC and EX.
#' @param vs An optional data.frame containing the VS (Vital Signs) SDTM
#'   domain. If provided, baseline vital signs are pivoted to wide format and
#'   merged onto PC and EX.
#' @param pc_args A named list of additional arguments passed to
#'   \code{\link{pc_to_PKNCAconc}} (e.g. custom column name mappings).
#' @param ex_args A named list of additional arguments passed to
#'   \code{\link{ex_to_PKNCAdose}} (e.g. custom column name mappings).
#' @param intervals Optional interval specification passed to
#'   \code{\link{PKNCAdata}}. If \code{NULL} (default), intervals are
#'   automatically derived from the dosing data.
#' @param units Optional unit table passed to \code{\link{PKNCAdata}}.
#' @param options A list of PKNCA options passed to \code{\link{PKNCAdata}}.
#' @return A \code{PKNCAdata} object
#' @importFrom dplyr left_join
#' @export
sdtm_to_PKNCAdata <- function(
  pc,
  ex,
  dm = NULL,
  vs = NULL,
  pc_args = list(),
  ex_args = list(),
  intervals = NULL,
  units = NULL,
  options = list()
) {
  checkmate::assert_data_frame(pc, min.rows = 1)
  checkmate::assert_data_frame(ex, min.rows = 1)

  # --- Prepare subject-level data for enrichment ---
  # Pivot VS to baseline wide format (one row per subject)
  vs_wide <- NULL
  if (!is.null(vs)) {
    checkmate::assert_data_frame(vs, min.rows = 1)
    vs_wide <- as.data.frame(vs_to_baseline(vs))
  }

  # --- Derive PCRFTDTC if missing from PC ---
  PCRFTDTC_col <- pc_args$PCRFTDTC %||% "PCRFTDTC"
  PCELTM_col <- pc_args$PCELTM %||% "PCELTM"
  if (!PCRFTDTC_col %in% names(pc) && !PCELTM_col %in% names(pc)) {
    pc <- derive_pcrftdtc(pc, ex)
  }

  # --- Enrich PC and EX with subject-level data ---
  pc <- sdtm_join(pc, dm, vs_wide)
  ex <- sdtm_join(ex, dm, vs_wide)

  # --- Build PKNCAconc and PKNCAdose ---
  conc_obj <- do.call(pc_to_PKNCAconc, c(list(pc = pc), pc_args))
  dose_obj <- do.call(ex_to_PKNCAdose, c(list(ex = ex), ex_args))

  # --- Build PKNCAdata ---
  pknca_args <- list(
    data.conc = conc_obj,
    data.dose = dose_obj,
    options = options
  )
  if (!is.null(intervals)) {
    pknca_args$intervals <- intervals
  }
  if (!is.null(units)) {
    pknca_args$units <- units
  }

  do.call(PKNCA::PKNCAdata, pknca_args)
}
