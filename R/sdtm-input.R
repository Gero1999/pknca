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

# --- Derive FANLDTM from PKNCAdose -------------------------------------------

#' Derive FANLDTM (first analyte dose datetime) for PC data from a PKNCAdose
#' object
#'
#' Links each PC record to its most recent dose by matching sample collection
#' times to dose start times, then computes the first dose datetime per
#' treatment group. The result is a continuous time reference for NCA analysis.
#'
#' @section Algorithm:
#' \enumerate{
#'   \item The PKNCAdose grouping variables (from the dose formula, e.g.
#'     \code{EXTRT + USUBJID}) and dose datetimes are extracted.
#'   \item PC and dose data are linked by the grouping variables that exist
#'     in both (typically \code{USUBJID}).
#'   \item For each PC record, the most recent dose at or before the sample
#'     collection time (\code{PCDTC}) is identified. This assigns each
#'     sample to its corresponding dosing period and treatment.
#'   \item Pre-dose samples collected before any dose are assigned to the
#'     earliest dose for that subject.
#'   \item \code{FANLDTM} is set to the minimum dose datetime for the
#'     full grouping (e.g. per \code{EXTRT + USUBJID}).
#' }
#'
#' @param pc A data.frame containing the PC (Pharmacokinetic Concentrations)
#'   SDTM domain. Must contain a \code{PCDTC} column (or the name specified
#'   via the parameter) and at least one column matching the PKNCAdose
#'   grouping variables.
#' @param dose_obj A \code{PKNCAdose} object (e.g. from
#'   \code{\link{ex_to_PKNCAdose}}).
#' @param PCDTC Column name for the PC collection date/time (ISO 8601) in
#'   \code{pc}.
#' @param FANLDTM Column name to assign for the derived first dose datetime.
#'   If this column already exists in \code{pc}, it is overwritten with a
#'   warning.
#' @return The \code{pc} data.frame with an added (or replaced) \code{FANLDTM}
#'   column (POSIXct in UTC).
#' @export
derive_fanldtm <- function(
  pc,
  dose_obj,
  PCDTC = "PCDTC",
  FANLDTM = "FANLDTM"
) {
  checkmate::assert_data_frame(pc, min.rows = 1)
  if (!inherits(dose_obj, "PKNCAdose")) {
    stop("dose_obj must be a PKNCAdose object")
  }
  checkmate::assert_string(PCDTC)
  checkmate::assert_string(FANLDTM)

  if (!PCDTC %in% names(pc)) {
    stop("Column '", PCDTC, "' not found in pc data")
  }

  if (FANLDTM %in% names(pc)) {
    rlang::warn(
      paste0("Column '", FANLDTM, "' already exists in pc and will be overwritten"),
      class = "pknca_fanldtm_overwrite"
    )
    pc[[FANLDTM]] <- NULL
  }

  # --- Extract dose schedule from PKNCAdose ---
  dose_data <- dose_obj$data
  group_vars <- dose_obj$columns$groups$group_vars

  # Determine which group_vars exist in PC (typically USUBJID) for the
  # subject-level match. The remaining group_vars (e.g. EXTRT) will be
  # assigned to each PC record based on dose-time proximity.
  shared_keys <- intersect(names(pc), group_vars)
  if (length(shared_keys) == 0) {
    stop(
      "No shared columns between pc and PKNCAdose grouping variables (",
      paste(group_vars, collapse = ", "), "). ",
      "Ensure both use the same subject identifier."
    )
  }
  assigned_vars <- setdiff(group_vars, shared_keys)

  # Find the POSIXct datetime column in dose data (EXSTDTC parsed by
  # ex_to_PKNCAdose)
  posix_cols <- names(dose_data)[vapply(dose_data, inherits, logical(1), "POSIXct")]
  posix_cols <- setdiff(posix_cols, "EX_reference")
  if (length(posix_cols) == 0) {
    stop(
      "No POSIXct datetime column found in PKNCAdose data. ",
      "Ensure ex_to_PKNCAdose was used to create the dose object."
    )
  }
  dose_dtc_col <- posix_cols[1]

  # Build minimal dose lookup: group_vars + dose datetime
  dose_cols <- unique(c(group_vars, dose_dtc_col))
  doses <- unique(dose_data[, dose_cols, drop = FALSE])

  # --- Parse PC collection times ---
  pc_dt <- std_dtc_to_rdate(pc[[PCDTC]])

  # --- For each PC record, find the most recent dose at or before PCDTC ---
  # Match on shared_keys (e.g. USUBJID), then pick by time proximity
  assigned_rows <- vapply(seq_len(nrow(pc)), function(i) {
    sample_time <- pc_dt[i]
    if (is.na(sample_time)) return(NA_integer_)

    # Filter doses to those matching on shared keys
    mask <- rep(TRUE, nrow(doses))
    for (key in shared_keys) {
      mask <- mask & (doses[[key]] == pc[[key]][i])
    }
    if (!any(mask)) return(NA_integer_)

    dose_times <- doses[[dose_dtc_col]][mask]
    mask_idx <- which(mask)

    # Most recent dose at or before sample time
    eligible <- mask_idx[dose_times <= sample_time]
    if (length(eligible) == 0) {
      # Pre-dose: assign to the earliest dose for this subject
      mask_idx[which.min(dose_times)]
    } else {
      eligible[which.max(doses[[dose_dtc_col]][eligible])]
    }
  }, integer(1), USE.NAMES = FALSE)

  # --- Compute FANLDTM = min(dose datetime) per full group_vars ---
  fanldtm_lookup <- do.call(
    rbind,
    lapply(
      split(seq_len(nrow(doses)), doses[, group_vars, drop = FALSE]),
      function(idx) {
        result <- doses[idx[1], group_vars, drop = FALSE]
        result[[FANLDTM]] <- min(doses[[dose_dtc_col]][idx], na.rm = TRUE)
        result
      }
    )
  )
  rownames(fanldtm_lookup) <- NULL

  # --- Assign group_vars from matched dose rows to PC, then join FANLDTM ---
  # Temporarily add the dose-assigned group vars (e.g. EXTRT) to PC
  for (av in assigned_vars) {
    pc[[av]] <- doses[[av]][assigned_rows]
  }

  pc <- dplyr::left_join(pc, fanldtm_lookup, by = group_vars)

  # Remove the temporary columns (they came from EX, not PC)
  for (av in assigned_vars) {
    pc[[av]] <- NULL
  }

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
#' The function derives two time variables:
#' \describe{
#'   \item{\code{AFRLT} (actual time from reference)}{Derived from
#'     \code{FANLDTM} as \code{PCDTC - FANLDTM} in hours. This gives a
#'     continuous time axis from the first dose. \code{FANLDTM} is required;
#'     use \code{\link{derive_fanldtm}} to populate it from a PKNCAdose
#'     object before calling this function.}
#'   \item{\code{NFRLT} (nominal time from reference)}{Optionally derived
#'     from \code{PCELTM} if present. ISO 8601 duration values (e.g.
#'     \code{"PT2H"}) are parsed to numeric hours and used as
#'     \code{time.nominal} in the PKNCAconc object.}
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
#' @param FANLDTM Column name for the first analyte dose datetime (POSIXct).
#'   Used to derive elapsed time when \code{PCELTM} is absent, giving a
#'   continuous time axis from the first dose. See
#'   \code{\link{derive_fanldtm}} to populate this from a PKNCAdose object.
#' @return A \code{PKNCAconc} object with:
#' \itemize{
#'   \item Concentration from \code{PCSTRESN} (BLQ set to 0)
#'   \item Time as \code{AFRLT} (hours from first dose, derived from
#'     \code{FANLDTM})
#'   \item Nominal time as \code{NFRLT} (hours, parsed from \code{PCELTM}
#'     if available)
#'   \item Groups: \code{PCSPEC + USUBJID / PCTEST}
#'   \item Units from \code{PCSTRESU} (if available)
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
  FANLDTM = "FANLDTM"
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
  has_fanldtm <- FANLDTM %in% names(pc)

  if (!PCDTC %in% names(pc)) {
    stop("Column '", PCDTC, "' is required in pc data")
  }

  # AFRLT (actual time from reference): always derived from FANLDTM
  if (!has_fanldtm) {
    stop(
      "Column '", FANLDTM, "' not found in pc data. ",
      "Use derive_fanldtm() to add the first dose datetime from a PKNCAdose object."
    )
  }
  pc_dt <- std_dtc_to_rdate(pc[[PCDTC]])
  # FANLDTM may already be POSIXct (from derive_fanldtm) or ISO 8601 string
  if (inherits(pc[[FANLDTM]], "POSIXct")) {
    ref_dt <- pc[[FANLDTM]]
  } else {
    ref_dt <- std_dtc_to_rdate(pc[[FANLDTM]])
  }
  pc[["AFRLT"]] <- as.numeric(difftime(pc_dt, ref_dt, units = "hours"))

  # NFRLT (nominal time from reference): optionally derived from PCELTM
  if (has_pceltm) {
    pceltm_hours <- parse_iso8601_duration(pc[[PCELTM]])
    pc[["NFRLT"]] <- pceltm_hours
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

  # Nominal time from PCELTM (parsed to hours as NFRLT)
  if (has_pceltm) {
    PKNCAconc_args$time.nominal <- "NFRLT"
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

  # --- Enrich EX and PC with subject-level data ---
  ex <- sdtm_join(ex, dm, vs_wide)
  pc <- sdtm_join(pc, dm, vs_wide)

  # --- Build PKNCAdose first (needed for FANLDTM derivation) ---
  dose_obj <- do.call(ex_to_PKNCAdose, c(list(ex = ex), ex_args))

  # --- Derive FANLDTM on PC if no time variable is available ---
  FANLDTM_col <- pc_args$FANLDTM %||% "FANLDTM"
  PCELTM_col <- pc_args$PCELTM %||% "PCELTM"
  if (!FANLDTM_col %in% names(pc) && !PCELTM_col %in% names(pc)) {
    pc <- derive_fanldtm(pc, dose_obj)
  }

  # --- Build PKNCAconc ---
  conc_obj <- do.call(pc_to_PKNCAconc, c(list(pc = pc), pc_args))

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
