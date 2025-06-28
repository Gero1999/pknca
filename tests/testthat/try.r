data <- read.csv("../workspace/code/shinyR/aNCA/inst/shiny/data/Dummy_complex_data.csv") %>%
    dplyr::filter(DOSNO %in% 1:2)
dose_df <- data %>%
    dplyr::mutate(TIME_DOSE = round(AFRLT - ARRLT)) %>%
    dplyr::select(USUBJID, DRUG, TIME_DOSE, DOSEA) %>%
    dplyr::filter(!duplicated(paste0(USUBJID, DRUG, TIME_DOSE)))

o_conc <- PKNCAconc(data, AVAL ~ AFRLT | PCSPEC + DRUG + USUBJID / PARAM)
o_dose <- PKNCAdose(dose_df, DOSEA ~ TIME_DOSE | DRUG + USUBJID)
o_data <- PKNCAdata(o_conc, o_dose)
o_nca <- pk.nca(o_data)
get_halflife_points(o_nca)


lz_intervals <- o_nca$data$intervals %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::filter(half.life == TRUE) %>%
    select(any_of(c(group_vars(o_nca), "start", "end", "row_id")))

o_conc <- o_nca$data$conc
time_col <- o_conc$columns$time
inc_hl_col <- o_conc$columns$incude_half.life
exc_hl_col <- o_conc$columns$exclude_half.life

i <- 1
lz_groups_i <- lz_intervals[i, ]
lz_res <- merge(lz_groups_i, o_nca$result) %>%
    pull(PPORRES, PPTESTCD) %>%
    as.list()

lz_points <- merge(lz_groups_i, o_conc) %>%
    # Select the time interval points
    dplyr::filter(start <= !!sym(time_col) & end >= !!sym(time_col)) %>%
    # Consider points excluded and selected if proceeding
    dplyr::mutate(
        hl.used = if (!is.null(inc_hl_col)) {
            .[[inc_hl_col]] == TRUE
        } else {
            NA
        },
        hl.used = if (!is.null(exc_hl_col)) {
            .[[inc_hl_col]] == FALSE
        } else {
            hl.used
        },
    # If present consider the first and last time points for lambda.z
        hl.used = if (!is.null(lz_res[["lambda.z.time.first"]])) {
            ifelse(!!sym(time_col) < lz_res[["lambda.z.time.first"]], FALSE, hl.used)
        } else {
            hl.used
        },
        hl.used = if (!is.null(lz_res[["lambda.z.time.last"]])) {
            ifelse(!!sym(time_col) > lz_res[["lambda.z.time.last"]], FALSE, hl.used)
        } else {
            hl.used
        }
    )



o_nca$data$conc$data <- o_nca$data$conc$data %>%
    dplyr::mutate(ROWID = row_number())
splitdata <- full_join_PKNCAdata(as_PKNCAdata(o_nca), extra_conc_cols = "ROWID")
i <- 1
curr_row <- splitdata[1, ]
curr_row[["intervals"]]


sub_res <- o_nca
sub_res$data$intervals <- lz_intervals
sub_res$result <- lz_res
sub_res$data$conc <- sub_res$data$conc %>%
    dplyr::mutate(ROWID = row_number())
PKNCA::get_halflife_points(sub_res)
PKNCA:::get_halflife_points_single(sub_res$data$conc, sub_res$result, "ROWID")

PKNCA:::get_halflife_points_single(
    conc = o_conc$data %>% mutate(ROWID = row_number()),
    results = lz_res,
    rowid_col = "ROWID"
)
