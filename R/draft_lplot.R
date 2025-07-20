
get_lambda_z_plot <- function(o_nca, add_annotations = TRUE) {
  # Ensure result columns are present
  if (!"PPSTRES" %in% names(o_nca$result)) {
    o_nca$result$PPSTRES <- o_nca$result$PPORRES
    if ("PPORRESU" %in% names(o_nca$result)) {
      o_nca$result$PPSTRESU <- o_nca$result$PPORRESU
    }
  }

  # Get grouping structure for lambda.z
  groups <- getGroups(o_nca %>% dplyr::filter(PPTESTCD == "lambda.z")) %>% unique()
  plots <- vector("list", nrow(groups))

  for (i in seq_len(nrow(groups))) {
    group <- groups[i, ]
    group_vars <- setdiff(names(group), c("start", "end"))
    # Subset data for this group
    df <- merge(o_nca$data$conc$data, group[, group_vars, drop = FALSE])

    # Column names
    time_col <- o_nca$data$conc$columns$time
    conc_col <- o_nca$data$conc$columns$conc
    timeu_col <- o_nca$data$conc$columns$timeu
    concu_col <- o_nca$data$conc$columns$concu
    exclude_hl_col <- o_nca$data$conc$columns$exclude_half.life
    if (is.null(exclude_hl_col)) {
      o_nca$data$conc$data[["exclude_half.life"]] <- FALSE
      exclude_hl_col <- "exclude_half.life"
    }

    # Filter and order by time
    df <- df[df[[time_col]] >= group$start & df[[time_col]] <= group$end, ]
    df[["ROWID"]] <- seq_len(nrow(df))
    df <- df[order(df[[time_col]]), ]
    df$IX <- seq_len(nrow(df))

    # Prepare NCA object for this group
    group_nca <- o_nca
    group_nca$data$conc$data <- df
    group_nca$result <- merge(group_nca$result, group[, group_vars, drop = FALSE])
    is_lz_used <- get_halflife_points(group_nca)
    df_fit <- df[is_lz_used, ]

    # Fit log-linear model to green points
    fit <- lm(as.formula(paste0("log10(", conc_col,") ~ ", time_col)), df_fit)

    # Extract NCA results for annotation
    get_res <- function(testcd) group_nca$result$PPORRES[group_nca$result$PPTESTCD == testcd]
    get_unit <- function(testcd) group_nca$result$PPSTRESU[group_nca$result$PPTESTCD == testcd]
    tlast <- get_res("tlast")
    half_life <- get_res("half.life")
    adj.r.squared <- get_res("adj.r.squared")
    lz_time_first <- get_res("lambda.z.time.first")
    lz_time_last <- get_res("lambda.z.time.last")
    time_span <- lz_time_last - lz_time_first

    # Prepare fit line (on log scale, then back-transform)
    fit_line_data <- data.frame(x = c(0, tlast))
    colnames(fit_line_data) <- time_col
    fit_line_data$y <- predict(fit, fit_line_data)

    # Plot data
    plot_data <- df
    plot_data$color <- ifelse(is_lz_used, "green", "red")
    title <- paste0(paste0(group_vars, ": "), group[, group_vars, drop = FALSE], collapse = ", ")
    xlab <- if (!is.null(timeu_col)) paste0(time_col, " (", timeu_col, ")") else time_col
    ylab <- if (!is.null(concu_col)) paste0(conc_col, " (", concu_col, ")") else conc_col
    subtitle_text <- paste0(
      "R<sup>2</sup><sub>adj</sub> = ", signif(adj.r.squared, 2),
      "&#9;&#9;",
      "ln(2)/ \u03BB<sub>z</sub> = ", signif(half_life, 2), " ", get_unit("half.life"),
      "&#9;&#9;",
      "(T<sub>", df$IX[which(df[[time_col]] == lz_time_first)],
      "</sub> - T<sub>", df$IX[which(df[[time_col]] == lz_time_last)], "</sub>)/2 = ",
      "&#9;&#9;",
      signif(time_span / 2, 2), " ", get_unit("lambda.z.time.first")
    )

    # Build plotly object
    p <- plot_ly() %>%
      add_lines(
        data = fit_line_data,
        x = ~get(time_col),
        y = ~10^y,
        line = list(color = "green", width = 2),
        name = "Fit",
        inherit = FALSE,
        showlegend = TRUE
      ) %>%
      layout(
        title = title,
        xaxis = list(
          title = xlab,
          linecolor = "black",
          gridcolor = "white",
          zeroline = FALSE
        ),
        yaxis = list(
          title = ylab,
          type = "log",
          tickformat = "f",
          linecolor = "black",
          gridcolor = "white",
          zeroline = FALSE
        ),
        annotations = if (add_annotations) list(
          text = subtitle_text,
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          y = max(plot_data[[conc_col]], na.rm = TRUE)
        ) else NULL
      ) %>%
      add_trace(
        data = plot_data,
        x = ~plot_data[[time_col]],
        y = ~plot_data[[conc_col]],
        text = ~paste0(
          "(",
          get(time_col),
          ", ",
          signif(get(conc_col), 3),
          ")"
        ),
        hoverinfo = "text",
        showlegend = FALSE,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = plot_data$color,
          size = 15,
          symbol = ifelse(plot_data[[exclude_hl_col]], "x", "circle"),
          size = 20
        ),
        customdata = ~plot_data[["ROWID"]] # Returns the row number in the object
      )
    plots[[i]] <- p
    names(plots)[i] <- title
  }
  return(plots)
}
