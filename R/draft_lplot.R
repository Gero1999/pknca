
  if (!"PPSTRES" %in% names(o_nca$result)) {
    o_nca$result$PPSTRES <- o_nca$result$PPORRES
  }

  groups <- getGroups(o_nca %>% dplyr::filter(PPTESTCD == "lambda.z")) %>%
  unique()

for (i in 1:nrow(groups)) {
  group <- groups[i, ]
  group_vars <- setdiff(names(group), c("start", "end"))
  df <- merge(o_nca$data$conc$data, group[, group_vars, drop = FALSE])

  time_col <- o_nca$data$conc$columns$time
  conc_col <- o_nca$data$conc$columns$conc
  timeu_col <- o_nca$data$conc$columns$timeu
  concu_col <- o_nca$data$conc$columns$concu

  df[[time_col]] <- df[[time_col]] + group$start
  df <- df[order(df[[time_col]]), ]
  df[["IX"]] <- 1:nrow(df)

  o_nca2 <- o_nca
  o_nca2$data$conc$data <- df
  o_nca2$result <- merge(o_nca2$result, group[, group_vars, drop = FALSE])
  is_lz_used <- get_halflife_points(o_nca2)
  df_fit <- df[is_lz_used, ]
  df_fit$log10_conc <- log10(df_fit[[conc_col]])
  fit <- lm(as.formula(paste0("log10(", conc_col, ")", "~", time_col)), data = df_fit)
  tlast <- o_nca2$result$PPORRES[o_nca2$result$PPTESTCD == "tlast"]
  half_life <- o_nca2$result$PPORRES[o_nca2$result$PPTESTCD == "half.life"]
  adj.r.squared <- o_nca2$result$PPORRES[o_nca2$result$PPTESTCD == "adj.r.squared"]
  span.ratio <- o_nca2$result$PPORRES[o_nca2$result$PPTESTCD == "span.ratio"]
  lz_time_first <- o_nca2$result$PPORRES[o_nca2$result$PPTESTCD == "lambda.z.time.first"]
  lz_time_last <- o_nca2$result$PPORRES[o_nca2$result$PPTESTCD == "lambda.z.time.last"]
  time_span <- lz_time_last - lz_time_first
  fit_line_data <- data.frame(
    x = c(0, tlast),
    y = predict(fit, data.frame(Time = c(0, tlast)))
  )

  x_var <- time_col
  y_var <- conc_col
  plot_data <- df
  plot_data$color <- ifelse(is_lz_used, "green", "red")
  title <- paste0(paste0(group_vars, ": "), group[, group_vars, drop = FALSE], collapse = ", ")
  xlab <- if (!is.null(timeu_col)) paste0(time_col, " (", timeu_col, ")") else time_col
  ylab <- if (!is.null(concu_col)) paste0(conc_col, " (", concu_col, ")") else conc_col
  subtitle_text <- paste0(
    "    R<sup>2</sup><sub>adj</sub>: ", adj.r.squared,
    "    HL \u03BB<sub>z</sub> = ", half_life, " ",
    lambda_res$PPSTRESU[lambda_res$PPTESTCD == "half.life"],
    "    (T<sub>", df$IX[which(df[[time_col]] == lz_time_first)], "</sub> - T<sub>",
    df$IX[which(df[[time_col]] == lz_time_last)], "</sub>)/2 = ", time_span / 2, " ",
    lambda_res$PPSTRESU[lambda_res$PPTESTCD == "lambda.z.time.first"]
  )

  # Create plot
  p <- plot_ly(
    data = plot_data,
    x = ~plot_data[[x_var]],
    y = ~plot_data[[y_var]],
    type = "scatter",
    mode = "markers",
    marker = list(color = plot_data$color, size = 10),
    text = ~paste0(
      x_var, ": ", plot_data[[x_var]], "<br>",
      y_var, ": ", signif(plot_data[[y_var]], 3)
    ),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    add_lines(
      data = fit_line_data,
      x = ~x,
      y = ~10**y,
      line = list(color = "green", width = 2),
      name = "Fit",
      inherit = FALSE,
      showlegend = TRUE
    ) %>%
    layout(
      title = title,
      xaxis = list(title = x_var),
      yaxis = list(title = y_var, type = "log"),
      annotations = 
    )
}

group_vars <- dplyr::group_vars(o_conc)
df$groups_to_split <- as.character(interaction(df[, group_vars]))


list_data <- split(df, df$groups_to_split)

lapply(names(list_data), function(ndata) {
  data <- list_data[[ndata]]
  cell_vals <- unique(as.numeric(as.factor(data[[cell_var]])))

  plot_ly(
    data = data,
    x = ~data[[col_var]],
    y = ~data[[row_var]],
    type = "scatter",
    source = "scatter",
    colors = colorRampPalette(c("black", "red3", "green4"))(length(cell_vals)),
    text = ~paste(
      paste0("<br>", row_var, ": ", data[[row_var]]),
      paste0("<br>", col_var, ": ", data[[col_var]]),
      apply(data[hover_var], 1, function(row) {
        paste0("<br>", paste(paste0(names(row), ": ", row), collapse = "<br>"))
      })
    ),
    hoverinfo = "text",
    customdata = unique(data[[group_var]])
  ) %>%
    layout(
      title = paste0(unique(data[[group_var]]), collapse = "\n"),
      xaxis = list(title = col_var),
      yaxis = list(title = row_var)
    )
})


plot_green_red_scatter <- function(
  plot_data,
  x_var,
  y_var,
  logical_col,
  title = "Scatter Plot"
) {
  library(plotly)
  library(dplyr)
  
  # Prepare colors: green for TRUE, red for FALSE
  plot_data <- plot_data %>%
    mutate(
      color = ifelse(.data[[logical_col]], "green", "red")
    )

  # Fit line only for green points
  green_data <- plot_data %>% filter(.data[[logical_col]])
  fit <- lm(green_data[[y_var]] ~ green_data[[x_var]], data = green_data)

  # Extrapolate fit line across the full x range of all data
  x_seq <- seq(min(plot_data[[x_var]], na.rm = TRUE),
               max(plot_data[[x_var]], na.rm = TRUE),
               length.out = 100)
  fit_line <- data.frame(
    x = x_seq,
    y = predict(fit, newdata = data.frame(n = 1:length(x_seq)) %>% mutate(!!x_var := x_seq) )
  )
  names(fit_line) <- c(x_var, y_var)

  # Create plot
  p <- plot_ly(
    data = plot_data,
    x = ~plot_data[[x_var]],
    y = ~plot_data[[y_var]],
    type = "scatter",
    mode = "markers",
    marker = list(color = plot_data$color, size = 10),
    text = ~paste0(
      x_var, ": ", plot_data[[x_var]], "<br>",
      y_var, ": ", signif(plot_data[[y_var]], 3)
    ),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    add_lines(
      data = fit_line,
      x = ~plot_data[[x_var]],
      y = ~plot_data[[y_var]],
      line = list(color = "green", width = 2),
      name = "Fit (green points only)",
      inherit = FALSE,
      showlegend = TRUE
    ) %>%
    layout(
      title = title,
      xaxis = list(title = x_var),
      yaxis = list(title = y_var)
    )

  return(p)
}
plot_green_red_scatter(
  plot_data = mtcars,
  x_var = "mpg",
  y_var = "cyl",
  logical_col = "logic",
  title = "Green/Red Scatter Plot with Fit Line"
)
