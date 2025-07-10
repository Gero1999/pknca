list_data <- df %>%
  split(as.character(.[[group_var]]))

lapply(names(list_data), function(ndata) {
  data <- list_data[[ndata]]
  cell_vals <- unique(as.numeric(as.factor(data[[cell_var]])))
  
  plot_ly(
    data = data,
    x = ~data[[col_var]],
    y = ~data[[row_var]],
    z = ~as.numeric(as.factor(data[[cell_var]])),
    type = "heatmap",
    source = "myheatmap",
    xgap = 0.1,
    ygap = 0.1,
    colors = colorRampPalette(c("black", "red3", "green4"))(length(cell_vals)),
    colorbar = list(
      tickmode = 'array',
      title = cell_var,
      tickvals = cell_vals,
      ticktext = levels(
        factor(x = data[[cell_var]],
               levels = unique(data[[cell_var]]),
               ordered = TRUE)
      ),
      len = 0.5
    ),
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