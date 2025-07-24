check_plotly_object <- function(
  p,
  x_title = NULL,
  y_title = NULL,
  n_traces = NULL,
  annotation_present = NULL,
  trace_names = NULL
) {
  # Check x axis title
  if (!is.null(x_title)) {
    expect_equal(p$x$layout$xaxis$title$text, x_title)
  }
  # Check y axis title
  if (!is.null(y_title)) {
    expect_equal(p$x$layout$yaxis$title$text, y_title)
  }
  # Check number of traces
  if (!is.null(n_traces)) {
    expect_equal(length(p$x$data), n_traces)
  }
  # Check annotation presence
  if (!is.null(annotation_present)) {
    if (annotation_present) {
      expect_true(!is.null(p$x$layout$annotations))
    } else {
      expect_true(is.null(p$x$layout$annotations))
    }
  }
  # Check trace names
  if (!is.null(trace_names)) {
    actual_names <- vapply(p$x$data, function(tr) tr$name %||% "", character(1))
    for (nm in trace_names) {
      expect_true(any(grepl(nm, actual_names)),
                  info = paste("Trace name", nm, "not found in plotly object"))
    }
  }
}

# Create a base testing dataset
d_conc <- data.frame(
    conc = c(
        1, 0.75, 0.5, 0.25, 0.125, 0.1, 0.05,
        0.5, 0.35, 0.25, 0.125, 0.0625, 0.03, 0.01,
        2, 1.5, 1, 0.5, 0.25, 0.1, 0.05,
        0.5, 0.35, 0.25, 0.125, 0.0625, 0.03, 0.01
    ),
    time = rep(0:6, 4),
    subject = rep(c(1, 2), each = 14),
    analyte = rep(rep(c("A", "B"), each = 7), 2)
)
d_dose <- data.frame(dose = 1, time = c(0, 0, 3, 3), subject = 1:2)
o_conc <- PKNCAconc(d_conc, conc ~ time | subject / analyte)
o_dose <- PKNCAdose(d_dose, dose ~ time | subject)
o_data <- PKNCAdata(o_conc, o_dose)
o_nca <- pk.nca(o_data)


test_that("get_halflife_plot creates as many plots as groups", {

  o_nca <- pk.nca(o_data)
  plots <- get_halflife_plot(o_nca)
  expect_type(plots, "list")
  expect_true(length(plots) >= 1)
  expect_true(all(sapply(plots, inherits, what = "plotly")))
})

test_that("get_halflife_plot works with exclusions", {
  d_conc <- data.frame(
    conc = c(1, 0.5, 0.25, 0.125, 0.05, 0),
    time = c(0, 1, 2, 3, 4, 5),
    exclude_hl = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
    subject = 1
  )
  d_dose <- data.frame(dose = 1, time = 0, subject = 1)
  o_conc <- PKNCAconc(d_conc, conc ~ time | subject, exclude_half.life = "exclude_hl")
  o_dose <- PKNCAdose(d_dose, dose ~ time | subject)
  o_data <- PKNCAdata(o_conc, o_dose)
  o_nca <- pk.nca(o_data)
  plots <- get_halflife_plot(o_nca)
  expect_type(plots, "list")
  expect_true(length(plots) >= 1)
  expect_true(all(sapply(plots, inherits, what = "plotly")))
})

test_that("get_halflife_plot works with multiple groups", {
  d_conc <- data.frame(
    conc = c(1, 0.5, 0.25, 0.125, 2, 1, 0.5, 0.25),
    time = rep(0:3, 2),
    subject = rep(1:2, each = 4)
  )
  d_dose <- data.frame(dose = 1, time = 0, subject = 1:2)
  o_conc <- PKNCAconc(d_conc, conc ~ time | subject)
  o_dose <- PKNCAdose(d_dose, dose ~ time | subject)
  o_data <- PKNCAdata(o_conc, o_dose)
  o_nca <- pk.nca(o_data)
  plots <- get_halflife_plot(o_nca)
  expect_type(plots, "list")
  expect_equal(length(plots), 2)
  expect_true(all(sapply(plots, inherits, what = "plotly")))
})

test_that("get_halflife_plot handles missing units and annotation toggle", {
  d_conc <- data.frame(
    conc = c(1, 0.5, 0.25, 0.125),
    time = c(0, 1, 2, 3),
    subject = 1
  )
  d_dose <- data.frame(dose = 1, time = 0, subject = 1)
  o_conc <- PKNCAconc(d_conc, conc ~ time | subject)
  o_dose <- PKNCAdose(d_dose, dose ~ time | subject)
  o_data <- PKNCAdata(o_conc, o_dose)
  o_nca <- pk.nca(o_data)
  plots <- get_halflife_plot(o_nca, add_annotations = FALSE)
  expect_type(plots, "list")
  expect_true(length(plots) >= 1)
  expect_true(all(sapply(plots, inherits, what = "plotly")))
})
