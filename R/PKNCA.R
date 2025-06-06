#' Compute noncompartmental pharmacokinetics
#'
#' Compute pharmacokinetic (PK) noncompartmental analysis (NCA)
#' parameters.
#'
#' PKNCA has been cross-validated with both Phoenix WinNonlin(R) and Pumas
#' (click here for the
#' [cross-validation
#' article](http://www.humanpredictions.com/wp-content/uploads/2020/01/ACOP_2019_T102_NCA_performance_evaluation_Yingbo_revised.pdf))
#'
#' A common workflow would load data from a file or database into a
#' data.frame then run the following code.
#' @examples
#' \dontrun{
#' # Load concentration-time data into a data.frame called d.conc
#' # with columns named "conc", "time", and "subject".
#' my.conc <- PKNCAconc(d.conc, conc~time|subject)
#' # Load dose-time data into a data.frame called d.dose
#' # with columns named "dose", "time", and "subject".
#' my.dose <- PKNCAdose(d.dose, dose~time|subject)
#' # Combine the concentration-time and dose-time data into an object
#' # ready for calculations.
#' my.data <- PKNCAdata(my.conc, my.dose)
#' # Perform the calculations
#' my.results <- pk.nca(my.data)
#' # Look at summary results
#' summary(my.results)
#' # Look at a listing of results
#' as.data.frame(my.results)
#' }
#' @docType package
#' @name PKNCA
"_PACKAGE"

# To work with the use of dplyr's pipe within the exclude function
utils::globalVariables(".")

.onLoad <- function(...) {
  if (requireNamespace("units", quietly = TRUE)) {
    # Allow "fraction" to be used as a unit for fraction excreted
    units::install_unit(symbol = "fraction", def = "1")
  }
}
