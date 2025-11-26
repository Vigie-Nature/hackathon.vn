#' Add labels to school year
#'
#' @param x a numeric value representing the school year (2026 = 2025-2026)
#' @param retour add \n to the string if false 2025-2026 if true 2025\n2026)
#'
#' @returns
#' @export
#'
#' @examples
label_school_year <- function (x, retour = FALSE){
  yearMoinsUn <- as.numeric(x) - 1
  if (retour) paste0(yearMoinsUn, "\n", x) else paste(yearMoinsUn, " - ", x)
}
