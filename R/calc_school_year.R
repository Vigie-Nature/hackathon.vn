#' Calculer les années scolaires
#'
#' @param observation_date date de l'observation
#'
#' @returns column with school year
#' @export
#'
#' @examples
#' lapply(file.path("R", dir("R")), source)
#' # import file
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' 
#' calc_school_year(aspifaune$session_date)
#' 
calc_school_year <- function (observation_date){
  observation_year <- ifelse(lubridate::month(observation_date) %in% 1:8,
                             lubridate::year(observation_date),
                             lubridate::year(observation_date) + 1)
  observation_year
}