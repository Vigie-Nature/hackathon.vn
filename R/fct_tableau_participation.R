#' Créer un tableau de participation
#' 
#' @param df_vn a dataframe
#' @param col_date a character/string
#' @param col_user a character/string
#' @param pretty_names a boolean
#'
#' @description
#' Cette fonction permet de generer un tableau de participation
#' avec en option une modification des noms de colonnes en output
#' 
#' @return A dataframe
#'
#' @examples
#' library(dplyr)
#' 
#' lapply(file.path("R", dir("R")), source)
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' df_participation_aspifaune <- fct_tableau_participation(df_vn = aspifaune,
#'                                                         pretty_names = FALSE)

fct_tableau_participation <- function(df_vn,
                                      col_date = "session_date",
                                      col_user = "user_id",
                                      pretty_names = TRUE){
  
  df_participation = df_vn %>%
    # Créer une colonne année
    dplyr::mutate(session_year = strftime(!!sym(col_date), "%Y")) %>%
    # Grouper par année
    dplyr::group_by(session_year) %>%
    # Compter le nombre de participants uniques 
    dplyr::summarise(nparticipants = n_distinct(!!sym(col_user))) %>%
    # Transformer en data.frame
    as.data.frame() 
  
  if (pretty_names) {
    # Modifier les noms des colonnes
    colnames(df_participation) = c("Années", "Nombre de participants")
  }
  
  return(df_participation)
}
