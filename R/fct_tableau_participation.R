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
#' lapply(file.path("R", dir("R")), source)
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' df_participation_aspifaune <- fct_tableau_participation(df_vn = aspifaune,
#'                                                         pretty_names = FALSE)

df_vn = aspifaune
count_variable = 'session'
by_year = TRUE
by_month = TRUE
add_week = TRUE
by_day = FALSE
add_school_year = FALSE
group_variable = NULL
col_date = "session_date"
col_count_variable = "user_id"
pretty_names = FALSE

fct_tableau_participation <- function(df_vn,
                                      count_variable = 'session',
                                      by_year = TRUE,
                                      by_month = TRUE,
                                      by_day = TRUE,
                                      add_week = FALSE,
                                      add_school_year = FALSE,
                                      group_variable = NULL,
                                      col_date = "session_date",
                                      col_count_variable = "user_id",
                                      pretty_names = TRUE) {
  
  if (!count_variable %in% c("session", "user", "site")){
    stop("L'argument count_variable doit prendre l'une des valeurs suivantes : 'session', 'user', 'site'")
  }
  
  # switch between 
  col_count_variable <- switch (count_variable,
                                session = "session_id",
                                user = "user_id",
                                site = "site_id"
  )
  
  
  # add time grouping variable according to time resolution
  if(any(by_year, by_month, by_day)){
    
    time_resolution_temp <- c(ifelse(by_year,"%Y",""),
                              ifelse(by_month,"%m",""),
                              ifelse(by_day,"%d",""))
    
    time_resolution = ""
    for ( i in time_resolution_temp){
      if (i != ""){
        if(time_resolution == ""){
          time_resolution = i
        } else {
          time_resolution = paste(time_resolution, i, sep = "-")
        }
      }
    }
    
    df_vn$session_time = strftime(df_vn[[col_date]], time_resolution)
    group_variable = c(group_variable, "session_time")
  }
  
  # add week number to the data
  if(add_week){
    df_vn$week <- strftime(df_vn[[col_date]], "%W")
    group_variable = c(group_variable, "week")
  }
  
  # add school year
  if(add_school_year){
    df_vn$school_year <- calc_school_year(df_vn[[col_date]])
    group_variable = c(group_variable, "week")
  }
  
  # count variable according to grouping preferences
  df_participation = df_vn %>%
    # Grouper par année
    group_by_at(group_variable) %>%
    # Compter le nombre de participants uniques 
    summarise(nb_variable = n_distinct(!!sym(col_count_variable))) %>%
    # Transformer en data.frame
    as.data.frame() 
  
  # rename variable
  colnames(df_participation)[colnames(df_participation) == "nb_variable"] <- paste0("nb_", count_variable)
  
  
  # if (pretty_names) {
  #   # Modifier les noms des colonnes
  #   colnames(df_participation) = c("Années", "Nombre de participants")
  # }
  
  return(df_participation)
}
