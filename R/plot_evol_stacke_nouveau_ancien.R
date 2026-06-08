#' Title
#'
#'
#' @param df_vn dataframe 
#' @param site_ou_participant colonne de df_vn dont on cherche à calculer les évolutions numériques et le turnover
#' -> colonne site ou colonne user_id
#' @param time la colonne temps de df_vn qui nous intéresse pour l'axe x : mois, semaine, années, etc.
#'
#' @returns plot avec le nombre de sites ou user en fonction du temps
#' @examples
#' 
#' 
#' @examples
#' lapply(file.path("R", dir("R")), source)
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' plot_aspifaune <- plot_evol_stacke_nouveau_ancien(df_vn = aspifaune, site_ou_participant = "user_id", time = "session_date")
#' print(plot_aspifaune)
#' 
plot_evol_stacke_nouveau_ancien = function(df_vn, site_ou_participant, time) {
  
  # df calculant la première fois que le site ou que le participant est présent 
  # dans le jeu de données
  dt_first_time = df_vn %>%
    dplyr::group_by(!!sym(site_ou_participant)) %>%
    dplyr::reframe(first_time = min(!!sym(time))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(first_time) %>%
    dplyr::reframe(nouveau = n_distinct(!!sym(site_ou_participant))) %>%
    dplyr::rename(date = first_time) %>%
    dplyr::ungroup()
  
  # calcul nombre nouvelles mentions en anciennes mentions aux différentes dates
  dt_plot_turnover = df_vn %>%
    dplyr::group_by(!!sym(time)) %>%
    dplyr::reframe(nb = n_distinct(!!sym(site_ou_participant))) %>%
    dplyr::ungroup() %>%
    dplyr::rename(date = time) %>%
    dplyr::left_join(dt_first_time) %>%
    dplyr::mutate(nouveau = coalesce(nouveau, 0)) %>% # replace_na
    dplyr::mutate(ancien = nb - nouveau) %>%
    # passage au format long, mention "nouveau" et "ancien"
    tidyr::pivot_longer(cols = c(nouveau, ancien),
                 names_to = "type",
                 values_to = "nombre")
  
  
  plot  = ggplot2::ggplot(dt_plot_turnover, 
                          aes(x = date,  
                              y = nombre, 
                              fill = type)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    # inclinaison labels axe x
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggplot2::labs(x = "date", 
                  y = "nombre")
  
  return(plot)
  
  
}
