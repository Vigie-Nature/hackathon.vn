#' Title
#'
#' @param dt_vn data frame 
#' 
#' @param axe_x colonne de dt_vn contenant le critère de comparaison des 
#' abondances moyennes : peut être une colonne date au format voulu (annee, mois,
#' saison, etc.), une colonne couverture du sol, une structure, etc.
#' 
#' @param col_tax colonne de dt_vn contenant les taxons au format voulu (brut, 
#' regroupements taxonomiques, selection de quelques taxons, etc.)
#' 
#' @param col_abond_par_session  colonne de dt_vn contenant les abondances des 
#' taxons par session 
#'
#' @returns plot stacke par taxon de l'abondance moyenne par session en fonction 
#' du critere de comparaison voulu
#'
#' @examples
#' 
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' 
#' dt_aspi_modif = aspifaune %>%
#' filter(taxon %in% c("Les Fourmis", "Les Diplopodes", "Les Iules", "Les Cloportes", "Les Chilopodes")) %>%
#' mutate(year = year(session_date))
#' 
#' plot_abondance_moy_par_session_stacked(dt_aspi_modif, axe_x = "year", col_tax = "taxon", col_abond_par_session = "taxon_count")
#' 
#' 
plot_abondance_moy_par_session_stacked = function(dt_vn, axe_x, col_tax, col_abond_par_session) {
  
  ## test s'il y a plus de 30 taxons dans dt_vn : message de warning car affichage du graphe pas adapté
  nb_tax = dt_vn %>%
    dplyr::pull(!!sym(col_tax)) %>%
    unique() %>%
    length()
  
  if (nb_tax > 30) {
    print("Le data frame contient plus de 30 taxons, ce graphe n'est pas adapté. Une solution consiste à faire des regroupements taxonomiques pour en favoriser la lisibilité ou sélectionner seulement certains taxons.")
  }
  ##
  
  
  
  # dt_vn nettoyé des na et sans sessions vides
  dt_clean = dt_vn %>%
    dplyr::rename(critere = axe_x) %>%
    dplyr::mutate(abond_session = as.numeric(!!sym(col_abond_par_session))) %>%
    dplyr::select(critere, session_id, !!sym(col_tax), abond_session) %>%
    stats::na.omit() %>%
    unique()
  
  # # stockage des sessions vides : prises en compte dans les calculs d'abondance moyenne par session
  dt_session_vides = dt_vn %>%
    rename(critere = axe_x) %>%
    filter(is.na(observation_id)) %>%
    select(critere, session_id, !!sym(col_tax), !!sym(col_abond_par_session)) %>%
    unique() %>%
    dplyr::rename(abond_session = col_abond_par_session) %>%
    dplyr::mutate(abond_session = 0)
  
  
  
  # calcul de l'abondance moyenne par session pour chaque taxon et annuellement
  dt_abond_moy = dt_clean %>%
    dplyr::bind_rows(dt_session_vides) %>%
    # calcul abondances moyennes par session et par an des taxons
    dplyr::group_by(critere, !!sym(col_tax)) %>%
    dplyr::reframe(abond_moy_session = mean(abond_session)) %>%
    dplyr::ungroup()
  

  
  #plot final
  p = ggplot2::ggplot(dt_abond_moy, aes(x = critere, y = abond_moy_session, fill = !!sym(col_tax))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    # theme(legend.position = "none") +
    ggplot2::labs(y = "abondance moyenne par session")
  
  return(p)
  
  
}

