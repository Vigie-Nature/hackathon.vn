#' Graphique de distribution de l'abondance totale
#' 
#' @description
#' Cette fonction affiche un barplot présentant le pourcentage de l'abondance totale 
#' pour chacun des N taxons les plus abondants du jeu de données, et retourne
#' 
#' @param df_vn un export de donnees VN
#' 
#' @param n_species le nombre de taxons que l'on veut afficher (valeur par défaut = 5)
#' 
#' @return un graphique
#'
#' @examples
#' # Produire un graphique pour le top 10 des especes
#' plot_top_n_abondance(df_vn, n_species = 10)
#' 
#' 

plot_top_n_abondance <- function(df_vn, n_species = 5) {
  # s'assurer que le champ taxon_count est numeric
  df_vn <- df_vn %>% mutate(taxon_count = as.numeric(taxon_count))
  
  # creer un objet avec le nombre total de sites, de sessions, et la somme de l'abondance
  totaux <- df_vn %>% reframe(nb_sites = n_distinct(site_id),
                              nb_sessions = n_distinct(session_id),
                              abondance = sum(as.numeric(na.omit(taxon_count))))
  
  df_abondance_tot <- df_vn %>%
    #retirer les NAs des champs taxon et taxon_count
    tidyr::drop_na(taxon, taxon_count) %>%
    #grouper par taxon
    dplyr::group_by(taxon) %>%
    dplyr::reframe(abondance = sum(taxon_count),
                   prop_abondance = abondance/totaux$abondance)
  
  plot_top_n_abondance <- ggplot(df_abondance_tot %>%
                                   arrange(desc(prop_abondance)) %>%
                                   top_n(n = n_species) %>%
                                   arrange(prop_abondance) %>% 
                                   # rearranger les niveaux de facteurs pour le plot
                                   mutate(taxon = factor(taxon, levels = taxon)), 
                                 aes(x=taxon, y=prop_abondance)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Pourcentage du total d'individus observés", x = "Taxon") +
    coord_flip() +
    theme_bw()
  
  return(plot_top_n_abondance)
  
}
