
#' Title : plot_freq_obs_taxon
#' Fonction qui renvoit un plot affichant les n taxons les plus frequemments observes 
#' sur les differentes sessions. On considere ici le pourcentage de sessions ou le 
#' taxon a ete observe, pas son abondance i.e. on ne prend pas en compte si le taxon 
#' a ete observe plusieurs fois au cours d'une session.
#'
#'
#' @param dt un data frame contenant une colonne session_id et une colonne taxon
#' @param n un parametre numeric = nombre de taxons que l'on veut afficher sur le graphe
#'
#' @returns un plot qui affiche les taxons en fonction de leur fréquence d'observation
#' @export
#'
#'
plot_freq_obs_taxon = function(dt, n) {
  
  # fréquence d'observation : % de sessions où le taxon est vu
  dt_freq = dt %>%
    select(session_id, taxon) %>%
    filter(!is.na(taxon)) %>%
    unique() %>%
    table() %>%
    as.data.frame() %>%
    group_by(taxon) %>%
    dplyr::reframe(freq_taxon = round(100 * sum(Freq) / dplyr::n_distinct(session_id), 1)) %>%
    ungroup() %>% 
    dplyr::arrange(dplyr::desc(freq_taxon))
  
  
  # si le df est trop long, on récupère les n premières lignes des espèces les plus fréquentes
  if (length(dt_freq$taxon) >= n) {
    
    # on récupère les n lignes d'intérêt
    dt_freq = slice_head(dt_freq, n = n)
    
  }
  
  # graphe de frequence des taxons
  plot = ggplot2::ggplot(dt_freq, aes(x = stats::reorder(taxon, freq_taxon), y = freq_taxon)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Taxon", 
         y = "Fréquence d'observation des taxons (%) \n (Pourcentage de sessions où le taxon est vu)")
  
  
  return(plot)
  
}

