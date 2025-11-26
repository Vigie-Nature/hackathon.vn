#' Histogramme de la distribution des données selon le jour de la semaine
#'
#' @param df_vn un dataframe avec les donnees d'un export standard VN
#' 
#' @param nom_observatoire character string avec le nom de l'observatoire tel qu'il doit s'afficher dans la figure 
#' 
#' @description visualise la distribution des données selon le jour de la semaine, permet notamment d'en savoir plus sur la sociologie de la participation
#'
#' @return a ggplot2 histogram (or just a warning if session_date column is missing)
#' 
#'
#' @examples
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' plot_JourDeLaSemaine(df_vn = aspifaune)
#' 
#' # Utiliser l'argument nom_observatoire pour l'ajouter dans le titre du graphique
#' plot_JourDeLaSemaine(df_vn = aspifaune, nom_observatoire = "(Qubs - Aspifaune)")
#' 

plot_JourDeLaSemaine <- function(df_vn, nom_observatoire= ""){
  
  # names(DataVN)
  # DataVN$session_date
  if("session_date" %in% colnames(df_vn)){
    # Extraire le jour de la semaine à partir de session_date
    DataVN <- df_vn %>%
      mutate(
        jour_semaine = weekdays(as.Date(session_date)),
        # Créer un facteur ordonné pour avoir l'ordre lundi-dimanche
        jour_semaine_ordre = factor(jour_semaine, 
                                    levels = c("lundi", "mardi", "mercredi", 
                                               "jeudi", "vendredi", "samedi", "dimanche"))
      )
    
    # Compter le nombre d'observations par jour
    repartition <- DataVN %>%
      count(jour_semaine_ordre) %>%
      na.omit()
    
    
    
    # Créer le graphique
    Graph=ggplot(repartition, aes(x = jour_semaine_ordre, y = n)) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Répartition des données par jour de la semaine", nom_observatoire),
        x = "Jour de la semaine",
        y = "Nombre d'observations"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(Graph)
  }else{
    warning("La colonne 'session_date' est absente. Impossible de créer le graphique.")
    return(NULL)

  }

}
