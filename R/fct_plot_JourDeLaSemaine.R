

plot_JourDeLaSemaine <- function(file){
  
  #DataVN=download_from_ftp(nom_fichier=paste0("export_",Observatoire,".csv"))
  DataVN=download_from_ftp(nom_fichier=file)
  
  #extraire le nom de l'observatoire
  Observatoire=gsub("export_","",file)
  Observatoire=gsub(".csv","",Observatoire)
  
  # names(DataVN)
  # DataVN$session_date
  if("session_date" %in% colnames(DataVN)){
    # Extraire le jour de la semaine à partir de session_date
    DataVN <- DataVN %>%
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
        title = paste("Répartition des données par jour de la semaine -",Observatoire),
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
