#' Distribution temporelle du nombre de session d'observation
#' 
#' @description
#' \code{fct_distribution_temporelle_sessions} est une fonction utilisée pour visualiser, selon le paramètre défini, la distribution annelle ou mensuelle du nombre de sessions d'observation. Elle génère un diagramme en barres où l’axe des x représente les années ou les mois, et l’axe des y indique le nombre de sessions d’observation.
#' 
#' @param data dataframe ; jeu de données d'un des programmes Vigie-Nature accessible depuis le ftp
#' @param periode chaine de caracteres ; par défaut "\code{annee}". La valeur de ce paramètre peut être soit "\code{annee}", soit "\code{mois}". D'autres entrées donneront une erreur.
#'
#' @return Visualisation
#' 
#' @export
#'
#' @examples
#' readRenviron(".env")
#' Sys.getenv("FTP_USER")
#' source("R/download_from_ftp.R")
#' df <- download_from_ftp("export_qubs_aspifaune.csv")
#' p <- fct_distribution_temporelle_sessions(data = df, periode = "annee")
#' print(p)

fct_distribution_temporelle_sessions <- function(data, periode="annee"){
  # Preparation des parametres
  periode <- tolower(periode) # "annee" ou "mois
  # Preparation des donnees ----
  
  ## Ajout de la colonne annee ----
  data$annee <- strftime(data$session_date, format = "%Y")
  
  ## Ajout de la colonne mois ----
  data$mois <- strftime(data$session_date, format = "%m")
  
  
  # Comptage du nombre de session d'observation par site ----
  
  ## > Par annee ----
  if(periode %in% "annee"){
    
    # Comptage
    nb_session_par_site <- data_session_unique %>%
      group_by(site_id, site_name, annee) %>%  # Regroupe par annee et par site
      summarise(nombre_session = n()) %>%  # Compte le nombre de comptages
      arrange(site_id, site_name, annee)  # Trie les résultats par annee et site
    
    # Creation du graphique pour le comptage par année
    p <- ggplot(nb_session_par_site, aes(x = annee, y = nombre_session)) +
      geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
      labs(title = "Nombre de sessions d'observation par année",
           x = "Année", 
           y = "Nombre de session") +
      theme_minimal() +  # Thème minimal pour le graphique
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels sur l'axe x
    
  }else{ # Fin de la condition periode %in% "annee"
    
    ## > Par mois ----
    if(periode %in% "mois"){
      
      # Comptage
      nb_session_par_site <- data_session_unique %>%
        group_by(site_id, site_name, mois) %>%  # Regroupe par année et par site
        summarise(nombre_session = n()) %>%  # Compte le nombre de comptages
        arrange(site_id, site_name, mois)  # Trie les résultats par annee et site
      
      # Creation du graphique pour le comptage par mois
      p <- ggplot(nb_session_par_site, aes(x = mois, y = nombre_session)) +
        geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
        labs(title = "Nombre de sessions d'observation par mois",
             x = "Mois", 
             y = "Nombre de sessions") +
        theme_minimal() +  # Thème minimal pour le graphique
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels sur l'axe x
      
    }else{ # Fin de la condition periode %in% "mois"
      
      warning("\n\nL'argument 'periode' n'est pas correctement renseigné. Veuillez compléter par 'annee' ou 'mois'\n")
      stop()
    }
    
    return(p)
    
  } # Fin de la condition
  
} # Fin de la fonction 




