#' Distribution temporelle (annuelle ou mensuelle) du nombre de sessions d'observation
#' 
#' @description
#' \code{plot_distrib_temporelle_sessions} est une fonction utilisée pour visualiser, selon le paramètre défini, la distribution annelle ou mensuelle du nombre de sessions d'observation. Elle génère un diagramme en barres où l’axe des x représente les années ou les mois, et l’axe des y indique le nombre de sessions d’observation.
#' 
#' @param df_vn dataframe ; jeu de données d'un des programmes Vigie-Nature accessible depuis le ftp
#' @param unite_de_temps chaine de caracteres ; par défaut "\code{annee}". La valeur de ce paramètre peut être soit "\code{annee}", soit "\code{mois}". D'autres entrées donneront une erreur.
#' @param periode nombre ; par défaut NULL. Année ou intervalle d'années sur lesquelles est généré le graphique
#' @param distrib_mensuelle_par_annee booleen ; Si TRUE et unite_de_temps = "moi", génère un graphique distinct par année de la distribution mensuelle des sessions d'observation.
#'
#' @return Visualisation
#' 
#' @export
#'
#' @examples
#' readRenviron(".env")
#' Sys.getenv("FTP_USER")
#' source("R/download_from_ftp.R")
#' aspifaune <- download_from_ftp("export_qubs_aspifaune.csv")
#' plot_distrib_temporelle_sessions(df_vn = aspifaune, unite_de_temps = "annee")

plot_distrib_temporelle_sessions <- function(df_vn, unite_de_temps = "annee", periode = NULL, distrib_mensuelle_par_annee = T){
  # 1. Preparation des parametres ----
  unite_de_temps <- tolower(unite_de_temps) # "annee" ou "mois
  
  # 2. Preparation des donnees ----
  
  ## Ajout de la colonne annee ----
  df_vn$annee <- strftime(df_vn$session_date, format = "%Y")
  
  ## Ajout de la colonne mois ----
  df_vn$mois <- strftime(df_vn$session_date, format = "%m")
  
  ## Extraction du jeu de donnees ----
  df_vn_session_unique <- unique(df_vn[, c("session_id", "annee", "mois")])
  
  
  # 3. Comptage du nombre de session d'observation par site ----
  ## Si periode non definie (periode = NULL) ----
  if(is.null(periode)){
    
    ## > Par annee ----
    if(unite_de_temps == "annee"){
      
      # Comptage
      df_nb_sessions <- df_vn_session_unique %>%
        group_by(annee) %>%  # Regroupe par annee
        summarise(nb_sessions = n()) %>%  # Compte le nombre de comptages
        arrange(annee)  # Trie les résultats par annee
      
      # Creation du graphique pour le comptage par année
      p <- ggplot(df_nb_sessions, aes(x = annee, y = nb_sessions)) +
        geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
        labs(title = "Nombre de sessions d'observation par année",
             x = "Année", 
             y = "Nombre de session") +
        theme_minimal() +  # Thème minimal pour le graphique
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels sur l'axe x
      
    }else{ # Fin de la condition UNITE_DE_TEMPS = "annee"
      
      ## > Par mois ----
      if(unite_de_temps == "mois"){
        
        # Si on souhaite avoir un graphique par annee
        if(distrib_mensuelle_par_annee == T ){ 
          
          # Comptage
          df_nb_sessions <- df_vn_session_unique %>%
            group_by(mois) %>%  # Regroupe par annee
            summarise(nb_sessions = n()) %>%  # Compte le nombre de comptages
            arrange(mois)  # Trie les résultats par annee
          
          # Creation du graphique pour le comptage par mois
          p <- ggplot(df_nb_sessions, aes(x = mois, y = nb_sessions)) +
            geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
            labs(title = "Nombre de sessions d'observation par mois",
                 x = "Mois", 
                 y = "Nombre de sessions") +
            theme_minimal() +  # Thème minimal pour le graphique
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotation des labels sur l'axe x
            facet_wrap(~annee)
          
        }else{ # si on ne souhaite pas avoir un graphique par annee
          
          # Comptage
          df_nb_sessions <- df_vn_session_unique %>%
            group_by(mois) %>%  # Regroupe par annee
            summarise(nb_sessions = n()) %>%  # Compte le nombre de comptages
            arrange(mois)  # Trie les résultats par annee
          
          # Creation du graphique pour le comptage par mois
          p <- ggplot(df_nb_sessions, aes(x = mois, y = nb_sessions)) +
            geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
            labs(title = "Nombre de sessions d'observation par mois",
                 x = "Mois", 
                 y = "Nombre de sessions") +
            theme_minimal() +  # Thème minimal pour le graphique
            theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels sur l'axe x
          
        } # Fin de la condition distrib_mensuelle_par_annee = T
        
      }else{ # Fin if de la condition UNITE_DE_TEMPS = "mois"
        
        warning("\n\nL'argument 'unite_de_temps' n'est pas correctement renseigné. Veuillez compléter par 'annee' ou 'mois'\n")
        stop()
        
      } # Fin else de la condition UNITE_DE_TEMPS = "mois"
      
    } # Fin else de la condition UNITE_DE_TEMPS = "annee"
    
  }else{ # Fin if de la condition PERIODE = NULL
    
    # Si periode definie
    if(!is.null(periode)){
      
      ## > Par annee ----
      if(unite_de_temps == "annee"){
        
        # Filtre annee
        df_vn_session_unique_filtre <- df_vn_session_unique %>%
          filter(annee %in% periode)
        
        # Comptage
        df_nb_sessions <- df_vn_session_unique_filtre %>%
          group_by(annee) %>%  # Regroupe par annee
          summarise(nb_sessions = n()) %>%  # Compte le nombre de comptages
          arrange(annee)  # Trie les résultats par annee
        
        # Creation du graphique pour le comptage par année
        p <- ggplot(df_nb_sessions, aes(x = annee, y = nb_sessions)) +
          geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
          labs(title = "Nombre de sessions d'observation par année",
               x = "Année", 
               y = "Nombre de session") +
          theme_minimal() +  # Thème minimal pour le graphique
          theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels sur l'axe x
        
      }else{ # Fin if de la condition UNITE_DE_TEMPS = "annee"
        
        ## > Par mois ----
        if(unite_de_temps == "mois"){
          
          # Filtre annee
          df_vn_session_unique_filtre <- df_vn_session_unique %>%
            filter(annee %in% periode)
          
          # Si on souhaite avoir un graphique par annee
          if(distrib_mensuelle_par_annee == T){
            # Comptage
            df_nb_sessions <- df_vn_session_unique_filtre %>%
              group_by(mois, annee) %>%  # Regroupe par annee
              summarise(nb_sessions = n()) %>%  # Compte le nombre de comptages
              arrange(mois)  # Trie les résultats par annee
            
            # Creation du graphique pour le comptage par mois
            p <- ggplot(df_nb_sessions, aes(x = mois, y = nb_sessions)) +
              geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
              labs(title = "Nombre de sessions d'observation par mois",
                   x = "Mois", 
                   y = "Nombre de sessions") +
              theme_minimal() +  # Thème minimal pour le graphique
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotation des labels sur l'axe x
              facet_wrap(~ annee)
            
          }else{ # si on ne souhaite pas avoir un graphique par annee
            
            # Comptage
            df_nb_sessions <- df_vn_session_unique_filtre %>%
              group_by(mois, annee) %>%  # Regroupe par annee
              summarise(nb_sessions = n()) %>%  # Compte le nombre de comptages
              arrange(mois)  # Trie les résultats par annee
            
            # Creation du graphique pour le comptage par mois
            p <- ggplot(df_nb_sessions, aes(x = mois, y = nb_sessions)) +
              geom_bar(stat = "identity") +  # Barres pour le nombre de comptages
              labs(title = "Nombre de sessions d'observation par mois",
                   x = "Mois", 
                   y = "Nombre de sessions") +
              theme_minimal() +  # Thème minimal pour le graphique
              theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels sur l'axe x
            
          } # Fin de la condition distrib_mensuelle_par_annee = T
          
        } else{ # Fin if de la condition UNITE_DE_TEMPS = "mois"
          
          warning("\n\nL'argument 'unite_de_temps' n'est pas correctement renseigné. Veuillez compléter par 'annee' ou 'mois'\n")
          stop()
          
        } # Fin else de la condition UNITE_DE_TEMPS = "mois"
        
      } # Fin else de la condition UNITE_DE_TEMPS = "annee"
      
    } # Fin if de la condition PERIODE = NON NULL
    
  } # Fin else dela condition PERIODE = NULL
  
  return(p)
  
} # Fin de la fonction 




