#' makeParticipationMap
#' 
#' Crée et sauvegarde une carte des participations par site.
#' 
#' Cette fonction affiche les sites de relevé (latitude/longitude) sur la carte de la France. 
#' Les points peuvent être coloriés de façon fixe ou selon le nombre d'années distinctes 
#' où chaque site a été observé (gradient inter-annuel).
#' 
#' @param data A `data.frame` contenant les observations, avec au minimum les colonnes :
#'   - `longitude` : longitude du site
#'   - `latitude` : latitude du site
#'   - `session_date` : date de la session au format `"YYYY-MM-DD"`
#'   - `session_id` : identifiant unique de la session
#' @param path `string` spécifiant le dossier où le fichier doit être sauvegardé
#' @param filename `string` nom du fichier PNG de sortie (défaut `"map_sessions.png"`)
#' @param point_color `string` couleur fixe des points si `color_by_year_count = FALSE` (défaut `"lightblue"`)
#' @param point_size `numeric` taille des points (défaut 2)
#' @param year NULL (toutes les années), un entier (ex: 2021) ou un vecteur représentant 
#'   un intervalle d'années (ex: `2015:2020`) pour filtrer les données
#' @param color_by_year_count `logical` si TRUE, applique un gradient de couleur selon le 
#'   nombre d'années distinctes où le site a été observé (défaut FALSE)
#' 
#' @return L'objet ggplot créé
#' 
#' @examples
#' # Carte classique toutes années
#' makeParticipationMap(data, path = "outputs/")
#' 
#' # Carte pour une seule année
#' makeParticipationMap(data, path = "outputs/", year = 2021)
#' 
#' # Carte pour un intervalle d'années
#' makeParticipationMap(data, path = "outputs/", year = 2018:2021)
#' 
#' # Carte avec gradient selon nombre d'années distinctes
#' makeParticipationMap(data, path = "outputs/", color_by_year_count = TRUE)
#' 
makeParticipationMap <- function(data, path, filename = "map_sessions.png",
                                 point_color = "lightblue", point_size = 2,
                                 year = NULL, color_by_year_count = FALSE){
  
  library(dplyr)
  library(ggplot2)
  
  if(!("longitude" %in% colnames(data) & "latitude" %in% colnames(data))){
    stop("Les colonnes longitude et latitude sont manquantes.")
  }
  
  if(!("session_date" %in% colnames(data))){
    stop("La colonne 'session_date' est manquante dans les données.")
  }
  
  ##############################
  #   DATE PARSING & YEAR      #
  ##############################
  
  data <- data %>%
    mutate(
      session_date = as.Date(session_date),
      year_extracted = as.integer(format(session_date, "%Y"))
    )
  
  ###################
  # YEAR FILTERING  #
  ###################
  
  if(!is.null(year)){
    if(length(year) == 1){
      data <- data %>% filter(year_extracted == year)
    } else if(length(year) >= 2){
      yr_min <- min(year)
      yr_max <- max(year)
      data <- data %>% filter(between(year_extracted, yr_min, yr_max))
    }
  }
  
  if(nrow(data) == 0){
    stop("Aucune donnée disponible pour l'année ou l'intervalle fourni.")
  }
  
  ###################
  # DATA FORMATTING #
  ###################
  
  data <- data %>% filter(!is.na(longitude), !is.na(latitude))
  
  # Unique sessions
  dataSess <- data %>% distinct(session_id, longitude, latitude, year_extracted)
  
  # Gradient optionnel : calcul du nombre d'années distinctes par site
  if(color_by_year_count){
    dataSess <- dataSess %>%
      group_by(longitude, latitude) %>%
      summarise(year_count = n_distinct(year_extracted), .groups = "drop")
  } else {
    dataSess$year_count <- NULL
  }
  
  ############################
  #   FRANCE POLYGON        #
  ############################
  
  dataFrance <- ggplot2::map_data("france")
  
  ################
  # MAP CREATION #
  ################
  
  if(color_by_year_count){
    plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) +
      geom_polygon(aes(group = group), col = "darkgray", fill = "white") +
      geom_point(
        data = dataSess,
        aes(x = longitude, y = latitude, color = year_count),
        size = point_size, alpha = 0.9
      ) +
      scale_color_viridis_c(name = "Nb d'années") +
      theme_void() +
      coord_quickmap(
        xlim = c(min(dataSess$longitude), max(dataSess$longitude)),
        ylim = c(min(dataSess$latitude), max(dataSess$latitude))
      ) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 18))
    
  } else {
    plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) +
      geom_polygon(aes(group = group), col = "darkgray", fill = "white") +
      geom_point(
        data = dataSess,
        aes(x = longitude, y = latitude, color = "Participations"),
        size = point_size, alpha = 0.9
      ) +
      scale_color_manual(name = "Légende", values = c("Participations" = point_color)) +
      theme_void() +
      coord_quickmap(
        xlim = c(min(dataSess$longitude), max(dataSess$longitude)),
        ylim = c(min(dataSess$latitude), max(dataSess$latitude))
      ) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 18))
  }
  
  ###############
  # SAVE PLOT   #
  ###############
  
  ggplot2::ggsave(plot = plot, filename = filename, device = "png",
                  path = path, width = 25, height = 20, units = "cm")
  
  return(plot)
}
