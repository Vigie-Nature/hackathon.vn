#' makeParticipationMap
#' 
#' A function that returns a map of the participations 
#' 
#' @param data : a `data.frame` containing all species observations
#' @param path : a `string` specifying the folder where the file should be saved
#' @param filename : a `string` specifying the name of the file to be saved
#' @param point_color : a `string` specifying the color to use for the plot
#' @param point_size : a `int` specifying the size of the points
#' @param year : NULL, a single year (int), or an interval of years (vector)
#' 

makeParticipationMap <- function(data, path, filename = "map_sessions.png",
                                 point_color = "lightblue", point_size = 2,
                                 year = NULL){


  
  ###################
  # YEAR FILTERING  #
  ###################
  
  if(!is.null(year)){
    
    # Extraire l'année
    data <- data %>%
      mutate(
        session_date = as.Date(session_date),
        year_extracted = as.integer(format(session_date, "%Y"))
      )
    
    # année simple : year = 2020
    if(length(year) == 1){
      data <- data %>% filter(year_extracted == year)
      
      # intervalle : 2015:2020 ou c(2015, 2020)
    } else if(length(year) >= 2){
      yr_min <- min(year)
      yr_max <- max(year)
      data <- data %>% filter(between(year_extracted, yr_min, yr_max))
    }
  }
  
  # Vérification après filtrage
  if(nrow(data) == 0){
    stop("Aucune donnée disponible pour l'année ou l'intervalle fourni.")
  }
  
  ###################
  # DATA FORMATTING #
  ###################
  
  data <- data[!is.na(data$longitude) & !is.na(data$latitude), ]
  
  # Unique sessions only
  dataSess <- unique(data[, c("session_id", "longitude", "latitude")])
  
  ############################
  #   FRANCE POLYGON        #
  ############################
  
  dataFrance <- ggplot2::map_data("france")
  
  ################
  # MAP CREATION #
  ################
  
  plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) +
    
    ggplot2::geom_polygon(ggplot2::aes(group = group),
                          col = "darkgray", fill = "white") +
    
    ggplot2::geom_point(
      data = dataSess,
      aes(x = longitude, y = latitude, color = "Participations"),
      size = point_size, alpha = 0.9
    ) +
    
    ggplot2::scale_color_manual(
      name = "Légende",
      values = c("Participations" = point_color)
    ) +
    
    ggplot2::theme_void() +
    ggplot2::coord_quickmap(
      xlim = c(min(dataSess$longitude), max(dataSess$longitude)),
      ylim = c(min(dataSess$latitude), max(dataSess$latitude))
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 18)
    )
  
  ###############
  # SAVE PLOT   #
  ###############
  
  ggplot2::ggsave(plot = plot, filename = filename, device = "png",
                  path = path, width = 25, height = 20, units = "cm")
  
  return(plot)
}
