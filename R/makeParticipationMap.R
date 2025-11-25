
#' makeParticipationMap
#' 
#' A function that returns a map of the participations 
#' 
#' @param data : a `data.frame` containing all species observations
#' @param path : a `string` specifying the folder where the file should be saved
#' @param filename : a `string` specifying the name of the file to be saved
#' @param point_color : a `string` specifying the color to use for the plot
#' @param path : a `int` specifying the shape of the points
#' 
makeParticipationMap <- function(data, path, filename = "map_sessions.png",
                            point_color = "lightblue", point_size = 2){
  
  if(!("longitude" %in% colnames(data) & "latitude" %in% colnames(data))){
    stop("Les colonnes longitude et latitude sont manquantes.")
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
    
    # France shape
    ggplot2::geom_polygon(ggplot2::aes(group = group),
                          col = "darkgray", fill = "white") +
    
    # Sessions points + legend
    ggplot2::geom_point(
      data = dataSess,
      aes(x = longitude, y = latitude, color = "Participations"),
      size = point_size, alpha = 0.9
    ) +
    
    # Legend for points
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
