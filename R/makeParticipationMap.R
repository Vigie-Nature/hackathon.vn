makeParticipationMap <- function(data, path, filename = "map_sessions.png",
                            point_color = "lightblue", point_size = 1){

  # Remove NAs
  data <- data[!is.na(data$longitude) & !is.na(data$latitude), ]
  
  # Unique sessions with coordinates
  dataSess <- unique(data[, c("session_id", "longitude", "latitude")])
  
  ############################
  #   FRANCE POLYGON        #
  ############################
  
  dataFrance <- ggplot2::map_data("france")
  
  ################
  # MAP CREATION #
  ################
  
  plot <- ggplot2::ggplot(dataFrance, ggplot2::aes(long, lat)) +
    
    # France polygon
    ggplot2::geom_polygon(ggplot2::aes(group = group),
                          col = "darkgray", fill = "white") +
    
    # Session points
    ggplot2::geom_point(data = dataSess,
                        aes(x = longitude, y = latitude),
                        color = point_color,
                        size = point_size,
                        alpha = 0.9) +
    
    ggplot2::theme_void() +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::coord_quickmap(
      xlim = c(min(dataSess$longitude), max(dataSess$longitude)),
      ylim = c(min(dataSess$latitude), max(dataSess$latitude))
    )
  
  ###############
  # SAVE PLOT   #
  ###############
  
  ggplot2::ggsave(plot = plot, filename = filename, device = "png",
                  path = path, width = 25, height = 20, units = "cm")
  
  return(plot)
}
