#' Title
#'
#' @param dt sortie de la fonction fct_tableau_participation 
#' @param time ce qu'on cherche à quantifier 
#' @param axe_y echelle de temps selectionnée
#'
#' @returns un plot représentant axe_y en fonction de time
#'
#' @examples
#' 
#' 
plot_evol_participation = function(dt, time, axe_y) {
  
  plot_evol = 
    ggplot2::ggplot(dt, aes(x = !!sym(time), y = !!sym(axe_y))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() 
  
  return(plot_evol)
  
}


