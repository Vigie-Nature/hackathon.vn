
#' Fonction pour afficher le nombre d'utilisateurs / sites / structures... etc
#' en fonction du nombre d'années uniques de participation
#'
#' @param df_vn a dataframe (raw from export on ftp servor)
#' @param col_x a character
#' @param is.year a boolean  - if you want to be classify by year
#'                           - !! col_x must be a date if is.year is TRUE)
#' @param col_stack a character (column who want to group by)
#' @param color_bar an character (format hexadecimal color) 
#' @param xlab a character
#' @param ylab a character
#'
#' @return a ggplot object
#'
#' @examples
#' invisible(lapply(file.path("R", dir("R")), source))
#' propage = download_from_ftp("export_propage.csv")
#' barplot_duree_participation(df_vn = propage)
#' barplot_duree_participation(df_vn = propage, col_stack = "site_id",
#'                             ylab = "Nombre de sites")
#' barplot_duree_participation(df_vn = propage, col_stack = "structure_id",
#'                             ylab = "Nombre de structures")
#' barplot_duree_participation(df_vn = propage, col_x = "session_id", is.year = F,
#'                             col_stack = "user_id", xlab = "Nombre de sessions uniques",
#'                             ylab = "Nombre d'utilisateurs")
#'                             
barplot_duree_participation <- function(df_vn,
                                        col_x = "session_date",
                                        is.year = T,
                                        col_stack = "user_id",
                                        color_bar = "#cdd644",
                                        xlab = "Nombres d'années de participation",
                                        ylab = "Nombre d'utilisateurs"){
    
    if (is.year) {
        df_stack_year = df_vn %>%
            mutate(col_x = strftime(!!dplyr::sym(col_x), "%Y"))
    }else{
        df_stack_year = df_vn %>%
            mutate(col_x = !!dplyr::sym(col_x))
    }
    
    df_stack_year = df_stack_year %>%
        dplyr::mutate(stack = as.character(!!dplyr::sym(col_stack))) %>%
        dplyr::group_by(stack) %>%
        dplyr::summarise(nbx = n_distinct(col_x))
    
    gg <- ggplot2::ggplot(df_stack_year, ggplot2::aes(x = nbx)) +
        ggplot2::geom_bar(fill = color_bar) +
        cowplot::theme_cowplot() +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab)
    
    return(gg)
}



#' Fonction pour tracer le suivi de participation par année
#'
#' @param df_vn a dataframe
#' @param col_group a character (column name)
#' @param sub_sample a vector (subsample of col_group)
#' @param col_date a character (column name -> format date)
#' @param col_name a character (column name)
#' @param color a character (hexadecimal )
#' @param size_p a double
#' @param xlab a character
#' @param ylab a character
#'
#' @return a ggplot object
#'
#' @examples
#' invisible(lapply(file.path("R", dir("R")), source))
#' propage = download_from_ftp("export_propage.csv")
#' suivi_temporel_transects(df_vn = propage)
#' suivi_temporel_transects(df_vn = propage, sub_sample = c(243, 310, 622, 75, 77, 1589, 1520, 88))
#' suivi_temporel_transects(df_vn = propage, col_group = "structure_id", col_name = "structure_nom")
#' suivi_temporel_transects(df_vn = propage, col_group = "structure_id", col_name = "structure_nom",
#'                          sub_sample = c(227, 327, 4, 364, 136, 90, 102, 130,6))
suivi_temporel_transects <- function(df_vn,
                                     col_group = "site_id",
                                     sub_sample = c(),
                                     col_date = "session_date",
                                     col_name = "site",
                                     color = "#aa3839", size_p = 1.5,
                                     xlab = "Années", ylab = "Nom du site"){
    
    # On filtre sur un sous-échantillon si demandé
    if (length(sub_sample) > 0) {
        df_ligne = df_vn %>%
            dplyr::filter(!!dplyr::sym(col_group) %in% sub_sample)
    }else{
        df_ligne = df_vn
    }
    
    # Data frame pour tracer les lignes continues de participation
    df_ligne = df_ligne %>%
        dplyr::filter(!is.na(!!dplyr::sym(col_date))) %>%
        dplyr::mutate(session_year = as.integer(strftime(!!dplyr::sym(col_date), "%Y"))) %>%
        dplyr::arrange(!!dplyr::sym(col_date)) %>%
        dplyr::select(session_year, !!dplyr::sym(col_group), !!dplyr::sym(col_name)) %>%
        unique() %>%
        dplyr::group_by(!!dplyr::sym(col_group), !!dplyr::sym(col_name)) %>%
        dplyr::mutate(index = dplyr::cur_group_id(), 
                      index_y = index + cumsum(c(TRUE, diff(session_year) != 1)),
                      name = !!dplyr::sym(col_name),
                      groupe = paste0(!!dplyr::sym(col_group), index_y)) %>%
        dplyr::arrange(session_year)
    
    # Data frame des points pour marquer les extrémités de ligne + marque les participations sur une année
    df_point = df_ligne %>%
        dplyr::group_by(groupe) %>%
        filter(session_year == min(session_year) | session_year == max(session_year)) %>%
        ungroup()
    
    # Tracer le graphique
    gg <- ggplot2::ggplot(df_ligne, ggplot2::aes(x = session_year, y = reorder(as.character(name),
                                                                               session_year))) +
        ggplot2::geom_line(ggplot2::aes(group = groupe), color = color) +
        ggplot2::geom_point(data = df_point, mapping = ggplot2::aes(x = session_year,
                                                                    y = as.character(name)),
                            size = size_p, color = color) +
        ggplot2::theme_minimal() +
        ggplot2::xlab(xlab) + ggplot2::ylab(ylab)+
        ggplot2::scale_x_continuous(sec.axis = dup_axis()) +
        theme(strip.text.y = ggplot2::element_text(angle = 0),
              axis.text.x.top = ggplot2::element_text())
    
    return(gg)
}

