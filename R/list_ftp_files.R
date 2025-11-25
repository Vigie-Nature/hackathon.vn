# library(httr2)
# library(xml2)
# library(rvest)

# Fonction pour lister les fichiers du FTP
#'
#' @param chemin 
#' 
#' @description
#' récupère la liste des fichiers se trouvant sur le FTP pour facilement faire des boucles de traitement qui balayent l'ensemble des données dispos
#' 
#'
#' @return a character vector containing all csv export files present in the ftp
#'
#'
#' @examples
#' list_ftp_files()
#' 
list_ftp_files <- function(chemin = "") {
  url <- paste0(Sys.getenv('SITE_NAME'), chemin)
  
  reponse <- httr2::request(url) |> 
    httr2::req_auth_basic(Sys.getenv('HTTPS_USER'), 
                          password = Sys.getenv('HTTPS_PASSWORD')) |>
    httr2::req_perform()
  
  # Parser le contenu HTML de la liste de répertoire
  contenu <- httr2::resp_body_html(reponse)
  
  # Extraire les liens (noms de fichiers)
  fichiers <- contenu |> 
    rvest::html_nodes("a") |> 
    rvest::html_attr("href")
  
  # Filtrer pour enlever les éléments de navigation (., .., etc.)
  fichiers <- fichiers[!fichiers %in% c(".", "..", "../")]
  #ne garder que les fichiers csv commençant par export_
  fichiers <- subset(grepl("export_"),fichiers) 
  fichiers <- subset(grepl(".csv"),fichiers) 
  
  return(fichiers)
}

