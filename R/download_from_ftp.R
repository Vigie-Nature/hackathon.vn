#' Telecharger les exports standardises VN/VNE
#' 
#' @description
#' La fonction permet de télécharger les données des exports standardisés VN et VNE
#' disponibles sur le serveur FTP. his function prints a simple message. This is a demo function to show good
#' practices in writing and documenting R function. If you delete this function
#' do not forget to delete the corresponding test file 
#' `tests/testthat/test-demo.R` if you used `new_package(test = TRUE)`.
#' 
#' @param nom_fichier a string of characters.
#'
#' @return Un dataframe qui contient les donnees de l'export à plat.
#' 
#' @export
#'
#' @examples
#' download_from_ftp("export_vigie_flore.csv")
#' 
#' 
#' 

download_from_ftp <- function(nom_fichier) {
  requete <- httr2::request(paste0(Sys.getenv('SITE_NAME'), nom_fichier)) |> 
    httr2::req_auth_basic(Sys.getenv('HTTPS_USER'), 
                   password = Sys.getenv('HTTPS_PASSWORD')) |>
    httr2::req_perform() |> httr2::resp_body_raw()
  
  
  
  df_serveur <- readr::read_csv2(requete)
  if(ncol(df_serveur)<=1){
    df_serveur <- readr::read_csv(requete)
  }
  return(df_serveur)
}
