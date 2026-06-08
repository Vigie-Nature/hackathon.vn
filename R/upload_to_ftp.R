#' Upload a file to ftp server
#'
#' @param file_to_upload file to upload
#' @param file_folder_local folder where your file is (local)
#' @param file_folder_destination folder where you want to upload (server)
#'
#' @return
#'
#' @examples
#' 
#' upload_to_ftp("test_upload.csv", file_folder_destination = "Vigie-Nature/" )
#' 
upload_to_ftp <- function (file_to_upload, file_folder_local = "", file_folder_destination = ""){
  # import environment variables
  readRenviron(".env")
  
  # get permissions
  user = Sys.getenv("FTP_USER")
  password = Sys.getenv("FTP_PASSWORD")
  
  # upload file via ftp
  system(paste0('curl -p --insecure  "ftp://v5.vigienature-ecole.fr/',
                file_folder_destination,
                '" --user "',
                user,
                ':',
                password,
                '" -T ',
                paste0(file_folder_local, file_to_upload),
                ' --ftp-create-dirs --ftp-ssl -k'))
}
