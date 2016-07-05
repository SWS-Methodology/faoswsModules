#' Add ignore file to module
#'
#' This function writes an ignore file that is parsed by
#' the temporarily archived \code{PublishModule} and describes files that are not to be packaged
#' up.
#'
#' @param dir character. Directory where the module file should be placed.
#' @param contents character. Manually specify lines to go into ignore file
#'
#' @export
AddModuleignore <- function(dir = ".", contents = NULL){

  file <- file.path(dir, ".moduleignore")

  if(file.exists(file)){
    message(sprintf("%s exists, not overwriting it", file))
    return(invisible(NULL))
  }

  ignored <- c("^sws\\.yml", "\\.zip$")

  if(!is.null(contents)){
    ignored <- contents
  }

  writeLines(ignored, file)
  message(sprintf("[SUCCESS] %s written", file))
}
