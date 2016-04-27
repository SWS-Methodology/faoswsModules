#' Read settings file
#'
#' A settings file allows a user of the SWS to make changes to the parameters of
#' a script without changing the script itself. The format itself is very
#' flexible yaml file, but has three special sections:
#'
#' \itemize{
#' \item \emph{current} - This required field indicates the current
#' server to which the script is directed
#' \item \emph{global} - This optional file indicates the location of a global
#' file from which you may fetch settings.
#' \item \emph{all} - This optional field specifies settings that should always
#' be used. It overrides settings in the global file and is overwritten by
#' specific server settings.}
#'
#'
#' @param file character. Name of the settings file.
#' @param current character. This overrides the 'current' field in the settings
#'   file.
#'
#' @seealso AddSettings
#'
#' @import yaml
#'
#' @export

ReadSettings <- function(file="sws.yml", current=NULL){

  if(!file.exists(file)){
    stop(sprintf("The settings file, %s, doesn't exist", file))
  }

  raw_yaml <- yaml.load_file(file)

  if(is.null(current)) {

    current <- raw_yaml[["current"]]

    if(is.null(current)){
      stop("No server specified in the 'current' field")
    }
  }

  ## -------- Extract special values --------
  globalfile <- raw_yaml[["global"]]

  if(!is.null(globalfile)){
    if(!file.exists(globalfile)){
      stop(sprintf("Global file %s specified in %s does not exist", globalfile, file))
    } else {
      global <- yaml.load_file(globalfile)
    }
  } else {
    global <- NULL
  }

  if(!(current %in% c(names(global), names(raw_yaml)))){
    stop(sprintf("Server '%s' is not in any settings file", current))
  }

  ## -------- Assign in priority order --------
  # Priority goes global, all, specific in increasing priority

  yaml_settings <- NULL

  #### Global settings ####
  if(!is.null(global)) {
    current_global  <- global[[current]]
    if(!is.null(current_global))
    yaml_settings <- current_global
  }

  #### All settings ####
  all_settings <- raw_yaml[["all"]]
  yaml_settings <- MergeList(yaml_settings, all_settings)

  #### Local settings ####
  local_settings <- raw_yaml[[current]]
  yaml_settings <- MergeList(yaml_settings, local_settings)

  # Combine current value
  yaml_settings <- c(current = current, yaml_settings)

  yaml_settings
}