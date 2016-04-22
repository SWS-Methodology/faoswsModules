#'Read settings file
#'
#' @param file character.
#'
#' @import yaml
#'
#' @export

ReadSettings <- function(file="sws.yml"){

  if(!file.exists(file)){
    stop(sprintf("The settings file, %s, doesn't exist", file))
  }

  raw_yaml <- yaml.load_file(file)
  server <- raw_yaml[["current"]]

  if(is.null(server)){
    stop("No server specified in the 'current' field")
  }

  ## -------- Extract special values --------
  globalfile <- raw_yaml[["global"]]

  if(!is.null(globalfile)){
    if(!file.exists(globalfile)){
      stop("Global file %s specified in %s does not exist.", globalfile, file)
    } else {
      global <- yaml.load_file(globalfile)
    }
  } else {
    global <- NULL
  }

  if(!(server %in% c(names(global), names(raw_yaml)))){
    stop(sprintf("Server '%s' is not in any settings file", server))
  }

  ## -------- Assign in priority order --------
  # Priority goes global, all, specific in increasing priority

  yaml_settings <- NULL

  #### Global settings ####
  if(!is.null(global)) {
    current_global  <- global[[server]]
    if(!is.null(current_global))
    yaml_settings <- current_global
  }

  #### All settings ####
  all_settings <- raw_yaml[["all"]]
  yaml_settings <- MergeList(yaml_settings, all_settings)

  #### Local settings ####
  local_settings <- raw_yaml[[server]]
  yaml_settings <- MergeList(yaml_settings, local_settings)

  # Combine current value
  yaml_settings <- c(current = raw_yaml[["current"]], yaml_settings)

  yaml_settings
}