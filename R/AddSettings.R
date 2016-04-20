#' Add settings file to project
#'
#' @param filename character.
#' @param gitignore character.
#' @param fields list.
#'
#' @import yaml
#'

AddSettings <- function(filename = "sws.yml", gitignore = TRUE, fields=NULL){

  yaml_list <- list(all = list(share="/media/share"),
                     qa = list(server="https://hqlqasws1.hq.un.fao.org:8181/sws",
                               token="abcdef0123456789"),
                     production = list(server="https://hqlprswsas1.hq.un.fao.org:8181/sws",
                                       token="9876543210fedcba"))

  if(!is.null(fields)){
    yaml_list <- fields
  }

  examplefile <- paste0(filename, ".example")
  alreadyFiles <- file.exists(filename, examplefile)

  #If files already exists, don't write them and stop
  if(any(alreadyFiles)){
    missing <- c(filename, examplefile)[alreadyFiles]
    stop(sprintf(ngettext(length(missing),
                          "Settings file %s already exists",
                          "Settings files %s already exist"),
                 paste0(missing, collapse = ", ")))
  }

  writeLines(as.yaml(yaml_list), filename)
  writeLines(as.yaml(yaml_list), examplefile)



  # Add to .gitignore
  if(gitignore){
    if(file.exists(".gitignore")){
      parsed_gitignore <- readLines(".gitignore")

      if(!(filename %in% parsed_gitignore)){
        ignore_connection <- file(".gitignore", "a")
        on.exit(close(ignore_connection))
        writeLines(filename, ignore_connection)
        message(sprintf("Filename %s added to .gitignore", filename))
      } else {
        message(sprintf("Filename %s already in .gitignore, not re-added", filename))
      }

    }
  }

  message(sprintf("[SUCCESS] Settings file %s prepared", filename))
}