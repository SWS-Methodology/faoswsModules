#' Add settings file to project
#'
#' @param file character.
#' @param gitignore character.
#' @param fields list.
#'
#' @import yaml
#' @export

AddSettings <- function(file = "sws.yml", gitignore = TRUE, fields=NULL){

  # Basic settings file
  yaml_list <- list(current = "qa",
    all = list(share="/media/share"),
    qa = list(server="https://hqlqasws1.hq.un.fao.org:8181/sws",
              token="abcdef0123456789"),
    production = list(server="https://hqlprswsas1.hq.un.fao.org:8181/sws",
                      token="9876543210fedcba"))

  # Overwrite the basic with a new one if it is provided
  if(!is.null(fields)){
    yaml_list <- fields
  }

  examplefile <- paste0(file, ".example")

  if(all(file.exists(file, examplefile))){
    # If files already exists, don't write them
    missing <- message(sprintf("Settings files %s already exist. Not overwriting them.",
                            paste0(c(file, examplefile), collapse = ", ")
                            )
                    )
  } else if(xor(file.exists(examplefile),file.exists(file))){
    # If one file exists and not the other, copy it
    if(file.exists(examplefile) && !file.exists(file))  {
      message(sprintf("Example file %s exists, settings file doesn't. Creating settings file %s"),
              examplefile, file)
      file.copy(examplefile, file)
    } else {
      message(sprintf("Every settings file should have an example file. Creating example file %s",
              examplefile))
      file.copy(file, examplefile)
    }

  } else {
    # If neither exist, create them
    message("Neither settings nor example file exist. Creating %s and %s", file, examplefile)
    writeLines(as.yaml(yaml_list), file)
    writeLines(as.yaml(yaml_list), examplefile)
  }


  # Add to .gitignore
  if(gitignore){
    if(file.exists(".gitignore")){
      parsed_gitignore <- readLines(".gitignore")

      if(!(file %in% parsed_gitignore)){
        ignore_connection <- file(".gitignore", "a")
        on.exit(close(ignore_connection))
        writeLines(file, ignore_connection)
        message(sprintf("filename %s added to .gitignore", file))
      } else {
        message(sprintf("filename %s already in .gitignore, not re-added", file))
      }

    }
  }

  message(sprintf("[SUCCESS] Settings file prepared"))
}
