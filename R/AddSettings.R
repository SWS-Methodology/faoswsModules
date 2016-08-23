#' Add settings file to project
#'
#' @description  Creates a sample settings file, by default (leading dashes for
#'   formatting, whitespace in the actual file):
#'
#' current: qa \cr
#' all: \cr
#' --- share: /media/share \cr
#' qa: \cr
#' --- certdir: path/to/qa/certs\cr
#' --- server: https://hqlqasws1.hq.un.fao.org:8181/sws \cr
#' --- token: abcdef0123456789 \cr
#' production: \cr
#' --- certdir: path/to/prod/certs\cr
#' --- server: https://hqlprswsas1.hq.un.fao.org:8181/sws \cr
#' --- token: 9876543210fedcba
#'
#' @param dir character. Directory in which to write settings files
#' @param filename character. Name of settings file to write
#' @param gitignore logical. Should this file be added to the .gitignore?
#' @param fields list. Manually specify the structure of the settings file
#'
#' @import yaml
#' @export

AddSettings <- function(dir = ".", filename = "sws.yml", gitignore = TRUE, fields=NULL){

  file <- file.path(dir, filename)

  # Basic settings file
  yaml_list <- list(current = "qa",
                    all = list(share="/media/share"),
                    qa = list(certdir="path/to/qa/certs",
                              server="https://hqlqasws1.hq.un.fao.org:8181/sws",
                              token="abcdef0123456789"),
                    production = list(certdir="path/to/prod/certs",
                                      server="https://hqlprswsas1.hq.un.fao.org:8181/sws",
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
      message(sprintf("Example file %s exists, settings file doesn't. Creating settings file %s",
              examplefile, file))
      file.copy(examplefile, file)
    } else {
      message(sprintf("Every settings file should have an example file. Creating example file %s",
              examplefile))
      file.copy(file, examplefile)
    }

  } else {
    # If neither exist, create them
    message(sprintf("Neither settings nor example file exist. Creating %s and %s", file, examplefile))
    writeLines(as.yaml(yaml_list), file)
    writeLines(as.yaml(yaml_list), examplefile)
  }


  # Add to .gitignore
  if(gitignore){
    if(file.exists(".gitignore")){
      no_newline <- FALSE
      parsed_gitignore <- withCallingHandlers(readLines(".gitignore"),
                                              warning = function(w){
                                                if(grepl("incomplete final line found on", w$message)){
                                                  no_newline <<- TRUE
                                                }
                                              })

      if(!(normalizePath(file) %in% normalizePath(parsed_gitignore, mustWork = FALSE))){
        ignore_connection <- file(".gitignore", "a")
        on.exit(close(ignore_connection))
        writeLines(paste0("\n"[no_newline], file), ignore_connection)
        message(sprintf("Filename %s added to .gitignore", file))
      } else {
        message(sprintf("Filename %s already in .gitignore, not re-added", file))
      }

    }
  }

  message(sprintf("[SUCCESS] Settings files prepared"))
}
