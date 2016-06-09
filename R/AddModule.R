#' Make module skeleton
#'
#' Make everything you need to get started on making modules for the Statisical
#' Working system including:
#'
#' \itemize{
#' \item main.R
#' \item metadata.xml
#' \item sws.yml settings files
#' \item .moduleignore file
#' \item script subdirectories (future release)
#' }
#'
#' @param name character. Name of the new module
#' @param dir character. Directory in which to place the new module
#' @param type character. Either a valid reference to an example in the package (see Details) or a filename.
#' @param gitignore character. Whether the gitignore should be modified to include the sws.yml file.
#'
#' @details
#'  The following are valid module types in the package:
#'  \itemize{
#'  \item "basic" - This is the most basic module which reads from a dataset, modifies it and writes back.
#'  \item "ensure" - This module performs validation checks from `faoswsEnsure`
#'  }
#'
#' @export AddModule

AddModule <- function(name="new_module", dir = sprintf("modules/%s", name), type = "basic", gitignore = TRUE){

  module_dir <- path.expand(dir)

  if (!file.exists(module_dir)){
    message(sprintf("Directory %s doesn't exist. Creating it.", dir))
    dir.create(dir, recursive = TRUE)
  }

  mainfilename <-  "main.R"
  metadatafilename <- "metadata.xml"

  AddSettings(file = file.path(module_dir, "sws.yml"), gitignore = gitignore)
  AddModuleignore(module_dir)

  type_choice <- data.frame(name = c("basic", "ensure"),
                            file = c("basic_main.R", "ensure_main.R"),
                            stringsAsFactors = FALSE)

  chosen_main <- type_choice[type_choice$name == type, "file"]
  if(length(chosen_main) == 0){
    if(file.exists(type)){
      message("Type is not in the list, assuming filename")
      chosen_main <- type
    } else {
      stop(sprintf("File %s does not exist. Please either use one of the types in ?AddModule or supply a valid filename for a main R file", type))
    }
  }

  chooseCopy(mainfilename, chosen_main, module_dir)
  chooseCopy(metadatafilename, "sample_metadata.xml", module_dir)

}

chooseCopy <- function(file, samplefile, dir){
  if (file.exists(file.path(dir, file))){
    message(sprintf("%s exists. Will not overwrite", file))

  } else {

    file.copy(file.path(path.package("faoswsModules"), samplefile), file.path(dir, file))
    message(sprintf("[SUCCESS] File %s created in %s", file, dir))
  }
}
