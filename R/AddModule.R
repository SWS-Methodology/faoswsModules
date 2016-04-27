#' Make module skeleton
#'
#' Make everything you need to get started on making modules for the Statisical
#' Working system including:
#'
#' \itemize{
#' \item main.R
#' \item metadata.xml (future release)
#' \item script subdirectories (future release)
#' }
#'
#' @param name character. Name of the new module
#' @param dir character. Directory in which to place the new module
#'
#' @export

AddModule <- function(name="new_module", dir = sprintf("modules/%s", name)){

  dir <- path.expand(dir)

  if (!file.exists(dir)){
    message(sprintf("Directory %s doesn't exist. Creating it.", dir))
    dir.create(dir, recursive = TRUE)
  }

  mainfilename <-  "main.R"
  metadatafilename <- "metadata.xml"

  package_path <- path.package("faoswsModules")

  if (file.exists(file.path(dir, mainfilename))){
    message(sprintf("%s exists. Will not overwrite", mainfilename))

  } else {

    file.copy(file.path(package_path, "sample_main.R"), file.path(dir, "main.R"))
    message(sprintf("File %s created in %s", mainfilename, dir))
  }

}