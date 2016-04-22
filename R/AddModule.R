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
#' @export

AddModule <- function(name="New module", dir = "modules/newmodule"){

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