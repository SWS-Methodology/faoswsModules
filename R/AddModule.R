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
#' @export AddModule

AddModule <- function(name="new_module", dir = sprintf("modules/%s", name)){

  module_dir <- path.expand(dir)

  if (!file.exists(module_dir)){
    message(sprintf("Directory %s doesn't exist. Creating it.", dir))
    dir.create(dir, recursive = TRUE)
  }

  mainfilename <-  "main.R"
  metadatafilename <- "metadata.xml"

chooseCopy(mainfilename, "sample_main.R", module_dir)
chooseCopy(metadatafilename, "sample_metadata.xml", module_dir)

}

chooseCopy <- function(file, samplefile, dir){
  if (file.exists(file.path(dir, file))){
    message(sprintf("%s exists. Will not overwrite", file))

  } else {

    file.copy(file.path(path.package("faoswsModules"), samplefile), file.path(dir, file))
    message(sprintf("File %s created in %s", file, dir))
  }
}
