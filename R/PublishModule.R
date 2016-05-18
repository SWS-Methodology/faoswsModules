#' Publish a module as a zip file
#'
#' When ready to upload your module, this function allows you to ignore files in
#' a .moduleignore file (as well as any files beginning with "."). It then
#' checks if the current files correspond to the current commit. If not, it
#' throws an error. If there is no difference, the files are copied to a
#' temporary folder and a line appended to the main R file with the current
#' commit hash. They are then zipped up and copied back to the module folder.
#'
#' @param modulepath character. Module path relative to the project root
#' @param ignore character. Location of ignore file
#'
#' @export

PublishModule <- function(modulepath, ignore = file.path(modulepath, ".moduleignore")){
  # Check for main.R
  allfiles <- list.files(modulepath, recursive = TRUE)

  if(file.exists(ignore)){
    ignoredfiles <- readLines(ignore)
    ignoredfiles <- ignoredfiles[ignoredfiles != "" & !grepl("^#", ignoredfiles)]
    allfiles <- grep(paste(ignoredfiles, collapse = "|"),
                     allfiles, value = TRUE, invert = TRUE)
  }
  # Check main.R
  if(length(grep(".*\\.[Rr]$", allfiles)) != 1){
    stop("A module must have one and only one .R file in the root directory")
  }
  # Check metadata.xml
  if(length(grep(".*\\.(xml|XML)$", allfiles)) != 1){
    stop("A module must have one and only one .R file in the root directory")
  }

  subdirfiles <- grep("/", allfiles, value = T)
  if(any(!grepl(".*\\.[rR]$", subdirfiles))){
    stop("All files in subdirectories must be .R files")
  }

  gitfunc <- file.path(path.package("faoswsModules"), "git-release.sh")
  command <- paste(gitfunc, modulepath, paste(allfiles, collapse = " "), collapse = " ")
  if(Sys.info()["sysname"] == "Windows"){
    shell(command, mustWork=TRUE)
  } else {
    system(command)
  }

}