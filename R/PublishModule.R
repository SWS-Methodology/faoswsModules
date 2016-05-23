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
#' @param destdir character. Destination directory for module. By default, \code{modulepath}
#' @param ignore character. Location of ignore file
#'
#' @importFrom git2r repository status revparse_single
#' @export

PublishModule <- function(modulepath, destdir = modulepath, ignore = file.path(modulepath, ".moduleignore")){

  abs_path <- function(paths){
    vapply(paths, tools::file_path_as_absolute, character(1))
  }

  destdir <- abs_path(destdir)

  allfiles <- list.files(modulepath, recursive = TRUE)
  alldirs <- list.dirs(modulepath, full.names=FALSE)
  # By default includes self. Excluding it
  alldirs <- alldirs[alldirs != ""]

  if(file.exists(ignore)){
    ignoredfiles <- readLines(ignore)
    ignoredfiles <- ignoredfiles[ignoredfiles != "" & !grepl("^#", ignoredfiles)]
    ignoreregex <- paste(ignoredfiles, collapse = "|")
    allfiles <- grep(ignoreregex, allfiles, value = TRUE, invert = TRUE)
    alldirs <- grep(ignoreregex, alldirs, value = TRUE, invert = TRUE)
  }
  subdirfiles <- grep("/", allfiles, value = TRUE)
  basefiles <- setdiff(allfiles, subdirfiles)
  # Check main.R
  mainRfile <- grep(".*\\.[Rr]$", basefiles, value = TRUE)
  if(length(mainRfile) != 1){
    stop("A module must have one and only one .R file in the root directory")
  }
  metadatafile <- grep(".*\\.(xml|XML)$", basefiles, value = TRUE)
  # Check metadata.xml
  if(length(metadatafile) != 1){
    stop("A module must have one and only one .R file in the root directory")
  }

  if(any(!grepl(".*\\.[rR]$", subdirfiles))){
    stop("All files in subdirectories must be .R files")
  }

  repo <- repository(".")
  uncommitted <- abs_path(unlist(status(repo, all_untracked = TRUE)))
  module_uncommitted_index <- abs_path(file.path(modulepath,allfiles)) %in% uncommitted

  if(any(module_uncommitted_index)){
    stop(paste0("Some of the files that you wish to add to the modules are not in a commit:
         ", paste(allfiles[module_uncommitted_index], collapse = ", "),
          "\nEither stash, commit or remove the file(s) to perfom an automatic publish"))
  }

  # Make temporary file where we'll be zipping things
  temp <- tempfile()
  dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE))

  vapply(file.path(temp, alldirs), dir.create, logical(1), recursive = TRUE)
  file.copy(file.path(modulepath, allfiles), file.path(temp, allfiles))
  # Write HEAD commit to top of main R file
  current_commit <- revparse_single(repo, "HEAD")@sha
  mainRcontent <- c(paste0("# Commit: ", current_commit),
                    readLines(file.path(temp, mainRfile))
  )
  writeLines(mainRcontent, file.path(temp, mainRfile))
  # save module
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(temp)
  zip(file.path(destdir, paste0("module-", substr(current_commit, 1,6), ".zip")),
      list.files(temp, recursive = TRUE))

}