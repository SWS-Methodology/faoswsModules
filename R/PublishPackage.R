#' Publish a package to remote repository
#'
#' Creates a tagged commit suitable for the Statistical Working System. The
#' documentation is checked, followed by a check that there are no uncommitted
#' files which could end up the the commit. The commit is then made and tagged.
#'
#' @param version character. Valid R version. There's no need to prefix it with
#'   a 'v'
#' @param message character. Message to go in tag label and
#' @param packpath character. Package path relative to the project root
#' @param date character or Date. Today's date to go in the Date field of the
#'   DESCRIPTION
#' @param destdir character. Destination directory for package By default,
#'   \code{packpath}
#' @param document logical, whether documentation should be run
#' @param check logical whether RCMD check should be run
#' @param ignorefile character. Location of .Rbuildignore file
#'
#' @importFrom git2r repository status revparse_single
#' @export PublishPackage

PublishPackage <- function(version,
                           message = sprintf("Release version %s", version),
                           packpath = ".",
                           destdir = packpath,
                           date = Sys.Date(),
                           document = TRUE,
                           check = TRUE,
                           ignorefile = file.path(packpath, ".Rbuildignore")){

  ## This function performs the following:
  ## 1. Fixes documentation
  ## 1. Checks that there are no unignored uncommitted changes (including documentation)
  ## 2. Checks that the current version is newer than the old one and

  if(missing(version)){
    stop("A new version must be supplied in order to publish a package")
  }

  if(document){
    message("Checking for documentation updates")
    devtools::document(packpath)
  }

  allfiles <- list.files(packpath, recursive = TRUE)
  alldirs <- list.dirs(packpath, full.names=FALSE)
  # By default includes self (""). Excluding it
  alldirs <- alldirs[alldirs != ""]

  if(file.exists(ignorefile)){
    ignorelines <- readLines(ignorefile)

    allfiles <- FilterIgnored(allfiles, ignorelines)
    alldirs <- FilterIgnored(alldirs, ignorelines)
  }

  # Check for uncommitted files
  repo <- repository(packpath)

  # Don't worry about files that are in .gitignore
  git_ignored <- GetAbsolutePath(file.path(packpath, allfiles)) %in%
    GetAbsolutePath(unlist(status(repo, ignored=TRUE)$ignored))

  allfiles <- allfiles[!git_ignored]

  # Check that there are no modified or uncommitted files. This will ensure that
  # the commit and the built package are the same and that the check will be
  # successful.

  uncommitted <- GetAbsolutePath(unlist(status(repo, all_untracked = TRUE)))
  module_uncommitted_index <- GetAbsolutePath(file.path(packpath,allfiles)) %in% uncommitted

  # These will most likely be forgotten files or documentation

  if(any(module_uncommitted_index)){
    stop(paste0("Some of the files in the package directory are not in a commit:
                ", paste(allfiles[module_uncommitted_index], collapse = ", "),
                "\nEither stash, commit, remove or add the file(s) to a .Rbuildignore file to perfom an automatic publish"))
  }

  # Note original HEAD so we can go back if everything goes wrong
  original_HEAD <- commits(repo, n = 1)[[1]]

  SUCCESS <- FALSE
  on.exit(if(!SUCCESS){reset(original_HEAD, "hard")})

  message("Updating DESCRIPTION")

  new_version <- package_version(version)
  description_path <- file.path(packpath, "DESCRIPTION")
  # Make backup to preserve the DESCRIPTION _exactly_ in the case of failure
  # including whitespace which might register a change in version control
  description_backup <- readLines(description_path)
  description <- read.dcf(file = textConnection(description_backup))
  package_name <- description[,"Package"]
  old_version <- package_version(description[,"Version"])
  if(new_version <= old_version){
    stop(sprintf("New version %s needs to be higher than existing version %s",
                 new_version, old_version))
  }
  new_description <- description
  new_description[,"Version"] <- version
  new_description[,"Date"] <- as.character(date)
  write.dcf(new_description, description_path)

  if(check){
    message("Running R CMD CHECK")
    devtools::check(packpath, document = document)
  }

  message("Adding DESCRIPTION to repo")
  add(repo, description_path)
  message("Committing new version")
  commit(repo, message)
  message("Tagging new release")
  tag_name <- paste0("v", version)
  tag(repo, tag_name, message, session = TRUE)

  on.exit(if(!SUCCESS){tag_delete(repo, tag_name)}, add = TRUE)

  SUCCESS = TRUE

  message(sprintf("Successfully tagged %s %s for release", package_name, version))

  invisible(data.frame(Package = package_name,
             oldVersion = as.character(old_version),
             newVersion = as.character(new_version),
             Message = message))

}

FilterIgnored <- function(filelist, ignorelines){
  ignorelines <- ignorelines[ignorelines != "" & !grepl("^#", ignorelines)]
  if(!length(ignorelines)) return(filelist)
  ignoreregex <- paste(ignorelines, collapse = "|")
  grep(ignoreregex, filelist, value = TRUE, invert = TRUE)
}

GetAbsolutePath <- function(paths){
  ab <- function(p){
    # Directories are normal paths
    tools::file_path_as_absolute(sub("/+$", "", p))
  }
  vapply(paths, ab, character(1))
}
