#' Publish a package to remote repository
#'
#' When ready to upload your new package version, this function allows you to
#' ignore files in a .moduleignore file (as well as any files beginning with
#' "."). It then checks if the current files correspond to the current commit.
#' If not, it throws an error. If there is no difference, the files are copied
#' to a temporary folder and a line appended to the main R file with the current
#' commit hash. They are then zipped up and copied back to the module folder.
#'
#' @param version character. Valid R version. There's no need to prefix it with
#'   a 'v'
#' @param packpath character. Package path relative to the project root
#' @param destdir character. Destination directory for package By default,
#'   \code{packpath}
#' @param ignorefile character. Location of .Rbuildignore file
#'
#' @importFrom git2r repository status revparse_single
#' @export PublishPackage

PublishPackage <- function(version,
                           message = sprintf("Release version %s", version),
                           packpath = ".",
                           destdir = packpath, date = Sys.Date(),
                           remote,
                           document = TRUE,
                           check = TRUE,
                           ignorefile = file.path(packpath, ".Rbuildignore"),
                           verbose = TRUE){

  ## This function performs the following:
  ## 1. Fixes documentation
  ## 1. Checks that there are no unignored uncommitted changes (including documentation)
  ## 2. Checks that the current version is newer than the old one and

  if(missing(version)){
    stop("A new version must be supplied in order to publish a package")
  }

  message("Checking for documentation updates")

  if(document){
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
  # Note original HEAD so we can go back if everything goes wrong
  original_HEAD <- commits(repo, n = 1)[[1]]

  SUCCESS <- FALSE
  on.exit(if(!SUCCESS){reset(original_HEAD, "hard")})

  # Don't worry about files that are in .gitignore
  git_ignored <- GetAbsolutePath(file.path(packpath, allfiles)) %in%
    GetAbsolutePath(unlist(status(repo, ignored=TRUE)$ignored))

  allfiles <- allfiles[!git_ignored]

  # Check that there are no modified or uncommitted files. This will ensure that
  # the commit and the built package are the same and that the check will be
  # successful.

  uncommitted <- GetAbsolutePath(unlist(status(repo, all_untracked = TRUE)))
  module_uncommitted_index <- GetAbsolutePath(file.path(packpath,allfiles)) %in% uncommitted

  if(any(module_uncommitted_index)){
    stop(paste0("Some of the files in the package directory are not in a commit:
                ", paste(allfiles[module_uncommitted_index], collapse = ", "),
                "\nEither stash, commit, remove or add the file(s) to a .Rbuildignore file to perfom an automatic publish"))
  }

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
    stop(sprintf("New version %s needs to be greater than existing version %s",
                 new_version, old_version))
  }
  new_description <- description
  new_description[,"Version"] <- version
  new_description[,"Date"] <- as.character(date)
  write.dcf(new_description, description_path)

  message("Running R CMD CHECK")

  if(check){
    devtools::check(packpath, document = document)
  }

  add(repo, description_path)
  commit(repo, message)
  tag(repo, paste0("v", version), message, session = TRUE)

  SUCCESS = TRUE

  sprintf("Successfully tagged %s %s for release", package_name, version)

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
