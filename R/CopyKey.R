#' Copy codes from a DatasetKey to clipboard
#'
#' One of the problems with the Statistical Working System is that of token
#' reuse. Tokens function as both authentication and data source for debugging,
#' so it can be very tempting for some developers to use the tokens of others.
#' This is risky as tokens allow authentication as other users and may also be
#' revoked by that user at any time. This function makes it easier to duplicate
#' someone else's session so that token reuse isn't necessary. It sequentially
#' copies codes to the keyboard so that they can be pasted into the web
#' interface of the Statistical Working System.
#'
#' @param key DatasetKey. Object obtained from the \code{faosws} package. It's
#'   the data type of the \code{swsContext.datasets[[1]]} object
#'
#' @importFrom methods is
#'
#' @export CopyKey

CopyKey <- function(key){
    if(!interactive()){
        stop("copyKey only works in interactive mode")
    }
    stopifnot(is(key, "DatasetKey"))

    dimension_names <- vapply(key@dimensions, function(x) x@name, character(1))

    readline("Clipboard will be overwritten. Press Esc to cancel. Press [Enter] to confirm.")
    message(sprintf("Keys are for dataset %s:%s", key@domain, key@dataset))

    for(i in seq_along(dimension_names)){
        writeClip(key@dimensions[[i]]@keys)
        readline(sprintf("Key %s written to clipboard. Go paste it in the web interface then [Enter]", dimension_names[i]))
    }

    message("All keys copied!")
    invisible(NULL)
}

## Shamelessly stolen from https://github.com/mrdwab/overflow-mrdwab/blob/master/R/clipboard.R
writeClip <- function(object) {
    OS <- Sys.info()["sysname"]
    if(!(OS %in% c("Darwin", "Windows", "Linux"))) {
        stop("Copying to clipboard not yet supported on your OS")
    }
    switch(
        OS,
        Darwin = {
            con <- pipe("pbcopy", "w")
            writeLines(object, con=con)
            close(con)
        },
        Windows = writeClipboard(object, format = 1),
        Linux = {
            if (Sys.which("xclip") == "") {
                if (Sys.which("xclip") == "") {
                    mess <- c("Clipboard on Linux requires 'xclip'. Try using:",
                              "sudo apt-get install xclip")
                    message(paste(mess, collapse = "\n"))
                }
            }
            con <- pipe("xclip -selection clipboard -i", open = "w")
            writeLines(object, con=con)
            close(con)
        })
}
