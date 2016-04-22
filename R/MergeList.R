#' Merge two lists together
#'
#'
#'
#'a <- list( a=list(a=1, b=2), b=3, c=1)
#'b <- list( a=2 , c=2)
#'merge(a,b)
#'x <- list( A=list(p=runif(5), q=list(y=3, z=2)), B=list(q=runif(5)) )
#'y <- list( A=list(r=runif(5), p=1:5, q = list(z=1, m=2)), C=list(s=runif(5)) )
#'
#' @export

MergeList <- function (list1, list2, no.rm = FALSE) {
  allNames <- unique(c(names(list1), names(list2)))
  merged <- list1 # we will copy over/replace values from list2 as necessary
  for (x in allNames) {
    # convenience
    a <- list1[[x]]
    b <- list2[[x]]
    if(!is.null(a) && !is.null(b) && xor(is.list(a), is.list(b)) && no.rm){
      merged[[x]] <- c(a,b)
    } else if (is.null(a)) {
      # only exists in list2, copy over
      merged[[x]] <- b
    } else if (is.list(a) && is.list(b)) {
      # recurse
      merged[[x]] <- MergeList(a, b)
    } else if (!is.null(b)) {
      # replace the list1 value with the list2 value (if it exists)
      merged[[x]] <- b
    }
  }
  return(merged)
}

