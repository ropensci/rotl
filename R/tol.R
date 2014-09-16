##' Basic information about the tree
##'
##' Summary information about the current draft tree of life,
##' including information about the list of trees and the taxonomy
##' used to build it.
##' @title Information about the tree of life
##' @return Some JSON
##' @author Francois Michonneau
##' @export
tol_about <- function() {
    otl_POST(path="tree_of_life/about", body=list())
}

##' Reurns the MRCA
##'
##' Most recent common ancestor
##' @title get MRCA
##' @param nds
##' @return the MRCA
##' @author Francois Michonneau
##' @export
tol_mrca <- function(q=list("ott_ids"=c(412129, 536234))) {
    otl_POST(path="tree_of_life/mrca", body=q)
}
