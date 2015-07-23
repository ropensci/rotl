##' An Interface to the Open Tree of Life API
##'
##' The Open Tree of Life is an NSF funded project that is generating
##' an online, comprehensive phylogenetic tree for 1.8 million
##' species. \code{rotl} provides an interface that allows you to
##' query and retrive the parts of the tree of life that is of
##' interest to you.
##'
##' \code{rotl} provides function to most of the end points the API
##' provides. The documentation of the API is available at:
##' \url{https://github.com/OpenTreeOfLife/opentree/wiki/Open-Tree-of-Life-APIs}
##'
##' @section Customizing API calls:
##'
##'     All functions that use API end points can take 2 arguments to
##'     customize the API call and are passed as \code{...} arguments.
##'
##'     \itemize{
##'
##'     \item{ \code{otl_v} } { This argument controls which version
##'     of the API your call is using. The default value for this
##'     argument is a call to the non-exported function
##'     \code{otl_version()} which returns the current version of the
##'     Open Tree of Life APIs (v2).}
##'
##'     \item{ \code{dev_url} } { This argument controls whether to use
##'     the development version of the API. By default, \code{dev_url}
##'     is set to \code{FALSE}, using \code{dev_url = TRUE} in your
##'     function calls will use the development version.}
##'
##'     }
##'
##'     For example, to use the development version of the API, you
##'     could use: \code{tnrs_match_names("anas", dev_url=TRUE)}
##'
##'     Additional arguments can also be passed to the
##'     \code{\link[httr]{GET}} and \code{\link[httr]{POST}} methods.
##'
##'
##' @section Acknowledgments:
##'
##'     This package was started during the Open Tree of Life
##'     \href{http://blog.opentreeoflife.org/2014/06/11/apply-for-tree-for-all-a-hackathon-to-access-opentree-resources/}{Hackathon}
##'     organized by OpenTree, the NESCent Hackathon Interoperability
##'     Phylogenetic group, and Arbor.
##'
##' @name rotl
##' @docType package
##' @import ape
NULL
