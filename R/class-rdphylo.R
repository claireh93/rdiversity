setOldClass("phylo")

#' Class 'rdphylo'
#'
#' @field hs.name \code{vector} of length \emph{hS}; historic species names
#' @field hs.pds \code{vector} of length \emph{hS}; descendant present day 
#' species
#' @field hs.edge \code{matrix}; ancestral and descendant nodes
#' @field hs.abundance \code{vector} of length \emph{hS}; historic species 
#' abundance
#' @field Lj \code{vector} of length \emph{S}; total evolutionary change of 
#' present day species
#' @field Tbar \code{numeric} element; mean total evolutionary change over 
#' present day species
#' @export
#' 
setClass("rdphylo",
         contains = "phylo",
         slots = c(hs.name = "character",
                   hs.pds = "numeric",
                   hs.edge = "matrix",
                   hs.length = "integer",
                   hs.abundance = "matrix",
                   Lj = "numeric",
                   Tbar = "numeric"
                   ))


is.rdphylo <-
  function (x)
  {
    inherits(x, "rdphylo")
  }


setMethod(f = "show", signature(object = "rdphylo"),
          definition = function(object){
            cat('Phylogenetic tree with', length(object$tip.label),
                'tips and', object$Nnode,
                'internal nodes (including the root.\n\n')

            cat('Tip labels:\n', head(object$tip.label), '\n\n')

            if(ape::is.rooted(object)) {
              rooted <- 'Rooted'
            } else rooted <- 'Unrooted'

            cat(rooted, '.')
          } )



