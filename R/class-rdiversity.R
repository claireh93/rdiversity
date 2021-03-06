#' Class 'rdiv'
#' 
#' Define S4 class \code{rdiv}.
#' 
#' @field measure object of class \code{character}; e.g. 'subcommunity.alpha.bar'
#' @field tag object of class \code{formula}; e.g. bquote('Subcommunity' ~ bar(alpha))
#' @field level object of class \code{character}; e.g. 'subcommunity'
#' 
#' @export
#' 
rdiv <- setClass("rdiv",
                 contains = 'data.frame',
                 slots = c(measure = "character",
                           tag = "formula",
                           level = "character"))


is.rdiv <-
  function (x) 
  {
    inherits(x, "rdiv")
  }

#' @describeIn rdiv Prints diversity results
#' @param object object of class \code{rdiv}
#' 
setMethod(f = "show", signature = "rdiv", 
          definition = function(object) {
            cat(object@measure, '\n\n')
            print(object) 
            } )

