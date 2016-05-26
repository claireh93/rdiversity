#' Calculate community diversity
#' 
#' Generic function for calculating community diversity.
#' 
#' \code{data} may be input as three different classes:
#' \itemize{
#' \item{\code{powermean} calculates comunity alpha, alphabar, rho, rhobar, 
#' or gamma diversity by taking the powermean of the subcommunity diversity components}
#' \item{\code{relativeentropy} calculates community beta or betabar 
#' diversity by taking the relative entropy of the subcommunity diversity components}
#' \item{\code{supercommunity} caculates all subcommunity measures of diversity}
#' }
#' 
#' @param data two-dimensional \code{matrix} of mode \code{numeric}; diversity 
#' components.
#' @param qs \code{vector} of mode \code{numeric}; parameter of conservatism.
#' 
#' @details as ds
#' 
#' @return Returns a two-dimensional \code{matrix} of mode \code{numeric}.
#' @export 
#' @examples 
#' pop <- sample(1:50, 5)
#' super <- supercommunity(pop)
#' 
#' # Calculate subcommunity gamma diversity (takes the power mean)
#' g <- gamma(super)
#' subdiv(g, 0:2)
#' 
#' # Calculate subcommunity beta diversity (takes the relative entropy)
#' b <- beta(super)
#' subdiv(b, 0:2)
#' 
#' # Calculate all measures of subcommunity diversity
#' subdiv(super, 0:2)
#' 
setGeneric(name = "commdiv",
           def = function(data, qs, levels) {
             standardGeneric("commdiv")
           } )

#' @rdname commdiv
#' 


#' @rdname commdiv
#' @return 
#' 
setMethod(f = "commdiv", signature(levels="matrix"), 
          definition = function(data, qs, levels) {
            data.sub <- subdiv(data, qs)
            output <- matrix(nrow=length(unique(levels)), ncol=length(qs))
            for (i in unique(levels)){
              chunk=data.sub[which(levels==i),]
              q.list <- as.list(seq_along(qs))
              output[i,] <- lapply(q.list, 
                                       function(y) power.mean(chunk, order=(1-qs[y])))}
            colnames(output) <- paste0("q",qs)
            rownames(output) <- paste("Community",unique(levels)+1)
            output
          } )

## sapply-test with different situations, list 1:length(qs) use lapply!
setMethod(f = "commdiv", signature(levels="numeric"), 
          definition = function(data, qs, levels) {
            levels <- as.matrix(levels)
            commdiv(data,qs,levels)
          } )
## Take in vector or a dataframe and convert into matrix, then re-call commdiv

setMethod(f = "commdiv", signature(levels="data.frame"), 
          definition = function(data, qs, levels) {
            levels <- as.matrix(levels)
            commdiv(data,qs,levels)
          } )

#' @rdname commdiv
#' 
setMethod(f = "commdiv", signature= "supercommunity", 
          definition = function(data, qs, levels) {  
            # Calculate terms
            div.measures <- list(alpha, alphabar, 
                                 beta, betabar,
                                 rho, rhobar,
                                 gamma)
            # Calculate subcommunity diversity
            results <- lapply(div.measures, function(x) 
              res <- commdiv(x(data), qs, levels))
            names(results) <- c("alpha", "alphabar", "beta", "betabar",
                                "rho", "rhobar", "gamma") 
            results
          } )






