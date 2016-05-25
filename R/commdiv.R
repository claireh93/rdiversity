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
          definition = function(data, qs, length) {
            data.sub <- subdiv(data, qs)
            span=calc.span(length, data.sub)
            output <- matrix(nrow=length(unique(span)), ncol=length(qs))
            for (i in unique(span)){
              chunk=data.sub[which(span==i),]
              output[(i+1),] <- sapply(seq_along(qs), 
                                       function(y) power.mean(chunk, order=(1-qs[y])))}
            colnames(output) <- paste0("q",qs)
            rownames(output) <- paste("Community",unique(span)+1)
            output
          } )

## sapply-test with different situations, list 1:length(qs) use lapply!
setMethod(f = "commdiv", signature(levels="numeric"), 
          definition = function(data, qs, levels) {
            levels <- as.matrix(levels)
            commdiv(data,qs,levels)
            #             # Calculate subcommunity diversity
            #             data.sub <- subdiv(data, qs)
            #             # Calculate diversities of communities
            #             output <- matrix(nrow=length(unique(levels)), ncol=length(qs))
            #             for (i in unique(levels)){
            #               chunk=data.sub[which(levels==i),]
            #               # Calculate powermean of subcommunity diversities for each q value
            #               output[i,] <- sapply(seq_along(qs), 
            #                                                function(y) power.mean(chunk, order=(1-qs[y])))}
            #             colnames(output) <- paste0("q",qs)
            #             rownames(output) <- paste("Community",unique(levels))
            #             output
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






