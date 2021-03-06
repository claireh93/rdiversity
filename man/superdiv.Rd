% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/superdiv.R
\docType{methods}
\name{superdiv}
\alias{superdiv}
\alias{superdiv,powermean-method}
\alias{superdiv,relativeentropy-method}
\alias{superdiv,supercommunity-method}
\title{Calculate supercommunity diversity}
\usage{
superdiv(data, qs)

\S4method{superdiv}{powermean}(data, qs)

\S4method{superdiv}{relativeentropy}(data, qs)

\S4method{superdiv}{supercommunity}(data, qs)
}
\arguments{
\item{data}{two-dimensional \code{matrix} of mode \code{numeric}; diversity 
components.}

\item{qs}{\code{vector} of mode \code{numeric}; parameter of conservatism.}
}
\value{
Returns a two-dimensional \code{data_frame} of mode \code{numeric}.
}
\description{
Generic function for calculating subcommunity diversity.
}
\details{
\code{data} may be input as three different classes:
\itemize{
\item{\code{powermean} calculates subcomunity alpha, alphabar, rho, rhobar, 
or gamma diversity by taking the powermean of diversity components}
\item{\code{relativeentropy} calculates subcommunity beta or betabar 
diversity by taking the relative entropy of diversity components}
\item{\code{supercommunity} caculates all subcommunity measures of diversity}
}
}
\examples{
pop <- data.frame(a = c(1,3), b = c(1,1))
pop <- pop / sum(pop)
super <- supercommunity(pop)

# Calculate subcommunity gamma diversity (takes the power mean)
g <- gamma(super)
superdiv(g, 0:2)

# Calculate subcommunity beta diversity (takes the relative entropy)
b <- beta(super)
subdiv(b, 0:2)

# Calculate all measures of subcommunity diversity
subdiv(super, 0:2)

}

