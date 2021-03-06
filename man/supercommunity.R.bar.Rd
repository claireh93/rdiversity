% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diversity-measures.R
\name{supercommunity.R.bar}
\alias{supercommunity.R.bar}
\title{Similarity-sensitive normalised supercommunity.R diversity}
\usage{
supercommunity.R.bar(super, qs)
}
\arguments{
\item{super}{object of class \code{supercommunity}}

\item{qs}{\code{vector} of \emph{q} values}
}
\value{
Returns a two-dimensional \code{matrix} of diversities, with columns  
representing subcommunities and rows representing values of \emph{q}.
}
\description{
\code{supercommunity.R.bar()} is used to calculate the 
average representativeness of subcommunities.
}
\details{
\code{supercommunity.R.bar()} calculates the total noramlised 
supercommunity rho diversity of a series of columns representing independent 
subcommunity counts relative to the supercommunity as a whole (by default 
the sum of the subcommunities). This measure may be calculated for a series 
of orders, repesented as a vector of \code{qs}.
}
\examples{
pop <- sample(1:50, 5)
super <- supercommunity(pop)

# Calculate noramlised supercommunity rho diversity (takes the power mean)
a <- rhobar(super)
superdiv(a, 0:2)

}

