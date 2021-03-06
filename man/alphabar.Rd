% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diversity-components.R
\name{alphabar}
\alias{alphabar}
\title{Low level diversity components: normalised alpha}
\usage{
alphabar(super)
}
\arguments{
\item{super}{object of class \code{supercommunity}.}
}
\value{
Returns
}
\description{
Calculates the low-level diversity component necessary for calculating 
normalised alpha diversity.
}
\details{
Values generated from \code{alphabar()} may be input into \code{subdiv()} 
and \code{superdiv} to calcualte normalised subcommunity/supercommunity 
alpha diversity.
}
\examples{
pop <- sample(1:50, 5)
super <- supercommunity(pop)

# Calculate normalised alpha component
a <- alphabar(super)

}

