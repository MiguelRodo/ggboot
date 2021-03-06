% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggbootMV-helper.R
\name{calcRoundPropVar}
\alias{calcRoundPropVar}
\title{Return proportion of variation explained.}
\usage{
calcRoundPropVar(vec, comp = 1:2)
}
\arguments{
\item{vec}{numeric vector. Contains component-wise proportion of
variation.}

\item{comp}{integer vector. Specifies the principal components
whose proportion of variation must be returned. Defaults to 1:2.}
}
\value{
Numeric vector.
}
\description{
Convenience function to return the proportion of variation explained
for the desired principal components, rounded to two decimal places.
}
