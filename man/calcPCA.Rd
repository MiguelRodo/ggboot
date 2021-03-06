% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxBiplot.R
\name{calcPCA}
\alias{calcPCA}
\title{Calculate PCA and associated statistics.}
\usage{
calcPCA(grid, centre = TRUE, scale, r = ncol(grid))
}
\arguments{
\item{grid}{matrix or dataframe. If a dataframe, then the numeric columns
are selected and converted to a matrix.}

\item{centre, scale}{logical. If TRUE, then the matrix has its mean subtracted
(centre) and/or has each column divided by its sd (scale). If scale = TRUE and the sd
of a column is 0, then that column is not divided by its sd and a message
is printed.}

\item{r}{integer. The number of principal compononents to show information for.
Defaults to \code{ncol(grid)}.}
}
\value{
A list containing elements:

\tabular{ll}{
VJMat \tab matrix of eigenvectors \cr
YMat \tab matrix of principal component scores \cr
singValVec \tab vector of singular values \cr
cumPropVarVec \tab vector of cumulative proportion of variation \cr
compPropVarVec \tab vector of component-wise proportion of variation
}
}
\description{
Calculate PCA
}
