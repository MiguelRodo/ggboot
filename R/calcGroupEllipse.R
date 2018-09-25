#' Calculate bivariate normal contour for groups.
#'
#' @param data dataframe. Observations in numeric columns, with grouping variable
#' a character column.
#' @param p numeric. Contour percentile.
#' @return A tibble with points representing coordinates composing the contour lines
#' for each group, and a column denoting group for each coordinate.
calcGroupEllipse = function( data, p = 0.95 ){

  numTbl = data %>% select_if( is.numeric )
  groupVec = data %>% select_if( is.character ) %>% `[[`(1)

  ldply( split( numTbl, groupVec ), function(x){
    calcEllipse( x, p = p )
  } ) %>%
    dplyr::rename( group = .id,
      x = V1,
      y = V2 ) %>%
    as_tibble()
}

#' Calculate bivariate normal contour.
#'
#' Calculate the \code{p}-th contour line for a distribution
#' using observations in \code{data}, assuming that the
#' distribution is bivariate normal.
#' @param data numeric dataframe
#' @inheritParams calcGroupEllipse
#' @return A matrix with points along the contour line.

calcEllipse = function( data, p = 0.95 ){

  # circle
  thetaVec = c( seq( -pi, pi, length = 50 ), seq( pi, -pi, length = 50 ) )
  circleMat = cbind( cos( thetaVec ), sin( thetaVec ) )

  # sample values
  meanVec = laply( data, mean )
  sigmaMat = var( as.matrix( data ) )
  baseRadius <- sqrt( qchisq( p, df = 2 ) )

  # calculate ellipse
  sweep( circleMat %*% chol( sigmaMat ) * baseRadius, 2, meanVec, FUN = '+' )

}

