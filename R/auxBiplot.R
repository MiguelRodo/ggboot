#' Centre and scale a dataframe.
#'
#' \code{centreAndScale} allows column-wise mean centring and
#' standard-deviation scaling for a dataframe or matrix. The extra
#' features of this function are the following:
#'
#' \itemize{
#'   \item If a column has zero standard deviation, then scaling
#'   has no effect. A message is outputted to inform the user.
#'   \item A dataframe may be provided with character columns. These
#'   are simply ignored for the purposes of centring and scaling.
#'   \item A dataframe or a matrix may be provided. If the
#'   input was a matrix, then the output is also a matrix. If the
#'   input inherits from the dataframe class, then the output is a
#'   tibble.
#' }
#'
#'
#' @param grid dataframe or matrix.
#' @param centre logical. If TRUE, then each column has its own
#' mean subtracted
#' @param scale logical. If TRUE, then each column is divided by
#' its own standard deviation. If a column has zero standard
#' deviation, then it is returned as is.
#' @param message logical. If TRUE, then
#' @return A tibble or a matrix, depending on whether \code{grid}
#' inherited from the dataframe class or was a matrix.
#' @export

centreAndScale = function( grid, centre, scale, message = TRUE ){
  # check if a matrix or a tibble
  if( is.matrix( grid ) ){
    mat = TRUE
    grid = as_tibble( grid )
  }
  if( !is_tibble( grid ) ) grid = as_tibble( grid ); mat = FALSE

  # if centring and scaling
  if( scale & centre ){
    # centre
    grid %<>% mutate_if( is.numeric, function(x) x - mean(x) )
    # scale
    grid %<>% mutate_if( is.numeric, function(x){
    if( sd(x) == 0 ){
      if( message ) message( "A column had an sd of zero. Its mean-subtracted value was: ", unique( x ), "." )
      return(x)
    }
    x/sd(x)
    } )
    if( mat ) grid %<>% as.matrix()
    return( grid )
  }
  if( centre ){
    grid %<>% mutate_if( is.numeric, function(x) x - mean(x) )
    if( mat ) grid %<>% as.matrix()
    return(grid)
  }

  if( scale ){
    # scale
    grid %<>% mutate_if( is.numeric, function(x){
      if( sd(x) == 0 ){
        if( message ) message( "A column had an sd of zero. Its mean-subtracted value was: ", unique( x ) )
        return(x)
      }
      x/sd(x)
    } )
    if( mat ) grid %<>% as.matrix()
    return( grid )
  }
}

#' Calculate PCA and associated statistics.
#'
#' Calculate PCA
#'
#' @param grid matrix or dataframe. If a dataframe, then the numeric columns
#' are selected and converted to a matrix.
#' @param centre,scale logical. If TRUE, then the matrix has its mean subtracted
#' (centre) and/or has each column divided by its sd (scale). If scale = TRUE and the sd
#' of a column is 0, then that column is not divided by its sd and a message
#' is printed.
#' @param r integer. The number of principal compononents to show information for.
#' Defaults to \code{ncol(grid)}.
#' @return A list containing elements:
#'
#' \tabular{ll}{
#' VJMat \tab matrix of eigenvectors \cr
#' YMat \tab matrix of principal component scores \cr
#' singValVec \tab vector of singular values \cr
#' cumPropVarVec \tab vector of cumulative proportion of variation \cr
#' compPropVarVec \tab vector of component-wise proportion of variation
#' }
#' @export

calcPCA = function ( grid, centre = TRUE, scale, r = ncol( grid ) ){

  ### CENTRING AND SCALING

  if( !is.matrix( grid ) ) grid = grid %>% as_tibble() %>%
      select_if( is.numeric ) %>% as.matrix()

  mat = centreAndScale( grid, centre, scale ) %>% as.matrix()

  ### SVD

  svdOutput = svd( mat )

  ### CALCULATIONS

  VJMat = svdOutput$v[ , 1:r ] # first r XtX eigenvectors
  YMat = mat %*% VJMat # PCA scores
  singValVec = svdOutput$d #eigenvalues
  cumPropVarVec = cumsum( svdOutput$d ^ 2 ) / sum( svdOutput$d ^ 2 )
  compPropVarVec = ( svdOutput$d ^ 2 ) / sum( svdOutput$d ^ 2 )

  ### OUTPUT

  outputList = list ( VJMat = VJMat, YMat = YMat, singValVec = singValVec, cumPropVarVec = cumPropVarVec, compPropVarVec = compPropVarVec )
  return( outputList )
}

#' Calculate calibration values.
#'
#' Calculate calibration values.
#' @export

calcCalVal = function( dataTibble, calOption ){

  ### PURPOSE: Give the min, median and max of the columns of a data tibble
  ### in a list.

  ### PRELIMINARIES

  nCol = ncol( dataTibble )
  calValList = list( )

  ### OPTIONS

  if( calOption == "m3" ){ # min, median and max

    for ( i in 1:nCol ){

      currCol = dataTibble[ , i ]
      medianString = summary( currCol )[3]
      colonLoc = str_locate( medianString, ":")[, 'start' ]
      medianVal = as.numeric( str_sub( medianString, start = colonLoc + 1 ) )
      minVal = min( currCol )
      maxVal = max( currCol )
      calValList[[ i ]] = c( 'min' = minVal, 'median' = medianVal, 'max' = maxVal)

    }

  }

  if( calOption == "prop" ){

    for ( i in 1:nCol ){

      # data prep
      currCol = dataTibble[[i]]
      minVal = min( currCol )
      maxVal = max( currCol )

      # calculation
      potValVec = seq( -1, 1, by = 0.2) # vector of possible calibration points
      calMinVal = potValVec[ potValVec <= minVal ] %>% max()
      calMaxVal = potValVec[ potValVec >= maxVal ] %>% min()
      calValList[[ i ]] = potValVec[ which( potValVec >= calMinVal & potValVec <= calMaxVal ) ]

    }


  }

  ### OUTPUT
  return( calValList )
}

#' Calculate ellipses
#'
#' Calculates ellipses
#' @export

calcEllipse = function( currTbl, type = "pcaBiplot", group = NULL, p = 0.69, medVec = NULL ){

  ### PURPOSE - Calculate the contour of the 69th quantile of
  ### a 2D distribution based, assuming a
  ### bivariate normal distribution with parameters from the data.

  ### CIRCLE

  thetaVec = c( seq( -pi, pi, length = 50 ), seq( pi, -pi, length = 50 ) )
  circleMat = cbind( cos( thetaVec ), sin( thetaVec ) )

  ### INPUT CALCULATIONS

  if( is.null( medVec  ) ){
    currMeanVec = c( mean( currTbl$V1 ), mean( currTbl$V2 ) )
  } else if ( !is.null( medVec ) ){
    currMeanVec = medVec
  }

  sigmaMat = var( cbind( currTbl[[ 'V1' ]], currTbl[[ 'V2' ]] ) )
  baseRadius <- sqrt( qchisq( p,  df = 2 ) )

  ### ELLIPSE CALCULATION

  ellipseCalcMat = sweep( circleMat %*% chol( sigmaMat ) * baseRadius, 2, currMeanVec, FUN = '+' )

  ### OUTPUT

  if( type == "pcaBiplot" ) {
    outputTbl =  tibble( x = ellipseCalcMat[ , 1 ], y = ellipseCalcMat[ , 2 ],
      group = currTbl$group[1] )  %>% mutate( group = as.character( group ) )
  } else if( type == "mvMed" ) {
    outputTbl =  tibble( x = ellipseCalcMat[ , 1 ], y = ellipseCalcMat[ , 2 ], group = group ) %>%
      mutate( group = as.character( group ) )
  }
  return( outputTbl )
}
