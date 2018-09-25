#' Plot cumulative axis predctivity
#'
#' Plot of the cumulative axis predictivity.
#' @param data dataframe. Named numeric columns in wide format required.
#' @inheritParams centreAndScale
#' @param legendTitle character. Title of legend. Default is "Variable".
#' @param legendLab named character vector. If NULL, then the labels used are
#' the names of the columns and "mean". If not NULL, then the names of the
#' labels must be the names of the columns in data, plus "mean".
#' @param legendCol character vector. Colours to be used. May be named using
#' the names of the columns in data, as well as "mean".
#'
#' @return Plot.
#' @export

plotCumQual = function( data, centre = TRUE, scale = FALSE,
  legendTitle = "Variable", legendLab = NULL, legendCol = NULL ){

  if( is.null( legendLab ) ) legendLab = setNames( tbl_vars( data ), tbl_vars( data ) ) %>%
      c( "mean" = "Mean" )
 if( is.null( legendCol ) ){
    legendCol = c( "darkorchid", "springgreen", "maroon1",
      "dodgerblue", "red", "cyan2", "yellow3", "orange2" )
    legendCol = setNames( legendCol, tbl_vars( data ) ) %>% c( "mean" = "black")
  }

  # svd
  numMat = centreAndScale( data, centre, scale ) %>% as.matrix()
  nVar = ncol( numMat )
  svdList = svd( numMat )

  # cumQMat
  cumQMat = matrix( 0, nrow = nVar, ncol = nVar )
  # compQMat
  compQMat = matrix( 0, nrow = nVar, ncol = nVar )

  for( r in 1:nVar ){
    # Jr
    Jr = matrix(0, ncol = nVar, nrow = nVar)
    for(i in 1:r) Jr[i,i] = 1

    # approx
    XHat = numMat %*% svdList$v %*% Jr %*% t( svdList$v )

    # predictivity
    cumQMat[r,] = ( diag(diag( t(XHat) %*% XHat )) %*% solve( diag( diag( t(numMat) %*% numMat ) ) ) ) %>% diag()

    if( r == 1 ) compQMat[1,] = cumQMat[1,]
    if( r > 1 ) compQMat[r,] = cumQMat[r,] - cumQMat[r-1,]
  }

  cumQTbl = as_tibble( cumQMat )
  compQTbl = as_tibble( compQMat )
  dimnames( cumQTbl )[[2]] = tbl_vars( data )
  dimnames( compQTbl )[[2]] = tbl_vars( data )
  # adequacy

  # gather
  cumQTbl %<>%
    gather( key = varName, value = cumQual ) %>%
    mutate( comp = rep( 1:nVar, nVar ) )



  # cumulative quality plot
  meanQualTbl = cumQTbl %>%
    group_by( comp ) %>%
    summarise( cumQual = mean( cumQual ) ) %>%
    mutate( varName = "mean" )

  cumQualPlotTbl = bind_rows( cumQTbl, meanQualTbl )

  ggplot( ) +
    geom_line( data = cumQualPlotTbl, aes( x = comp, y = cumQual, col = varName ),
      size = 1.5 ) +
    scale_x_continuous( breaks = 1:7 ) +
    labs( x = "Component number", y = "Cumulative Quality" ) +
    scale_colour_manual( name = legendTitle,
      label = legendLab,
      values = legendCol )
}


#' Component-wise axis predictivity plot.
#'
#' Outputs a plot of the component-wise axis predictivity.
#' @export
plotCompQual = function( data, pc1, pc2, colVarName = "Cytokine\nCombination",
  colVec = cytComboColVec, scale ){

  # svd
  numMat = centreAndScale( data, TRUE, scale ) %>% as.matrix()
  nVar = ncol( numMat )
  svdList = svd( numMat )

  # cumQMat
  cumQMat = matrix( 0, nrow = nVar, ncol = nVar )
  # compQMat
  compQMat = matrix( 0, nrow = nVar, ncol = nVar )

  for( r in 1:nVar ){
    # Jr
    Jr = matrix(0, ncol = nVar, nrow = nVar)
    for(i in 1:r) Jr[i,i] = 1

    # approx
    XHat = numMat %*% svdList$v %*% Jr %*% t( svdList$v )

    # predictivity
    cumQMat[r,] = ( diag(diag( t(XHat) %*% XHat )) %*% solve( diag( diag( t(numMat) %*% numMat ) ) ) ) %>% diag()

    if( r == 1 ) compQMat[1,] = cumQMat[1,]
    if( r > 1 ) compQMat[r,] = cumQMat[r,] - cumQMat[r-1,]
  }

  cumQTbl = as_tibble( cumQMat )
  compQTbl = as_tibble( compQMat )
  dimnames( cumQTbl )[[2]] = tbl_vars( data )
  dimnames( compQTbl )[[2]] = tbl_vars( data )

  # adequacy

  # gather
  cumQTbl %<>%
    gather( key = varName, value = cumQual ) %>%
    mutate( comp = rep( 1:nVar, nVar ) )

  compQTbl %<>%
    gather( key = varName, value = compQual ) %>%
    mutate( comp = rep( 1:nVar, nVar ) )

  # join
  qualTbl = full_join( cumQTbl, compQTbl, by = c( "varName", "comp" ))

  # component-wise quality
  compQPlotTbl = compQTbl %>%
    spread( key = comp, value = compQual )

  # component-wise quality tbl
  ggplot( compQPlotTbl, aes( x = compQPlotTbl[[1+pc1]], y = compQPlotTbl[[1+pc2]] ) ) +
    geom_text_repel( aes( label = varName, col = varName ), size = 5 ) +
    labs( x = paste0( "PC", pc1 ), y = paste0( "PC", pc2 )) +
    lims( x = c(0,1), y = c(0,1) ) +
    annotate( geom = "line", x = c( 0, 1 ), y = c( 0, 1 ), size = 1.2 ) +
    scale_colour_manual( values = cytComboColVec ) +
    theme( legend.position = "none" )
}

#' Calculate axis predictivity
#'
#' Calculates the axis predictivity for a dataset.
#'
#' @param data dataframe. May
#' @export
calcAxisPred = function( data, centre = TRUE, scale = FALSE ){
  # svd
  numMat = centreAndScale( data, centre, scale ) %>% as.matrix()
  nVar = ncol( numMat )
  svdList = svd( numMat )

  # cumQMat
  cumQMat = matrix( 0, nrow = nVar, ncol = nVar )
  # compQMat
  compQMat = matrix( 0, nrow = nVar, ncol = nVar )

  for( r in 1:nVar ){
    # Jr
    Jr = matrix(0, ncol = nVar, nrow = nVar)
    for(i in 1:r) Jr[i,i] = 1

    # approx
    XHat = numMat %*% svdList$v %*% Jr %*% t( svdList$v )

    # predictivity
    cumQMat[r,] = ( diag(diag( t(XHat) %*% XHat )) %*% solve( diag( diag( t(numMat) %*% numMat ) ) ) ) %>% diag()

    if( r == 1 ) compQMat[1,] = cumQMat[1,]
    if( r > 1 ) compQMat[r,] = cumQMat[r,] - cumQMat[r-1,]
  }

  cumQTbl = as_tibble( cumQMat )
  compQTbl = as_tibble( compQMat )
  dimnames( cumQTbl )[[2]] = tbl_vars( data )
  dimnames( compQTbl )[[2]] = tbl_vars( data )
  # adequacy

  # gather
  cumQTbl %<>%
    gather( key = varName, value = cumQual ) %>%
    mutate( comp = rep( 1:nVar, nVar ) )

  compQTbl %<>%
    gather( key = varName, value = compQual ) %>%
    mutate( comp = rep( 1:nVar, nVar ) )

  # join
  qualTbl = full_join( cumQTbl, compQTbl, by = c( "varName", "comp" ))

  qualTbl
}
