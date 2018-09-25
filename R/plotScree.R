# scree plot
plot_scree = function( numTbl, centre = TRUE, scale = FALSE, nVar = NULL ){

  ### PRELIM
  if( is.null( nVar ) ) nVar = ncol( numTbl ) # get desired number of variables to plot

  ### CALC
  pcaOutput = calcPCA( dataTibble = as.matrix( numTbl ), scale = scale, centre = centre ) # calc pca

  ### PLOT PREP
  plotTbl = tibble( x = rep( 1:nVar, 2 ), y = c( pcaOutput[[ 5 ]][1:nVar], pcaOutput[[ 4 ]][1:nVar] ),
    type = rep( c( "comp", "cum" ), each = nVar ) ) # prep plot

  ### PLOT
  ggplot( plotTbl, aes( x, y ) ) +
    geom_line( size = 1) +
    geom_point( size = 2 ) +
    labs( x = "Component number", y = "Proportion of variation" ) +
    facet_wrap( ~ type, ncol = 2, scale = "free_y",
      labeller = labeller( type = c( "comp" = "Component-wise", "cum" = "Cumulative" ) ) ) +
    scale_x_continuous( breaks = 1:nVar ) +
    background_grid( major = 'y', minor = 'none',
      colour.major = 'grey75' )
}
