# uv boot grid plot func
ggbootGrid = function( numVec, xVec, yVec, B,
  fdr, nullValue, locType = "mean",
  xLabVec, xLab, yLab, minEff = 0 ){
  if( locType == "mean" ){
    bootFunc = function(x,w) mean(x[w])
    locFunc = mean
  }

  if( locType == "med" ){
    bootFunc = function(x,w) median(x[w])
    locFunc = median
  }

  # make a splitVec of above
  splitVec = str_c( xVec, ".", yVec )

  # mean tbl
  meanTbl = ldply( split( numVec, splitVec ), function(x) locFunc(x) ) %>%
    mutate( mean = V1,
      V1 = str_sub( .id, 1, 1 ), V2 = str_sub( .id, 3) ) %>%
    as_tibble()

  # rawbootTbl
  rawBootTbl = ldply( split( numVec, splitVec ), function(x) boot( x, bootFunc, B )$t ) %>%
    rename( stat = `1` ) %>%
    separate( .id, into = c( "V1", "V2" ) )

  ggplot( lPropTbl0, aes( x = vaccine, y = resp, fill = vaccine ) ) +
    geom_boxplot() +
    facet_wrap( ~ cytCombo, ncol = 2 )

  resTbl = rawBootTbl %>%
    group_by( V1, V2 ) %>%
    summarise( pVal = sum( (stat <= nullValue )*1 )/B ) %>%
    ungroup() %>%
    mutate( V1.V2 = str_c( V1, ".", V2 ) )

  # bh correction
  pVec = resTbl$pVal
  names( pVec ) = resTbl$`V1.V2`
  pVec = pVec[!is.nan(pVec)]

  sortPVec = sort( pVec )
  ind = 0; i = 0; m = length(sortPVec); k = 0

  while( i <= m -1 ){
    i = i + 1
    if( sortPVec[i] <= (i/m * fdr) ) k = i
  }

  if( k > 0 ){
    sigNameVec = names(sortPVec)[1:k]
  } else{
    sigNameVec = NULL
  }

  # plotTbl
  plotTbl = resTbl %>%
    full_join( meanTbl ) %>%
    mutate( sig = ( ( V1.V2  %in% sigNameVec ) * 1 ) %>% as.character(),
      sig = Vectorize(ifelse)( mean > minEff, sig, 0 ) )

  ggplot( plotTbl, aes( V1, V2, fill = sig)) +
    geom_tile(colour = "black") +
    scale_x_discrete( labels = xLabVec,
      name = xLab ) +
    scale_y_discrete( name = yLab ) +
    scale_fill_manual( values = c( "lightblue2", "firebrick1" ),
      name = "Significance\nLevel",
      labels =  c( "0" = str_c( "q>", fdr ),
        "1" = str_c( "q<", fdr ) )) +
    theme( axis.title = element_blank() )
}
