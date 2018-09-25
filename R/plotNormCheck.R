# qq plot
qqPlotFunc = function( dataTbl, selVar, nCol, pointSize = 1, lineSize = 1, lineAlpha = 0.7){
  
  # centre and standardise
  stdTbl = dataTbl %>%
    group_by( group ) %>%
    mutate(V1 = (V1-mean(V1))/sd(V1),
      V2 = (V2-mean(V2))/sd(V2) ) %>%
    ungroup()
  
  B = dataTbl %>%
    group_by( group ) %>%
    summarise( count = n() ) %>%
    `[[`( "count" ) %>%
    unique()
  
  if( selVar == "V1" ){
    
    # quantiles
    quantTbl = stdTbl %>%
      group_by( group ) %>%
      arrange( V1 ) %>%
      mutate( stdQuant = qnorm( seq(1/B,1-1/B,length.out=B)) ) %>%
      ungroup()
    
    # plot
    p = ggplot( quantTbl, aes( x = stdQuant, y = V1, col = group ) )
    
    
  } else if( selVar == "V2" ){
    
    # quantiles
    quantTbl = stdTbl %>%
      group_by( group ) %>%
      arrange( V2 ) %>%
      mutate( stdQuant = qnorm( seq(1/2000,1-1/2000,length.out=2000)) ) %>%
      ungroup()
    
    # plot
    p = ggplot( quantTbl, aes( x = stdQuant, y = V2, col = group ) )
  }
  
  p +
    geom_point( size = pointSize ) +
    facet_wrap( ~ group, ncol = nCol, labeller = labeller( group = vaccLabelVec ) ) +
    labs( x = "Standard Normal Quantile", y = "Standardised Sample Quantile" ) +
    annotate( geom = "line", x = c(-3,3), y = c(-3, 3), size = lineSize, alpha = lineAlpha ) +
    scale_colour_manual( values = vaccColVec ) +
    theme( legend.position = "none" )
  
}

# normal histogram plot
histNormFunc = function( dataTbl, selVar, xlab, nCol ){
  
  # centre and standardise
  stdTbl = dataTbl %>%
    group_by( group ) %>%
    mutate(V1 = (V1-mean(V1))/sd(V1),
      V2 = (V2-mean(V2))/sd(V2) ) %>%
    ungroup()
  
  # normal density
  xVec = seq(-3,3, length.out=1e4)
  normVec = dnorm( xVec )
  
  # plot
  if( selVar == "V1" ){
    ggplot( stdTbl, aes( x = V1, fill = group, col = group ) ) +
      facet_wrap( ~ group, labeller = labeller( group = vaccLabelVec ),
        scales = 'free_x', ncol = nCol ) +
      geom_density() +
      theme( legend.position = "none" ) +
      scale_fill_manual( values = vaccColVec ) +
      scale_colour_manual( values = vaccColVec ) +
      labs( x = xlab, y = "Density" ) +
      annotate( geom = "line", x = xVec, y = normVec, size = 1.5, linetype = 1 )
  } else if( selVar == "V2" ){
    ggplot( stdTbl, aes( x = V2, fill = group, col = group ) ) +
      facet_wrap( ~ group, labeller = labeller( group = vaccLabelVec ),
        scales = 'free_x', ncol = nCol ) +
      geom_density() +
      theme( legend.position = "none" ) +
      scale_fill_manual( values = vaccColVec ) +
      scale_colour_manual( values = vaccColVec ) +
      labs( x = xlab, y = "Density" ) +
      annotate( geom = "line", x = xVec, y = normVec, size = 1.5, linetype = 1 )
  }
}
