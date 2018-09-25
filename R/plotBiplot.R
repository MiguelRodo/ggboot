
pcaBiplotPlotFunc = function( numTbl, infoTbl, pcCompNum1 = 1, pcCompNum2 = 2, ellipseVec = NULL,
  legendTitle, labelVec, colVec, scale = FALSE, centre = TRUE, ellipse = TRUE,
  medPos = TRUE, textAdjVal = 0.2, pointSize = 3, xMult = 1 ){

  # pca Obj
  pcaObj = calcBiplot( dataTibble = numTbl, infoTbl = infoTbl, scale = scale, centre = centre, pcCompNum1 = pcCompNum1,
    pcCompNum2 = pcCompNum2, ellipseGroupVec = ellipseVec, textAdjVal = textAdjVal )

  # median positions


  # cumulative proportion of variation
  varProp1 = round( pcaObj$compPropVarVec[ pcCompNum1 ], 2 ) * 100
  varProp2 = round( pcaObj$compPropVarVec[ pcCompNum2 ], 2 ) * 100

  # plot

  p = ggplot( pcaObj$pcaScoreAndInfoTbl ) +
    geom_line( data = pcaObj$axesCoordTbl %>% filter( segmentPos != 'max' ),
      aes( x = x * xMult, y = y, group = varName ), col = 'gray10'  ) +
    geom_line( data = pcaObj$axesCoordTbl %>% filter( segmentPos != 'min' ),
      aes( x = x * xMult, y = y, group = varName ), col = 'gray10'  ) +
    labs( x = str_c( 'PC', pcaObj$pcCompNumVec[1], " - ", varProp1, "%" ),
      y = str_c( 'PC', pcaObj$pcCompNumVec[2], " - ", varProp2, "%" ) ) +
    coord_fixed()

  # ellipses and point colours
  if( is.null( ellipseVec ) ){
    p = p +
      geom_point( aes( x = V1 * xMult, y = V2 ), size = pointSize ) # +
    # geom_text( data = medPosTbl, aes( x = medV1, y = medV2, label = labelVec ),
    #           size = 4, fontface = 'bold' ) +
  } else {
    p  = p +
      geom_point( aes( x = V1 * xMult, y = V2, col = ellipseVec ), size = pointSize ) +
      scale_colour_manual( name = legendTitle,
        label = labelVec,
        values = colVec )

    # ellipse
    if( ellipse ){
      p = p + geom_path( data = pcaObj$ellipseTbl, aes( x = x * xMult, y = y, colour = group, group = group ), size = 1 )
    }

    #if medPos == TRUE
    if( medPos == TRUE ){
      medPosTbl = pcaObj$pcaScoreAndInfoTbl %>%
        # select( -ellipseGroupVec ) %>%
        # mutate( vaccine = wInfoTbl$vaccine, vaccInfGroup = wInfoTbl$vaccInfGroup ) %>%
        group_by( ellipseGroupVec ) %>%
        summarise( medV1 = median(V1), medV2 = median(V2) )
      p = p + geom_text( data = medPosTbl, aes( x = medV1 * xMult, y = medV2, label = labelVec ), size = 4, fontface = 'bold' )
    }


  }

  # add labels
  p = p +
    annotate( 'text', x = pcaObj$textXVec * xMult, y = pcaObj$textYVec,
      label = tbl_vars( numTbl ), size = 4,
      col = 'brown4' )

  # return
  p

}
