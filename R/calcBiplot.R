#' Bootstrap PCA biplot statistics
#'
#' Returns necessary statistics for PCA biplot.
#'
calcBiplot = function( dataTibble, scale, centre = TRUE, pcCompNum1 = 1, pcCompNum2 = 2,
  ellipseGroupVec = NULL, ellipseProb = 0.69, textAdjVal = 0.2 ){

  # set-up
  pcCompNumVec = sort( c( pcCompNum1, pcCompNum2 ))
  nCol = ncol( dataTibble )
  subSegmentPosVec = c( 'min', 'median', 'max' )

  # pca
  pcaOutput = calcPCA( dataTibble, centre, scale )

  # output saving
  Y = pcaOutput[[ 2 ]][ , pcCompNumVec ] # pc scores
  H = pcaOutput[[ 1 ]][ , pcCompNumVec ] # pc loadings
  eigenValVec = pcaOutput[[ 3 ]] # eigenvalues
  cumPropVarVec = pcaOutput[[ 4 ]] # cumulative proportion of variance
  compPropVarVec = pcaOutput[[ 5 ]]

  ######### PC SCORES

  pcaScoreTbl = as_tibble( Y )

  ######### CALIBRATED AXEs

  # a one unit increase in the variable
  varLengthVec = rep( 0, nCol )
  for ( row in 1:nCol ){
    varLengthVec[ row ] = H[ row, 1 ] ^ 2 + H[ row, 2] ^ 2
  }

  ### FIND VALUES TO MARK AT

  ####column means
  meanTbl = as_tibble( apply( dataTibble, 2, mean) )

  #column standard deviations
  #sdTbl  = as_tibble( apply( dataTibble, 2, sd ) )

  ### CALIBRATION VALUES

  calValList = calcCalVal(  dataTibble,  "m3" )

  ### CALIBRATION FACTORS AND HYPOTENUSE LENGTHS

  calFactList = list()
  hypLenList = list()
  nCalVal = length( calValList[[ 1 ]] )

  for( i in 1:nCol ){
    currCalValVec = calValList[[ i ]]
    currMean = as.numeric( meanTbl[ i, 1 ] )
    currSd = 1 #as.numeric( sdTbl[ i, 1 ] )
    currVarLength = varLengthVec[ i ]
    calFactList[[ i ]] = ( currCalValVec - currMean ) / currSd / currVarLength
    hypLenList[[ i ]] = calFactList[[ i ]] * currVarLength
  }

  ### FIND LINE MARKERS

  # preliminaries
  gradVec = H[ , 2 ] / H[ , 1 ] # gradient
  baseAngleVec = atan( abs( gradVec ) ) # angle in radians
  xPosNegIndVec = ifelse( H[,  1 ] > 0, 1, -1 ) # max x positivity
  yPosNegIndVec = ifelse( H[ , 2 ] > 0, 1, -1 ) # max y positivity
  axesCoordTbl = tibble( x = rep( 0, nCalVal * nCol ), y = 0, varName = 'a', segmentPos = rep( subSegmentPosVec, nCol ) )

  for( i in 1:nCol ){
    # from the hypotenuse lengtha and the variable angle, get the
    # x and y displacement.
    # adjust these from the signvec to get the correct placement of min and max.
    currBaseAngle = baseAngleVec[ i ]
    currHypLen = hypLenList[[ i ]]
    currFirstRow = ( i - 1 ) * length( subSegmentPosVec ) + 1
    currLastRow = i * length( subSegmentPosVec )
    axesCoordTbl$x[ currFirstRow:currLastRow ] = cos( currBaseAngle ) * currHypLen * xPosNegIndVec[ i ]
    axesCoordTbl$y[ currFirstRow:currLastRow ] = sin( currBaseAngle ) * currHypLen * yPosNegIndVec[ i ]
    axesCoordTbl$varName[ currFirstRow:currLastRow  ] = tbl_vars( dataTibble )[ i ]
  }

  ######### TEXT

  ### Goal: Get the positions and the angles of the text to add to the ends
  ### of the lines.

  textXVec = rep( 0, nCol )
  textYVec = rep( 0, nCol )

  for( i in 1:nCol ){
    # from the hypotenuse lengtha and the variable angle, get the
    # x and y displacement.
    # adjust these from the signvec to get the correct placement of min and max.
    currBaseAngle = baseAngleVec[ i ]
    CurrAdjHypLen = hypLenList[[ i ]][ 'max' ] + textAdjVal
    textXVec[ i ] = cos( currBaseAngle ) * CurrAdjHypLen * xPosNegIndVec[ i ]
    textYVec[ i ] = sin( currBaseAngle ) * CurrAdjHypLen * yPosNegIndVec[ i ]
  }

  ######### PATIENT INFO

  if( ! is.null( ellipseGroupVec ) ){
    pcaScoreAndInfoTbl = as_tibble( pcaScoreTbl ) %>%
      mutate( group = ellipseGroupVec )
  } else {
    pcaScoreAndInfoTbl = pcaScoreTbl
  }

  ######### ELLIPSE

  # Overlay a concentration ellipse if there are groups
  #if( !is.null( ellipseGroupVec ) ){

    ### PRELIMINARIES

    #ellipseRawDataTbl = tibble( V1 = pcaScoreTbl$V1,
    #  V2 = pcaScoreTbl$V2,
    #  group = ellipseGroupVec )

    ### ELLIPSE

    #ellipseTbl = ellipseRawDataTbl %>%
    #  group_by( group ) %>%
    #  do( calcEllipse( . ) )

#  }

  ### OUTPUT
  if ( !is.null( ellipseGroupVec ) ){
    outputList= list(
      'fullPCAScoreTbl' = pcaOutput[[2]],
      'pcaScoreAndInfoTbl' = pcaScoreAndInfoTbl,
      'axesCoordTbl' = axesCoordTbl,
      'textXVec' = textXVec,
      'textYVec' = textYVec,
      'pcCompNumVec' = pcCompNumVec,
      'cumPropVarVec' = cumPropVarVec,
      # 'ellipseTbl' = ellipseTbl,
      'compPropVarVec' = compPropVarVec,
      'VJMat' = H )
  } else {
    outputList= list(
      'fullPCAScoreTbl' = pcaOutput[[2]],
      'pcaScoreAndInfoTbl' = pcaScoreAndInfoTbl,
      'axesCoordTbl' = axesCoordTbl,
      'textXVec' = textXVec,
      'textYVec' = textYVec,
      'pcCompNumVec' = pcCompNumVec,
      'cumPropVarVec' = cumPropVarVec,
      'compPropVarVec' = compPropVarVec,
      'VJMat' = H )
  }

  return( outputList )
}
