plotTbl
x = split( data, data$facet )[[1]]
trim = 0.2
llply( split( data, data$facet ), function(x){

  hierMat = x %>%
    group_by( group ) %>%
    summarise( calcStat = mean( resp, 0.2) ) %>%
    spread( key = group, value = calcStat ) %>%
    as.matrix( hierMat )
    select( - facet ) %>%
    spread( key = group, value = resp )
}
library(cluster)
hclust( hierMat )
is.matrix( hierMat )

upperTriMat = plotTbl %>%
  filter( .id == 0 ) %>%
  select( x, y, origStat ) %>%
  spread( key = x, value = origStat ) %>%
  select( -y ) %>%
  as.matrix()

lowerTriMat = t( upperTriMat )

currPlotTbl = plotTbl %>% filter( .id == "1" )
groupNameVec = unique( c( currPlotTbl$x, currPlotTbl$y ) )
nGroup = length( groupNameVec )
hierMat = matrix( 0, ncol = nGroup, nrow = nGroup )


library(stringr)
i = 1
j = 2
for( i in 1:nGroup ){
  print(i)
  for( j in setdiff( 1:nGroup, i ) ){
   print(j)
   iName = groupNameVec[ i ]
   jName = groupNameVec[ j ]
   splitIJ = str_c( iName, "_", jName )
   splitJI = str_c( jName, "_", iName )
   print( c( iName, jName ) )
   if( splitIJ %in% currPlotTbl$split ){
     splitRow = which( currPlotTbl$split == splitIJ )
     hierMat[i,j] = currPlotTbl$origStat[ splitRow ]
     hierMat[j,i] = currPlotTbl$origStat[ splitRow ]
   } else if( splitJI %in% currPlotTbl$split ){
     splitRow = which( currPlotTbl$split == splitJI )
     hierMat[i,j] = currPlotTbl$origStat[ splitRow ]
     hierMat[j,i] = currPlotTbl$origStat[ splitRow ]
   }
  }
}
groupOrderVec = groupNameVec[ hclust( dist( hierMat ) )$order ]

if( revOrder ) groupOrderVec = rev( groupOrderVec )


p = ggplot( plotTbl %>% filter( .id == 1 ), aes( x, y ) ) +
  labs( x = axisLab, y = axisLab ) +
  scale_x_discrete( labels = groupLabVec[-c(1:2)],
                    limits = groupOrderVec[-length(groupOrderVec)] ) +
  scale_y_discrete( labels = groupLabVec,
                    limits = groupOrderVec[-length(groupOrderVec) ] )

p = ggplot( plotTbl, aes( x, y ) ) +
  labs( x = axisLab, y = axisLab ) +
  scale_x_discrete( labels = groupLabVec,
                    limits = setNames( groupLabVec, NULL ) ) +
  scale_y_discrete( labels = groupLabVec,
                    limits = setNames( groupLabVec, NULL ) )

if( rotX ) p = p + theme( axis.text.x = element_text( angle = 90 ) )

# just estimates
if( is.null( fdr ) ){
  p = p +
    geom_tile( aes( fill = origStat ), col = "black" ) +
    scale_fill_gradient2( low = "royalblue1",
                          high = "orange",
                          name = "Decrease\nin Row" )

  if( text ) p = p +
      geom_text( aes( label = as.character( round( origStat, 2 ) ) ), size = textSize )

  # if to be faceted
  if( is.null( substitute( facet ) ) ) return( p )

  if( is.null( facetLabVec ) ) facetLabVec = setNames( data$facet, data$facet )
  p = p + facet_wrap( ~ .id, labeller = labeller( .id = facetLabVec ),
                      ncol = ncol, scale = "free"  )


  return( p )
}

p = p +
  geom_tile( aes( fill = xLarger.Sig ), col = "black" ) +
  scale_fill_manual( values = fillColVec,
                     name = "Statistical\nSignificance",
                     labels = fillLabVec )

if( text ) p = p +
  geom_text( aes( label = as.character( round( mbf, 0 ) ) ), size = textSize )
