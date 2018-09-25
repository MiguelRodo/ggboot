context( "Testing calcPTbl" )


# Data --------------------------------------------------------------------

# Functions
calcStat = function( x ) mean( x, 0.1 )
calcSE = function(x) { boot::boot( x, R = 20,
                                   function(x,w) calcStat( x[w] ) )$t %>% sd() }
calcBootStatVec = function( x, w ) c( calcStat( x[w] ), calcSE( x[w] ) )

# Data

respVec = c( 0.060426, 0.066152, 0.07093, 0.056687, 0.18716, 0.790952, 0.20803, 0.245396, 0.087918,
             0.02499226, 0.2234444308, 0.0148025445, 0.2425358694, 0.025742255, 0.011875387, 0.0148772707,
             0.0086363158, 0.014349707, 0.0087206469, 0.055159961, 0.0366530097, 0.000260970000000001, 0.0111716045, 0.011851586,
             0.0109080046, 0.002077461, 0.011693718, 0.0182342151, 0.0031248769, 0.0067057876, 0.0172261736,
             0.0491776258, 0.026822441, 0.0062511869, 0.0135163775, 0.003760361, 0.0196274421, 0.004280863, 0.0143105389,
             0.0150681214, 0.0063319547, 0.0087206469, 0.000260970000000001, 0.0111716045, 0.002077461, 0.011693718, 0.0148772707,
             0.055159961, 0.0366530097, 0.011851586, 0.0109080046, 0.0182342151, 0.0172261736, 0.026822441, 0.0135163775 )
splitVec = c( rep( "1", 9), rep( "2", 46 ) )
nullValue = 0.02

# Intermediate calculations
origTbl = tibble( resp = respVec, split = splitVec )
origStatTbl = ldply( split( origTbl, origTbl$split ), function(x){
  c( origStat = calcStat( x$resp ), origSE = calcSE( x$resp ) )
}) %>%
  rename( split = .id )
sd( respVec[splitVec==1])
sd( respVec[splitVec==2])
set.seed(1)
bootStatTbl = ldply( split( respVec, splitVec ), function(x){
  boot::boot( x, R = 2e2, calcBootStatVec )$t } ) %>%
  as_tibble() %>%
  rename( split = .id, bootStat = `1`, bootSE = `2` )

# Prop values
currTbl = bootStatTbl %>% filter( split == 1 )
nullBootStat1 = currTbl$bootStat - origStatTbl$origStat[1] + nullValue # centred at null
nullOrigStat1 = origStatTbl$origStat[1] - nullValue # difference from null
nullBootTStat1 = (currTbl$bootStat - origStatTbl$origStat[1] + nullValue )/currTbl$bootSE
nullOrigTStat1 = ( origStatTbl$origStat[1] - nullValue ) / origStatTbl$origSE[1]
prop1 = sum( nullBootStat1 < nullOrigStat1 ) / nrow( currTbl ) # prop less than
propT1 = sum( nullBootTStat1 < nullOrigTStat1 ) / nrow( currTbl )
propTVecLow1 = c( prop1, propT1 )
propTVecHigh1 = 1 - propTVecLow1
z0P1 = calcBCaZ0( currTbl$bootStat, origStatTbl$origStat[1])
z0PT1 = calcBCaZ0( ( currTbl$bootStat - nullValue ) / currTbl$bootSE,
                  ( origStatTbl$origStat[1] - nullValue ) / origStatTbl$origSE[1] )
hist( ( currTbl$bootStat - nullValue ) / currTbl$bootSE )
hist( currTbl$bootSE )
origStatTbl$origSE
currTbl = bootStatTbl %>% filter( split == 2 )
nullBootStat2 = currTbl$bootStat - origStatTbl$origStat[2] + nullValue # centred at null
nullOrigStat2 = origStatTbl$origStat[2] - nullValue # difference from null
nullBootTStat2 = (currTbl$bootStat - origStatTbl$origStat[2] + nullValue )/currTbl$bootSE
nullOrigTStat2 = ( origStatTbl$origStat[2] - nullValue ) / origStatTbl$origSE[2]
prop2 = sum( nullBootStat2 < nullOrigStat2 ) / nrow( currTbl ) # prop less than
propT2 = sum( nullBootTStat2 < nullOrigTStat2 ) / nrow( currTbl )
propTVecLow2 = c( prop2, propT2 )
propTVecHigh2= 1 - propTVecLow2
z0P2 = calcBCaZ0( currTbl$bootStat, origStatTbl$origStat[2])
z0PT2 = calcBCaZ0( ( currTbl$bootStat - nullValue ) / currTbl$bootSE,
                   ( origStatTbl$origStat[2] - nullValue ) / origStatTbl$origSE[2] )

outTblPercLow = tibble( split = c("1","2"), percP = c( prop1, prop2 ),
                        z0P = c( z0P1, z0P2 ) %>% signif(3) )

outTblPercTHigh = tibble( split = c("1","2"), percP = 1 - c( prop1, prop2 ),
                          percTP = 1 -  c( propT1, propT2 ),
                        z0P = c( z0P1, z0P2 ) %>% signif(3),
                        z0PT = c( z0PT1, z0PT2 ) %>% signif(3) )


# Testing -----------------------------------------------------------------

test_that( 'calcPTbl works',{
  expect_identical( calcPTbl( bootData = bootStatTbl, origData = origStatTbl,
                          method = 'perc', altSide = 'low', nullValue = 0.02 ),
                    outTblPercLow )
  expect_identical( calcPTbl( bootData = bootStatTbl, origData = origStatTbl,
                              method = 'percT', altSide = 'high', nullValue = 0.02 ),
                    outTblPercTHigh )
} )
