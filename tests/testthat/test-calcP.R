context ( "testing calcP and calcPSub" )

# Data --------------------------------------------------------------------

# Functions
calcStat = function( x ) mean( x, trim = 0.02 )
calcSE = function(x) { boot::boot( x, R = 20,
                                   function(x,w) calcStat( x[w] ) )$t %>% sd() }
calcBootStat = function( x ) mean( x, trim = 0.02 )
calcBootSE = function(x) { boot::boot( x, R = 20,
                                   function(x,w) calcStat( x[w] ) )$t %>% sd() }
calcBootStatVec = function( x, w ) c( calcBootStat( x[w] ), calcBootSE( x[w] ) )

# Data
respVec = c( 0.498767, 0.154696, 0.317299, 0.25632, 0.428,
             1.48165, 1.601475, 0.385599, 0.217408 )
nullValue = 0.02

# Intermediate calculations
origStat = calcStat( respVec )
origSE = calcSE( respVec )
set.seed(1)
bootStatTbl = boot::boot( respVec, R = 2e2, calcBootStatVec )$t %>%
  as_tibble() %>%
  rename( bootStat = V1, bootSE = V2 )

# Percentile
nullBootStat = bootStatTbl$bootStat - origStat + nullValue # centred at null
nullOrigStat = origStat - nullValue # difference from null
nullBootTStat = (bootStatTbl$bootStat - origStat + nullValue )/bootStatTbl$bootSE
nullOrigTStat = ( origStat - nullValue ) / origSE
prop = sum( nullBootStat < nullOrigStat ) / nrow( bootStatTbl ) # prop less than
propT = sum( nullBootTStat < nullOrigTStat ) / nrow( bootStatTbl )
propHigh = 1 - prop
propBoth = 2 * min( prop, propHigh )
propTVecLow = c( prop, propT )
propTVecHigh = 1 - propTVecLow
diffNullProp =  sum( ( bootStatTbl$bootStat - origStat + 0.3 ) < (origStat - 0.3 ) ) / nrow( bootStatTbl )
if( FALSE ){
  par(mfrow=c(2,2))
  hist(respVec)
  hist(bootStatTbl$bootStat)
  hist( nullBootStat )
  par(mfrow=c(2,2))
  hist( ( bootStatTbl$bootStat - nullValue ) / bootStatTbl$bootSE )
  hist( nullBootTStat )
  plot( bootStatTbl$bootStat,  ( bootStatTbl$bootStat - nullValue ) / bootStatTbl$bootSE )
  plot( bootStatTbl$bootStat,  ( bootStatTbl$bootStat ) / bootStatTbl$bootSE )
  plot( bootStatTbl$bootStat, bootStatTbl$bootSE )
}


# Tests -------------------------------------------------------------------

test_that( "calcP works", {
  # perc
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.02,
         altSide = 'high', method = 'perc' ), propHigh ) #altSide high
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.02,
         altSide = 'low', method = 'perc' ), prop ) # altSide low
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.02,
         altSide = 'both', method = 'perc' ), propBoth ) # altSide both
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.02,
         altSide = NULL, method = 'perc' ), propBoth ) # altSide NULL
  # percT
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.02,
         altSide = 'low', method = 'percT', origSE = origSE ),
         propTVecLow ) #altSide high
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.02,
                       altSide = 'high', method = 'percT', origSE = origSE ),
                 propTVecHigh ) #altSide high
  # null value 0
  expect_equal( calcP( bootData = bootStatTbl, origStat = origStat, nullValue = 0.3,
         altSide = "low", method = 'perc' ),
         diffNullProp )
})

test_that( "calcPSub works", {
  expect_equal( calcPSub( 0.01, "high" ), 0.99 )
  expect_equal( calcPSub( 0.01, "low" ), 0.01 )
  expect_equal( calcPSub( 0.01, "both" ), 0.02 )

})
