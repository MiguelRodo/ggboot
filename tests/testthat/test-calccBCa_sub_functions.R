context( "Testing sub functions for calcBCa" )

# Data
respVec = c( 0.096888, 0.115579, 0.13671, 0.086497, 0.5248,
             1.676752, 0.46717, 0.360975, 0.118718 )
calcStat = mean
set.seed(3)
bootObj = boot::boot( respVec, function(x,w) calcStat(x[w]), R = 1e4 )

# Tests
test_that( "Testing that calcBCaA works properly",{
  expect_equal( calcBCaA( respVec, calcStat ), 0.1102757 )
} )

test_that( "Testing that calcBCaZ0 works properly",{
  expect_equal( calcBCaZ0( bootObj$t, calcStat( respVec ) ),
                0.09791473 )
})


