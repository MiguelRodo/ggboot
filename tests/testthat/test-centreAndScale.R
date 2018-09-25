context( "Testing centreAndScale" )

testDFReg = data.frame( x = 1:10 )
testDFZeroSD = data.frame( x = rep(1,10))

corrDFRegCentre = tibble( x = 1:10 - 5.5)
corrDFRegScale = tibble( x = 1:10 / sd( 1:10 ) )
corrDFRegCentreScale = tibble( x = (1:10 - 5.5) / sd( 1:10 ) )
corrDFZeroSDCentre = tibble( x = rep(0,10) )
corrDFZeroSDScale = tibble( x = rep(1,10) )

testMatReg = matrix( 1:10 )
corrMatRegCentre = matrix( 1:10 - 5.5)
corrMatRegScale = matrix( 1:10 / sd( 1:10 ) )
corrMatRegCentreScale = matrix( (1:10 - 5.5) / sd( 1:10 ) )

test_that( "Testing correct output", {
  expect_identical( centreAndScale( testDFReg, TRUE, FALSE ), corrDFRegCentre )
  expect_equal( centreAndScale( testDFReg, FALSE, TRUE ), corrDFRegScale )
  expect_equal( centreAndScale( testDFReg, TRUE, TRUE ), corrDFRegCentreScale, tolerance = 1 )
} )

test_that( "Testing correct output with a zero-sd column", {
  expect_identical( centreAndScale( testDFZeroSD, TRUE, FALSE ), corrDFZeroSDCentre )
  expect_identical( suppressMessages( centreAndScale( testDFZeroSD, FALSE, TRUE ) ), corrDFZeroSDScale )
  expect_identical( suppressMessages( centreAndScale( testDFZeroSD, TRUE, TRUE ) ), corrDFZeroSDCentre )
  expect_message( centreAndScale( testDFZeroSD, TRUE, TRUE ), "A column had an sd of zero. Its mean-subtracted value was: 0." )
  expect_silent( centreAndScale( testDFZeroSD, TRUE, TRUE, FALSE ) )
} )

test_that( "Testing correct output", {
  expect_equal( centreAndScale( testMatReg, TRUE, FALSE ), corrMatRegCentre )
  expect_equal( centreAndScale( testMatReg, FALSE, TRUE ), corrMatRegScale )
  expect_equal( centreAndScale( testMatReg, TRUE, TRUE ), corrMatRegCentreScale )
} )
