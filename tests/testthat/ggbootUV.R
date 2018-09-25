data(mtcars)
library(ggplot2)
library(magrittr)
library(tibble)
library(testthat)

p = ggplot( mtcars , aes( cyl, mpg ) ) +
  geom_point()
test_that( "ggplotUV works" , {
  expect_equal_to_reference(c("a"), file = "basicTest.rds" )
})
