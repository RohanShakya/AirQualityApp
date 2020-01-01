library(testthat)
library(assertr)
test_that("Test NA values",
          assertthat::assert_that(not_na(data1$AQI))
          )

#test_that("Test NA values",
          #not_na(data2$PM2.5, allow.NaN = FALSE),
          #is.na(data3)
#          assertthat::assert_that(not_na(data1$AQI))
          #assertthat::assert_that(any(is.na(data1)))
          
#)
#test_that("Test x2=7",
 #         expect_equal(x2,7)
#)