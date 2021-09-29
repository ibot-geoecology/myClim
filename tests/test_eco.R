library(testthat)
library(microclim)


test_that("mc_eco_snow", {
    data <- microclim::mc_feed_TMS_directory("data/eco-snow")
    snow <- microclim::mc_eco_snow(data, "T2", dr=2, tmax=2)
    expect_equal(ncol(snow), 3)
    expect_equal(nrow(snow), 7)
    expect_equal(snow[["94184102"]], c(NA, F, F, T, T, T, F))
})
