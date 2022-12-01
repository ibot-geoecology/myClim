test_that("mc_calib_moisture", {
    params <- mc_calib_moisture(120, 3650)
    expect_equal(names(params), c("slope", "intercept"))
})
