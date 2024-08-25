test_that("We can read wavescan data from the Jenway 7315 spectrophotometer...", {
    expect_silent(td <- read_spectro("./test-pigments-files/test_spectro.xml"))
    expect_named(td, c("Name", "WL", "Time", "Tran", "Abs", "Conc"))
    expect_type(td$WL, "double")
    expect_type(td$Tran, "double")
    expect_type(td$Abs, "double")
    expect_type(td$Conc, "double")
})
