test_that("It 'works' in a general sense", {
    expect_silent(caldat <- read_licor("./test-odyssey-files/CALJUL22.TXT"))
    expect_silent(odydat <- read_odyssey(
        "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"))
    expect_silent(cal1 <- make_odycal(caldat = caldat, odydat = odydat))
    expect_output(print(cal1), "Remember to inspect")
    plot(cal1) # How to test this?
    o1 <- predict(cal1, odydat$dat$value)
})



