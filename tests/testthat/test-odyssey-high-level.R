test_that("Odyssey and LiCor Calibration 'works' in a general sense", {
    licor <- "./test-odyssey-files/CALJUL22.TXT"
    expect_silent(caldat <- read_licor(licor, utc_offset = 12, medium = "water"))
    ody_v2 <- "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"
    expect_silent(odydat <- read_odyssey(ody_v2, sensitivity = "high", medium = "water"))
    expect_silent(cal1 <- make_odycal(caldat = caldat, odydat = odydat))
    expect_output(print(cal1), "Remember to inspect")
    plot(cal1) # How to test this?
    o1 <- predict(cal1, odydat$dat$value)
})

test_that("Odyssey and miniPAR Calibration 'works' in a general sense", {
    miniPAR <- "./test-odyssey-files/miniPAR/7530-728632/2024-08-23 031500Z.txt"
    ody_v2 <- "./test-odyssey-files/miniPAR/daniel@tmk.nz-D6F4A78CB1F9-1724620219908.csv"
    expect_silent(caldat <- read_miniPAR(miniPAR, medium = "air"))
    expect_silent(odydat <- read_odyssey(ody_v2, sensitivity = "high", medium = "air"))
    expect_silent(cal1 <- make_odycal(caldat = caldat, odydat = odydat))
    expect_output(print(cal1), "Remember to inspect")
    plot(cal1) # How to test this?
    o1 <- predict(cal1, odydat$dat$value)
})
