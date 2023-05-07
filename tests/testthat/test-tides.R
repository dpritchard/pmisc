test_that("We can read tide data...", {
    expect_silent(td <- read_linz_tides("./test-tides-files/Spit Wharf 2021.csv"))
    expect_named(td, c("time", "level", "date", "rdt"))
})

td <- read_linz_tides("./test-tides-files/Spit Wharf 2021.csv")

test_that("Tide data has no <NA> levels...", {
    expect_false(any(is.na(td$level)))
})

test_that("Tide data is sorted sequentially...", {
    expect_true(all(diff(td$rdt) > 0))
})

test_that("We can interpolate a single tide...", {
    expect_equal(interpolate_tide("2021-01-01 05:42", td), 1.900)
    expect_equal(interpolate_tide("2021-03-01 17:59", td), 2.000)

    expect_equal(interpolate_tide("2021-01-01 08:42", td), 1.163)
    expect_equal(interpolate_tide("2021-03-01 14:59", td), 1.230)
})

test_that("We can't interpolate outside the range supplied...", {
    expect_error(interpolate_tide("2021-01-01 00:01", td),
                 'outside range')
    expect_error(interpolate_tide("2021-12-31 23:59", td),
                 'outside range')
})

test_that("We accept POSIX-x objects and ISO strings...", {
    expect_silent(interpolate_tide("2021-01-01 05:42", td))
    dt <- as.POSIXct("2021-01-01 05:42")
    expect_silent(interpolate_tide(dt, td))
    expect_error(interpolate_tide("01/10/2021 12:33", td),
                 "not in a standard unambiguous format")
})

test_that("We can interpolate mulitple tides", {
    tds <- c("2021-01-01 05:42", "2021-03-01 17:59", "2021-01-01 08:42", "2021-03-01 14:59")
    lvs <- c(1.900, 2.000, 1.163, 1.230)
    expect_equal(interpolate_tides(tds, td), lvs)
})
