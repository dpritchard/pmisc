test_that("It can't run unless a timezone is supplied (for v1 files)", {
    v1 <- "./test-odyssey-files/15220_001_006.CSV"
    err_msg <- "UTC offset not supplied and cannot be guessed from file."
    expect_error(x <- read_odyssey(v1, medium = "water"), err_msg)
    expect_error(x <- read_odyssey(v1, utc_offset = NA, medium = "water"), err_msg)
    expect_error(x <- read_odyssey(v1, utc_offset = NULL, medium = "water"), err_msg)
})

test_that("It can't run unless a medium (air or water) is defined", {
    v1 <- "./test-odyssey-files/15220_001_006.CSV"
    err_msg <- "Deployment medium must be one of 'air' or 'water'"
    expect_error(read_odyssey(v1, utc_offset = 13), err_msg)
    expect_error(read_odyssey(v1, utc_offset = 13, medium = NA), err_msg)
    expect_error(read_odyssey(v1, utc_offset = 13, medium = NULL), err_msg)
    expect_error(read_odyssey(v1, utc_offset = 13, medium = 'soil'), err_msg)
    v2 <- "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"
    expect_error(read_odyssey(v2, sensitivity = "high"), err_msg)
    expect_error(read_odyssey(v2, sensitivity = "high", medium = NA), err_msg)
    expect_error(read_odyssey(v2, sensitivity = "high", medium = NULL), err_msg)
    expect_error(read_odyssey(v2, sensitivity = "high", medium = 'soil'), err_msg)
})

test_that("Can read a old-style (v1) CSV file", {
    expect_silent(x <- read_odyssey("./test-odyssey-files/15220_001_006.CSV",
                                    date_format = "%d/%m/%Y", utc_offset = 13,
                                    medium = "water"))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'odydat')

    # The `meta` object should be a list with (at least) 'uid', 'scan_rate' and 'medium':
    expect_true('scan_rate' %in% names(x$meta))
    expect_s3_class(x$meta$scan_rate, "difftime")
    expect_true('uid' %in% names(x$meta))
    expect_true(is.character(x$meta$uid))
    expect_true('medium' %in% names(x$meta))
    expect_true(is.character(x$meta$medium))
    expect_true(x$meta$medium %in% c("air", "water"))

    # The `dat` object should be a data frame with (only)
    #   'Sdt', 'Rdt', 'value' and 'temp':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('sdt', 'rdt', 'value', 'temp'))
})

test_that("Date or Time format mismatches throw a warning...", {
    expect_warning(x <- read_odyssey("./test-odyssey-files/15220_001_006.CSV",
                                     date_format = "%Y-%m-%d", utc_offset = 13,
                                     medium = "water"),
                   regexp = "Datetime conversion produced NAs")
})

test_that("Can read a new-style (v2) CSV file", {
    f <- "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"
    expect_silent(x <- read_odyssey(f, sensitivity = "high", medium = "water"))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'odydat')

    # The `meta` object should be a list with (at least): 'uid', 'scan_rate', 'sensitivity' and 'medium':
    expect_true('scan_rate' %in% names(x$meta))
    expect_s3_class(x$meta$scan_rate, "difftime")
    expect_true('uid' %in% names(x$meta))
    expect_true(is.character(x$meta$uid))
    expect_true('sensitivity' %in% names(x$meta))
    expect_true(x$meta$sensitivity %in% c('high', 'low'))
    expect_true('medium' %in% names(x$meta))
    expect_true(is.character(x$meta$medium))
    expect_true(x$meta$medium %in% c("air", "water"))

    # The `dat` object should be a data frame with (only)
    #   'Sdt', 'Rdt', 'value' and 'temp':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('sdt', 'rdt', 'value', 'temp'))
})

test_that("It warns about overflows", {
    expect_warning(x <- read_odyssey("./test-odyssey-files/ody_v2_w_overflow.csv",
                                     sensitivity = "high",  medium = "air"), "overflow")
})

test_that("It enforces 'sensitivity' for v2 files on input", {
    v2 <- "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"
    expect_error(x <- read_odyssey(v2, sensitivity = "beep", medium = "water"), "'high' or 'low'")
    expect_error(x <- read_odyssey(v2, medium = "water"), "'high' or 'low'")
    v1 <- "./test-odyssey-files/15220_001_006.CSV"
    expect_silent(read_odyssey(v1, date_format = "%d/%m/%Y", utc_offset = 13,  medium = "water"))

})
