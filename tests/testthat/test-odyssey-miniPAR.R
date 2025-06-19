test_that("It can't run unless a medium (air or water) is defined", {
    f <- "./test-odyssey-files/miniPAR/7530-728632/2024-08-22 232500Z.txt"
    err_msg <- "Deployment medium must be one of 'air' or 'water'"
    expect_error(read_miniPAR(f), err_msg)
    expect_error(read_miniPAR(f, medium = NA), err_msg)
    expect_error(read_miniPAR(f, medium = NULL), err_msg)
    expect_error(read_miniPAR(f, medium = 'soil'), err_msg)
})

test_that("It can read a (single) TXT file", {
    expect_silent(x <- read_miniPAR("./test-odyssey-files/miniPAR/7530-728632/2024-08-22 232500Z.txt", medium = "air"))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'par_dat')

    # The `meta` object should be a list with (at least) 'units', 'uid' and 'medium':
    expect_true('units' %in% names(x$meta))
    expect_true(is.character(x$meta$units))
    expect_true('uid' %in% names(x$meta))
    expect_true(is.character(x$meta$uid))
    expect_true('medium' %in% names(x$meta))
    expect_true(is.character(x$meta$medium))
    expect_true(x$meta$medium %in% c("air", "water"))

    # The `dat` object should be a data frame with
    #   'sdt', 'sdt', 'par', 'temperature' and 'angle':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('sdt', 'rdt', 'par', 'temperature', 'angle'))
})

test_that("It can read a multiple TXT files in a folder", {
    expect_message(x <- read_miniPAR("./test-odyssey-files/miniPAR/7530-728632", medium = "air"), ".+ 5 files\\.")
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'par_dat')

    # The `meta` object should be a list with (at least) 'uid' and 'units':
    # (possibly the 'readings_averaged' should compulsory too, currently it is not)
    expect_true('units' %in% names(x$meta))
    expect_true(is.character(x$meta$units))
    expect_true('uid' %in% names(x$meta))
    expect_true(is.character(x$meta$uid))

    # The `dat` object should be a data frame with
    #   'sdt', 'sdt', 'par', 'temperature' and 'angle':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('sdt', 'rdt', 'par', 'temperature', 'angle'))
})
