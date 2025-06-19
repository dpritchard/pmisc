test_that("It can't run unless a offset to UTC is supplied", {
    f <- "./test-odyssey-files/CALJUL22.TXT"
    err_msg <- "UTC offset not supplied and cannot be guessed from file."
    expect_error(read_licor(f), err_msg)
    expect_error(read_licor(f, utc_offset = NA, medium = "water"), err_msg)
    expect_error(read_licor(f, utc_offset = NULL, medium = "water"), err_msg)

    err_msg_2 <- "UTC offset must be a number between \\-12 and \\+14"
    expect_error(read_licor(f, utc_offset = "beep", medium = "water"), err_msg_2)
    expect_error(read_licor(f, utc_offset = 42, medium = "water"), err_msg_2)
})

test_that("It can't run unless a medium (air or water) is defined", {
    f <- "./test-odyssey-files/CALJUL22.TXT"
    err_msg <- "Deployment medium must be one of 'air' or 'water'"
    expect_error(read_licor(f, utc_offset = 12), err_msg)
    expect_error(read_licor(f, utc_offset = 12, medium = NA), err_msg)
    expect_error(read_licor(f, utc_offset = 12, medium = NULL), err_msg)
    expect_error(read_licor(f, utc_offset = 12, medium = 'soil'), err_msg)
})

test_that("Can read a TXT file", {
    expect_silent(x <- read_licor("./test-odyssey-files/CALJUL22.TXT", utc_offset = 12, medium = "water"))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'caldat')

    # The `meta` object should be a list with (at least) 'units' and 'medium':
    expect_true('units' %in% names(x$meta))
    expect_true(is.character(x$meta$units))
    expect_true('medium' %in% names(x$meta))
    expect_true(is.character(x$meta$medium))

    # The `dat` object should be a data frame with (only)
    #   'Sdt', 'Rdt' and 'par':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('sdt', 'rdt', 'par'))
})

test_that("Date or Time format mismatches throw a warning...", {
    expect_warning(
        x <- read_licor("./test-odyssey-files/CALJUL22.TXT",
                        date_format = "%d/%m/%Y", utc_offset = 12,
                        medium = "water"),
        regexp = "Datetime conversion produced NAs")
})
