test_that("Can read a TXT file", {
    expect_silent(x <- read_licor("./test-odyssey-files/CALJUL22.TXT"))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'caldat')

    # The `meta` object should be a list with (at least) 'units':
    expect_true('units' %in% names(x$meta))
    expect_true(is.character(x$meta$units))

    # The `dat` object should be a data frame with (only)
    #   'Sdt', 'Rdt' and 'light':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('Sdt', 'Rdt', 'light'))
})

test_that("Date or Time format mismatches throw a warning...", {
    expect_warning(x <- read_licor("./test-odyssey-files/CALJUL22.TXT", date_format = "%d/%m/%Y"), regexp = "Datetime conversion produced NAs")
})
