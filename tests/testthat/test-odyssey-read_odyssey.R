test_that("Can read a old-style (v1) CSV file", {
    expect_silent(x <- read_odyssey("./test-odyssey-files/15220_001_006.CSV",
                                    date_format = "%d/%m/%Y"))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'odydat')

    # The `meta` object should be a list with (at least) 'scan_rate':
    expect_true('scan_rate' %in% names(x$meta))
    expect_true(is.numeric(x$meta$scan_rate))

    # The `dat` object should be a data frame with (only)
    #   'Sdt', 'Rdt', 'value' and 'temp':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('Sdt', 'Rdt', 'value', 'temp'))
})

test_that("Date or Time format mismatches throw a warning...", {
    expect_warning(x <- read_odyssey("./test-odyssey-files/15220_001_006.CSV",
                                     date_format = "%Y-%m-%d"),
                   regexp = "Datetime conversion produced NAs")
})

test_that("Can read a new-style (v2) CSV file", {
    f <- "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"
    expect_silent(x <- read_odyssey(f))
    # Should produce a list with 2 sections (meta and dat)
    expect_type(x, 'list')
    expect_length(x, 2)
    expect_named(x, c('meta', 'dat'))
    expect_s3_class(x, 'odydat')

    # The `meta` object should be a list with (at least) 'scan_rate':
    expect_true('scan_rate' %in% names(x$meta))
    expect_true(is.numeric(x$meta$scan_rate))

    # The `dat` object should be a data frame with (only)
    #   'Sdt', 'Rdt', 'value' and 'temp':
    expect_s3_class(x$dat, 'data.frame')
    expect_named(x$dat, c('Sdt', 'Rdt', 'value', 'temp'))
})
