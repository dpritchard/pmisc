expect_silent(caldat <- read_licor("./test-odyssey-files/CALJUL22.TXT", utc_offset = 12, medium = "water"))
v2_file <- "./test-odyssey-files/daniel.pritchard@otago.ac.nz-D9D3F8936A0A-1657758591368.csv"
expect_silent(odydat <- read_odyssey(v2_file, sensitivity = "high", medium = "water"))
expect_silent(cal <- make_odycal(caldat = caldat, odydat = odydat))
expect_silent(calni <- make_odycal(caldat = caldat, odydat = odydat, intercept = FALSE))

test_that("It returns a valid structure", {
    # model, dat, units, type (linear)?
    # calibration formula? String - for printing?
    # Should produce a list with 3 sections (meta, dat, model)
    expect_type(cal, 'list')
    expect_length(cal, 3)
    expect_named(cal, c('meta', 'dat', 'model'))
    expect_s3_class(cal, 'odycal')

    # The `meta` object should be a list with (at least) 'units' and 'type'.
    # It should also maintain the serial number (uid) of the calibrated sensor.
    expect_true('units' %in% names(cal$meta))
    expect_type(cal$meta$units, 'character')
    expect_true('type' %in% names(cal$meta))
    expect_type(cal$meta$type, 'character')
    expect_true('uid_cal' %in% names(cal$meta))
    expect_type(cal$meta$uid_cal, 'character')
    # For this test data, it should be the uid of the Odyssey sensor
    expect_equal(cal$meta$uid_cal, "D9D3F8936A0A")

    # The `dat` object should be a data frame with (only)
    #   'par' and 'value':
    expect_s3_class(cal$dat, 'data.frame')
    expect_named(cal$dat, c('rdt', 'value', 'par'))
})

test_that("We can define the two types of model", {
    expect_equal(cal$meta$type, 'linear')
    expect_equal(calni$meta$type, 'linear-no-intercept')
})

test_that("predict.odycal recieves an `odydat` or a vector", {
    o1 <- predict(cal, odydat)
    o1a <- predict(cal, odydat$dat$value)
    expect_equal(o1, o1a)

    o2 <- predict(calni, odydat)
    o2a <- predict(calni, odydat$dat$value)
    expect_equal(o2, o2a)

    expect_error(predict(cal, cal), "must be a numeric vector or an `odydat`")
})

test_that("predict.odycal returns sensible output", {
    o1 <- predict(cal, odydat)
    expect_equal(o1[42], 0.09990949)
    expect_equal(o1[19], 182.665252603)

    o2 <- predict(calni, odydat)
    expect_equal(o2[42], 0.98673461)
    expect_equal(o2[19], 182.206713591)
})

# test_that("We can easily idenity outliers", {
#     out <- outliers(cal)
# })
