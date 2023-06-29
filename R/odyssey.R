# TODO: Include file name in meta.
# TODO: Carry over sensor serial numbers in Cal object.
# TODO: Warn (with override) if serial numbers don't match.

read_licor <- function(file, cal_col = "INPUT1",
                       date_format = "%Y-%m-%d",
                       time_format = "%H:%M:%S"){

    dat <- read.table(file, skip = 6,
                      header = TRUE, strip.white = TRUE)

    dat <- dat[,c("Date", "Time", cal_col)]
    names(dat) <- c("date", "time", "light")

    dat$Sdt <- paste(dat$date, dat$time)
    dat$Rdt <- as.POSIXct(strptime(dat$Sdt, format = paste(date_format, time_format)))
    if(any(is.na(dat$Rdt))) warning("Datetime conversion produced NAs")

    imeta <- read.table(file, skip = 1,
                        nrows = 5, sep = "\t")
    ometa <- list()
    for (a in 1:nrow(imeta)) {
        nme <- stringr::str_replace_all(imeta[a,1], ":", "")
        nme <- stringr::str_trim(nme)
        nme <- stringr::str_replace_all(nme, "\\s+", "_")
        nme <- stringr::str_to_lower(nme)
        val <- stringr::str_trim(imeta[a,2])
        if(nme != ""){
            ometa[[nme]] <- val
        }
    }
    out <- list(meta = ometa, dat = dat[, c('Sdt', 'Rdt', 'light')])
    out$meta$units <- "µmol photons / m^2 / s^1"
    class(out) <- c("caldat", class(out))
    return(out)
}

read_odyssey <- function(file,
                         date_format = "%Y-%m-%d",
                         time_format = "%H:%M:%S",
                         scan_rate = NULL, is_old = NA){
    ## Need to distinguish between old and new file formats...
    tester <- read.csv(file, nrows = 4, header = FALSE)

    # TODO: Farm this out to a function, test and enhance it...
    if(is.na(is_old)){
        is_old <- all(is.na(suppressWarnings(as.numeric(tester[2:4, 1]))))
    }
    #TODO: Add version to the metadata...
    if(is_old) {
        out <- rody_v1(file, date_format, time_format)
    } else {
        out <- rody_v2(file)
    }

    if(is.null(scan_rate)) {
        # Guess the scan rate...
        minz <- diff(out$dat$Rdt)
        units(minz) <- "mins"
        out$meta$scan_rate <- getmode(minz)
    } else {
        if(! is.numeric(scan_rate)) stop("`scan_rate` must be a number.")
        out$meta$scan_rate <- scan_rate
    }

    class(out) <- c("odydat", class(out))
    return(out)
}

rody_v1 <- function(file, date_format, time_format){
    dat <- read.csv(file, skip = 9,
                    header = FALSE, strip.white = TRUE)
    dat <- dat[,2:4]

    names(dat) <- c("date", "time", "value")
    dat$Sdt <- paste(dat$date, dat$time)
    dat$Rdt <- as.POSIXct(strptime(dat$Sdt, format = paste(date_format, time_format)))
    if(any(is.na(dat$Rdt))) warning("Datetime conversion produced NAs")
    # These old loggers don't record temperature, so:
    dat$temp <- NA

    imeta <- read.csv(file, nrows = 4, header = FALSE)
    ometa <- list()
    for (a in 1:nrow(imeta)) {
        nme <- stringr::str_replace_all(imeta[a,1], ":", "")
        nme <- stringr::str_trim(nme)
        nme <- stringr::str_replace_all(nme, "\\s+", "_")
        nme <- stringr::str_to_lower(nme)
        val <- stringr::str_trim(imeta[a,2])
        if(nme != ""){
            ometa[[nme]] <- val
        }
    }

    list(meta = ometa, dat = dat[, c('Sdt', 'Rdt', 'value', 'temp')])
}

rody_v2 <- function(file){
    dat <- read.csv(file)

    odat <- dat[, c(1,2,4,5)]
    loggeruid <- unique(dat[, 3])

    names(odat) <- c("temp", "value", "sse", "Sdt")
    odat$Rdt <- as.POSIXct(odat$sse, origin = "1970-01-01")
    if(any(is.na(odat$Rdt))) warning("Datetime conversion produced NAs")

    ometa <- list(logger_uid = loggeruid)

    list(meta = ometa, dat = odat[, c('Sdt', 'Rdt', 'value', 'temp')])
}

make_odycal <- function(caldat, odydat, intercept = TRUE){
    # TODO: Check inputs (class based)

    dat <- odydat$dat[, c('Rdt', 'value')]
    out_len <- nrow(dat)
    dat$light <- double(out_len)

    for(a in seq_len(out_len)){
        dt <- dat$Rdt[a]
        # Assuming (correctly) that we take calibration value from
        # prior to the recorded time.
        indx <- caldat$dat$Rdt >= dt-odydat$meta$scan_rate*60 & caldat$dat$Rdt <= dt
        val <- mean(caldat$dat[,'light'][indx])
        dat$light[a] <- ifelse(is.na(val), NA, val)
    }

    ## TODO: Should this always be linear?
    ## Including a no-intercept model, at least...

    if(intercept){
        m1 <- lm(value ~ light, data = dat)
        ty <- "linear"
        fm <- paste0("light = (value - ", round(coef(m1)[1],2), ") / ", round(coef(m1)[2],2))
    } else {
        m1 <- lm(value ~ 0 + light, data = dat)
        ty <- "linear-no-intercept"
        fm <- paste0("light = value / ", round(coef(m1)[1],2))
    }

    meta = list(
        units = caldat$meta$units,
        type = ty,
        formula = fm
    )

    ret <- list(meta = meta, dat = dat, model = m1)
    class(ret) <- c("odycal", class(ret))
    return(ret)
}

plot.odycal <- function(x, ...) {
    with(na.omit(x$dat),
         plot(light, value, ...))
    abline(x$model, lty = 2, col = 4, lwd = 2)
}

#trim, filter... other methods
#print... odydat, caldat,
print.odycal <- function(x, ...) {
    ty <- paste(toupper(substring(x$meta$type, 1, 1)),
                substring(x$meta$type, 2), sep = "", collapse = " ")
    cat(ty, "`odycal` with", nrow(na.omit(x$dat)), "data points.\n")
    cat("Model R-squared : ", round(summary(x$model)$adj.r.squared, 3), "\n")
    cat("Model formula   : ", x$meta$formula, "\n")
    cat("Remember to inspect the model directly.\n")
}

predict.odycal <- function(object, newdata, ...){
    if(inherits(newdata, 'odydat')) newdata <- newdata$dat$value
    if(!is.numeric(newdata)) stop("New data must be a numeric vector or an `odydat` object.")
    if(!object$meta$type %in% c('linear', 'linear-no-intercept')) {
        stop("Model type not supported!")
    }

    m1 <- object$model
    y <- newdata

    if(object$meta$type == "linear"){
        # Normal linear model:
        # mx + c =  y
        # mx     = (y - c)
        # x      = (y - c)/m
        (y-coef(m1)[1])/coef(m1)[2]
    } else {
        # No intercept model
        # mx =  y
        # x  =  y/m
        y/coef(m1)[1]
    }

}


