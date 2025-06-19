read_licor <- function(file, cal_col = "INPUT1",
                       date_format = "%Y-%m-%d",
                       time_format = "%H:%M:%S",
                       utc_offset = NA, medium = NA) {
    if(is.na(utc_offset) || is.null(utc_offset)){
        stop("UTC offset not supplied and cannot be guessed from file.")
    }

    if(!is.numeric(utc_offset) || utc_offset > 14 || utc_offset < -12){
        stop("UTC offset must be a number between -12 and +14")
    }

    med_err <- "Deployment medium must be one of 'air' or 'water'"
    if(!is.character(medium) || length(medium) != 1){
        stop(med_err)
    }
    med_opts <- c("air", "water")
    med_ndx <- pmatch(medium, med_opts, nomatch = NA_character_)
    if(is.na(med_ndx)) stop(med_err)
    medium <- med_opts[med_ndx]

    dat <- read.table(file, skip = 6, header = TRUE, strip.white = TRUE)

    dat <- dat[, c("Date", "Time", cal_col)]
    names(dat) <- c("date", "time", "par")

    dat$sdt <- paste(dat$date, dat$time)
    # See links starting in ?strptime for discussion about time zones
    # Here we **assume** UTC and add a manual offset, which is a bit different.
    # Also implemented for the LiCor (which sufferers from the same problem)
    dat$rdt <- as.POSIXct(strptime(dat$sdt, format = paste(date_format, time_format),
                                   tz = "UTC"))
    dat$rdt <- dat$rdt-(utc_offset*60*60)

    if (any(is.na(dat$rdt))) warning("Datetime conversion produced NAs")

    imeta <- read.table(file,
        skip = 1,
        nrows = 5, sep = "\t"
    )
    ometa <- list()
    for (a in 1:nrow(imeta)) {
        nme <- stringr::str_replace_all(imeta[a, 1], ":", "")
        nme <- stringr::str_trim(nme)
        nme <- stringr::str_replace_all(nme, "\\s+", "_")
        nme <- stringr::str_to_lower(nme)
        val <- stringr::str_trim(imeta[a, 2])
        if (nme != "") {
            ometa[[nme]] <- val
        }
    }
    ometa$medium <- medium
    out <- list(meta = ometa, dat = dat[, c("sdt", "rdt", "par")])
    out$meta$units <- "µmol photons / m^2 / s"
    class(out) <- c("caldat", class(out))
    return(out)
}

read_miniPAR <- function(file_or_folder, medium = NA){
    med_err <- "Deployment medium must be one of 'air' or 'water'"
    if(!is.character(medium) || length(medium) != 1){
        stop(med_err)
    }
    med_opts <- c("air", "water")
    med_ndx <- pmatch(medium, med_opts, nomatch = NA_character_)
    if(is.na(med_ndx)) stop(med_err)
    medium <- med_opts[med_ndx]

    if(dir.exists(file_or_folder)){
        # It is a folder
        folder <- normalizePath(file_or_folder)
        tfiles <- Sys.glob(paste0(folder, "/*.txt"))
        tfiles <- tfiles[!stringr::str_detect(tfiles, "[cC][aA][tT].txt$")]
        n <- length(tfiles)
        message("Reading ", n, " file", if(n != 1) "s", ".")
        o1 <- list()
        for(a in seq_along(tfiles)){
            o1[[a]] <- rminipar(tfiles[a], medium)
        }
        # Check that relevant metadata is all the same. Error if not.
        sn <- vapply(o1, function(x) x$meta$uid, character(1L))
        un <- vapply(o1, function(x) x$meta$units, character(1L))
        tests <- c(uid =  all(sn[1] == sn),
                   units = all(un[1] == un))
        #TODO: More test - test all metadata? Especially consider 'readings_averaged'
        if(!all(tests)){
            stop("Key metadata (`uid` or `units`) varies between files.\nCheck all text files are from the same sensor, with the same settings.")
        }
        dat <- do.call("rbind", lapply(o1, function(x) x$dat))
        dat <- dat[order(dat$rdt), ]
        rownames(dat) <- NULL
        o2 <- list(meta = o1[[1]]$meta, dat = dat)
        class(o2) <- class(o1[[1]])
        return(o2)
    } else {
        # Assume it is a file
        rminipar(file_or_folder, medium)
    }
}

rminipar <- function(file, medium) {
    # This is what we have to play with...
    # Note that the "PAR" column has no comma:
    #
    # 7530-728632
    # OS REV: 1.08 Sensor Cal: 1720594800 Averages: 30
    # Time (sec),  Bat (Volts),   T (deg C),  PAR (umol/(s m^2))  Ax (mg), Ay (mg), Az (mg)
    # 1724369760, +3.43, +21.269,    +0.6,   -64.0,  +272.0,  -920.0
    # 1724369820, +3.44, +21.177,    +0.4,   -58.0,  +280.0,  -964.0

    hdr <- readLines(file, n = 3)

    col_names <- stringr::str_split(hdr[3], "\\)+,?\\s+")[[1]]
    col_names <- stringr::str_split(col_names, "\\s", n = 2)
    col_units <- vapply(col_names, function(x){x[2]}, character(1L))
    col_names <- vapply(col_names, function(x){x[1]}, character(1L))

    col_names <- stringr::str_to_lower(col_names)
    col_units <- stringr::str_replace(col_units, "^\\(", "")
    col_units <- stringr::str_replace(col_units, "\\)$", "")
    indx <- stringr::str_detect(col_units, "umol/")
    col_units[indx] <- "µmol photons / m^2 / s"

    dat <- read.csv(file, header = FALSE, skip = 3, col.names = col_names)
    dat$rdt <- as.POSIXct(dat$time, tz = "UTC")
    if (any(is.na(dat$rdt))) warning("Datetime conversion produced NAs")
    dat$sdt <- format(dat$rdt)
    dat$angle <- 90+atan2(-dat[,'ax'], sqrt(dat[,'ay']*dat[,'ay']+dat[,'az']*dat[,'az']))*(180/pi)

    osv <- stringr::str_extract(hdr[2], "OS REV: (\\d+\\.\\d+)", group = 1)
    scal <-  stringr::str_extract(hdr[2], "Sensor Cal: (\\d+)", group = 1)
    av <- stringr::str_extract(hdr[2], "Averages: (\\d+)", group = 1)
    ometa <- list(uid = hdr[1],
                  os_rev = as.numeric(osv),
                  sensor_cal_date = as.POSIXct(as.numeric(scal), tz="UTC"),
                  readings_averaged = as.numeric(av),
                  units = col_units,
                  tz = "UTC",
                  medium = medium)
    names(dat)[names(dat) == "t"] <- "temperature"
    out <- list(meta = ometa, dat = dat[, c("sdt", "rdt", "par", "temperature", "angle")])
    out$meta$units <- "µmol photons / m^2 / s^1"
    class(out) <- c("par_dat", class(out))
    return(out)
}

read_odyssey <- function(file, version = NA, scan_rate = NA,
                         date_format = "%Y-%m-%d",
                         time_format = "%H:%M:%S",
                         utc_offset = NA,
                         sensitivity = NA,
                         medium = NA) {
    med_err <- "Deployment medium must be one of 'air' or 'water'"
    if(!is.character(medium) || length(medium) != 1){
        stop(med_err)
    }
    med_opts <- c("air", "water")
    med_ndx <- pmatch(medium, med_opts, nomatch = NA_character_)
    if(is.na(med_ndx)) stop(med_err)
    medium <- med_opts[med_ndx]

    # Need to distinguish between old and new file formats...
    tester <- read.csv(file, nrows = 4, header = FALSE)
    # TODO: Farm this out to a function, test and enhance it...
    if (is.na(version)) {
        is_old <- all(is.na(suppressWarnings(as.numeric(tester[2:4, 1]))))
        if(is_old) {
            version = 1
        } else {
            version = 2
        }
    }
    # TODO: Add version to the metadata...
    if (version == 1) {
        out <- rody_v1(file = file, date_format = date_format,
                       time_format = time_format, utc_offset = utc_offset)
    } else if (version == 2){
        out <- rody_v2(file, sensitivity = sensitivity)
    } else {
        stop("Version supplied is not supported.")
    }

    if (is.na(scan_rate)) {
        # Guess the scan rate...
        minz <- diff(out$dat$rdt)
        units(minz) <- "mins"
        out$meta$scan_rate <- getmode(minz)
    } else {
        if (!is.numeric(scan_rate)) stop("`scan_rate` must be a number.")
        out$meta$scan_rate <- scan_rate
    }

    out$meta$medium <- medium

    class(out) <- c("odydat", class(out))
    return(out)
}

rody_v1 <- function(file, date_format, time_format, utc_offset) {
    if(is.na(utc_offset) || is.null(utc_offset)){
        stop("UTC offset not supplied and cannot be guessed from file.")
    }
    # TODO: Need to check that utc_offset is valid (a number between -13 and +13?)
    dat <- read.csv(file,
        skip = 9,
        header = FALSE, strip.white = TRUE
    )
    dat <- dat[, 2:4]

    names(dat) <- c("date", "time", "value")
    dat$sdt <- paste(dat$date, dat$time)
    # See links starting in ?strptime for discussion about time zones
    # Here we **assume** UTC and add a manual offset, which is a bit different.
    # Also implemented for the LiCor (which sufferers from the same problem)
    dat$rdt <- as.POSIXct(strptime(dat$sdt, format = paste(date_format, time_format),
                                   tz = "UTC"))
    dat$rdt <- dat$rdt-(utc_offset*60*60)
    if (any(is.na(dat$rdt))) warning("Datetime conversion produced NAs")
    # These old loggers don't record temperature, so:
    dat$temp <- NA

    imeta <- read.csv(file, nrows = 4, header = FALSE)
    ometa <- list()
    for (a in 1:nrow(imeta)) {
        nme <- stringr::str_replace_all(imeta[a, 1], ":", "")
        nme <- stringr::str_trim(nme)
        nme <- stringr::str_replace_all(nme, "\\s+", "_")
        nme <- stringr::str_to_lower(nme)
        val <- stringr::str_trim(imeta[a, 2])
        if (nme != "") {
            ometa[[nme]] <- val
        }
    }
    names(ometa)[names(ometa) == "logger_serial_number"] <- "uid"

    list(meta = ometa, dat = dat[, c("sdt", "rdt", "value", "temp")])
}

rody_v2 <- function(file, sensitivity) {
    sen_opts <- c("high", "low")
    sen_ndx <- pmatch(sensitivity, sen_opts, nomatch = NA_character_)

    if(is.na(sen_ndx)) stop("Sensitivity must be one of 'high' or 'low'")
    sensitivity <- sen_opts[sen_ndx]
    dat <- read.csv(file)
    odat <- dat[, c('data2', 'data1', 'logDateTime', 'dateTime')]
    names(odat) <- c("temp", "value", "sse", "sdt")
    odat$rdt <- as.POSIXct(odat$sse, tz = "UTC")
    if (any(is.na(odat$rdt))) warning("Datetime conversion produced NAs")
    if (any(odat$value >= 65535)) warning("Some values have reached saturation / overflow (65535).")
    ometa <- list(uid = unique(dat[, 'loggerUid']),
                  sensitivity = sensitivity)

    list(meta = ometa, dat = odat[, c("sdt", "rdt", "value", "temp")])
}

make_odycal <- function(caldat, odydat, intercept = TRUE) {
    # TODO: Check inputs (class based)
    dat <- odydat$dat[, c("rdt", "value")]
    out_len <- nrow(dat)
    dat$par <- double(out_len)

    for (a in seq_len(out_len)) {
        dt <- dat$rdt[a]
        # Assuming (correctly) that we take calibration value from
        # prior to the recorded time.
        indx <- caldat$dat$rdt >= dt - odydat$meta$scan_rate & caldat$dat$rdt <= dt
        val <- mean(caldat$dat[, "par"][indx])
        dat$par[a] <- ifelse(is.na(val), NA, val)
    }

    ## TODO: Should this always be linear?
    ## Including a no-intercept model, at least...

    if (intercept) {
        m1 <- lm(value ~ par, data = dat)
        ty <- "linear"
        fm <- paste0("par = (value - ", round(coef(m1)[1], 2), ") / ", round(coef(m1)[2], 2))
    } else {
        m1 <- lm(value ~ 0 + par, data = dat)
        ty <- "linear-no-intercept"
        fm <- paste0("par = value / ", round(coef(m1)[1], 2))
    }

    meta <- list(
        uid_cal = odydat$meta$uid,
        units = caldat$meta$units,
        type = ty,
        formula = fm
    )

    ret <- list(meta = meta, dat = dat, model = m1)
    class(ret) <- c("odycal", class(ret))
    return(ret)
}

plot.odycal <- function(x, ...) {
    args <- list(
        main = paste("Calibration for", x$meta$uid_cal),
        xlab = paste0("PAR (", x$meta$units, ")"),
        ylab = paste("Raw Odyssey Values")
    )
    args <- modifyList(args, list(...))
    dat <- na.omit(x$dat)
    do.call(plot, c(list(x = dat$par, y = dat$value), args))
    abline(x$model, lty = 2, col = 4, lwd = 2)
}

# trim, filter... other methods
# print... odydat, caldat,
print.odycal <- function(x, ...) {
    ty <- paste(toupper(substring(x$meta$type, 1, 1)),
        substring(x$meta$type, 2),
        sep = "", collapse = " "
    )
    cat(ty, "`odycal` with", nrow(na.omit(x$dat)), "data points.\n")
    cat("Model R-squared : ", round(summary(x$model)$adj.r.squared, 3), "\n")
    cat("Model formula   : ", x$meta$formula, "\n")
    cat("Remember to inspect the model directly.\n")
}

predict.odycal <- function(object, newdata, ...) {
    if (inherits(newdata, "odydat")) newdata <- newdata$dat$value
    if (!is.numeric(newdata)) stop("New data must be a numeric vector or an `odydat` object.")
    if (!object$meta$type %in% c("linear", "linear-no-intercept")) {
        stop("Model type not supported!")
    }

    m1 <- object$model
    y <- newdata

    if (object$meta$type == "linear") {
        # Normal linear model:
        # mx + c =  y
        # mx     = (y - c)
        # x      = (y - c)/m
        (y - coef(m1)[1]) / coef(m1)[2]
    } else {
        # No intercept model
        # mx =  y
        # x  =  y/m
        y / coef(m1)[1]
    }
}
