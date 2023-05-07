read_linz_tides <- function(filename){
    dat <- read.csv(filename, header = F, stringsAsFactors = F, skip = 3)
    names(dat) <- c("day", "dow", "month", "year", rep(c("time", "level"), 4))
    td <- rbind(dat[, c(5,6)], dat[, c(7,8)],
                      dat[, c(9,10)], dat[, c(11,12)])
    date <- paste0(dat[,'year'], "-",
                   sprintf("%02d", dat[,'month']), "-",
                   sprintf("%02d", dat[,'day']))
    td$date <- rep(date, 4)
    names(td) <- c("time", "level", "date")
    td$rdt <- as.POSIXct(strptime(paste(td$date, td$time),
                                        format = "%Y-%m-%d %H:%M"))
    td <- td[!is.na(td$level), ] # Remove NA levels (usually 3 tides per day, not 4)
    td <- td[order(td$rdt), ]
    rownames(td) <- NULL
    return(td)
}

interpolate_tides <- function(datetimes, td){
    vapply(X = datetimes, FUN = interpolate_tide, FUN.VALUE = double(1),
           td = td, USE.NAMES = FALSE)
}

interpolate_tide <- function(datetime, td){
    tf = c("%Y-%m-%d %H:%M:%OS","%Y-%m-%d %H:%M")
    datetime <- as.POSIXct(datetime, tryFormats = tf)

    if(datetime < min(td$rdt) | datetime > max(td$rdt)){
        stop("Cannot interpolate outside range supplied")
    }

    ndx <- which(td$rdt == datetime)

    # Exact match:
    if(length(ndx) != 0){
        return(td$level[ndx])
    }

    # Not an exact match...
    t <- as.numeric(datetime)/60
    difs <- abs(td$rdt-datetime)
    ndx <- order(difs)[1:2]
    td <- td[ndx, ]
    td <- td[order(td$rdt), ]
    t1 <- as.numeric(td$rdt[1])/60
    t2 <- as.numeric(td$rdt[2])/60
    h1 <- td$level[1]
    h2 <- td$level[2]

    td <- (t - t1)/(t2 - t1)
    A <- pi*(td+1)
    hd <- h2 - h1
    h <- h1 + hd*((cos(A) + 1)/2)
    return(round(h,3))
}
