#' Read LINZ-formatted tide information
#'
#' @param filename
#'
#' @return A dataframe representation of the LINZ tide data.
#'
#' @examples
#' # NB: Usually, it is best to download these files locally, rather than
#' # rely on the URL structure on the LINZ site remaining stable.
#' td <- read_linz_tides("https://static.charts.linz.govt.nz/tide-tables/maj-ports/csv/Spit%20Wharf%202021.csv")
#'
#' @export
#'
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

#' Interpolate tide levels from dates
#'
#' @param datetimes A vector of [`POSIXt`] datetime objects, or sensible character representations of these.
#' @param td Tide data. As returned by [`read_linz_tides`]
#'
#' @return A numeric vector equal in length to `datetimes`. Tide level in meters, usually.
#'
#' @examples
#' td <- read_linz_tides("https://static.charts.linz.govt.nz/tide-tables/maj-ports/csv/Spit%20Wharf%202021.csv")
#' dt <- c("2021-01-01 05:42", "2021-03-01 17:59", "2021-01-01 05:30", "2021-03-01 19:00")
#' interpolate_tides(datetimes = dt, td = td)
#' # 1.900 2.000 1.896 1.877
#'
#' @export
#'
interpolate_tides <- function(datetimes, td){
    vapply(X = datetimes, FUN = interpolate_tide, FUN.VALUE = double(1),
           td = td, USE.NAMES = FALSE)
}

#' Interpolate a tide level from a date
#'
#' @inheritParams interpolate_tides
#' @param datetime A [`POSIXt`] datetime object, or a sensible character representation of this.
#'
#' @return A numeric vector of length one. Tide level in meters, usually.
#'
#' @examples
#' td <- read_linz_tides("https://static.charts.linz.govt.nz/tide-tables/maj-ports/csv/Spit%20Wharf%202021.csv")
#' interpolate_tides("2021-01-01 05:42", td = td) # 1.90 (an exact match)
#' interpolate_tides("2021-01-01 05:30", td = td) # 1.896 (calculated)
#'
#' with(td[1:8,], plot(rdt, level, type = "l"))
#' newx <- seq(from = td$rdt[1], to = td$rdt[8], length.out = 200)
#' lines(newx, interpolate_tides(newx, td = td), col = 2)
#'
#' @export
#'
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
    # out <- data.frame(rdt = datetime, level = h)
}
