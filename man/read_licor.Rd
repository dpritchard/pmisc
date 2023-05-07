\name{read_licor}
\alias{read_licor}
\title{
Function to read data files from Odyssey PAR loggers.
}
\description{
This function reads TXT files produced by an LI-1500, handheld 3-channel light logger.
}
\usage{
read_licor(file, cal_col = "INPUT1", date_format = "\%Y-\%m-\%d", time_format = "\%H:\%M:\%S")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The file to read. Must be a TXT file in a format comptable with that produced by an LI-1500. See Details for further information.}
  \item{cal_col}{The column in the file to be preserved as calibration data.}
  \item{date_format}{Date format, compatible with \code{\link{strptime}} used for reading files.}
  \item{time_format}{Time format, compatible with \code{\link{strptime}} used for reading files.}
}
\details{
TODO: Describe data formats, with tables, if nessecary.
}
\value{

Returns a list of class \code{caldat} with two named components:
\item{meta }{A list containing, at a minimum \code{units} (which is currently hard coded to 'µmol photons / m^2 / s^1')}
\item{dat }{A data frame, with columns \code{Sdt}, \code{Rdt} and \code{light}}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file, cal_col = "INPUT1", date_format = "\%Y-\%m-\%d",
    time_format = "\%H:\%M:\%S")
{
    dat <- read.table(file, skip = 6, header = TRUE, strip.white = TRUE)
    dat <- dat[, c("Date", "Time", cal_col)]
    names(dat) <- c("date", "time", "light")
    dat$Sdt <- paste(dat$date, dat$time)
    dat$Rdt <- as.POSIXct(strptime(dat$Sdt, format = paste(date_format,
        time_format)))
    if (any(is.na(dat$Rdt)))
        warning("Datetime conversion produced NAs")
    imeta <- read.table(file, skip = 1, nrows = 5, sep = "\t")
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
    out <- list(meta = ometa, dat = dat[, c("Sdt", "Rdt", "light")])
    out$meta$units <- "µmol photons / m^2 / s^1"
    class(out) <- c("caldat", class(out))
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
