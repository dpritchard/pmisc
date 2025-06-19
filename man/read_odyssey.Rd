\name{read_odyssey}
\alias{read_odyssey}
\title{
Function to read data files from Odyssey PAR loggers.
}
\description{
This function reads both original (cabled / pre-'Xtreem') files (hereafter \code{v1} files) and those exported from the web interface associated with the new Xtreem loggers (\code{v2} files).
}
\usage{
read_odyssey(file, version = NA, scan_rate = NA, date_format = "\%Y-\%m-\%d", time_format = "\%H:\%M:\%S", utc_offset = NA)
}

\arguments{
  \item{file}{The file to read. Must be a CSV in \code{v1} or \code{v2} formats. See Details for further information.}
  \item{verison}{Either \code{1} or \code{2}, but guessed from the file if \code{NA} (the default)}
  \item{scan_rate}{The scan rate. If \code{NA} the funciton will guess from the datetime values.}
  \item{date_format}{Date format, compatible with \code{\link{strptime}} used for reading \code{v1} files.}
  \item{time_format}{Time format, compatible with \code{\link{strptime}} used for reading \code{v1} files.}
  \item{utc_offset}{An offset to convert the date / times values back to UTC. Must be supplied for \code{v1} files.}


}
\details{
TODO: Describe data formats, with tables, if nessecary.

\code{utc_offset} is a niggly implimentation. Ideally all sensors would log a standard value (e.g. UTC seconds since the UNIX epoch) irrespective of what they display to the user. If they don't do that, then the user should really set the time to UTC. However experience has taught me that this very rarely happens.

So, for \code{v1} Odyssey files (and the LI-1500 supported by \code{\link{read_licor}}) we have data files that record date time in a human readable format, with the timezone set at the beginning of the deployment, which does not take into account daylight savings shifts.

Normally this isn't the end of the world becuase

a) The shift happens at night, when light is zero anyway. You just need to close you mind to the gaps or the \code{NA} value.
b) If two sensors are being compared, then hopefully both are in the same time zone (and are equally wrong)

But when using two sensors set to two different timezones, this is a problem. This happens more frequently than you might like when (for example) a LiCor reference sensor is set in Standard Time, even though it is summer time (or vice versa).

Initally toyed with using a \code{tz} argument (like other date/time R functions do), but this isn't really the same thing.

}
\value{
Returns a list of class \code{odydat} with two named components:
\item{meta }{A list containing, at a minimum \code{scan_rate} (class difftime)}
\item{dat }{A data frame, with columns \code{sdt}, \code{rdt}, \code{value} and \code{temp}}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Pritchard
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

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
