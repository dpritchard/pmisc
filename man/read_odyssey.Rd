\name{read_odyssey}
\alias{read_odyssey}
\title{
Function to read data files from Odyssey PAR loggers.
}
\description{
This function reads both original (cabled / pre-'Xtreem') files (hereafter \code{v1} files) and those exported from the web interface associated with the new Xtreem loggers (\code{v2} files).
}
\usage{
read_odyssey(file, date_format = "\%Y-\%m-\%d", time_format = "\%H:\%M:\%S", scan_rate = NULL)
}

\arguments{
  \item{file}{The file to read. Must be a CSV in \code{v1} or \code{v2} formats. See Details for further information.}
  \item{date_format}{Date format, compatible with \code{\link{strptime}} used for reading \code{v1} files.}
  \item{time_format}{Time format, compatible with \code{\link{strptime}} used for reading \code{v1} files.}
  \item{scan_rate}{The scan rate. If \code{NULL} the funciton will guess from the datetime values.}
}
\details{
TODO: Describe data formats, with tables, if nessecary.
}
\value{
Returns a list of class \code{odydat} with two named components:
\item{meta }{A list containing, at a minimum \code{scan_rate}}
\item{dat }{A data frame, with columns \code{Sdt}, \code{Rdt}, \code{value} and \code{temp}}
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
