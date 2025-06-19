\name{read_spectro}
\alias{read_spectro}
\title{Read Spectrophotomter File}
\description{Reads an XML file produced by the Jenway 7315 spectrophotometer at the Portobello Marine Laboratory}
\usage{
read_spectro(file, read_tag = "Readings", meas_tag = "Measurement")
}
\arguments{
  \item{file }{A string representing the (XML) file to read}
  \item{read_tag }{A string representing the tag for the "Readings" node in the XML file}
  \item{meas_tag }{A string representing the tag for the "Measurement" node in the XML file.}
}
\details{
This function reads an XML file produced by the Jenway 7315 spectrophotometer at the Portobello Marine Laboratory. Most often applied when calculating pigment concentrations.
}
\value{
A dataframe with columns `Name`, `WL`, `Time`, `Tran`, `Abs`, `Conc`.
}
\author{
Daniel Pritchard
}

\seealso{
\code{\link{beer_eshel_1985}}, \code{\link{ritchie_2008}}
}
\examples{
## An example of these functions in use:
# Using the sample file that Hang provided
fpath <- system.file("tests", "test_spectro.xml", package="pmisc")
dat <- read_spectro(file = fpath)
head(dat)
str(dat)
}
