\name{pigments}
\alias{ritchie_2008}
\alias{beer_eshel_1985}
\title{Calculate pigment concentrations based on a spectral wavescan}
\description{
These functions use the output from \code{\link{read_spectro}} to calculate pigment concentrations.
}
\usage{
ritchie_2008(ws)
beer_eshel_1985(ws)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ws }{The output from \code{\link{read_spectro}}, or something very much like it}
}
\details{
Details to follow!
}

\value{
A list, with the following components:
\describe{
    \item{data }{A dataframe. The data used for the calculation}
    \item{conc }{A dataframe. Calculated concentration(s)}
}
}

\references{
Ritchie, R. J.  2008.  Universal chlorophyll equations for estimating chlorophylls a, b, c, and d and total chlorophylls in natural assemblages of photosynthetic organisms using acetone, methanol, or ethanol solvents.  Photosynthetica, 46: 115--126.

Beer, S. and Eshel, A.  1985.  Determining phycoerythrin and phycocyanin concentrations in aqueous crude extracts of red algae.  Australian Journal of Marine and Freshwater Research, 36: 785--792.
}
\author{
Daniel Pritchard
}

\seealso{
\code{\link{read_spectro}}
}
\examples{
fpath <- system.file("extdata", "test_spectro.xml", package="seaweedR")
dat <- read_spectro(file = fpath)
head(dat)
concPyco <- beer_eshel_1985(dat)
str(concPyco)
print(concPyco$data)
print(concPyco$conc)
}
