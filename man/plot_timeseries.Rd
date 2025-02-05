% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_site_MODIS.R
\name{plot_timeseries}
\alias{plot_timeseries}
\title{Plot a time series of averaged pixel values from MODIS images.}
\usage{
plot_timeseries(deimsid, product, output_dir, output_proj = "3035")
}
\arguments{
\item{deimsid}{A \code{character}. The DEIMS ID of the site from
DEIMS-SDR website. DEIMS ID information
\href{https://deims.org/docs/deimsid.html}{here}.}

\item{product}{A \code{character}. The requested product. One of:
"LST", VI". Default is "VI".}

\item{output_dir}{a \code{character}, where MODIS images were saved
This directory is returned by \code{get_site_MODIS()}
The final graph as png image file will be saved here also.}

\item{output_proj}{\code{character}: The EPSG code of output rasters}
}
\value{
Full path to the saved png image.
}
\description{
Create a time series of averaged pixel values from MODIS images
cropped to site boundaries. Display a line plot and save to png.
\href{https://deims.org/docs/deimsid.html}{here}.
}
\details{
Read all images in \code{output_dir} and prepare line plots
of average pixel values over the site boundary for each band.

This function is not exported. It is called by \code{get_site_MODIS()}
}
\examples{
 \dontrun{
 # Example in Northern Negev LTER
deimsid <- "https://deims.org/871a90b2-e372-456a-93e3-518ad1e11239"
output_dir <- tempdir()
png_files <- ReLTER::plot_timeseries(deimsid, product = "VI",
    output_dir = output_dir, output_proj = "6991")
message("Output plots: ", png_files)
}
}
\references{
\insertRef{sfR}{ReLTER}

\insertRef{terraR}{ReLTER}
}
\author{
Micha Silver, phD (2020) \email{silverm@post.bgu.ac.il}

Alessandro Oggioni, phD (2020) \email{oggioni.a@irea.cnr.it}
}
\keyword{Internal}
