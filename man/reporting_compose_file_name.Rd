% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eLTER_reporting.R
\name{reporting_compose_file_name}
\alias{reporting_compose_file_name}
\title{eLTER reporting format naming convention for files}
\usage{
reporting_compose_file_name(
  deimsid = NULL,
  country_code = NULL,
  site_name = NULL,
  data_topic,
  variable_group = "",
  time_span,
  version = Sys.Date() \%>\% format("V\%Y\%m\%d")
)
}
\arguments{
\item{deimsid}{A \code{character} The DEIMS ID of the site from
DEIMS-SDR website. More information about DEIMS ID in this pages:
\href{https://deims.org/docs/deimsid.html}{page}.}

\item{country_code}{A \code{character} automatically evaluated if
DEIMS ID is provided.
Otherwise reference to the country of the site as two-digit country
code according to ISO 3166-1 alpha-2.}

\item{site_name}{A \code{character} Automatically evaluated if DEIMS ID is
provided.
Otherwise the name of the site according to DEIMS-SDR, if the name is
too long the site name can be shortened.}

\item{data_topic}{A \code{character}. Max 5-digit code for data topic or
observation programme, e.g. METEO (Meteorology), BIODIV (Biodiversity),
DEPO (deposition), GHG (Green House gas), SW (Soil water), VEG (Vegetation).
The abbreviation is defined by the data provider depending on the data.}

\item{variable_group}{A \code{character}. Optional, list of variables or
variable groups contained in the data. The abbreviation is defined by the
data provider depending on the data.}

\item{time_span}{A \code{numeric} or a \code{character}. Time span covered in the
data.}

\item{version}{version in format "VYYYYMMDD". Data version in the format
“V”YYYYMMDD.
Defaults to current date.}
}
\value{
filename (without extension) following naming convention
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Compose file name following eLTER naming convention
}
\note{
This method must be intended as a signpost for future implementation
}
\examples{
\dontrun{
## Not run:

deimsid <- "https://deims.org/8eda49e9-1f4e-4f3e-b58e-e0bb25dc32a6"
time_span <- 2015 # e.g. whole year
# time_span <- "20150302-20180415" # e.g. span between two dates
data_topic <- "VEG" # data provider defined abbreviation of "vegetation"
variable_group <- "SPECCOVER" # data provider defined abbreviation
version <- "V20220907"

filename <- reporting_compose_file_name(
  deimsid = deimsid,
  data_topic = data_topic,
  variable_group = variable_group,
  time_span = time_span,
  version = version
)

## End (Not run)
}

}
\seealso{
Peterseil, Geiger et al. (2020)
Field Specification for data reporting. Technical Document.
TechDoc.01. EU Horizon 2020 eLTER PLUS Project, Grant agreement No. 871128
\url{https://zenodo.org/record/6373410}
}
\author{
Paolo Tagliolato, phD \email{tagliolato.p@irea.cnr.it}

Alessandro Oggioni, phD \email{oggioni.a@irea.cnr.it}
}
