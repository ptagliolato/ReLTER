% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{elter_write_rdata}
\alias{elter_write_rdata}
\title{eLTER write Rdata data}
\usage{
elter_write_rdata(myfiles, delim)
}
\arguments{
\item{myfiles}{A \code{character}. The list of the files to deposit in Zenodo.
Please provide all files only with 'csv' extension.}

\item{delim}{A \code{character}. Provide the character used to separate fields
within a record. Only if the extension of the file(s) are 'csv', 'tsv', or
'txt'.}
}
\value{
This function returns a rds files.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
This function write a Rdata file from csv, tsv, txt, xls or xlsx
dataset
}
\examples{
\dontrun{
## Not run:

elter_write_rdata(
  myfiles = c(
   "miscellaneus/file_show/data_mapping.csv",
   "miscellaneus/file_show/reference_TAXA.csv",
   "miscellaneus/file_show/reference_VARIABLES.csv"
  ),
  delim = ";"
)
}

## End (Not run)

}
\author{
Alessandro Oggioni, phD \email{oggioni.a@irea.cnr.it}
}
