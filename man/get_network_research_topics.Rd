% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_network_research_topics.R
\name{get_network_research_topics}
\alias{get_network_research_topics}
\title{Obtain a list of Research Topics handled in an eLTER Network.}
\usage{
get_network_research_topics(networkDEIMSID)
}
\arguments{
\item{networkDEIMSID}{A \code{character}. The DEIMS ID of a network
from DEIMS-SDR website. DEIMS ID information
\href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
networks \href{https://deims.org/search?f\link{0}=result_type:network}{here}.
The DEIMS ID of network is the URL for the network page.}
}
\value{
The output of the function is a \code{tibble} containing the research
topics and their URI (Uniform Resource Identifier) collected by network's
sites.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#defunct}{\figure{lifecycle-defunct.svg}{options: alt='[Defunct]'}}}{\strong{[Defunct]}}
This function was defunct because the section
about research topics of the site in DEIMS-SDR API
version 1.1 has been removed.

This function obtains Research Topics collected by all
of the eLTER sites belonging to an eLTER Network (e.g.
\href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER-
Italy network}), as a stored into \href{https://deims.org}{DEIMS-SDR}.
}
\examples{
\dontrun{
listResearchTopics <- get_network_research_topics(
  networkDEIMSID =
  "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3"
)
listResearchTopics[1:10, ] \%>\%
  dplyr::rows_insert(
    dplyr::tibble(
      researchTopicsLabel = "...",
      researchTopicsUri = "..."
    )
  )
}

}
\references{
\insertRef{jsonliteR}{ReLTER}

\insertRef{dplyrR}{ReLTER}
}
\author{
Alessandro Oggioni, phD (2020) \email{oggioni.a@irea.cnr.it}
}
