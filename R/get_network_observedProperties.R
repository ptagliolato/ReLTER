#' Obtain a list of all the observed properties of sites in an
#' eLTER Network.
#' @description `r lifecycle::badge("stable")`
#' This function obtains all observed properties
#' collected in an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER-
#' Italy network}), through the DEIMS-SDR API.
#' @param networkDEIMSID A `character`. The DEIMS ID of network
#' from DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' The DEIMS ID of network is the URL for the network page.
#' @return The output of the function is a `tibble` containing the list
#' of observed properties and their URI (Uniform Resource Identifier) collected
#' by the network's sites.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct as_tibble
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' listParams <- get_network_observedProperties(
#'   networkDEIMSID =
#'   "https://deims.org/networks/e0f680c2-22b1-4424-bf54-58aa9b7476a0"
#' )
#' listParams[1:10, ] %>%
#' dplyr::rows_insert(
#'   dplyr::tibble(parameterLabel = "...", parameterUri = "...")
#' )
#' }
#'
### function get_network_observedProperties
get_network_observedProperties <- function(networkDEIMSID) {
  deimsbaseurl <- get_deims_base_url()
  lterNetworkSites <- as.list(
    jsonlite::fromJSON(
      paste0(deimsbaseurl,
             "api/sites?network=",
             sub("^.+/", "", networkDEIMSID)
            )
    )
  )
  allSiteObservedProperties <- lapply(
    as.list(
      paste0(
        lterNetworkSites$id$prefix,
        lterNetworkSites$id$suffix
      )
    ),
    ReLTER::get_site_info,
    category = "observedProperties"
  )
  if (length(allSiteObservedProperties) != 0) {
    uniteSiteObservedProperties <- dplyr::bind_rows(allSiteObservedProperties)
    observedPropertiesNetworkList <- uniteSiteObservedProperties$observedProperties
    observedPropertiesNetworkDF <- dplyr::bind_rows(observedPropertiesNetworkList)
    uniqueSiteObservedProperties <- dplyr::as_tibble(
      dplyr::distinct(
        observedPropertiesNetworkDF
      )
    )
    uniqueSiteObservedProperties
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the Network.iD\n----\n")
    uniqueSiteObservedProperties <- NULL
  }
}
