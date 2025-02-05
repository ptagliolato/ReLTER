#' Provide a map (image) of sites in an eLTER Network.
#' @description `r lifecycle::badge("stable")`
#' Return a image map object of all of the eLTER sites belonging
#' to an eLTER Network (e.g.
#' \href{https://deims.org/networks/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3}{LTER
#' Italy network}), as a stored into \href{https://deims.org}{DEIMS-SDR}.
#' @param networkDEIMSID A `character`. The DEIMS ID of the network
#' from DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here} and Complete list of ILTER
#' networks \href{https://deims.org/search?f[0]=result_type:network}{here}.
#' @param countryCode A `character` following the ISO 3166-1 alpha-3 codes.
#' This ISO convention consists of three-letter country codes as defined in
#' ISO 3166-1. The ISO 3166 standard published by the International
#' Organization for Standardization (ISO), to represent countries, dependent
#' territories, and special areas of geographical interest. The map produced by
#' this function will be limited only to the country indicated in this
#' parameter, if the network has a extraterritorial sites those will not
#' represented.
#' @return The output of the function is a `tmap` plot containing an image of
#' geographic distribution of the network of sites present in the chosen
#' country.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom jsonlite fromJSON
#' @importFrom sf as_Spatial st_as_sf st_crs st_is_valid
#' @importFrom tmap tm_shape tm_borders tm_dots
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom httr RETRY content
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{jsonliteR}{ReLTER}
#'
#'   \insertRef{sfR}{ReLTER}
#'
#'   \insertRef{tmapR}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#'
#'   \insertRef{tibbleR}{ReLTER}
#'
#'   \insertRef{httrR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' # Italian sites
#' map <- produce_network_points_map(
#'   networkDEIMSID =
#'   "https://deims.org/network/7fef6b73-e5cb-4cd2-b438-ed32eb1504b3",
#'   countryCode = "ITA"
#' )
#' map
#'
#' # German sites
#' map_LTERGermanSites <- produce_network_points_map(
#'   networkDEIMSID =
#'   "https://deims.org/networks/e904354a-f3a0-40ce-a9b5-61741f66c824",
#'   countryCode = "DEU"
#' )
#' map_LTERGermanSites +
#'   tmap::tm_compass(type = "8star", position = c("right", "bottom")) +
#'   tmap::tm_scale_bar(position = c("right", "bottom"))
#' }
#'
#' @section The function output:
#' \figure{produce_network_points_map_fig.png}{Map of LTER-D Germany sites}
#'
### function produce_network_points_map
produce_network_points_map <- function(networkDEIMSID, countryCode) {
  deimsbaseurl <- get_deims_base_url()
  # suppressWarnings({
    url <- paste0(
      deimsbaseurl,
      "api/sites?network=",
      sub("^.+/", "", networkDEIMSID)
    )
    export <- httr::RETRY("GET", url = url, times = 5)
    lterNetworkSitesCoords <- jsonlite::fromJSON(
      httr::content(export, as = "text", encoding = "UTF-8")
    )
    if (length(lterNetworkSitesCoords) != 0) {
      lterNetworkSitesCoords$uri <- paste0(
        lterNetworkSitesCoords$id$prefix,
        lterNetworkSitesCoords$id$suffix
      )
      lterNetworkSitesCoords <- lterNetworkSitesCoords %>%
        dplyr::select("title", "uri", "changed", "coordinates")

      networkSitesGeo <- sf::st_as_sf(
        tibble::as_tibble(lterNetworkSitesCoords),
        wkt = "coordinates"
      )
      sf::st_crs(networkSitesGeo) <- 4326
      networkSitesGeo_valid <- sf::st_is_valid(
        networkSitesGeo
      )
      if (any(networkSitesGeo_valid)) {
        if (countryCode %in% isoCodes$Alpha_3 == TRUE) {
          try({
            country <- geodata::gadm(country = countryCode, level = 0) %>%
              terra::simplifyGeom(tol = 0.01, topologyPreserve = TRUE
              )
          })
          mapOfSites <- if (exists("country")) {
            tmap::tm_shape(country) +
              tmap::tm_borders("grey75", lwd = 1)
          } else {
            NULL
          }
          mapOfSites <- mapOfSites +
            tmap::tm_shape(networkSitesGeo) +
            tmap::tm_dots(
              col = NA,
              size = 0.1,
              shape = 16,
              title = NA,
              legend.show = FALSE
            )
          return(mapOfSites)
        } else {
          mapOfSites <- tmap::tm_shape(networkSitesGeo) +
            tmap::tm_dots(
              col = NA,
              size = 0.1,
              shape = 16,
              title = NA,
              legend.show = FALSE
            )
          message("\n----\nThe map of country cannot be created.
  Please check again the Country code.
  Compare the code provided with the list of code in
  https://en.wikipedia.org/wiki/ISO_3166\n----\n")
          return(mapOfSites)
        }
      } else {
        message("\n----\nThe maps cannot be created because coordinates,
  provided in DEIMS-SDR, have invalid geometry.
  Please check the content and refers this error to DEIMS-SDR contact person
  of the network, citing the Network.iD.\n----\n")
        mapOfSites <- tmap::tm_shape(country) +
          tmap::tm_borders("grey75", lwd = 1)
        mapOfSites
      }
    } else {
      message("\n----\nThe requested page could not be found.
  Please check again the Network.iD\n----\n")
      mapOfSites <- NULL
    }
  # })
}
