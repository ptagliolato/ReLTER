#' Obtain the information about the feature of interest.
#' @description `r lifecycle::badge("experimental")`
#' This function obtains the information about Feature(s) Of Interest (FOI(s))
#' provided by a Sensor Observation Services (SOS).
#' @param sosURL A `character`. The endpoint of the Sensor Observation Service
#' (SOS) service.
#' @param show_map A `logical`. When TRUE the boundary will be plotted on a
#' Leaflet map. Default FALSE.
#' @return The output of the function is a `sf`. The table contains all the information
#' about Feature(s) Of Interest (FOI(s)) provided by a Sensor Observation
#' Services (SOS).
#' The columns are about: sampling feature (typeSf,
#' description, name), sampled feature id (sampledFeature), coordinate reference
#' system code (srsName) and coordinates (geometry).
#' @author Alessandro Oggioni, phD \email{oggioni.a@@irea.cnr.it}
#' @author Paolo Tagliolato, phD \email{tagliolato.p@@irea.cnr.it}
#' @importFrom xml2 read_xml xml_text xml_find_all xml_ns xml_attr
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter
#' @importFrom tidyr separate
#' @importFrom sf st_as_sf
#' @importFrom leaflet leaflet addTiles addMarkers
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' 
#' LTERItaly <- get_sos_foi(
#'   sosURL = "http://getit.lteritalia.it/observations/service",
#'   show_map = TRUE
#' )
#' LTERItaly
#'
#' eurac_monalisa <- get_sos_foi(
#'   sosURL = "http://monalisasos.eurac.edu/sos/service",
#'   show_map = FALSE
#' )
#' eurac_monalisa
#'
#' NIVA <- get_sos_foi(
#'   sosURL = "https://hydro-sos.niwa.co.nz/",
#'   show_map = FALSE
#' )
#' NIVA
#' }
#' 
#' ## End (Not run)
#'
#' @section The function output:
#' \figure{get_sos_foi_fig.png}{Map of the Feature(s) Of Interest (FOI(s))
#' provided by LTER-Italy Sensor Observation Services (SOS)}
#'
### function get_sos_foi
get_sos_foi <- function(sosURL, show_map = FALSE) {
  requestObs <- paste0(
    sosURL,
    "?service=SOS&version=2.0.0&request=GetCapabilities&Sections=OperationsMetadata"
  )
  observationDataXML <- xml2::read_xml(requestObs)
  foiID <- 
    xml2::xml_text(
      xml2::xml_find_all(observationDataXML, 
                         xpath = ".//ows:Operation[@name='GetObservation']/ows:Parameter[@name='featureOfInterest']/ows:AllowedValues/ows:Value"))
  requestFOI <- list()
  for (i in 1:length(foiID)) {
    requestFOI[[i]] <- paste0(
      sosURL,
      "?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=",
      foiID[[i]]
    )
  }
  # description of FOI ----
  foiXML <- list()
  foi <- list()
  for (l in 1:length(requestFOI)) {
    x <- xml2::read_xml(requestFOI[[l]])
    y <- list()
    prefix <- if ("sam" %in% names(xml2::xml_ns(x))) "sam:" else "sf:"
    if(prefix=="sf:"){
      ns_additional = c("sf"="http://www.opengis.net/sampling/2.0")
    } else {
      ns_additional = c("sam"="http://www.opengis.net/sampling/2.0")
    }
    iserror <- "ows" %in% names(xml2::xml_ns(x)) && !is.na(xml2::xml_find_first(x, "ows:Exception"))
    if (iserror) {
      y["typeSf"] <- ""
      y["description"] <-  ""
      y["name"] <- ""
      y["foiID"] <- ""
      y["sampledFeature"] <- ""
      y["srsName"] <- ""
      y["pos"] <- ""
      foi[[l]] <- y
    } else if (!is.na(xml2::xml_attr(xml2::xml_find_first(x, ".//sos:featureMember"), "href"))) {
      y["typeSf"] <- ""
      y["description"] <-  ""
      y["name"] <- ""
      y["foiID"] <- xml2::xml_attr(xml2::xml_find_first(x, ".//sos:featureMember"), "href")
      y["sampledFeature"] <- ""
      y["srsName"] <- ""
      y["pos"] <- ""
      foi[[l]] <- y
    } else {
      y["typeSf"] <- xml2::xml_attr(xml2::xml_find_first(x, xpath = paste0(".//", prefix, "type"), ns = ns_additional), "href")
      y["description"] <-  xml2::xml_text(xml2::xml_find_first(x, ".//gml:description"))
      y["name"] <- xml2::xml_text(xml2::xml_find_first(x, ".//gml:name"))
      y["foiID"] <- xml2::xml_text(xml2::xml_find_first(x, ".//gml:identifier"))
      y["sampledFeature"] <- xml2::xml_attr(xml2::xml_find_first(x, xpath = paste0(".//", prefix, "sampledFeature"), ns = ns_additional), "href")
      y["srsName"] <- xml2::xml_attr(xml2::xml_find_first(x, ".//gml:pos"), "srsName")
      y["pos"] <- xml2::xml_text(xml2::xml_find_first(x, ".//gml:pos/text()"))
      foi[[l]] <- y
    }
  }
  foiTibble <- purrr::map_dfr(
    .x = foi, .f = ~.x
  ) %>%
    unique() %>%
    dplyr::filter(pos != "") %>%
    tidyr::separate(
      col = pos,
      into = c("lon", "lat"),
      sep = " "
    )
  if (show_map == TRUE) {
    crs <- foiTibble$srsName[min(which(foiTibble$srsName != ""))]
    foiGeo <- sf::st_as_sf(
      foiTibble,
      coords = c("lat", "lon"),
      crs = crs
    )
    map <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addMarkers(
        data = foiGeo,
        popup = paste0(
          "<b>Feature Of Interest (FOI) name: </b>",
          "<br>",
          foiGeo$name,
          "<br>",
          "<b>FOI ID: </b>",
          "<br>",
          foiGeo$foiID,
          "<br>",
          "<b>FOI coordinates: </b>",
          "<br>",
          foiGeo$geometry
        )
      )
    print(map)
    message(
      "\n----\n",
      "The number of Features Of Interest (FOIs) presented in this tibble",
      " are only those with geographical reference.\n",
      "----\n"
    )
    return(foiGeo)
  } else {
    message(
      "\n----\n",
      "The number of Features Of Interest (FOIs) presented in this tibble",
      " are all the FOIs shared in service (with and without geographical",
      " reference).\n",
      "----\n"
    )
    return(foiTibble)
  }
}
