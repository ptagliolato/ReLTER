#' eLTER get_site_observedProperties function
#' @description `r lifecycle::badge("stable")`
#' This internal function obtains the list of observed properties measured
#' in the eLTER site through the DEIMS-SDR sites API.
#' @param deimsid A `character`. It is the DEIMS ID of the site from
#' DEIMS-SDR website. DEIMS ID information
#' \href{https://deims.org/docs/deimsid.html}{here}.
#' @return The output of the function is a `tibble` with main features of the
#' site and the observed properties collected.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom utils capture.output
#' @importFrom dplyr as_tibble
#' @keywords internal
#'
### function get_site_observedProperties
get_site_observedProperties <- function(deimsid) {
  qo <- queries_jq[[get_deims_API_version()]]$site_observedProperties
  jj <- get_id(deimsid, qo$path)
  if (is.na(attr(jj, "status"))) {
    invisible(
      utils::capture.output(
        observedProperties <- dplyr::as_tibble(do_Q(qo$query, jj))
      )
    )
    if (!is.na(observedProperties$observedProperties)) {
      colnames(observedProperties$observedProperties[[1]]) <- c(
        "observedPropertiesLabel",
        "observedPropertiesUri"
      )
    } else {
      observedPropertiesLabel <- NULL
      observedPropertiesUri <- NULL
      observedProperties$observedProperties <- list(
        data.frame(
          observedPropertiesLabel,
          observedPropertiesUri
        )
      )
    }
  } else {
    message("\n----\nThe requested page could not be found.
Please check again the DEIMS ID\n----\n")
    observedProperties <- NULL
  }
  observedProperties
}
