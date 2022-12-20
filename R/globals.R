# removes false positives from the check
utils::globalVariables(
  c(
    "freq",
    "hjust",
    "label",
    "middle",
    "n",
    "parameterGroups",
    "vjust",
    "end",
    "start",
    "perc"
  )
)

#' Package settings that can be changed by the user
#' @family package_customizable_settings
#' @export
package_settings <- (function() {
  pe <- new.env()
  deimsBaseUrl <- "https://deims.org/"
  jsoncachedir <- tempdir()
  
  assign("deimsBaseUrl", deimsBaseUrl, envir = pe)
  assign("jsoncachedir", jsoncachedir, envir = pe)
  
  pe
})()

entitylists_cache <- (function(){
  ee <- new.env()
  
  networks = list() # dictionary: each named element should contain a separate list of sites of the given network
  
  sites = NULL
  sensors = NULL
  activities = NULL
  datasets = NULL
  
  
  assign("networks", networks, envir = ee)
  
  assign("sites", sites, envir = ee)
  assign("sensors", sensors, envir = ee)
  assign("activities", activities, envir = ee)
  assign("datasets", datasets, envir = ee)
  
  ee
})()

get_deims_entity_list <- function(type="sites", network=NULL){
  
  if(!any(type %in% c("sites", "sensors", "activities", "datasets")))
    stop("type must be one of \"sites\", \"sensors\", \"activities\", \"datasets\"")
  
  if(is.null(network)){
    
    if(is.null(get(type, envir = entitylists_cache))) { 
      assign(type, value = as.list(jsonlite::fromJSON(paste0(deimsbaseurl,"api/", type))), envir = entitylists_cache)
    }
    return(get(type, envir = entitylists_cache))
  } else {
    
  }
  
}

#' Set DEIMS-SDR API base URL
#' @param url A `character`. Set the base URL to DEIMS-SDR.
#' @param force A `boolean`. Default FALSE.
#' @export
#' @importFrom RCurl url.exists
set_deims_base_url <- function(url = "https://deims.org/", force = FALSE) {
  if (!endsWith(url, "/")) {
    url <- paste0(url, "/")
  }
  if (!RCurl::url.exists(url)) {
    if (force) {
      warning("The URL ", url, " is not reachable, I set it because
              force TRUE is specified")
    } else stop("The URL ", url, " is not reachable")
  }
  message("Changing DEIMS-SDR base URL to: ", url)
  assign("deimsBaseUrl", url, envir = package_settings)
}

#' Get DEIMS-SDR base URL
#' @return DEIMS-SDR base URL
#' @family package_customizable_settings
#' @export
get_deims_base_url <- function() {
  get("deimsBaseUrl", envir = package_settings)
}

#' Get json cache directory path
#' @return dir for caching json resources from deims
#' @family package_customizable_settings
#' @export
get_jsoncachedir <- function() {
  get("jsoncachedir", envir = package_settings)
}

#' Set json cache directory path
#' @param path `character` path to local directory for caching json deims resources. 
#' Defaults to `tempdir()`
#' @param save_sys_variable Save the given path in user system environment variable (only if it is not tempdir).
#' Defaults to FALSE
#' @family package_customizable_settings
#' @export
set_jsoncachedir <- function(path=tempdir(), save_sys_variable=FALSE) {
  if(!dir.exists(path)){
    stop("The given path does not exist yet. Please create it before using this method")
  } else {
    message("Changing jsoncachedir to: ", path)
    if(save_sys_variable && path != tempdir()){
      Sys.setenv(ReLTER_jsoncachedir=path)
    }
    assign("jsoncachedir", path, envir = package_settings)
  }
}

# # Clean the cache directory
# clean_jsoncachedir <- function(){
#   file.remove()
#   if(get_jsoncachedir()
# }
