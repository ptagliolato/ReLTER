#' Provide a taxon ID \href{https://en.wikipedia.org/wiki/LSID}{LSID} to a
#' taxon list.
#' @description `r lifecycle::badge("stable")`
#' This function provide a taxon ID, usually a
#' \href{https://en.wikipedia.org/wiki/LSID}{LSID}, from a taxonomic list.
#' The input of the function is a csv file with a list of taxa. The Taxon
#' ID provided by this function is currently taken from Pan-European Species
#' directories Infrastructure - \href{http://eu-nomen.eu/pesi/}{PESI}. This
#' function takes advantage of taxize's `eubon_search` function
#' \url{https://docs.ropensci.org/taxize/} and the
#' \href{http://www.eu-nomen.eu/portal/rest/}{PESI RestAPI}.
#' @param table A `data.frame` containing column with a taxa
#' (e.g. Sphaerosoma seidlitzi, Malthinus, etc.).
#' @param taxaColumn A `numeric` that identify the column containing taxa
#' value.
#' @return The output of the function is a `tibble` containing all the
#' columns provided as input and new columns as: 'canonicalName', 'authorship',
#' 'synonyms', 'LSID', 'url', 'accordingTo', 'checkStatus' gathered from PESI.
#'
#' An example to export dataset obtained by this function is:
#' datasetMerged <- dplyr::bind_rows(table)
#' write.csv(
#'   datasetMerged,
#'   "table.csv",
#'   row.names = FALSE,
#'   fileEncoding = "UTF-8"
#' )
#'
#' Someone could have problems of characters encoding when CSV file is
#' written.
#' To resolve we suggest two different solutions:
#'
#' Solution 1 -
#'
#' 1. Open the CSV in Notepad.
#' 2. Click “File” and “Save As”.
#' 3. In the new popup that displays,  select “ANSI” from the “Encoding”
#'    field.
#' 4. Click “Save”.
#' 5. Now, you should be able to open the file in Excel and display the
#'    characters correctly.
#'
#' Solution 2 -
#'
#' 1. Open Excel
#'
#' 2. Click “File” and “New”
#'
#' 3. Click on the “Data” tab
#'
#' 4. Click “From Text” and select the CSV file
#'
#' 5. Select “Delimited”
#'
#' 6. For “File origin”, select “65001 : Unicode (UTF-8)”
#'
#' 7. Click “Next”
#'
#' 8. Select “Comma”
#'
#' 9. Click “Finish”
#'
#' 10. Excel should now show you the CSV file and display the characters
#'     correctly.
#' @author Alessandro Oggioni, phD (2020) \email{oggioni.a@@irea.cnr.it}
#' @importFrom taxize eubon_search
#' @importFrom dplyr bind_rows
#' @importFrom Rdpack reprompt
#' @references
#'   \insertRef{taxizeR1}{ReLTER}
#'
#'   \insertRef{taxizeR2}{ReLTER}
#'
#'   \insertRef{dplyrR}{ReLTER}
#' @export
#' @examples
#' \dontrun{
#' insects <- data.frame(
#'    taxonID = c(1, 2, 3, 4, 5, 6),
#'    family = c(
#'      "Alexiidae", "Anthicidae",
#'      "Anthribidae", "Anthribidae",
#'      "Biphyllidae", "Brentidae"
#'    ),
#'    scientificName = c(
#'      "Sphaerosoma seidlitzi", "Endomia tenuicollis tenuicollis",
#'      "Anthribus fasciatus", "Phaenotherion fasciculatum fasciculatum",
#'      "Diplocoelus fagi", "Holotrichapion (Apiops) pisi"
#'    )
#' )
#'
#' output <- taxon_id_pesi(
#'   table = insects,
#'   taxaColumn = 3
#' )
#'
#' # The annotated URIs of columns label are achieved by:
#' attributes(output)$uri
#'
#' }
#'
### function taxon_id_pesi
taxon_id_pesi <- function(table, taxaColumn) {
  n_cols_input <- length(table)
  colnames(table)[taxaColumn] <- "originalNameUsage"
  cols_without_uri <- rep("", (n_cols_input - 1))
  URIs <- c(
    cols_without_uri,
    "http://rs.tdwg.org/dwc/terms/originalNameUsage",
    "http://rs.tdwg.org/dwc/terms/scientificName",
    "http://rs.tdwg.org/dwc/terms/scientificNameAuthorship",
    "",
    "http://rs.tdwg.org/dwc/terms/scientificNameID",
    "",
    "http://rs.tdwg.org/dwc/terms/nameAccordingTo",
    "http://rs.tdwg.org/dwc/terms/taxonomicStatus",
    "http://rs.tdwg.org/dwc/terms/taxonRank"
  )
  table[, c(
    "canonicalName",
    "authorship",
    "synonyms",
    "LSID",
    "url",
    "accordingTo",
    "checkStatus",
    "rank"
  )] <- NA
  table <- as.list(table)
  i <- 1
  while (i <= length(table[[taxaColumn]])) {
    a <- taxize::eubon_search(
      query = table[[taxaColumn]][1],
      providers = "pesi"
    )
    if (length(a) == 0) {
      i <- i + 1
    } else if (length(a[[1]]) == 1) {
      table$canonicalName[[i]] <-
        a$taxon.taxonName.canonicalName
      table$authorship[[i]] <-
        a$taxon.taxonName.authorship
      table$LSID[[i]] <- a$taxon.identifier
      table$url[[i]] <- a$taxon.url
      table$rank[[i]] <- a$taxon.taxonName.rank
      if (is.null(a$taxon.accordingTo)) {
        table$accordingTo[[i]] <- NA
      } else {
        table$accordingTo[[i]] <- a$taxon.accordingTo
      }
      table$checkStatus[[i]] <- a$taxon.taxonomicStatus
      i <- i + 1
    } else if (length(a[[1]]) > 1) {
      a <- subset(a, "matchingNameType" == "TAXON")
      if (nrow(a) == 0) {
        i <- i + 1
      } else {
        table$canonicalName[[i]] <-
          a$taxon.taxonName.canonicalName
        table$authorship[[i]] <-
          a$taxon.taxonName.authorship
        # TODO add synonyms as a nested list
        # e.g. for this record table$scientificName[53] the result is:
        # matchingNameType
        # SYNONYM
        # SYNONYM
        # TAXON
        table$LSID[[i]] <- a$taxon.identifier
        table$url[[i]] <- a$taxon.url
        if (is.null(a$taxon.accordingTo)) {
          table$accordingTo[[i]] <- NA
        } else {
          table$accordingTo[[i]] <- a$taxon.accordingTo
        }
        table$checkStatus[[i]] <- a$taxon.taxonomicStatus
        i <- i + 1
      }
    }
  }
  datasetMerged <- dplyr::bind_rows(table) %>%
    dplyr::rename(
      scientificName = canonicalName,
      scientificNameAuthorship = authorship,
      scientificNameID = LSID,
      nameAccordingTo = accordingTo,
      taxonomicStatus = checkStatus,
      taxonRank = rank
    )
  attr(x = datasetMerged, which = "uri") <- URIs
  return(datasetMerged)
}
