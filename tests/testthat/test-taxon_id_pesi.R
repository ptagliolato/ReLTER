message("\n---- Test taxon_id_pesi() ----")

library(testthat)

test_that("Output of taxon pesi function constructs 'tibble' as expected", {
  table <- data.frame(
    taxonID = c(1, 2, 3, 4, 5, 6),
    family = c(
      "Alexiidae", "Anthicidae",
      "Anthribidae", "Anthribidae",
      "Biphyllidae", "Brentidae"
    ),
    scientificName = c(
      "Sphaerosoma seidlitzi", "Endomia tenuicollis tenuicollis",
      "Anthribus fasciatus", "Phaenotherion fasciculatum fasciculatum",
      "Diplocoelus fagi", "Holotrichapion (Apiops) pisi"
    )
  )
  result <- ReLTER::taxon_id_pesi(
    table = table,
    taxaColumn = 3
  )
  expect_s3_class(result, "tbl_df")
  # expect_true(ncol(result) == ncol(table) + 8)
  expect_true(all(names(result) %in% c(
    names(table[c(1:2)]), "originalNameUsage",
    "scientificName", "scientificNameAuthorship", "synonyms",
    "scientificNameID", "url", "nameAccordingTo", "taxonomicStatus",
    "taxonRank"
  )))
})
