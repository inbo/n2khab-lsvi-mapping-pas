verify_n2khab_data <- function(reference, requirements) {
  checksums <-
    reference %>%
    filter(version_id %in% requirements) %>%
    mutate(
      filepath_expected = file.path(
        locate_n2khab_data(),
        ifelse(processed, "20_processed", "10_raw"),
        source_id,
        filename
      ),
      file_exists = file.exists(filepath_expected),
      xxh64sum_user = ifelse(
        file_exists,
        xxh64sum(filepath_expected),
        NA_character_
      )
    )
  checksum_missingfile <- checksums %>% filter(!file_exists)
  if (nrow(checksum_missingfile) > 0) {
    assign(
      "checksum_missingfile",
      checksum_missingfile,
      envir = parent.env(environment())
    )
    stop(
      "Missing or incomplete data sources:\n",
      str_flatten(unique(checksum_missingfile$source_id), collapse = "\n"),
      "\nPlease inspect the required versions in `checksum_missingfile`. ",
      "Use n2khab::download_zenodo(\"<doi>\", \"<path>\") as needed. ",
      "See https://inbo.github.io/n2khab/articles/v020_datastorage.html ."
    )
  }
  checksum_diff <- checksums %>% filter(file_exists, xxh64sum != xxh64sum_user)
  if (nrow(checksum_diff) > 0) {
    assign(
      "checksum_diff",
      checksum_diff,
      envir = parent.env(environment())
    )
    stop(
      "Wrong data source versions detected for:\n",
      str_flatten(unique(checksum_diff$source_id), collapse = "\n"),
      "\nPlease inspect the required versions in `checksum_diff`. ",
      "Use n2khab::download_zenodo(\"<doi>\", \"<path>\") as needed. ",
      "See https://inbo.github.io/n2khab/articles/v020_datastorage.html ."
    )
  }
  message("All n2khab_data requirements are fulfilled!")
}





#' Divide a series of subcells in two panels
#'
#' Divide a series of subcells in two panels of equal size (if possible), taking
#' into account the GRTS address.
#'
#' The algorithm decides at random which panel is larger, in the case of an odd
#' number of subcells.
divide_subcell_addresses_in_two <- function(subcell_addresses) {
  toss <- sample(1:2, 1) %>% as.integer()
  int_groupsize <- length(subcell_addresses) %/% 2
  memships <- c(
    rep(1L, int_groupsize),
    if (length(subcell_addresses) / 2 != int_groupsize) toss,
    rep(2L, int_groupsize)
  )
  memships[match(subcell_addresses, sort(subcell_addresses))]
}
