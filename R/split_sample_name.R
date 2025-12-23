split_sample_name <- function(df,
                              n_parts,
                              part_labels,
                              keep_sample_name = FALSE) {
  
  out <- df |>
    dplyr::filter(!grepl("\\bntc\\b", `Sample Name`, ignore.case = TRUE, perl = TRUE)) |>
    tidyr::separate(
      col    = `Sample Name`,
      into   = part_labels,
      sep    = "_",
      remove = FALSE,
      extra  = "merge",
      fill   = "right"
    ) |>
    dplyr::filter(!dplyr::if_all(dplyr::all_of(part_labels), ~ is.na(.)))
  
  # return: split columns + Target Name + CT (+ optional Sample Name)
  if (isTRUE(keep_sample_name)) {
    out |>
      dplyr::select(`Sample Name`, dplyr::all_of(part_labels), `Target Name`, CT)
  } else {
    out |>
      dplyr::select(dplyr::all_of(part_labels), `Target Name`, CT)
  }
}