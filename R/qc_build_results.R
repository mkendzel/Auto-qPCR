# ============================================================
# R/qc_helper.R
# ============================================================

qc_build_results <- function(df_all, df_long, id_col, trt_col, meta_cols,
                             ref_gene, mock_value, r3) {
  
  # Extra metadata columns carried into QC output (excluding id + treatment)
  meta_extra <- setdiff(meta_cols, c(id_col, trt_col))
  
  # Map sample_id -> metadata values (first seen per sample)
  meta_map <- df_all %>%
    dplyr::transmute(
      sample_id = as.character(.data[[id_col]]),
      dplyr::across(dplyr::all_of(meta_extra), ~ as.character(.x))
    ) %>%
    dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(meta_extra), ~ dplyr::first(.x)),
      .groups = "drop"
    )
  
  # QC1: reference gene CT outside expected range (18–22)
  qc1 <- df_all %>%
    dplyr::filter(as.character(.data[["Target Name"]]) == ref_gene) %>%
    dplyr::mutate(
      observed = r3(as.numeric(CT)),
      how_off = dplyr::case_when(
        is.na(observed) ~ NA_real_,
        observed < 18   ~ 18 - observed,
        observed > 22   ~ observed - 22,
        TRUE            ~ 0
      ),
      severity = dplyr::case_when(
        is.na(how_off) ~ NA_character_,
        how_off >= 4   ~ "RED",
        how_off > 0    ~ "YELLOW",
        TRUE           ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(severity)) %>%
    dplyr::transmute(
      sample_id = as.character(.data[[id_col]]),
      treatment = as.character(.data[[trt_col]]),
      target    = as.character(ref_gene),
      flag      = "Ref CT out of range",
      observed  = observed,
      expected  = "18–22",
      how_off   = r3(how_off),
      severity  = severity
    ) %>%
    dplyr::distinct()
  
  # Add .is_mock flag for mock-only QC checks
  df_long2 <- df_long %>%
    dplyr::mutate(.is_mock = as.character(.data[[trt_col]]) == mock_value)
  
  # QC2: mock dCt SD too high within (sample_id, treatment, target)
  qc2 <- df_long2 %>%
    dplyr::filter(.is_mock) %>%
    dplyr::group_by(
      sample_id = as.character(.data[[id_col]]),
      treatment = as.character(.data[[trt_col]]),
      target    = as.character(.data[["Target Name"]])
    ) %>%
    dplyr::summarise(
      observed = r3(stats::sd(dCt, na.rm = TRUE)),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      how_off = dplyr::case_when(
        is.na(observed) ~ NA_real_,
        observed >= 0.5 ~ observed - 0.5,
        TRUE            ~ 0
      ),
      severity = dplyr::case_when(
        is.na(observed) ~ NA_character_,
        observed > 1.0  ~ "RED",
        observed >= 0.5 ~ "YELLOW",
        TRUE            ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(severity)) %>%
    dplyr::transmute(
      sample_id = sample_id,
      treatment = treatment,
      target    = target,
      flag      = "Mock dCt SD high",
      observed  = observed,
      expected  = "≤0.5 ok; >1 fail",
      how_off   = r3(how_off),
      severity  = severity
    ) %>%
    dplyr::distinct()
  
  # QC3: mock relative expression too far from 1 (absolute deviation thresholds)
  qc3 <- df_long2 %>%
    dplyr::filter(.is_mock) %>%
    dplyr::mutate(
      observed = r3(as.numeric(relative_expression)),
      how_off  = dplyr::if_else(is.na(observed), NA_real_, abs(observed - 1)),
      severity = dplyr::case_when(
        is.na(how_off) ~ NA_character_,
        how_off >= 4   ~ "RED",
        how_off > 1    ~ "YELLOW",
        TRUE           ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(severity)) %>%
    dplyr::transmute(
      sample_id = as.character(.data[[id_col]]),
      treatment = as.character(.data[[trt_col]]),
      target    = as.character(.data[["Target Name"]]),
      flag      = "Mock rel expr far from 1",
      observed  = observed,
      expected  = "1±1 warn; 1±4 fail",
      how_off   = r3(how_off),
      severity  = severity
    ) %>%
    dplyr::distinct()
  
  # Combine QC flags + attach metadata + order by severity
  qc_table <- dplyr::bind_rows(qc1, qc2, qc3) %>%
    dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
    dplyr::left_join(meta_map, by = "sample_id") %>%
    dplyr::mutate(
      severity_rank = dplyr::case_when(
        severity == "RED"    ~ 2L,
        severity == "YELLOW" ~ 1L,
        TRUE                 ~ 0L
      )
    ) %>%
    dplyr::arrange(dplyr::desc(severity_rank), treatment, sample_id, flag, target) %>%
    dplyr::select(
      sample_id,
      treatment,
      dplyr::all_of(meta_extra),
      target, flag, observed, expected, how_off, severity
    )
  
  list(qc_table = qc_table)
}
