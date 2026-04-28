# ============================================================
# R/qc_helper.R
# ============================================================

qc_build_results <- function(df_all, df_long, id_col, trt_col, meta_cols,
                             ref_gene, mock_value, r3) {
  
  # Extra metadata columns carried into QC output (excluding id + treatment)
  meta_extra <- setdiff(meta_cols, c(id_col, trt_col))
  
  # Grouping columns used throughout QC checks: sample_id + treatment + meta_extra
  # This preserves tissue (or any other extra parsed column) instead of collapsing it
  qc_group_ids <- c("sample_id", "treatment", meta_extra)
  
  # QC1: reference gene CT outside expected range (18–22)
  qc1_all <- df_all %>%
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
        TRUE           ~ "PASS"
      )
    ) %>%
    dplyr::transmute(
      sample_id = as.character(.data[[id_col]]),
      treatment = as.character(.data[[trt_col]]),
      dplyr::across(dplyr::all_of(meta_extra), ~ as.character(.x)),
      target    = as.character(ref_gene),
      flag      = "Ref CT out of range",
      observed  = observed,
      expected  = "18\u201322",
      how_off   = r3(how_off),
      severity  = severity
    ) %>%
    dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
    dplyr::distinct()
  
  qc1_flags <- qc1_all %>%
    dplyr::filter(severity %in% c("YELLOW", "RED"))
  
  # Add .is_mock flag for mock-only QC checks
  df_long2 <- df_long %>%
    dplyr::mutate(.is_mock = as.character(.data[[trt_col]]) == mock_value)
  
  # QC2: mock dCt SD within (sample_id, treatment, meta_extra, target)
  qc2_all <- df_long2 %>%
    dplyr::filter(.is_mock) %>%
    dplyr::group_by(
      sample_id = as.character(.data[[id_col]]),
      treatment = as.character(.data[[trt_col]]),
      dplyr::across(dplyr::all_of(meta_extra)),
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
        TRUE            ~ "PASS"
      )
    ) %>%
    dplyr::transmute(
      sample_id = sample_id,
      treatment = treatment,
      dplyr::across(dplyr::all_of(meta_extra)),
      target    = target,
      flag      = "Mock dCt SD high",
      observed  = observed,
      expected  = "\u22640.5 ok; >1 fail",
      how_off   = r3(how_off),
      severity  = severity
    ) %>%
    dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
    dplyr::distinct()
  
  qc2_flags <- qc2_all %>%
    dplyr::filter(severity %in% c("YELLOW", "RED"))
  
  # QC3: mock relative expression deviation from 1
  qc3_all <- df_long2 %>%
    dplyr::filter(.is_mock) %>%
    dplyr::mutate(
      observed = r3(as.numeric(relative_expression)),
      how_off  = dplyr::if_else(is.na(observed), NA_real_, abs(observed - 1)),
      severity = dplyr::case_when(
        is.na(how_off) ~ NA_character_,
        how_off >= 4   ~ "RED",
        how_off > 1    ~ "YELLOW",
        TRUE           ~ "PASS"
      )
    ) %>%
    dplyr::transmute(
      sample_id = as.character(.data[[id_col]]),
      treatment = as.character(.data[[trt_col]]),
      dplyr::across(dplyr::all_of(meta_extra), ~ as.character(.x)),
      target    = as.character(.data[["Target Name"]]),
      flag      = "Mock rel expr far from 1",
      observed  = observed,
      expected  = "1\u00b11 warn; 1\u00b14 fail",
      how_off   = r3(how_off),
      severity  = severity
    ) %>%
    dplyr::filter(!is.na(sample_id) & nzchar(sample_id)) %>%
    dplyr::distinct()
  
  qc3_flags <- qc3_all %>%
    dplyr::filter(severity %in% c("YELLOW", "RED"))
  
  # Full QC calculations table (PASS + flags)
  qc_calc_table <- dplyr::bind_rows(qc1_all, qc2_all, qc3_all) %>%
    dplyr::mutate(
      severity_rank = dplyr::case_when(
        severity == "RED"    ~ 2L,
        severity == "YELLOW" ~ 1L,
        severity == "PASS"   ~ 0L,
        TRUE                 ~ -1L
      )
    ) %>%
    dplyr::arrange(dplyr::desc(severity_rank), treatment, sample_id, flag, target) %>%
    dplyr::select(
      sample_id,
      treatment,
      dplyr::all_of(meta_extra),
      target, flag, observed, expected, how_off, severity
    )
  
  # Flag-only QC table
  qc_table <- dplyr::bind_rows(qc1_flags, qc2_flags, qc3_flags) %>%
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
  
  list(qc_table = qc_table, qc_calc_table = qc_calc_table)
}