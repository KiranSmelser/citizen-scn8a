# R/functions/analysis_functions.R
# Functions for common data analysis tasks

library(dplyr)
library(tidyr)
library(lubridate)

calculate_seizure_index_comparisons <- function(df_type, df_duration, appointment_summary) {
  # 1. Identify medications to keep
  df_combined_all_types <- df_type %>%
    left_join(df_duration, by = "patient_uuid", relationship = "many-to-many")
  
  # Exclude meds with < 3-month duration
  df_combined_all_types <- df_combined_all_types %>%
    filter((end_med_age - start_med_age) > 91)
  
  # Adjust medication start age by adding 91 days when possible
  df_combined_all_types <- df_combined_all_types %>%
    mutate(
      adjusted_start_med_age = if_else(
        (start_med_age + 91) < end_med_age,
        start_med_age + 91,
        start_med_age
      )
    )
  
  # Define logical flags
  df_combined_all_types <- df_combined_all_types %>%
    mutate(
      on_med = (age_days >= adjusted_start_med_age) & (age_days <= end_med_age),
      before_med = age_days < start_med_age,
      after_med = age_days > end_med_age,
      within_3months_before = age_days >= pmax(start_med_age - 91, 0) & age_days < start_med_age,
      within_3months_after  = age_days > end_med_age & age_days <= (end_med_age + 91)
    )
  
  # Handle overlapping medication periods
  df_assigned_all <- df_combined_all_types %>%
    arrange(patient_uuid, age_days, adjusted_start_med_age) %>%
    group_by(patient_uuid, age_days) %>%
    filter(on_med) %>%
    ungroup()
  
  df_combined_all_types <- df_combined_all_types %>%
    left_join(
      df_assigned_all %>%
        select(patient_uuid, age_days, medication) %>%
        rename(assigned_med = medication),
      by = c("patient_uuid", "age_days"),
      relationship = "many-to-many"
    ) %>%
    group_by(patient_uuid, age_days) %>%
    mutate(on_med_final = medication %in% assigned_med) %>%
    ungroup()
  
  # Summarize meds that have at least one seizure on-med or within 3 months
  meds_to_keep <- df_combined_all_types %>%
    group_by(patient_uuid, medication) %>%
    summarise(
      has_seizure_on_med       = any(on_med_final, na.rm = TRUE),
      has_seizure_before_after = any(within_3months_before | within_3months_after, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(has_seizure_on_med | has_seizure_before_after) %>%
    select(patient_uuid, medication)
  
  # 2. Compute weighted index comparisons
  seizure_types <- unique(df_type$type)
  seizures_summary_list <- list()
  
  for (s_type in seizure_types) {
    # Filter
    df_type_filtered <- df_type %>% filter(type == s_type)
    df_combined <- df_type_filtered %>% left_join(df_duration, by = "patient_uuid", relationship = "many-to-many")
    
    # Exclude medications < 3 months
    df_combined <- df_combined %>%
      filter((end_med_age - start_med_age) > 91)
    
    # Adjust medication start age by adding 91 days
    df_combined <- df_combined %>%
      mutate(
        adjusted_start_med_age = if_else(
          (start_med_age + 91) < end_med_age,
          start_med_age + 91,
          start_med_age
        )
      )
    
    # Define seizure time periods (on_med, before, after)
    df_combined <- df_combined %>%
      mutate(
        on_med    = (age_days >= adjusted_start_med_age) & (age_days <= end_med_age),
        before_med = age_days < start_med_age,
        after_med  = age_days > end_med_age
      )
    
    # Handle overlapping medication intervals, by keeping all that apply
    df_assigned <- df_combined %>%
      arrange(patient_uuid, age_days, adjusted_start_med_age) %>%
      group_by(patient_uuid, age_days) %>%
      filter(on_med) %>%
      ungroup()
    
    df_combined <- df_combined %>%
      left_join(
        df_assigned %>%
          select(patient_uuid, age_days, medication) %>%
          rename(assigned_med = medication),
        by = c("patient_uuid", "age_days"),
        relationship = "many-to-many"
      ) %>%
      group_by(patient_uuid, age_days) %>%
      mutate(on_med_final = medication %in% assigned_med) %>%
      ungroup()
    
    # Identify seizures within 3 months before/after
    df_combined <- df_combined %>%
      mutate(
        within_3months_before = age_days >= pmax(start_med_age - 91, 0) & age_days < start_med_age,
        within_3months_after  = age_days > end_med_age & age_days <= (end_med_age + 91)
      )
    
    # Keep meds with at least one seizure on or within 3 months
    df_combined <- df_combined %>%
      inner_join(meds_to_keep, by = c("patient_uuid", "medication"))
    
    # Join first/last appointment
    df_combined <- df_combined %>%
      left_join(appointment_summary, by = "patient_uuid") %>%
      mutate(
        adjusted_start_med_age_months = adjusted_start_med_age / 30,
        end_med_age_months = end_med_age / 30,
        duration_before = pmax(adjusted_start_med_age_months - first_appointment, 0),
        duration_on     = pmax(end_med_age_months - adjusted_start_med_age_months, 0),
        duration_after  = pmax(last_appointment - end_med_age_months, 0),
        duration_off    = duration_before + duration_after
      )
    
    # Compute time normalized averages
    seizures_summary <- df_combined %>%
      group_by(
        patient_uuid, medication, start_med_age, end_med_age,
        adjusted_start_med_age_months, end_med_age_months,
        first_appointment, last_appointment
      ) %>%
      summarise(
        on_med_avg     = safe_mean(index[on_med_final]),
        before_med_avg = safe_mean(index[before_med]),
        after_med_avg  = safe_mean(index[after_med]),
        off_med_avg    = safe_mean(index[!on_med_final]),
        duration_before = first(duration_before),
        duration_on     = first(duration_on),
        duration_after  = first(duration_after),
        duration_off    = first(duration_off),
        .groups = "drop"
      ) %>%
      mutate(
        weighted_index_med    = if_else(duration_on > 0, on_med_avg / sqrt(duration_on), 0),
        weighted_index_before = if_else(duration_before > 0, before_med_avg / sqrt(duration_before), 0),
        weighted_index_after  = if_else(duration_after > 0, after_med_avg / sqrt(duration_after), 0),
        weighted_index_off    = if_else(duration_off > 0, off_med_avg / sqrt(duration_off), 0),
        diff_on_vs_off    = weighted_index_med - weighted_index_off,
        diff_on_vs_before = weighted_index_med - weighted_index_before,
        diff_on_vs_after  = weighted_index_med - weighted_index_after,
        type = s_type
      )
    
    seizures_summary_list[[s_type]] <- seizures_summary
  }
  
  seizures_summary_combined <- bind_rows(seizures_summary_list)
  return(seizures_summary_combined)
}

# Determines seizure counts per patient
get_seizure_counts <- function(df_table_data) {
  df_table_data %>%
    group_by(patient_uuid) %>%
    summarise(
      seizure_count = n(),
      mean_index = round(mean(index, na.rm = TRUE), 2),
      number_seizure_types = n_distinct(type),
      seizure_types = paste(unique(type), collapse = ", "),
      .groups = "drop"
    )
}

# Calculates largest gap without seizure events
get_seizure_gaps <- function(df_table_data) {
  df_table_data %>%
    group_by(patient_uuid) %>%
    arrange(age_in_months) %>%
    summarise(
      gap = ifelse(n() > 1, max(diff(age_in_months), na.rm = TRUE), NA_real_),
      start_gap_age = ifelse(n() > 1, age_in_months[which.max(diff(age_in_months))], NA_real_),
      end_gap_age = ifelse(n() > 1, age_in_months[which.max(diff(age_in_months)) + 1], NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      gap = round(replace_na(gap, 0), 2),
      start_gap_age = round(start_gap_age, 2),
      end_gap_age = round(end_gap_age, 2),
      gap_period = if_else(gap > 0, paste0(start_gap_age, " - ", end_gap_age), "No gap")
    )
}

# Determines current vs. weened medications for each patient
get_medication_status <- function(df_duration, appointment_summary) {
  df_duration %>%
    mutate(
      end_age_months = end_med_age / 30,
      start_age_months = start_med_age / 30
    ) %>%
    left_join(appointment_summary, by = "patient_uuid") %>%
    group_by(patient_uuid, medication) %>%
    summarise(
      is_current = any(end_age_months >= last_appointment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(patient_uuid) %>%
    summarise(
      current_medications = paste(medication[is_current], collapse = ", "),
      weened_medications = paste(medication[!is_current], collapse = ", "),
      number_current_medications = sum(is_current),
      number_weened_medications = sum(!is_current),
      .groups = "drop"
    )
}

# Determines active medications during seizure gaps
get_gap_medications <- function(seizure_gaps, seizures_summary) {
  meds_during_gap <- seizure_gaps %>%
    inner_join(seizures_summary, by = "patient_uuid") %>%
    filter(
      round(start_med_age / 30) <= end_gap_age,
      round(end_med_age / 30) >= start_gap_age
    ) %>%
    select(patient_uuid, medication, start_med_age, end_med_age, gap_period) %>%
    mutate(
      start_in_months = round(start_med_age / 30, 2),
      end_in_months = round(end_med_age / 30, 2)
    ) %>%
    distinct()
  
  gap_meds_join <- meds_during_gap %>%
    group_by(patient_uuid) %>%
    summarise(
      number_med_types_gap = n_distinct(medication),
      med_types_gap = paste(unique(medication), collapse = ", "),
      .groups = "drop"
    )
  
  gap_meds_join
}

# Computes number of unique seizure types for each patient
get_unique_seizure_types <- function() {
  seizure_data <- read_seizure_history() %>%
    rename(UUID = patient_uuid) %>%
    filter(seizure_history_age_days < AGE_CUTOFF_DAYS) %>%
    select(UUID, seizure_history_type)
  
  # Exclude non-specific/uninformative seizure types
  exclude_types <- c("Seizure", "Unclassified seizure", "Provoked seizure", "Unprovoked seizure")
  seizure_data <- seizure_data %>%
    filter(!seizure_history_type %in% exclude_types)
  
  # Read classifier
  classifier <- read_excel(PATH_CLASSIFIER)
  types_classifier <- classifier %>% select(8:13)
  
  # Helper function to map seizure type to corresponding group from classifier
  find_column_name <- function(value, df) {
    for (col_name in names(df)) {
      if (value %in% df[[col_name]]) {
        return(col_name)
      }
    }
    return(NA)
  }
  
  # Map each seizure_history_type to seizure group based on classifier
  seizure_data <- seizure_data %>%
    mutate(seizure_group = sapply(seizure_history_type, find_column_name, df = types_classifier))
  
  # Count number of unique seizure types per patient
  unique_seizure_types <- seizure_data %>%
    group_by(UUID) %>%
    summarise(unique_types = n_distinct(seizure_group, na.rm = TRUE)) %>%
    ungroup()
  
  return(unique_seizure_types)
}