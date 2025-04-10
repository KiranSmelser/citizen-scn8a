# R/scripts/figures.R
# Generates figures

# Import Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Source Files
source(file.path(".", "R", "functions", "config.R"))
source(file.path(".", "R", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "functions", "data_import_functions.R"))
source(file.path(".", "R", "functions", "plotting_functions.R"))

# Figure 2
# --------

# Figure 2B

# Data Cleaning and Setup
df_seizure <- clean_seizure_data(include_spasms = TRUE)

df_seizure <- df_seizure %>%
  mutate(age_years = age_days / 365)

df_seizure <- df_seizure %>% filter(tolower(type) != "clonic")

# Count unique patients per seizure type at each age
patients_by_type <- df_seizure %>%
  group_by(type, age_year = floor(age_years)) %>%
  summarise(n = n_distinct(patient_uuid), .groups = "drop")

# Calculate total patients at each age
patients_total <- df_seizure %>%
  group_by(age_year = floor(age_years)) %>%
  summarise(total = n_distinct(patient_uuid), .groups = "drop")

# Join counts and compute proportion
normalized_df <- left_join(patients_by_type, patients_total, by = "age_year") %>%
  mutate(prop = n / total)

# Define and adjust factor levels for seizure type
seizure_levels <- c("Tonic-clonic", "Focal", "Tonic", "Myoclonic", "Absence", "Spasms")
normalized_df$type <- factor(normalized_df$type, levels = seizure_levels)

# Generate plot
p_seizure <- plot_seizure_types_over_age(normalized_df)
ggsave(filename = "output/figures/fig_2b.jpeg", plot = p_seizure, width = 10, height = 7, dpi = 600)

# Figure 2C

# Data Cleaning and Setup
hosp_data <- clean_hospitalization_data()

# Calculate frequency counts for hospitalization groups
freq_list <- calculate_hospitalization_frequencies(hosp_data)

# Generate combined bar plot
combined_plot <- plot_hospitalization_barplots(freq_list$other_freq, 
                                               freq_list$seizure_freq, 
                                               freq_list$specific_freq)

ggsave(filename = "output/figures/fig_2c.jpeg", plot = combined_plot, width = 10, height = 7, dpi = 600)

# Figure 2D

# Data Cleaning and Setup
hosp_data <- clean_hospitalization_data(select_cols = c(1:2, 10))

# Generate smooth line plot for hospitalizations
p_line <- plot_hospitalization_lineplot(hosp_data)

ggsave(filename = "output/figures/fig_2d.jpeg", plot = p_line, width = 10, height = 7, dpi = 600)

# Figure 3
# --------

# Data Cleaning and Setup
diagnosis_clean <- clean_diagnoses_data()
diagnoses <- diagnosis_clean$diagnoses
sys_pcts <- diagnosis_clean$sys_pcts
diagnosis_pcts <- diagnosis_clean$diagnosis_pcts

# Generate diagnoses by system figure 
plot_diagnoses_by_system(sys_pcts, diagnosis_pcts, file.path("output", "figures", "fig_3.jpeg"))