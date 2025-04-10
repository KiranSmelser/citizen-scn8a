# R/functions/plotting_functions.R
# Functions for creating common plots

library(ggplot2)
library(ggh4x)
library(dplyr)
library(tidyr)
library(stringr)

# Helper: Compute the number of observations for a given predictor and outcome.
compute_n_for_predictor <- function(data, results_df, predictor, outcome_col = "target") {
  n_values <- c()
  for (outcome in results_df[[outcome_col]]) {
    if (grepl("Onset_group", predictor)) {
      if (predictor == "Onset_group_(Intercept)") {
        # Use "1-4 Months" as the reference level
        subset_data <- data[data[["Onset_group"]] == "1-4 Months" & data[[outcome]] == 1, ]
      } else {
        # Remove prefix and trim spaces so the level exactly matches the factor level
        level_val <- trimws(sub("Onset_group_", "", predictor))
        subset_data <- data[data[["Onset_group"]] == level_val & data[[outcome]] == 1, ]
      }
      n_values <- c(n_values, nrow(subset_data))
    } else if (grepl("unique_types", predictor)) {
      if (predictor == "unique_types_(Intercept)") {
        subset_data <- data[data[["unique_types"]] == "0" & data[[outcome]] == 1, ]
        n_values <- c(n_values, nrow(subset_data))
      } else {
        if (grepl("2\\+", predictor)) {
          subset_data <- data[data[["unique_types"]] == "2+" & data[[outcome]] == 1, ]
          n_values <- c(n_values, nrow(subset_data))
        } else {
          level_val <- trimws(sub("unique_types", "", predictor))
          subset_data <- data[data[["unique_types"]] == level_val & data[[outcome]] == 1, ]
          n_values <- c(n_values, nrow(subset_data))
        }
      }
    } else if (predictor == "age_onset_m") {
      subset_data <- data[data[[outcome]] == 1, ]
      n_values <- c(n_values, nrow(subset_data))
    } else {
      subset_data <- data[data[[predictor]] == 1 & data[[outcome]] == 1, ]
      n_values <- c(n_values, nrow(subset_data))
    }
  }
  results_df$n <- n_values
  return(results_df)
}

# Plot odds ratios for a given predictor.
plot_odds_ratio <- function(results_df, predictor_name, data, outcome_col = "target", outcome_label = "Outcome Type") {
  # Update n
  results_df <- compute_n_for_predictor(data, results_df, predictor_name, outcome_col)
  results_df$midpoint <- (results_df$ci_lower + results_df$ci_upper) / 2
  # Set color: red if odds ratio < 1, blue otherwise; if not significant, set to transparent.
  results_df$color <- ifelse(results_df$midpoint < 1, "#a30234", "#0076c0")
  results_df$color <- ifelse(results_df$p_value > 0.05, "transparent", results_df$color)
  
  p <- ggplot(results_df, aes(x = .data[[outcome_col]], y = midpoint, color = color)) +
    geom_point() +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_color_identity() +
    coord_flip(clip = "off") +
    labs(title = predictor_name, x = outcome_label, y = "Odds Ratio") +
    theme_bw() +
    geom_text(aes(y = -Inf, label = paste("n =", as.character(n))), 
              vjust = 2.5, hjust = 1.75, size = 3)
  return(p)
}

# Plots a heatmap for a given patient.
create_heatmap <- function(data, fill_var, title_suffix, limits = NULL) {
  ggplot(data, aes(x = type, y = medication, fill = .data[[fill_var]])) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(.data[[fill_var]], 2)), size = 3, color = "black", na.rm = TRUE) +
    scale_fill_gradient2(
      low = "#083681",
      mid = "#F7F7F7",
      high = "#C80813FF",
      midpoint = 0,
      na.value = "#F7F7F7",
      limits = limits
    ) +
    theme_classic() +
    labs(
      title = title_suffix,
      x = "Seizure Type",
      y = "Medication",
      fill = "Change in Seizure Index"
    )
}

# Plots a heatmap with a modified x-axis for a given patient.
create_combined_heatmap_modified <- function(data, limits = NULL) {
  data_long <- data %>%
    pivot_longer(
      cols = c(diff_on_vs_before, diff_on_vs_after),
      names_to = "comparison",
      values_to = "diff_value"
    ) %>%
    mutate(
      comparison = recode(
        comparison,
        diff_on_vs_before = "Before",
        diff_on_vs_after  = "After"
      ),
      comparison = factor(comparison, levels = c("Before", "After"))
    )
  
  # Build combined x-axis
  data_long <- data_long %>%
    mutate(x_axis = paste(comparison, type, sep = "|"))
  
  # Define factor levels so "Before" precedes "After"
  seizure_types <- unique(data_long$type)
  x_levels <- unlist(lapply(seizure_types, function(t) {
    c(paste("Before", t, sep = "|"), paste("After", t, sep = "|"))
  }))
  data_long$x_axis <- factor(data_long$x_axis, levels = x_levels)
  
  # Positions for vertical lines
  n_groups <- length(seizure_types)
  vline_positions <- if (n_groups > 1) sapply(1:(n_groups - 1), function(i) i * 2 + 0.5) else NULL
  
  ggplot(data_long, aes(x = x_axis, y = medication, fill = diff_value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(diff_value, 2)), size = 3, color = "black", na.rm = TRUE) +
    # vertical lines
    geom_vline(xintercept = vline_positions, linetype = "solid", color = "black", size = 1) +
    scale_fill_gradient2(
      low = "#083681",
      mid = "#F7F7F7",
      high = "#C80813FF",
      midpoint = 0,
      na.value = "#F7F7F7",
      limits = limits
    ) +
    theme_classic() +
    labs(
      title = "Seizure Index Comparisons",
      x = "Comparison and Seizure Type",
      y = "Medication",
      fill = "Change in Seizure Index"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(guide = ggh4x::guide_axis_nested(delim = "|"))
}

# Creates a chart for a given patient that contains their heatmap, timeline, and index vs. time plots.
plot_patient_chart <- function(patient_data) {
  # Creates a line plot for seizure index over time
  p_line <- ggplot(patient_data$pt_data_type, aes(x = age_months, y = index, color = type)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_line(linetype = "dashed", alpha = 0.8) +
    facet_grid(type ~ .) +
    theme_light() +
    labs(x = "Age (months)", y = "Seizure Index", color = "Seizure Type") +
    guides(color = "none")
  
  # Create timeline plot
  p_timeline <- ggplot() +
    # Medication timeline segments (first 3 months in gray, remainder blue)
    geom_segment(
      data = patient_data$pt_data_duration,
      aes(x = start_med_age_months, xend = first_3_months_end,
          y = medication_base, yend = medication_base),
      size = 2, color = "#8A9197FF"
    ) +
    geom_segment(
      data = patient_data$pt_data_duration,
      aes(x = first_3_months_end, xend = end_med_age_months,
          y = medication_base, yend = medication_base),
      size = 2, color = "#709AE1FF"
    ) +
    # Seizure events
    geom_point(
      data = patient_data$pt_data_type,
      aes(x = age_months, y = type),
      color = "#C80813FF", size = patient_data$pt_data_type$index + 1, alpha = 0.6
    ) +
    # Infantile Spasms periods
    geom_segment(
      data = patient_data$pt_spasm_periods,
      aes(x = spasm_start_age, xend = spasm_end_age, y = "Infantile Spasms", yend = "Infantile Spasms"),
      size = 2, color = "#C80813FF"
    ) +
    # Infantile Spasms (single reports)
    geom_point(
      data = patient_data$pt_spasm_periods %>% filter(is_single_report),
      aes(x = spasm_start_age, y = "Infantile Spasms"),
      size = 2, color = "#C80813FF", shape = 15
    ) +
    # EEG reports
    geom_point(
      data = patient_data$pt_eeg,
      aes(x = age_months, y = "Infantile Spasms"),
      size = 3, color = "black", shape = 124, position = position_nudge(y = 0.14)
    ) +
    # Hypsarrhythmia reports
    geom_point(
      data = patient_data$pt_hyps,
      aes(x = age_months, y = "Infantile Spasms"),
      size = 3, color = "black", shape = 124, position = position_nudge(y = -0.16)
    ) +
    # Adverse effects
    geom_point(
      data = patient_data$pt_data_adverse,
      aes(x = age_months, y = "Adverse Effects"),
      color = "#FD7446FF", size = 2, shape = 15, alpha = 0.8
    ) +
    # Hospitalizations for Status epilepticus
    geom_point(
      data = patient_data$pt_data_status,
      aes(x = age_months, y = "Status epilepticus"),
      color = "#FED439FF", size = 5, shape = 18, alpha = 0.9
    ) +
    # Developmental milestones (Loss/Gained)
    geom_point(
      data = patient_data$pt_dev_data,
      aes(x = age_months, y = "Developmental Milestones", color = status_change, shape = domain),
      alpha = 0.6, size = 3, position = position_nudge(y = patient_data$pt_dev_data$y_offset * 0.2)
    ) +
    scale_color_manual(values = c("Loss" = "red", "Gained" = "green3"), guide = "none") +
    scale_shape_manual(values = c("Academic Performance" = 3, "Fine Motor Development" = 8,
                                  "Gross Motor Development" = 5, "Language Development" = 2),
                       name = "Developmental Domain") +
    # Appointment markers
    geom_point(
      data = patient_data$appointment_data,
      aes(x = appointment_age_months, y = "Appointments"),
      color = "#1A9993FF", size = 3, shape = 17, alpha = 0.6
    ) +
    theme_linedraw() +
    labs(
      title = patient_data$timeline_title,
      x = "Age (months)",
      y = ""
    ) +
    scale_y_discrete(limits = c("Appointments", "Developmental Milestones",
                                "Status epilepticus",
                                "Infantile Spasms",
                                rev(unique(patient_data$pt_data_type$type)),
                                "Adverse Effects",
                                rev(patient_data$med_order)))
  
  # Compute legend limits for heatmap using diff
  legend_limits <- max(abs(patient_data$pt_data$diff_on_vs_after),
                       abs(patient_data$pt_data$diff_on_vs_before), na.rm = TRUE) * c(-1, 1)
  
  # Create combined heatmap
  combined_heatmap <- create_combined_heatmap_modified(patient_data$pt_data, limits = legend_limits)
  
  combined_plot <- (p_line | combined_heatmap) /
    p_timeline + patchwork::plot_layout(heights = c(1, 1))
  
  return(combined_plot)
}

# Plot Seizure Types Over Age
plot_seizure_types_over_age <- function(norm_df, xlim = c(0, 10), ylim = c(0, 1)) {
  
  p <- ggplot(norm_df, aes(x = age_year, y = prop, fill = type)) +
    geom_area(stat = "smooth", method = "loess", position = "identity") +
    geom_line(stat = "smooth", method = "loess", formula = y ~ x, se = FALSE, linetype = 1,
              aes(color = type), show.legend = FALSE) +
    labs(x = "Age (Years)", y = "Proportion", fill = "Seizure Type") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    scale_x_continuous(breaks = 0:10) +
    theme_classic() +
    theme(legend.justification = c(0.05, 1), legend.position = c(0.05, 1)) +
    scale_fill_manual(values = c("#5698a3", "#ffde76", "#67771a", "#0076c0", "#e37c1d", "#7a5072")) +
    scale_color_manual(values = c("#5698a3", "#ffde76", "#67771a", "#0076c0", "#e37c1d", "#7a5072"))
  
  return(p)
}

# Function to plot hospitalization bar plots (Figure 2C)
plot_hospitalization_barplots <- function(other_freq, seizure_freq, specific_freq) {
  library(ggplot2)
  library(gridExtra)
  
  p1 <- ggplot(other_freq, aes(x = `Admission Type`, y = n, fill = Subgroup)) +
    geom_bar(stat = "identity") +
    labs(x = "Admission Type", y = "# of Reports", fill = "Subgroup") +
    theme_classic() +
    scale_fill_manual(values = c("#ffde76", "#bacfec", "#8A9197FF", "#f1b682", 
                                 "#e37c1d", "#abb47d", "#a30234", "#67771a", 
                                 "#e4b8b4", "#5698a3")) +
    ggtitle("Other")
  
  p1 <- p1 + 
    geom_text(data = specific_freq, aes(label = label, y = cumulative_n), 
              color = "white", size = 2.5) +
    geom_segment(data = specific_freq, 
                 aes(x = 0, xend = 1.5, y = cumulative_n - 3, yend = cumulative_n - 3),
                 color = "white")
  
  # Plot for hospitalizations pertaining to seizures
  p2 <- ggplot(seizure_freq, aes(x = `Admission Type`, y = n, fill = admission_diagnosis)) +
    geom_bar(stat = "identity") +
    labs(x = "Admission Type", y = "# of Reports", fill = "Type") +
    theme_classic() +
    scale_fill_manual(values = c("#7a5072", "#002F30")) +
    ggtitle("Seizure")
  
  combined <- grid.arrange(p1, p2, ncol = 2)

  return(combined)
}

# Function to plot a smooth line plot for hospitalization data (Figure 2D)
plot_hospitalization_lineplot <- function(hosp_data) {
  library(dplyr)
  library(ggplot2)
  
  # Convert admission_age_days_firstDate to years
  hosp_data <- hosp_data %>%
    mutate(age_years = admission_age_days_firstDate / 365)
  
  # Split seizure reports into subgroups
  hosp_data <- hosp_data %>%
    mutate(Subgroup = ifelse(Subgroup == "Seizure" & admission_diagnosis == "Status epilepticus",
                             "Status epilepticus", 
                             ifelse(Subgroup == "Seizure", "Non-status epilepticus", Subgroup)))
  
  hosp_data <- hosp_data %>%
    mutate(Subgroup = ifelse(Subgroup == "Non-status epilepticus", "Seizure", Subgroup))
  
  # Group data by floored age and count unique patients per subgroup
  patients_by_subgroup <- hosp_data %>%
    group_by(Subgroup, age_years_floor = floor(age_years)) %>%
    summarise(n = n_distinct(patient_uuid), .groups = "drop")
  
  # Total patients per floored age
  patients_total <- hosp_data %>%
    group_by(age_years_floor = floor(age_years)) %>%
    summarise(total = n_distinct(patient_uuid), .groups = "drop")
  
  # Join and compute proportions.
  normalized_df <- left_join(patients_by_subgroup, patients_total, by = "age_years_floor") %>%
    mutate(prop = n / total) %>%
    ungroup() %>%
    na.omit()
  
  filtered_df <- normalized_df %>%
    filter(Subgroup %in% c("Seizure", "Status epilepticus", "Pulmonary", "Infection", "GI"))
  
  colors <- c("Seizure" = "#a30234", 
              "Status epilepticus" = "#7a5072", 
              "Pulmonary" = "#67771a", 
              "Infection" = "#0076c0", 
              "GI" = "#e37c1d")
  line_types <- c("Seizure" = "solid", 
                  "Status epilepticus" = "solid", 
                  "Pulmonary" = "dotted", 
                  "Infection" = "dotdash", 
                  "GI" = "dashed")
  
  # Create smooth line plot
  p <- ggplot(filtered_df, aes(x = age_years_floor, y = prop)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                aes(color = Subgroup, linetype = Subgroup), method.args = list(span = 1)) +
    labs(x = "Age (years)", y = "Proportion", color = "Admission Type", linetype = "Admission Type") +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
    scale_x_continuous(breaks = 0:10) +
    theme_classic() +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = line_types)
  
  return(p)
}

# Diagnoses by body system (Figure 3)
plot_diagnoses_by_system <- function(sys_pcts, diagnosis_pcts, output_file) {
  library(ggpubr)
  library(gridExtra)
  library(grid)
  library(png)
  library(colorspace)
  
  colors <- c("#a30234", "#e4b8b4", "#e37c1d", "#bacfec", "#ffde76", lighten("#00545f", 0.25),
              "#0076c0", lighten("#67771a", 0.25), "#abb47d", "#a1c5fb", "#7a5072")
  unique_systems <- c("Musculoskeletal", "Gastrointestinal", "Behavioral", "Neurological", 
                      "Sensory", "Respiratory", "Cardiovascular", "Immunological")
  
  # Read anatomy image
  anatomy_img_path <- file.path("data", "raw", "anatomy.png")
  img <- readPNG(anatomy_img_path)
  
  sys_dfs <- list()
  
  # Build table data
  for (i in seq_along(unique_systems)) {
    system <- unique_systems[i]
    
    # Get system percentage
    system_pct <- sys_pcts %>% filter(System == system) %>% pull(sys_pct)
    system_pct <- round(system_pct, 2)
    system_pct <- paste0(system_pct, "%")
    
    # Diagnosis percentages for system
    diag_pct <- diagnosis_pcts %>% filter(System == system)
    diag_pct <- subset(diag_pct, select = -c(System))
    colnames(diag_pct) <- c("Diagnosis", "Percentage")
    
    # Order (descending) 
    diag_pct$Percentage <- as.numeric(diag_pct$Percentage)
    diag_pct <- diag_pct[order(-diag_pct$Percentage), ]
    if (nrow(diag_pct) > 7) {
      diag_pct <- diag_pct[1:7, ]
    }
    
    diag_pct$Percentage <- round(diag_pct$Percentage, 2)
    diag_pct$Percentage <- paste0(diag_pct$Percentage, "%")
    
    df <- data.frame(Diagnosis = system, Percentage = system_pct, stringsAsFactors = FALSE)
    df <- rbind(df, diag_pct)
    colnames(df) <- as.character(unlist(df[1,]))
    df <- df[-1, ]
    sys_dfs[[system]] <- df
  }
  
  jpeg(filename = output_file, width = 12, height = 14, units = "in", res = 600)
  
  # Set up a grid layout (5 x 3)
  pushViewport(viewport(layout = grid.layout(5, 3, 
                                             widths = unit(c(0.325, 0.3, 0.25), "npc"), 
                                             heights = unit(c(0.175, 0.175, 0.175, 0.175, 0.135), "npc"))))
  
  # Loop through each system and place table in determined location
  for (i in seq_along(unique_systems)) {
    system <- unique_systems[i]
    
    if (system %in% c("Neurological", "Sensory", "Gastrointestinal")) {
      table <- ggtexttable(sys_dfs[[system]], rows = NULL, theme = ttheme(
        colnames.style = colnames_style(color = "black", fill = colors[i]),
        tbody.style = tbody_style(color = "black", fill = lighten(colors[i], 0.5))
      ))
    } else {
      table <- ggtexttable(sys_dfs[[system]], rows = NULL, theme = ttheme(
        colnames.style = colnames_style(color = "white", fill = colors[i]),
        tbody.style = tbody_style(color = "black", fill = lighten(colors[i], 0.5))
      ))
    }
    
    # Determine position within grid
    if (system == "Neurological") {
      row <- 1; col <- 3
    } else if (system == "Immunological") {
      row <- 1; col <- 2
    } else if (system == "Cardiovascular") {
      row <- 2; col <- 3
    } else if (system == "Respiratory") {
      row <- 3; col <- 3
    } else if (system == "Sensory") {
      row <- 4; col <- 3
    } else {
      row <- ((i - 1) %% 3) + 2
      col <- ceiling(i / 3)
      if (col == 2) {
        col <- 3
      } else if (col == 3) {
        col <- 1
      }
    }
    
    if (!(row == 1 && col == 1)) {
      print(table, vp = viewport(layout.pos.row = row, layout.pos.col = col))
    }
  }
  
  # Overlay anatomy image in center
  grid.raster(img, width = unit(1, "npc"), height = unit(1, "npc"),
              vp = viewport(layout.pos.row = 2:4, layout.pos.col = 2))
  dev.off()
}