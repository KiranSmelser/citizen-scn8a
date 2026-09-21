# R/cluster/plot_cluster_pca.R
# Regenerate PCA figures from saved cluster assignments without refitting models.

library(dplyr)
library(ggplot2)

source(file.path(".", "R", "config.R"))

time_labels <- c("1yr", "3yr", "5yr", "8yr", "10yr")
run_suffixes <- c("unknown_excluded", "lof_excluded")

for (run_suffix in run_suffixes) {
  dir.create(file.path(FIGS, "clusters", run_suffix),
             showWarnings = FALSE, recursive = TRUE)

  for (label in time_labels) {
    cluster_file <- file.path(DATA_PROCESSED, run_suffix,
                              paste0(label, "_clusters.csv"))
    cluster_feature_data <- read.csv(cluster_file, stringsAsFactors = FALSE)
    unmapped_clusters <- setdiff(unique(as.character(cluster_feature_data$cluster)),
                                names(CLUSTER_COLORS))
    if (length(unmapped_clusters) > 0) {
      stop("No configured color for cluster(s): ",
           paste(sort(unmapped_clusters), collapse = ", "))
    }

    # clusters.R writes the unscaled feature matrix and fixed assignments here.
    # Reapply only the continuous-feature scaling used immediately before PCA.
    pca_features <- cluster_feature_data %>%
      select(-patient_uuid, -cluster)
    pca_features$age_onset_m <- scale(pca_features$age_onset_m)

    pca_res <- prcomp(pca_features, center = TRUE, scale. = FALSE)
    pca_df <- data.frame(pca_res$x[, 1:2]) %>%
      setNames(c("PC1", "PC2")) %>%
      mutate(
        patient_uuid = cluster_feature_data$patient_uuid,
        cluster = factor(cluster_feature_data$cluster)
      )

    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(size = 2, alpha = 0.8) +
      scale_color_manual(values = CLUSTER_COLORS) +
      labs(
        title = paste(label, "Patient Clusters"),
        x = "PC1",
        y = "PC2",
        color = "Cluster"
      ) +
      theme_classic()

    ggsave(
      filename = file.path(FIGS, "clusters", run_suffix,
                           paste0("clusters_", label, ".pdf")),
      plot = p, width = 6, height = 5, units = "in"
    )
  }
}
