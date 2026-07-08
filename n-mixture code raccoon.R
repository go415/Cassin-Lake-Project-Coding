
############################################################
# RACCOON PIPELINE (30-MIN + 15-MIN) MATCHING BOAR WORKFLOW
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)

############################################################
# 1. PER-CAMERA RACCOON ABUNDANCE (30-MIN RULE)
############################################################

# Use existing 'counts' from 30-min events
raccoon_counts <- counts %>% filter(label == "raccoon")

raccoon_matrix <- raccoon_counts %>%
  select(camera, capture_period, count) %>%
  pivot_wider(
    names_from = capture_period,
    values_from = count,
    values_fill = 0
  ) %>%
  arrange(as.numeric(camera))

# Only proceed if there are raccoon detections
if (nrow(raccoon_matrix) > 0) {
  
  y_raccoon <- as.matrix(raccoon_matrix[, -1])
  site_covs_raccoon <- data.frame(camera = raccoon_matrix$camera)
  
  umf_raccoon <- unmarkedFramePCount(
    y = y_raccoon,
    siteCovs = site_covs_raccoon
  )
  
  # Null model
  fit_raccoon_null <- pcount(~1 ~1, data = umf_raccoon, K = 200)
  summary(fit_raccoon_null)
  
  # Per-camera model
  fit_raccoon_camera <- pcount(~1 ~ camera, data = umf_raccoon, K = 200)
  
  lambda_raccoon_df <- predict(fit_raccoon_camera, type = "state")
  lambda_raccoon_df$camera <- raccoon_matrix$camera
  
  raccoon_camera_abundance <- lambda_raccoon_df[, c("camera", "Predicted", "lower", "upper")]
  
  # Plot per-camera raccoon abundance
  raccoon_camera_abundance$camera <- factor(
    as.numeric(as.character(raccoon_camera_abundance$camera)),
    levels = sort(as.numeric(as.character(raccoon_camera_abundance$camera)))
  )
  
  raccoon_camera_abundance$label <- round(raccoon_camera_abundance$Predicted, 0)
  
  ggplot(raccoon_camera_abundance, aes(x = camera, y = Predicted)) +
    geom_col(fill = "darkgreen") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_text(aes(label = label),
              vjust = -0.5,
              size = 4) +
    labs(
      x = "Camera",
      y = "Predicted Abundance (λ)",
      title = "N-mixture Predicted Raccoon Abundance per Camera"
    ) +
    theme_minimal(base_size = 14) +
    expand_limits(y = max(raccoon_camera_abundance$upper) * 1.1)
}

############################################################
# 2. RACCOON ACTIVITY OVER TIME (CORRECTED EVENTS, 30-MIN)
############################################################

trend_raccoon <- events %>%
  group_by(retrieval) %>%
  summarise(total_raccoon = sum(label == "raccoon"))

ggplot(trend_raccoon, aes(x = retrieval, y = total_raccoon)) +
  geom_line(group = 1, color = "darkgreen", size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Corrected Raccoon Events",
    title = "Raccoon Activity Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 3. RETRIEVAL-LEVEL N-MIXTURE FOR RACCOON (30-MIN RULE)
############################################################

retrieval_list_raccoon <- split(events, events$retrieval)

build_matrix_raccoon <- function(df) {
  df %>%
    filter(label == "raccoon") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_raccoon <- lapply(retrieval_list_raccoon, function(df) {
  mat <- build_matrix_raccoon(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_raccoon <- lapply(umf_list_raccoon, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_raccoon <- sapply(fit_list_raccoon, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

df_plot_raccoon <- data.frame(
  retrieval = names(lambda_total_raccoon),
  lambda = lambda_total_raccoon
)

df_plot_raccoon$retrieval <- factor(
  df_plot_raccoon$retrieval,
  levels = c(
    "Original",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)

df_plot_raccoon$label <- round(df_plot_raccoon$lambda, 0)

ggplot(df_plot_raccoon, aes(x = retrieval, y = lambda, group = 1)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.8, size = 5) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    title = "N-mixture Estimated Raccoon Abundance Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 4. RETRIEVAL-LEVEL N-MIXTURE FOR RACCOON (15-MIN RULE)
############################################################

# events_15 already created in your boar 15-min block:
# events_15 <- raw %>% filter(independent_15 == 1)

retrieval_list_15_raccoon <- split(events_15, events_15$retrieval)

build_matrix_raccoon_15 <- function(df) {
  df %>%
    filter(label == "raccoon") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_15_raccoon <- lapply(retrieval_list_15_raccoon, function(df) {
  mat <- build_matrix_raccoon_15(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_15_raccoon <- lapply(umf_list_15_raccoon, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_15_raccoon <- sapply(fit_list_15_raccoon, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

############################################################
# 5. COMPARISON: 30-MIN VS 15-MIN FOR RACCOON
############################################################

df_compare_raccoon <- data.frame(
  retrieval = names(lambda_total_15_raccoon),
  lambda_30 = lambda_total_raccoon[names(lambda_total_15_raccoon)],
  lambda_15 = lambda_total_15_raccoon
)

print(df_compare_raccoon)

df_long_raccoon <- pivot_longer(
  df_compare_raccoon,
  cols = c(lambda_30, lambda_15),
  names_to = "window",
  values_to = "lambda"
)

df_long_raccoon$retrieval <- factor(
  df_long_raccoon$retrieval,
  levels = c(
    "Original",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)

ggplot(df_long_raccoon, aes(x = retrieval, y = lambda, color = window, group = window)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    color = "Independence Window",
    title = "Raccoon Abundance Over Time: 30-min vs 15-min Independence Rule"
  ) +
  theme_minimal(base_size = 14)
