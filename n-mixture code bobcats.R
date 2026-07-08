

############################################################
# BOBCAT PIPELINE (30-MIN + 15-MIN) — FULL COPY/PASTE BLOCK
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)

############################################################
# 1. PER-CAMERA BOBCAT ABUNDANCE (30-MIN RULE)
############################################################

bobcat_counts <- counts %>% filter(label == "bobcat")

bobcat_matrix <- bobcat_counts %>%
  select(camera, capture_period, count) %>%
  pivot_wider(
    names_from = capture_period,
    values_from = count,
    values_fill = 0
  ) %>%
  arrange(as.numeric(camera))

if (nrow(bobcat_matrix) > 0) {
  
  y_bobcat <- as.matrix(bobcat_matrix[, -1])
  site_covs_bobcat <- data.frame(camera = bobcat_matrix$camera)
  
  umf_bobcat <- unmarkedFramePCount(
    y = y_bobcat,
    siteCovs = site_covs_bobcat
  )
  
  fit_bobcat_null <- pcount(~1 ~1, data = umf_bobcat, K = 200)
  summary(fit_bobcat_null)
  
  fit_bobcat_camera <- pcount(~1 ~ camera, data = umf_bobcat, K = 200)
  
  lambda_bobcat_df <- predict(fit_bobcat_camera, type = "state")
  lambda_bobcat_df$camera <- bobcat_matrix$camera
  
  bobcat_camera_abundance <- lambda_bobcat_df[, c("camera", "Predicted", "lower", "upper")]
  
  bobcat_camera_abundance$camera <- factor(
    as.numeric(as.character(bobcat_camera_abundance$camera)),
    levels = sort(as.numeric(as.character(bobcat_camera_abundance$camera)))
  )
  
  bobcat_camera_abundance$label <- round(bobcat_camera_abundance$Predicted, 0)
  
  ggplot(bobcat_camera_abundance, aes(x = camera, y = Predicted)) +
    geom_col(fill = "firebrick") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_text(aes(label = label), vjust = -0.5, size = 4) +
    labs(
      x = "Camera",
      y = "Predicted Abundance (λ)",
      title = "N-mixture Predicted Bobcat Abundance per Camera"
    ) +
    theme_minimal(base_size = 14) +
    expand_limits(y = max(bobcat_camera_abundance$upper) * 1.1)
}

############################################################
# 2. BOBCAT ACTIVITY OVER TIME (CORRECTED EVENTS, 30-MIN)
############################################################

trend_bobcat <- events %>%
  group_by(retrieval) %>%
  summarise(total_bobcat = sum(label == "bobcat"))

ggplot(trend_bobcat, aes(x = retrieval, y = total_bobcat)) +
  geom_line(group = 1, color = "firebrick", size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Corrected Bobcat Events",
    title = "Bobcat Activity Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 3. RETRIEVAL-LEVEL N-MIXTURE FOR BOBCAT (30-MIN RULE)
############################################################

retrieval_list_bobcat <- split(events, events$retrieval)

build_matrix_bobcat <- function(df) {
  df %>%
    filter(label == "bobcat") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_bobcat <- lapply(retrieval_list_bobcat, function(df) {
  mat <- build_matrix_bobcat(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_bobcat <- lapply(umf_list_bobcat, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_bobcat <- sapply(fit_list_bobcat, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

df_plot_bobcat <- data.frame(
  retrieval = names(lambda_total_bobcat),
  lambda = lambda_total_bobcat
)

df_plot_bobcat$retrieval <- factor(
  df_plot_bobcat$retrieval,
  levels = c("Original", "4-15-25", "6-6-25", "7-31-25", "10-10-25", "1-12-26")
)

df_plot_bobcat$label <- round(df_plot_bobcat$lambda, 0)

ggplot(df_plot_bobcat, aes(x = retrieval, y = lambda, group = 1)) +
  geom_line(color = "firebrick", size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.8, size = 5) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    title = "N-mixture Estimated Bobcat Abundance Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 4. RETRIEVAL-LEVEL N-MIXTURE FOR BOBCAT (15-MIN RULE)
############################################################

retrieval_list_15_bobcat <- split(events_15, events_15$retrieval)

build_matrix_bobcat_15 <- function(df) {
  df %>%
    filter(label == "bobcat") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_15_bobcat <- lapply(retrieval_list_15_bobcat, function(df) {
  mat <- build_matrix_bobcat_15(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_15_bobcat <- lapply(umf_list_15_bobcat, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_15_bobcat <- sapply(fit_list_15_bobcat, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

############################################################
# 5. COMPARISON: 30-MIN VS 15-MIN FOR BOBCAT
############################################################

df_compare_bobcat <- data.frame(
  retrieval = names(lambda_total_15_bobcat),
  lambda_30 = lambda_total_bobcat[names(lambda_total_15_bobcat)],
  lambda_15 = lambda_total_15_bobcat
)

print(df_compare_bobcat)

df_long_bobcat <- pivot_longer(
  df_compare_bobcat,
  cols = c(lambda_30, lambda_15),
  names_to = "window",
  values_to = "lambda"
)

df_long_bobcat$retrieval <- factor(
  df_long_bobcat$retrieval,
  levels = c("Original", "4-15-25", "6-6-25", "7-31-25", "10-10-25", "1-12-26")
)

ggplot(df_long_bobcat, aes(x = retrieval, y = lambda, color = window, group = window)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    color = "Independence Window",
    title = "Bobcat Abundance Over Time: 30-min vs 15-min Independence Rule"
  ) +
  theme_minimal(base_size = 14)
