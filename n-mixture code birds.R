

############################################################
# BIRD PIPELINE (30-MIN + 15-MIN) MATCHING BOAR WORKFLOW
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)

############################################################
# 1. PER-CAMERA BIRD ABUNDANCE (30-MIN RULE)
############################################################

bird_counts <- counts %>% filter(label == "bird")

bird_matrix <- bird_counts %>%
  select(camera, capture_period, count) %>%
  pivot_wider(
    names_from = capture_period,
    values_from = count,
    values_fill = 0
  ) %>%
  arrange(as.numeric(camera))

if (nrow(bird_matrix) > 0) {
  
  y_bird <- as.matrix(bird_matrix[, -1])
  site_covs_bird <- data.frame(camera = bird_matrix$camera)
  
  umf_bird <- unmarkedFramePCount(
    y = y_bird,
    siteCovs = site_covs_bird
  )
  
  fit_bird_null <- pcount(~1 ~1, data = umf_bird, K = 200)
  summary(fit_bird_null)
  
  fit_bird_camera <- pcount(~1 ~ camera, data = umf_bird, K = 200)
  
  lambda_bird_df <- predict(fit_bird_camera, type = "state")
  lambda_bird_df$camera <- bird_matrix$camera
  
  bird_camera_abundance <- lambda_bird_df[, c("camera", "Predicted", "lower", "upper")]
  
  bird_camera_abundance$camera <- factor(
    as.numeric(as.character(bird_camera_abundance$camera)),
    levels = sort(as.numeric(as.character(bird_camera_abundance$camera)))
  )
  
  bird_camera_abundance$label <- round(bird_camera_abundance$Predicted, 0)
  
  ggplot(bird_camera_abundance, aes(x = camera, y = Predicted)) +
    geom_col(fill = "goldenrod") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_text(aes(label = label),
              vjust = -0.5,
              size = 4) +
    labs(
      x = "Camera",
      y = "Predicted Abundance (λ)",
      title = "N-mixture Predicted Bird Abundance per Camera"
    ) +
    theme_minimal(base_size = 14) +
    expand_limits(y = max(bird_camera_abundance$upper) * 1.1)
}

############################################################
# 2. BIRD ACTIVITY OVER TIME (CORRECTED EVENTS, 30-MIN)
############################################################

trend_bird <- events %>%
  group_by(retrieval) %>%
  summarise(total_bird = sum(label == "bird"))

ggplot(trend_bird, aes(x = retrieval, y = total_bird)) +
  geom_line(group = 1, color = "goldenrod", size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Corrected Bird Events",
    title = "Bird Activity Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 3. RETRIEVAL-LEVEL N-MIXTURE FOR BIRD (30-MIN RULE)
############################################################

retrieval_list_bird <- split(events, events$retrieval)

build_matrix_bird <- function(df) {
  df %>%
    filter(label == "bird") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_bird <- lapply(retrieval_list_bird, function(df) {
  mat <- build_matrix_bird(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_bird <- lapply(umf_list_bird, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_bird <- sapply(fit_list_bird, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

df_plot_bird <- data.frame(
  retrieval = names(lambda_total_bird),
  lambda = lambda_total_bird
)

df_plot_bird$retrieval <- factor(
  df_plot_bird$retrieval,
  levels = c(
    "Original",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)

df_plot_bird$label <- round(df_plot_bird$lambda, 0)

ggplot(df_plot_bird, aes(x = retrieval, y = lambda, group = 1)) +
  geom_line(color = "goldenrod", size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.8, size = 5) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    title = "N-mixture Estimated Bird Abundance Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 4. RETRIEVAL-LEVEL N-MIXTURE FOR BIRD (15-MIN RULE)
############################################################

retrieval_list_15_bird <- split(events_15, events_15$retrieval)

build_matrix_bird_15 <- function(df) {
  df %>%
    filter(label == "bird") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_15_bird <- lapply(retrieval_list_15_bird, function(df) {
  mat <- build_matrix_bird_15(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_15_bird <- lapply(umf_list_15_bird, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_15_bird <- sapply(fit_list_15_bird, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

############################################################
# 5. COMPARISON: 30-MIN VS 15-MIN FOR BIRD
############################################################

df_compare_bird <- data.frame(
  retrieval = names(lambda_total_15_bird),
  lambda_30 = lambda_total_bird[names(lambda_total_15_bird)],
  lambda_15 = lambda_total_15_bird
)

print(df_compare_bird)

df_long_bird <- pivot_longer(
  df_compare_bird,
  cols = c(lambda_30, lambda_15),
  names_to = "window",
  values_to = "lambda"
)

df_long_bird$retrieval <- factor(
  df_long_bird$retrieval,
  levels = c(
    "Original",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)

ggplot(df_long_bird, aes(x = retrieval, y = lambda, color = window, group = window)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    color = "Independence Window",
    title = "Bird Abundance Over Time: 30-min vs 15-min Independence Rule"
  ) +
  theme_minimal(base_size = 14)
