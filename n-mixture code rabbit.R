

############################################################
# RABBIT PIPELINE (30-MIN + 15-MIN) — FULL COPY/PASTE BLOCK
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(unmarked)

############################################################
# 1. PER-CAMERA RABBIT ABUNDANCE (30-MIN RULE)
############################################################

rabbit_counts <- counts %>% filter(label == "rabbit")

rabbit_matrix <- rabbit_counts %>%
  select(camera, capture_period, count) %>%
  pivot_wider(
    names_from = capture_period,
    values_from = count,
    values_fill = 0
  ) %>%
  arrange(as.numeric(camera))

if (nrow(rabbit_matrix) > 0) {
  
  y_rabbit <- as.matrix(rabbit_matrix[, -1])
  site_covs_rabbit <- data.frame(camera = rabbit_matrix$camera)
  
  umf_rabbit <- unmarkedFramePCount(
    y = y_rabbit,
    siteCovs = site_covs_rabbit
  )
  
  fit_rabbit_null <- pcount(~1 ~1, data = umf_rabbit, K = 200)
  summary(fit_rabbit_null)
  
  fit_rabbit_camera <- pcount(~1 ~ camera, data = umf_rabbit, K = 200)
  
  lambda_rabbit_df <- predict(fit_rabbit_camera, type = "state")
  lambda_rabbit_df$camera <- rabbit_matrix$camera
  
  rabbit_camera_abundance <- lambda_rabbit_df[, c("camera", "Predicted", "lower", "upper")]
  
  rabbit_camera_abundance$camera <- factor(
    as.numeric(as.character(rabbit_camera_abundance$camera)),
    levels = sort(as.numeric(as.character(rabbit_camera_abundance$camera)))
  )
  
  rabbit_camera_abundance$label <- round(rabbit_camera_abundance$Predicted, 0)
  
  ggplot(rabbit_camera_abundance, aes(x = camera, y = Predicted)) +
    geom_col(fill = "tan3") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_text(aes(label = label), vjust = -0.5, size = 4) +
    labs(
      x = "Camera",
      y = "Predicted Abundance (λ)",
      title = "N-mixture Predicted Rabbit Abundance per Camera"
    ) +
    theme_minimal(base_size = 14) +
    expand_limits(y = max(rabbit_camera_abundance$upper) * 1.1)
}

############################################################
# 2. RABBIT ACTIVITY OVER TIME (CORRECTED EVENTS, 30-MIN)
############################################################

trend_rabbit <- events %>%
  group_by(retrieval) %>%
  summarise(total_rabbit = sum(label == "rabbit"))

ggplot(trend_rabbit, aes(x = retrieval, y = total_rabbit)) +
  geom_line(group = 1, color = "tan3", size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Corrected Rabbit Events",
    title = "Rabbit Activity Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 3. RETRIEVAL-LEVEL N-MIXTURE FOR RABBIT (30-MIN RULE)
############################################################

retrieval_list_rabbit <- split(events, events$retrieval)

build_matrix_rabbit <- function(df) {
  df %>%
    filter(label == "rabbit") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_rabbit <- lapply(retrieval_list_rabbit, function(df) {
  mat <- build_matrix_rabbit(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_rabbit <- lapply(umf_list_rabbit, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_rabbit <- sapply(fit_list_rabbit, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

df_plot_rabbit <- data.frame(
  retrieval = names(lambda_total_rabbit),
  lambda = lambda_total_rabbit
)

df_plot_rabbit$retrieval <- factor(
  df_plot_rabbit$retrieval,
  levels = c("Original", "4-15-25", "6-6-25", "7-31-25", "10-10-25", "1-12-26")
)

df_plot_rabbit$label <- round(df_plot_rabbit$lambda, 0)

ggplot(df_plot_rabbit, aes(x = retrieval, y = lambda, group = 1)) +
  geom_line(color = "tan3", size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.8, size = 5) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    title = "N-mixture Estimated Rabbit Abundance Over Time (30-min Rule)"
  ) +
  theme_minimal(base_size = 14)

############################################################
# 4. RETRIEVAL-LEVEL N-MIXTURE FOR RABBIT (15-MIN RULE)
############################################################

retrieval_list_15_rabbit <- split(events_15, events_15$retrieval)

build_matrix_rabbit_15 <- function(df) {
  df %>%
    filter(label == "rabbit") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}

umf_list_15_rabbit <- lapply(retrieval_list_15_rabbit, function(df) {
  mat <- build_matrix_rabbit_15(df)
  if (nrow(mat) == 0) return(NULL)
  y <- as.matrix(t(mat))
  site_covs <- data.frame(camera = rownames(y))
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})

fit_list_15_rabbit <- lapply(umf_list_15_rabbit, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total_15_rabbit <- sapply(fit_list_15_rabbit, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})

############################################################
# 5. COMPARISON: 30-MIN VS 15-MIN FOR RABBIT
############################################################

df_compare_rabbit <- data.frame(
  retrieval = names(lambda_total_15_rabbit),
  lambda_30 = lambda_total_rabbit[names(lambda_total_15_rabbit)],
  lambda_15 = lambda_total_15_rabbit
)

print(df_compare_rabbit)

df_long_rabbit <- pivot_longer(
  df_compare_rabbit,
  cols = c(lambda_30, lambda_15),
  names_to = "window",
  values_to = "lambda"
)

df_long_rabbit$retrieval <- factor(
  df_long_rabbit$retrieval,
  levels = c("Original", "4-15-25", "6-6-25", "7-31-25", "10-10-25", "1-12-26")
)

ggplot(df_long_rabbit, aes(x = retrieval, y = lambda, color = window, group = window)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    color = "Independence Window",
    title = "Rabbit Abundance Over Time: 30-min vs 15-min Independence Rule"
  ) +
  theme_minimal(base_size = 14)
