
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(unmarked)

# Import AddaxAI Excel files and keep only label + timestamp.
files <- list.files(getwd(), pattern = "\\.xlsx$", full.names = TRUE)

raw_list <- lapply(files, function(f) {
  df <- read_excel(f, sheet = 1)
  
  # Standardize column names
  names(df) <- tolower(names(df))
  
  # Keep only the columns we need
  keep_cols <- c("label", "datetimeoriginal")
  df <- df[, intersect(names(df), keep_cols)]
  
  # Convert datetimeoriginal to character so bind_rows won't fail
  if ("datetimeoriginal" %in% names(df)) {
    df$datetimeoriginal <- as.character(df$datetimeoriginal)
  }
  
  # Add filename
  df$source_file <- basename(f)
  
  df
})

raw <- bind_rows(raw_list)


# Parse timestamps into a usable datetime format
raw$timestamp <- parse_date_time(
  raw$datetimeoriginal,
  orders = c("Y-m-d H:M:S", "d/m/y H:M:S", "Y/m/d H:M:S")
)


# Extract camera number and retrieval label from filenames
raw <- raw %>%
  mutate(
    camera = str_extract(source_file, "Camera\\s*\\d+"),
    camera = str_extract(camera, "\\d+"),
    retrieval = str_extract(source_file, "\\d{1,2}-\\d{1,2}-\\d{2}|Original")
  )


# Assign capture periods based on retrieval order
raw <- raw %>%
  group_by(camera) %>%
  mutate(capture_period = dense_rank(retrieval)) %>%
  ungroup()


# Apply 30‑minute independence rule
raw <- raw %>%
  arrange(camera, label, timestamp) %>%
  group_by(camera, label) %>%
  mutate(
    time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "mins")),
    independent = ifelse(is.na(time_diff) | time_diff > 30, 1, 0)
  ) %>%
  ungroup()

events <- raw %>% filter(independent == 1)


# Count independent events per camera × capture period × species
counts <- events %>%
  group_by(camera, capture_period, label) %>%
  summarise(count = n(), .groups = "drop")


# Build the boar detection matrix
boar_counts <- counts %>% filter(label == "boar")

boar_matrix <- boar_counts %>%
  select(camera, capture_period, count) %>%
  pivot_wider(
    names_from = capture_period,
    values_from = count,
    values_fill = 0
  ) %>%
  arrange(as.numeric(camera))


# Prepare boar matrix for unmarked
y_boar <- as.matrix(boar_matrix[ , -1])
site_covs <- data.frame(camera = boar_matrix$camera)

umf_boar <- unmarkedFramePCount(
  y = y_boar,
  siteCovs = site_covs
)


# Fit the N‑mixture model for boar
fit_boar <- pcount(~1 ~1, data = umf_boar, K = 200)
summary(fit_boar)


# -----------------------------------------
# 0. Fit the null N-mixture model (~1 ~ 1)
# -----------------------------------------
fit_boar_null <- pcount(~1 ~1, data = umf_boar, K = 200)

summary(fit_boar_null)


# -----------------------------------------
# 1. Extract log-scale abundance (λ on log scale)
# -----------------------------------------
log_lambda <- coef(fit_boar_null, type = "state")

# Convert to real abundance
lambda <- exp(log_lambda)


# -----------------------------------------
# 2. Extract logit-scale detection probability (p on logit scale)
# -----------------------------------------
logit_p <- coef(fit_boar_null, type = "det")

# Convert to probability
p <- 1 / (1 + exp(-logit_p))


# -----------------------------------------
# 3. Print results cleanly
# -----------------------------------------
cat("Global abundance estimate (lambda):", lambda, "\n")
cat("Detection probability (p):", p, "\n")


# -----------------------------------------
# 4. Optional: rounded values for readability
# -----------------------------------------
cat("Rounded lambda:", round(lambda, 2), "\n")
cat("Rounded p:", round(p, 3), "\n")






# Fit the model with camera as a site covariate
fit_boar_camera <- pcount(~1 ~ camera, data = umf_boar, K = 200)

# Extract the coefficients for abundance (on log scale)
log_lambda_camera <- coef(fit_boar_camera, type = "state")

# Convert log-scale λ to real abundance
lambda_camera <- exp(log_lambda_camera)

# Put into a clean data frame
camera_abundance <- data.frame(
  camera = boar_matrix$camera,
  lambda = lambda_camera
)

camera_abundance



# Fit the model with camera as a factor
fit_boar_camera <- pcount(~1 ~ camera, data = umf_boar, K = 200)

# Get per-camera abundance estimates
lambda_df <- predict(fit_boar_camera, type = "state")

# Add camera IDs
lambda_df$camera <- boar_matrix$camera

# Clean table
camera_abundance <- lambda_df[, c("camera", "Predicted", "lower", "upper")]

camera_abundance


library(ggplot2)

# Ensure camera is ordered numerically
camera_abundance$camera <- factor(
  as.numeric(as.character(camera_abundance$camera)),
  levels = sort(as.numeric(as.character(camera_abundance$camera)))
)

# Add a whole-number label column
camera_abundance$label <- round(camera_abundance$Predicted, 0)

ggplot(camera_abundance, aes(x = camera, y = Predicted)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_text(aes(label = label),
            vjust = -0.5,
            size = 4) +
  labs(
    x = "Camera",
    y = "Predicted Abundance (λ)",
    title = "N-mixture Predicted Boar Abundance per Camera"
  ) +
  theme_minimal(base_size = 14) +
  expand_limits(y = max(camera_abundance$upper) * 1.1)



trend <- events %>%
  group_by(retrieval) %>%
  summarise(total_boar = sum(label == "boar"))


ggplot(trend, aes(x = retrieval, y = total_boar)) +
  geom_line(group = 1, color = "darkred", size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Corrected Boar Events",
    title = "Boar Activity Over Time"
  ) +
  theme_minimal(base_size = 14)



retrieval_list <- split(events, events$retrieval)


build_matrix <- function(df) {
  df %>%
    filter(label == "boar") %>%
    group_by(camera) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = camera,
      values_from = count,
      values_fill = 0
    )
}



umf_list <- lapply(retrieval_list, function(df) {
  
  mat <- build_matrix(df)
  
  # If no boar detected in this retrieval, skip
  if (nrow(mat) == 0) return(NULL)
  
  y <- as.matrix(t(mat))   # cameras as rows
  site_covs <- data.frame(camera = rownames(y))
  
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})


fit_list <- lapply(umf_list, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})

lambda_total <- sapply(fit_list, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})


retrieval_names <- names(retrieval_list)

df_plot <- data.frame(
  retrieval = retrieval_names,
  lambda = lambda_total
)

ggplot(df_plot, aes(x = retrieval, y = lambda, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    title = "N-mixture Estimated Boar Abundance Over Time"
  ) +
  theme_minimal(base_size = 14)


df_plot$retrieval <- factor(
  df_plot$retrieval,
  levels = c(
    "Original",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)

df_plot$label <- round(df_plot$lambda, 0)

ggplot(df_plot, aes(x = retrieval, y = lambda, group = 1)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label), vjust = -0.8, size = 5) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    title = "N-mixture Estimated Boar Abundance Over Time"
  ) +
  theme_minimal(base_size = 14)











############################################################
# 15-MINUTE INDEPENDENCE RULE + N-MIXTURE COMPARISON
############################################################

# 1. Apply 15-minute independence rule
raw <- raw %>%
  arrange(camera, label, timestamp) %>%
  group_by(camera, label) %>%
  mutate(
    time_diff_15 = as.numeric(difftime(timestamp, lag(timestamp), units = "mins")),
    independent_15 = ifelse(is.na(time_diff_15) | time_diff_15 > 15, 1, 0)
  ) %>%
  ungroup()

events_15 <- raw %>% filter(independent_15 == 1)


# 2. Split into retrieval periods
retrieval_list_15 <- split(events_15, events_15$retrieval)


# 3. Reuse your existing build_matrix() function
# (No changes needed — it works perfectly for 15-min data)


# 4. Build unmarked frames for each retrieval (15-min)
umf_list_15 <- lapply(retrieval_list_15, function(df) {
  
  mat <- build_matrix(df)
  
  if (nrow(mat) == 0) return(NULL)
  
  y <- as.matrix(t(mat))   # cameras as rows
  site_covs <- data.frame(camera = rownames(y))
  
  unmarkedFramePCount(y = y, siteCovs = site_covs)
})


# 5. Fit N-mixture models for each retrieval (15-min)
fit_list_15 <- lapply(umf_list_15, function(umf) {
  if (is.null(umf)) return(NULL)
  pcount(~1 ~1, data = umf, K = 200)
})


# 6. Extract λ for each retrieval (15-min)
lambda_total_15 <- sapply(fit_list_15, function(fit) {
  if (is.null(fit)) return(NA)
  sum(exp(coef(fit, type = "state")))
})


# 7. Build comparison table (30-min vs 15-min)
df_compare <- data.frame(
  retrieval = names(lambda_total_15),
  lambda_30 = lambda_total[names(lambda_total_15)],
  lambda_15 = lambda_total_15
)

print(df_compare)


# 8. OPTIONAL: Plot comparison
library(ggplot2)
library(tidyr)

df_long <- pivot_longer(
  df_compare,
  cols = c(lambda_30, lambda_15),
  names_to = "window",
  values_to = "lambda"
)




df_long$retrieval <- factor(
  df_long$retrieval,
  levels = c(
    "Original",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)



ggplot(df_long, aes(x = retrieval, y = lambda, color = window, group = window)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    x = "Retrieval Period",
    y = "Estimated Abundance (λ)",
    color = "Independence Window",
    title = "Boar Abundance Over Time: 30-min vs 15-min Independence Rule"
  ) +
  theme_minimal(base_size = 14)






