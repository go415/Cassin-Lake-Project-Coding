

library(readxl)
library(tidyverse)
library(stringr)

# ================================
# 1. Define the exact sheet names
# ================================
file <- "Pictures Empties and Animal Identification Accuracy.xlsx"

picture_sheets <- c(
  "Original Pictures",
  "4-15 Pictures",
  "6-6 Pictures",
  "7-31 Pictures",
  "10-10 Pictures",
  "1-12-26 Pictures"
)

# ================================
# 2. Read and combine ALL sheets
# ================================
all_data <- map_df(picture_sheets, function(sheet_name) {
  
  df <- read_excel(file, sheet = sheet_name, skip = 1)
  
  df_clean <- df %>%
    select(starts_with("Camera")) %>%
    pivot_longer(
      cols = everything(),
      names_to = "camera",
      values_to = "value"
    ) %>%
    filter(str_detect(value, "/")) %>%   # keep only x/y rows
    separate(value, into = c("detections", "total_empty_candidates"),
             sep = "/", convert = TRUE) %>%
    mutate(sheet = sheet_name)
  
  return(df_clean)
})

# ================================
# 3. Summarize across ALL retrieval dates (overall per camera)
# ================================
overall_camera <- all_data %>%
  group_by(camera) %>%
  summarise(
    detections_in_empty = sum(detections, na.rm = TRUE),
    total_empty_candidates = sum(total_empty_candidates, na.rm = TRUE),
    prop_detected = detections_in_empty / total_empty_candidates
  ) %>%
  ungroup() %>%
  mutate(
    cam_num = as.numeric(str_extract(camera, "\\d+"))
  ) %>%
  arrange(cam_num) %>%
  mutate(camera = factor(camera, levels = camera))

# ================================
# 4. Plot overall per camera (PROPORTIONS)
# ================================
ggplot(overall_camera, aes(x = camera, y = prop_detected)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = round(prop_detected, 4)),   # <-- raw proportion
    vjust = -0.5,
    size = 4
  ) +
  expand_limits(y = max(overall_camera$prop_detected, na.rm = TRUE) * 1.15) +
  labs(
    title = "Proportion of Detections in Supposedly Empty Images (All Retrieval Dates)",
    x = "Camera",
    y = "Detections / Empty Candidates (Proportion)"
  ) +
  theme_minimal(base_size = 14)

overall_camera

# ================================
# 5. Summarize per retrieval date (overall across cameras)
# ================================
overall_by_sheet <- all_data %>%
  group_by(sheet) %>%
  summarise(
    detections_in_empty = sum(detections, na.rm = TRUE),
    total_empty_candidates = sum(total_empty_candidates, na.rm = TRUE),
    prop_detected = detections_in_empty / total_empty_candidates
  ) %>%
  ungroup()

# ================================
# 6. Plot per retrieval date (PROPORTIONS)
# ================================
ggplot(overall_by_sheet, aes(x = sheet, y = prop_detected)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = round(prop_detected, 4)),   # <-- raw proportion
    vjust = -0.5,
    size = 4
  ) +
  expand_limits(y = max(overall_by_sheet$prop_detected, na.rm = TRUE) * 1.15) +
  labs(
    title = "Proportion of Detections in Supposedly Empty Images (By Retrieval Date)",
    x = "Retrieval Date (Sheet)",
    y = "Detections / Empty Candidates (Proportion)"
  ) +
  theme_minimal(base_size = 14)

overall_by_sheet
