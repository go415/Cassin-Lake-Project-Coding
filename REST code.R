# ================================
# Shortened REST model (all species)
# 60-second independence threshold
# ================================

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(tidyr)

# ----------------
# USER PARAMETERS
# ----------------

# Independence threshold in seconds
independence_threshold <- 60

# Name of the sheet in each AddaxAI file (change if needed)
sheet_name <- 1   # or "Sheet1" if they are named

# Column names in AddaxAI output (change if yours differ)
col_species   <- "label"
col_datetime  <- "DateTimeOriginal"

# ---------------
# LIST ALL FILES
# ---------------

# Assumes you already set the working directory in RStudio
# setwd("...your folder...")

files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

# Helper: extract camera number from filename like "Camera 7 - 4-15-25.xlsx"
extract_camera_id <- function(path) {
  fname <- basename(path)
  cam   <- str_extract(fname, "Camera\\s*\\d+")
  cam_id <- str_extract(cam, "\\d+")
  as.integer(cam_id)
}

# Helper: extract retrieval label (date or "Original Pictures")
extract_retrieval_label <- function(path) {
  fname <- tools::file_path_sans_ext(basename(path))
  # Split on " - " and take the second part
  parts <- str_split(fname, " - ", simplify = TRUE)
  if (ncol(parts) >= 2) {
    parts[, 2]
  } else {
    NA_character_
  }
}

# -----------------------------
# READ AND COMBINE ALL DETECTIONS
# -----------------------------

read_addax_file <- function(path) {
  cam_id   <- extract_camera_id(path)
  retr_lab <- extract_retrieval_label(path)
  
  df <- suppressMessages(read_excel(path, sheet = sheet_name))
  
  # Keep only needed columns if they exist
  needed_cols <- c(col_species, col_datetime)
  missing_cols <- setdiff(needed_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("File ", basename(path), " is missing columns: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  df %>%
    transmute(
      Camera      = cam_id,
      Retrieval   = retr_lab,
      Species_raw = as.character(.data[[col_species]]),
      DateTimeRaw = as.character(.data[[col_datetime]])
    )
}

all_detections_raw <- map_df(files, read_addax_file)

# Drop NULLs if any file failed
all_detections_raw <- all_detections_raw %>% filter(!is.na(Camera))

# -----------------------------
# CLEAN SPECIES + TIMESTAMPS
# -----------------------------

detections <- all_detections_raw %>%
  mutate(
    Species = Species_raw %>%
      tolower() %>%
      str_trim() %>%
      str_replace_all("_", " "),
    DateTime = ymd_hms(DateTimeRaw, quiet = TRUE)
  ) %>%
  filter(!is.na(DateTime)) %>%
  select(Camera, Retrieval, Species, DateTime)

# -----------------------------
# DEFINE INDEPENDENT EVENTS
# -----------------------------

# For each Camera–Species, sort by time and start a new event
# when the gap from the previous detection > independence_threshold

detections_events <- detections %>%
  arrange(Camera, Species, DateTime) %>%
  group_by(Camera, Species) %>%
  mutate(
    TimeDiff = as.numeric(difftime(DateTime, lag(DateTime), units = "secs")),
    NewEvent = if_else(is.na(TimeDiff) | TimeDiff > independence_threshold, 1L, 0L),
    EventID  = cumsum(replace_na(NewEvent, 0L))
  ) %>%
  ungroup()

# -----------------------------
# COMPUTE STAYING TIME (τ) PER EVENT
# -----------------------------

# For each Camera–Species–EventID, staying time is
# (last detection time - first detection time) in seconds.
# If only one detection in the event, set staying time to independence_threshold
# as a conservative estimate (or 1 second if you prefer).

events_summary <- detections_events %>%
  group_by(Camera, Species, EventID) %>%
  summarise(
    StartTime = min(DateTime),
    EndTime   = max(DateTime),
    Duration  = as.numeric(difftime(EndTime, StartTime, units = "secs")),
    .groups   = "drop"
  ) %>%
  mutate(
    Duration = if_else(Duration <= 0, independence_threshold, Duration)
  )

# -----------------------------
# COMPUTE λ (ENCOUNTER RATE) AND τ (MEAN STAYING TIME)
# -----------------------------

# Effort time per camera: from first to last detection (in hours)
camera_effort <- detections %>%
  group_by(Camera) %>%
  summarise(
    EffortStart = min(DateTime),
    EffortEnd   = max(DateTime),
    EffortHours = as.numeric(difftime(EffortEnd, EffortStart, units = "hours")),
    .groups     = "drop"
  )

# Join effort to events and compute λ and τ per Camera–Species
rest_summary <- events_summary %>%
  group_by(Camera, Species) %>%
  summarise(
    Events       = n(),
    MeanDuration = mean(Duration, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  left_join(camera_effort, by = "Camera") %>%
  mutate(
    Lambda = Events / EffortHours,          # events per hour
    Tau    = MeanDuration,                  # seconds
    RelDensity = Lambda / Tau               # proportional density (shortened REST)
  )

# -----------------------------
# CLEAN OUTPUT TABLE
# -----------------------------

rest_results <- rest_summary %>%
  select(Camera, Species, Events, EffortHours, Lambda, Tau, RelDensity) %>%
  arrange(Camera, desc(RelDensity))

# View results
print(rest_results)

# Optionally write to Excel/CSV
# write.csv(rest_results, "REST_relative_density_all_species.csv", row.names = FALSE)


write.csv(rest_results, "REST_relative_density_all_species.csv", row.names = FALSE)



library(ggplot2)

ggplot(rest_results, aes(x = reorder(Species, RelDensity), y = RelDensity, fill = Species)) +
  geom_col() +
  facet_wrap(~ Camera, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Relative Density by Species and Camera",
    x = "Species",
    y = "Relative Density (λ / τ)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(rest_results, aes(x = factor(Camera), y = Species, fill = RelDensity)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Species Relative Density Across Cameras",
    x = "Camera",
    y = "Species",
    fill = "RelDensity"
  ) +
  theme_minimal()


ggplot(rest_results, aes(x = reorder(Species, -RelDensity), y = RelDensity)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  facet_wrap(~ Camera, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Ranked Relative Density per Camera",
    x = "Species (ranked)",
    y = "Relative Density"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ================================
# OVERALL RELATIVE DENSITY (ALL CAMERAS COMBINED)
# ================================

overall_results <- rest_results %>%
  group_by(Species) %>%
  summarise(
    TotalEvents   = sum(Events, na.rm = TRUE),
    TotalEffort   = sum(EffortHours, na.rm = TRUE),
    MeanTau       = mean(Tau, na.rm = TRUE),   # average staying time across cameras
    LambdaOverall = TotalEvents / TotalEffort, # events per hour across all cameras
    RelDensityOverall = LambdaOverall / MeanTau,
    .groups = "drop"
  ) %>%
  arrange(desc(RelDensityOverall))

print(overall_results)

write.csv(overall_results, "REST_relative_density_all_species_all_cameras.csv", row.names = FALSE)


ggplot(overall_results, aes(x = reorder(Species, RelDensityOverall), 
                            y = RelDensityOverall, fill = Species)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Overall Relative Density Across All Cameras",
    x = "Species",
    y = "Relative Density (λ / τ)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")





# ================================
# MOVEMENT PATTERNS (ACTIVITY CURVES)
# ================================

library(ggplot2)
library(dplyr)
library(lubridate)

# Top 5 species
top_species <- c("boar", "bobcat", "rabbit", "raccoon", "opossum")

df_top <- detections %>%
  filter(Species %in% top_species) %>%
  mutate(
    hour = hour(DateTime) + minute(DateTime)/60 + second(DateTime)/3600
  )

# Plot with consistent y-axis (0 to 0.15)
ggplot(df_top, aes(x = hour)) +
  geom_density(fill = "steelblue", alpha = 0.4, adjust = 1.2) +
  facet_wrap(~ Species, ncol = 2, scales = "fixed") +
  scale_x_continuous(breaks = seq(0, 24, 6), limits = c(0, 24)) +
  ylim(0, 0.10) +
  labs(
    title = "Activity Patterns (0–24 hours)",
    x = "Time of Day (hours)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)







