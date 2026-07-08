library(tidyverse)
library(readxl)

matrix_files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

clean_matrix_species <- function(x) {
  x <- trimws(tolower(x))
  recode(x,
         "unidentified animal" = "unidentified",
         "unknown" = "unidentified",
         "other" = "other/object"
  )
}

all_cm <- list()
all_species <- c()

# ------------------------------------------------------------
# FIRST PASS — Collect all species names across all matrices
# ------------------------------------------------------------
for (file in matrix_files) {
  sheets <- excel_sheets(file)
  
  for (sheet in sheets) {
    df <- suppressWarnings(read_excel(file, sheet = sheet))
    if (ncol(df) < 2) next
    
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    df[[1]] <- clean_matrix_species(df[[1]])
    names(df) <- c("actual", clean_matrix_species(names(df)[-1]))
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    all_species <- union(all_species, actual)
    all_species <- union(all_species, predicted)
  }
}

all_species <- sort(all_species)

# ------------------------------------------------------------
# SECOND PASS — Build aligned matrices
# ------------------------------------------------------------
for (file in matrix_files) {
  sheets <- excel_sheets(file)
  
  for (sheet in sheets) {
    df <- suppressWarnings(read_excel(file, sheet = sheet))
    if (ncol(df) < 2) next
    
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    df[[1]] <- clean_matrix_species(df[[1]])
    names(df) <- c("actual", clean_matrix_species(names(df)[-1]))
    
    df_num <- df %>% mutate(across(-actual, ~ suppressWarnings(as.numeric(.))))
    df_num[is.na(df_num)] <- 0
    
    cm <- as.matrix(df_num[, -1])
    rownames(cm) <- df$actual
    colnames(cm) <- names(df_num)[-1]
    
    # Expand to full species list
    expanded <- matrix(0, nrow = length(all_species), ncol = length(all_species))
    rownames(expanded) <- all_species
    colnames(expanded) <- all_species
    
    expanded[rownames(cm), colnames(cm)] <- cm
    
    all_cm[[paste(file, sheet)]] <- expanded
  }
}

# ------------------------------------------------------------
# Combine all aligned matrices
# ------------------------------------------------------------
combined_cm <- Reduce("+", all_cm)



#----------------------------------------#


precision_table <- tibble(
  Species = intersect(rownames(combined_cm), colnames(combined_cm)),
  Precision = map_dbl(Species, function(sp) {
    TP <- combined_cm[sp, sp]
    FP <- sum(combined_cm[, sp]) - TP
    if ((TP + FP) == 0) return(0)
    TP / (TP + FP)
  })
)


#________________________________________use other folder with AddaxAI csv files#

addax_files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

extract_info <- function(filename) {
  base <- basename(filename)
  base <- str_remove(base, "\\.xlsx$")
  parts <- str_split(base, " - ", simplify = TRUE)
  tibble(Camera = parts[1], Retrieval = parts[2])
}

read_addax <- function(file) {
  info <- extract_info(file)
  sheets <- excel_sheets(file)
  
  df <- suppressWarnings(read_excel(file, sheet = sheets[1]))
  
  # ---- FIX: Convert ALL columns to character safely ----
  df <- df %>%
    mutate(across(everything(), ~ as.character(.)))
  
  df %>% mutate(Camera = info$Camera, Retrieval = info$Retrieval)
}




addax_all <- map_df(addax_files, read_addax)


#-----------------------------------------------------#

species_map <- c(
  "person"="Person","rabbit"="Rabbit","bobcat"="Bobcat","coyote"="Coyote",
  "boar"="Boar","deer"="Deer","raccoon"="Raccoon","opossum"="Opossum",
  "fox"="Fox","dog"="Dog","bird"="Bird","other"="Other/Object",
  "unidentified"="Unidentified","rodent"="Rodent","skunk"="Skunk",
  "squirrel"="Squirrel","beaver"="Beaver","corvid"="Corvid"
)

addax_all <- addax_all %>%
  mutate(
    Species = tolower(label),
    Species = species_map[Species],
    Species = str_to_title(Species)
  ) %>%
  filter(!is.na(Species))

precision_table <- precision_table %>%
  mutate(Species = str_to_title(Species)) %>%
  add_row(Species = "Unidentified", Precision = 0) %>%
  distinct(Species, .keep_all = TRUE)

addax_corrected <- addax_all %>%
  left_join(precision_table, by = "Species") %>%
  mutate(
    Precision = ifelse(is.na(Precision), 0, Precision),
    Corrected = Precision
  )

effectiveness_curve <- addax_corrected %>%
  group_by(Retrieval) %>%
  summarize(MeanPrecision = mean(Corrected))

#____________________________________________________________#

effectiveness_curve <- addax_corrected %>%
  group_by(Retrieval) %>%
  summarize(Corrected_Detections = sum(Corrected, na.rm = TRUE))


# ============================================================
# STEP 14 — Order retrieval periods chronologically
# ============================================================

effectiveness_curve <- effectiveness_curve %>%
  mutate(
    Retrieval = factor(
      Retrieval,
      levels = c("Original Pictures", "4-15-25", "6-6-25", "7-31-25", "10-10-25", "1-12-26")
    )
  ) %>%
  arrange(Retrieval)

# ============================================================
# STEP 15 — Flip x-axis (oldest to newest)
# ============================================================

effectiveness_curve <- effectiveness_curve %>%
  mutate(Retrieval = fct_rev(Retrieval))

#-----------------------------------------------------------#



ggplot(effectiveness_curve, aes(x = Retrieval)) +
  geom_line(aes(y = Corrected_Detections), size = 1.2, color = "steelblue") +
  geom_point(aes(y = Corrected_Detections), size = 3, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "AddaxAI Model Effectiveness Across Retrieval Periods",
    x = "Retrieval Period",
    y = "Precision‑Corrected Detections"
  )

# ============================================================
# STEP 16 — Plot raw vs corrected detections
# ============================================================

ggplot(effectiveness_curve, aes(x = Retrieval)) +
  geom_line(aes(y = Raw_Detections, color = "Raw"), size = 1.1) +
  geom_point(aes(y = Raw_Detections, color = "Raw"), size = 3) +
  geom_line(aes(y = Corrected_Detections, color = "Corrected"), size = 1.1) +
  geom_point(aes(y = Corrected_Detections, color = "Corrected"), size = 3) +
  scale_color_manual(values = c("Raw" = "gray40", "Corrected" = "steelblue")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Raw vs Precision‑Corrected AddaxAI Detections",
    x = "Retrieval Period",
    y = "Detections",
    color = "Detection Type"
  )


# ============================================================
# STEP 17 — Add bootstrapped confidence intervals
# ============================================================

library(boot)

bootstrap_corrected <- function(data, i) {
  sum(data$Corrected[i])
}

ci_df <- addax_corrected %>%
  group_by(Retrieval) %>%
  group_modify(~{
    b <- boot(.x, bootstrap_corrected, R = 1000)
    tibble(
      Corrected_mean = mean(.x$Corrected),
      CI_low = boot.ci(b, type = "perc")$percent[4],
      CI_high = boot.ci(b, type = "perc")$percent[5]
    )
  })


# ============================================================
# STEP 19 — Species-specific effectiveness curves
# ============================================================

species_curve <- addax_corrected %>%
  group_by(Retrieval, Species) %>%
  summarise(Corrected = sum(Corrected), .groups = "drop") %>%
  mutate(Retrieval = factor(Retrieval, levels = levels(effectiveness_curve$Retrieval)))

ggplot(species_curve, aes(x = Retrieval, y = Corrected, group = Species, color = Species)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right") +
  labs(
    title = "Species‑Specific AddaxAI Effectiveness",
    x = "Retrieval Period",
    y = "Corrected Detections"
  )


#--------------------------------------#

addax_all <- addax_all %>%
  mutate(
    DateTimeOriginal = ymd_hms(DateTimeOriginal, quiet = TRUE)
  )

#-----------------------------------------#

# ============================================================
# STEP B — Exclude detections before 2022
# ============================================================

addax_all <- addax_all %>%
  filter(DateTimeOriginal >= ymd("2022-01-01"))

# ============================================================
# STEP C — Extract Year-Month
# ============================================================

addax_all <- addax_all %>%
  mutate(
    Month = floor_date(DateTimeOriginal, "month")
  )


addax_corrected <- addax_corrected %>%
  mutate(
    DateTimeOriginal = ymd_hms(DateTimeOriginal, quiet = TRUE),
    Month = floor_date(DateTimeOriginal, "month")
  ) %>%
  filter(
    DateTimeOriginal >= ymd("2024-03-01"),
    DateTimeOriginal <= ymd("2026-01-26")   # adjust upper bound if needed
  )


# ============================================================
# STEP D — Compute corrected detections by month
# ============================================================

monthly_effectiveness <- addax_corrected %>%
  filter(!is.na(Month)) %>%
  filter(DateTimeOriginal >= ymd("2022-01-01")) %>%   # <-- safety filter again
  group_by(Month) %>%
  summarise(
    Raw_Detections = n(),
    Corrected_Detections = sum(Corrected)
  ) %>%
  arrange(Month)



# ============================================================
# STEP E — Plot month-by-month AddaxAI effectiveness
# ============================================================

ggplot(monthly_effectiveness, aes(x = Month)) +
  geom_line(aes(y = Corrected_Detections), size = 1.2, color = "steelblue") +
  geom_point(aes(y = Corrected_Detections), size = 3, color = "steelblue") +
  theme_minimal(base_size = 14) +
  labs(
    title = "AddaxAI Model Effectiveness (Month-by-Month, Post-2022 Only)",
    x = "Month",
    y = "Precision‑Corrected Detections"
  )


# ============================================================
# STEP F — Raw vs Corrected (monthly)
# ============================================================

ggplot(monthly_effectiveness, aes(x = Month)) +
  geom_line(aes(y = Raw_Detections, color = "Raw"), size = 1.1) +
  geom_point(aes(y = Raw_Detections, color = "Raw"), size = 3) +
  geom_line(aes(y = Corrected_Detections, color = "Corrected"), size = 1.1) +
  geom_point(aes(y = Corrected_Detections, color = "Corrected"), size = 3) +
  scale_color_manual(values = c("Raw" = "gray40", "Corrected" = "steelblue")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Raw vs Precision‑Corrected AddaxAI Detections (Monthly, Post-2022)",
    x = "Month",
    y = "Detections",
    color = "Detection Type"
  )


addax_corrected <- addax_corrected %>%
  mutate(
    DateTimeOriginal = ymd_hms(DateTimeOriginal, quiet = TRUE),
    Month = floor_date(DateTimeOriginal, "month")
  )


monthly_effectiveness <- addax_corrected %>%
  mutate(
    DateTimeOriginal = ymd_hms(DateTimeOriginal, quiet = TRUE),
    Month = as.Date(floor_date(DateTimeOriginal, "month"))
  ) %>%
  filter(!is.na(Month)) %>%
  group_by(Month) %>%
  summarise(
    Raw_Detections = n(),
    Corrected_Detections = sum(Corrected)
  ) %>%
  arrange(Month)



ggplot(monthly_effectiveness, aes(x = Month)) +
  geom_line(aes(y = Corrected_Detections), size = 1.2, color = "steelblue") +
  geom_point(aes(y = Corrected_Detections), size = 3, color = "steelblue") +
  scale_x_date(
    limits = range(monthly_effectiveness$Month),
    date_breaks = "2 months",
    date_labels = "%m-%y"   # <-- Windows-safe
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "AddaxAI Model Effectiveness (Month-by-Month)",
    x = "Month",
    y = "Precision Corrected Detections"
  )



monthly_effectiveness <- monthly_effectiveness %>%
  mutate(
    Effectiveness_Percent = (Corrected_Detections / Raw_Detections) * 100
  )




ggplot(monthly_effectiveness, aes(x = Month, y = Effectiveness_Percent)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  scale_x_date(
    limits = range(monthly_effectiveness$Month),
    date_breaks = "2 months",
    date_labels = "%m-%y"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "AddaxAI Monthly Precision (%)",
    x = "Month",
    y = "Monthly Precision (%)"
  )


ggplot(monthly_effectiveness, aes(x = Month, y = Effectiveness_Percent)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  scale_x_date(
    limits = range(monthly_effectiveness$Month),
    date_breaks = "2 months",
    date_labels = "%m-%y"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "AddaxAI Monthly Precision (%)",
    x = "Month",
    y = "Monthly Precision (%)"
    
  )





ggplot(monthly_effectiveness, aes(x = Month, y = Effectiveness_Percent)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(
    aes(label = sprintf("%d%%", round(Effectiveness_Percent))),
    vjust = -0.8,
    size = 4,
    color = "black"
  ) +
  scale_x_date(
    limits = range(monthly_effectiveness$Month),
    date_breaks = "2 months",
    date_labels = "%m-%y"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "AddaxAI Monthly Precision (%)",
    x = "Month",
    y = "Monthly Precision (%)"
  )
