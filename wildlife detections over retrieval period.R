

# ============================================================
# STEP 1 — Load libraries
# ============================================================
library(tidyverse)
library(readxl)
library(lubridate)



# ============================================================
# STEP 2 — List all AddaxAI Excel files
# ============================================================
addax_files <- list.files(
  pattern = "\\.xlsx$",
  full.names = TRUE
)



# ============================================================
# STEP 3 — Extract Camera + Retrieval from filename
# ============================================================
extract_info <- function(filename) {
  base <- basename(filename)
  base <- str_remove(base, "\\.xlsx$")
  
  parts <- str_split(base, " - ", simplify = TRUE)
  
  camera <- str_remove(parts[1], "Camera ") %>% as.integer()
  retrieval <- parts[2]
  
  tibble(Camera = camera, Retrieval = retrieval)
}



# ============================================================
# STEP 4 — Read AddaxAI files (pick sheet with most rows)
# ============================================================
read_addax <- function(file) {
  info <- extract_info(file)
  sheets <- excel_sheets(file)
  
  candidates <- lapply(sheets, function(s) {
    df <- suppressWarnings(read_excel(file, sheet = s))
    df$sheet_name <- s
    df
  })
  
  df <- candidates[[which.max(sapply(candidates, nrow))]]
  
  df <- df %>% mutate(across(everything(), as.character))
  
  df %>%
    mutate(
      Camera = info$Camera,
      Retrieval = info$Retrieval
    )
}

addax_all <- map_df(addax_files, read_addax)



# ============================================================
# STEP 5 — Clean AddaxAI species + timestamps
# ============================================================
addax_all <- addax_all %>%
  rename(Species = label) %>%
  mutate(
    confidence = as.numeric(confidence),
    Timestamp = case_when(
      !is.na(DateTimeOriginal) ~ ymd_hms(DateTimeOriginal, quiet = TRUE),
      !is.na(DateTime) ~ ymd_hms(DateTime, quiet = TRUE),
      TRUE ~ NA
    )
  )



# ============================================================
# STEP 6 — COMPLETE species map (AddaxAI → matrix)
# ============================================================
species_map <- c(
  "person"             = "Person",
  "human"              = "Person",
  "rabbit"             = "Rabbit",
  "bobcat"             = "Bobcat",
  "coyote"             = "Coyote",
  "boar"               = "Boar",
  "deer"               = "Deer",
  "raccoon"            = "Raccoon",
  "opossum"            = "Opossum",
  "fox"                = "Fox",
  "dog"                = "Dog",
  "bird"               = "Bird",
  "unidentified animal"= "Unidentified",
  "unknown"            = "Unidentified",
  "other"              = "Other/Object",
  "cow"                = "Cow",
  "cougar"             = "Cougar",
  "cat"                = "Cat",
  "corvid"             = "Corvid",
  "beaver"             = "Beaver",
  "rodent"             = "Rodent",
  "skunk"              = "Skunk",
  "squirrel"           = "Squirrel",
  "weasel"             = "Weasel",
  "reptile"            = "Reptile",
  "raptor"             = "Raptor",
  "armadillo"          = "Armadillo"
)

addax_all <- addax_all %>%
  mutate(
    Species = tolower(Species),
    Species = species_map[Species],
    Species = str_to_title(Species)
  ) %>%
  filter(!is.na(Species))



# ============================================================
# STEP 7 — List Picture Matrix files
# ============================================================
matrix_files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)



# ============================================================
# STEP 8 — Normalize matrix species names
# ============================================================
clean_matrix_species <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- recode(x,
              "unidentified animal" = "unidentified",
              "unknown" = "unidentified",
              "other" = "other/object"
  )
  x
}



# ============================================================
# STEP 9 — Compute precision per species
# ============================================================
precision_list <- list()

for (file in matrix_files) {
  sheets <- excel_sheets(file)
  
  for (sheet in sheets) {
    df <- suppressWarnings(read_excel(file, sheet = sheet))
    if (ncol(df) < 2) next
    
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    df[[1]] <- clean_matrix_species(df[[1]])
    names(df) <- c("actual", clean_matrix_species(names(df)[-1]))
    
    actual <- df$actual
    predicted <- names(df)[-1]
    
    df_num <- df %>% mutate(across(-actual, ~ suppressWarnings(as.numeric(.))))
    df_num[is.na(df_num)] <- 0
    
    cm <- as.matrix(df_num[, -1, drop = FALSE])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    common_species <- intersect(actual, predicted)
    
    for (sp in common_species) {
      TP <- cm[sp, sp]
      FP <- sum(cm[, sp]) - TP
      if ((TP + FP) > 0) {
        precision_list[[sp]] <- c(precision_list[[sp]], TP / (TP + FP))
      }
    }
  }
}

precision_table <- tibble(
  Species = names(precision_list),
  Precision = sapply(precision_list, function(x) mean(x, na.rm = TRUE))
)



# ============================================================
# STEP 10 — Clean precision table and add Unidentified = 0
# ============================================================
precision_table <- precision_table %>%
  mutate(Species = str_to_title(Species)) %>%
  group_by(Species) %>%
  summarise(Precision = mean(Precision, na.rm = TRUE), .groups = "drop") %>%
  add_row(Species = "Unidentified", Precision = 0) %>%
  distinct(Species, .keep_all = TRUE)



# ============================================================
# STEP 11 — Join precision to AddaxAI detections
# ============================================================
addax_corrected <- addax_all %>%
  left_join(precision_table, by = "Species") %>%
  mutate(
    Precision = ifelse(is.na(Precision), 0, Precision),
    Corrected = Precision
  )



# ============================================================
# STEP 12 — Aggregate corrected detections by retrieval period
# ============================================================
corrected_time <- addax_corrected %>%
  group_by(Retrieval) %>%
  summarise(Corrected_Detections = sum(Corrected)) %>%
  arrange(Retrieval)



# ============================================================
# STEP 13 — Force chronological order for plotting
# ============================================================
corrected_time$Retrieval <- factor(
  corrected_time$Retrieval,
  levels = c(
    "Original Pictures",
    "4-15-25",
    "6-6-25",
    "7-31-25",
    "10-10-25",
    "1-12-26"
  )
)



# ============================================================
# STEP 14 — Plot corrected detections over time
# ============================================================
ggplot(corrected_time, aes(x = Retrieval, y = Corrected_Detections, group = 1)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Corrected Wildlife Detections Over Retrieval Periods",
    x = "Retrieval Period",
    y = "Precision‑Corrected Detections"
  )
