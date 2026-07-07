library(readxl)
library(tidyverse)
library(pheatmap)

# ---------------------------------------------------------
# LIST ALL EXCEL FILES YOU WANT TO INCLUDE
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# FIRST PASS: COLLECT ALL SPECIES NAMES ACROSS ALL FILES
# ---------------------------------------------------------

all_species <- c()

for (file in files) {
  sheets <- excel_sheets(file)
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    
    # Skip empty sheets
    if (ncol(df) == 0) next
    
    # Remove Total column
    if ("Total" %in% names(df)) {
      df <- df %>% select(-Total)
    }
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    all_species <- union(all_species, actual)
    all_species <- union(all_species, predicted)
  }
}

# Alphabetize species
all_species <- sort(all_species)

# ---------------------------------------------------------
# SECOND PASS: BUILD AND ALIGN MATRICES ACROSS ALL FILES
# ---------------------------------------------------------

combined_cm <- matrix(0, nrow = length(all_species), ncol = length(all_species))
rownames(combined_cm) <- all_species
colnames(combined_cm) <- all_species

for (file in files) {
  sheets <- excel_sheets(file)
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    
    # Skip empty sheets
    if (ncol(df) == 0) next
    
    if ("Total" %in% names(df)) {
      df <- df %>% select(-Total)
    }
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>%
      mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    # Expand to full species list
    expanded <- matrix(0, nrow = length(all_species), ncol = length(all_species))
    rownames(expanded) <- all_species
    colnames(expanded) <- all_species
    
    expanded[rownames(cm), colnames(cm)] <- cm
    
    # Add to global matrix
    combined_cm <- combined_cm + expanded
  }
}

# ---------------------------------------------------------
# BUILD MISCLASSIFICATION MATRIX
# ---------------------------------------------------------

cm_mis <- combined_cm
diag(cm_mis) <- 0

# Normalize rows
cm_prop <- cm_mis / rowSums(cm_mis + 1e-9)

# Remove species with zero actual detections
rows_to_keep <- rowSums(combined_cm) > 0
cm_prop_filtered <- cm_prop[rows_to_keep, ]

# Reverse x-axis (predicted)
cm_prop_final <- cm_prop_filtered[, rev(colnames(cm_prop_filtered))]

# ---------------------------------------------------------
# FINAL HEATMAP
# ---------------------------------------------------------


pheatmap(
  cm_prop_final,
  color = colorRampPalette(c("white", "red"))(200),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize = 10,
  main = "Combined Misclassification Heatmap Original Pictures"
)



pheatmap(
  cm_prop_final,
  color = colorRampPalette(c("white", "#FF9999", "#FF0000", "#990000"))(200),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize = 10,
  main = "Combined Misclassification Heatmap Overall Pictures",
  breaks = seq(0, 1, length.out = 201)
)


library(readxl)
library(tidyverse)
library(pheatmap)

# Keep all predicted species on the x-axis
all_predicted <- colnames(cm_prop)

# Keep only species with actual detections on the y-axis
species_order_rows <- rownames(cm_prop_filtered)

# Build final matrix: rows filtered, columns full
cm_prop_final <- cm_prop_filtered[species_order_rows, all_predicted, drop = FALSE]



pheatmap(
  cm_prop_final,
  color = colorRampPalette(c("white", "#FF9999", "#FF0000", "#990000"))(200),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize = 10,
  main = "Combined Misclassification Heatmap Overall Pictures",
  breaks = seq(0, 1, length.out = 201)
)


# ---------------------------------------------------------
# USE THE SAME combined_cm YOU ALREADY BUILT
# ---------------------------------------------------------

# combined_cm = your full confusion matrix (actual x predicted)

# ---------------------------------------------------------
# BUILD PRECISION MATRIX (normalize by columns)
# ---------------------------------------------------------

cm_precision <- combined_cm

# Avoid division by zero
cm_precision <- sweep(cm_precision, 2, colSums(cm_precision) + 1e-9, FUN = "/")

# Remove predicted species with zero predictions
cols_to_keep <- colSums(combined_cm) > 0
cm_precision_filtered <- cm_precision[, cols_to_keep]

# Reverse x-axis if you want consistency with the first heatmap
cm_precision_final <- cm_precision_filtered[, rev(colnames(cm_precision_filtered))]

# ---------------------------------------------------------
# PLOT PRECISION HEATMAP (0–1 scale)
# ---------------------------------------------------------

pheatmap(
  cm_precision_final,
  color = colorRampPalette(c("white", "#FF9999", "#FF0000", "#990000"))(200),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  fontsize = 10,
  main = "Precision Heatmap: Fraction of Predictions That Were Correct",
  breaks = seq(0, 1, length.out = 201)
)









# ---------------------------------------------------------
# ACTUAL DETECTION COUNTS (EXCLUDING ZEROES)
# ---------------------------------------------------------

# Calculate true actual detections for each species
actual_counts <- rowSums(combined_cm)

# Remove species with zero detections
actual_counts <- actual_counts[actual_counts > 0]

# Convert to a clean table
actual_counts_table <- data.frame(
  Species = names(actual_counts),
  Actual_Detections = as.numeric(actual_counts)
)

# Sort by highest detections
actual_counts_table <- actual_counts_table %>%
  arrange(desc(Actual_Detections))

print(actual_counts_table)


library(ggplot2)

ggplot(actual_counts_table, aes(x = reorder(Species, -Actual_Detections),
                                y = Actual_Detections)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Actual_Detections),
            vjust = -0.5,
            size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Actual Animal Detections Across All Cameras and Retrieval Dates",
    x = "Species",
    y = "Number of Actual Detections"
  ) +
  expand_limits(y = max(actual_counts_table$Actual_Detections) * 1.1)







# ---------------------------------------------------------
# ANIMALS PER RETRIEVAL DATE (TOTAL DETECTIONS OVER TIME)
# ---------------------------------------------------------

animals_time <- data.frame(
  Retrieval_Date = character(),
  Total_Detections = numeric()
)

for (file in files) {
  sheets <- excel_sheets(file)
  combined_temp <- NULL
  
  # First pass: collect species for this file
  species_temp <- c()
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix for this file
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Total detections for this retrieval date
  total_detect <- sum(rowSums(combined_temp))
  
  animals_time <- rbind(
    animals_time,
    data.frame(
      Retrieval_Date = file,
      Total_Detections = total_detect
    )
  )
}

animals_time$Retrieval_Date <- factor(
  animals_time$Retrieval_Date,
  levels = c(
    "Original Pictures Matrix.xlsx",
    "4-15-25 Pictures Matrix.xlsx",
    "6-6-25 Pictures Matrix.xlsx",
    "7-31-25 Pictures Matrix.xlsx",
    "10-10-25 Pictures Matrix.xlsx",
    "1-12-26 Pictures Matrix.xlsx"
  )
)


print(animals_time)

ggplot(animals_time, aes(x = Retrieval_Date, y = Total_Detections, group = 1)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(
    title = "Total Animal Detections Over Time",
    x = "Retrieval Date",
    y = "Total Detections"
  )


##### updated code #######

library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# LIST ALL EXCEL FILES YOU WANT TO INCLUDE
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# CLEAN LABELS FOR THE X-AXIS
# ---------------------------------------------------------

label_lookup <- c(
  "Original Pictures Matrix.xlsx" = "Original Pictures",
  "4-15-25 Pictures Matrix.xlsx" = "04-15-2025",
  "6-6-25 Pictures Matrix.xlsx"  = "06-06-2025",
  "7-31-25 Pictures Matrix.xlsx" = "07-31-2025",
  "10-10-25 Pictures Matrix.xlsx" = "10-10-2025",
  "1-12-26 Pictures Matrix.xlsx" = "01-12-2026"
)

# ---------------------------------------------------------
# BUILD TOTAL DETECTIONS PER RETRIEVAL DATE
# ---------------------------------------------------------

animals_time <- data.frame(
  Retrieval_Date = character(),
  Total_Detections = numeric()
)

for (file in files) {
  sheets <- excel_sheets(file)
  combined_temp <- NULL
  
  # First pass: collect species for this file
  species_temp <- c()
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix for this file
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Total detections for this retrieval date
  total_detect <- sum(rowSums(combined_temp))
  
  animals_time <- rbind(
    animals_time,
    data.frame(
      Retrieval_Date = file,
      Total_Detections = total_detect
    )
  )
}

# ---------------------------------------------------------
# ADD CLEAN LABELS + ORDERED FACTOR FOR EVEN SPACING
# ---------------------------------------------------------

animals_time$Label <- label_lookup[animals_time$Retrieval_Date]

animals_time$Label <- factor(
  animals_time$Label,
  levels = c(
    "Original Pictures",
    "04-15-2025",
    "06-06-2025",
    "07-31-2025",
    "10-10-2025",
    "01-12-2026"
  )
)

# ---------------------------------------------------------
# FINAL PLOT (EVEN SPACING)
# ---------------------------------------------------------

ggplot(animals_time, aes(x = Label, y = Total_Detections, group = 1)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(
    title = "Total Animal Detections Over Time",
    x = "Retrieval Date",
    y = "Total Detections"
  )



###### updated ##########

library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# LIST OF FILES IN TRUE CHRONOLOGICAL ORDER
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# STEP 1: BUILD GLOBAL SPECIES LIST ACROSS ALL FILES
# ---------------------------------------------------------

all_species <- c()

for (file in files) {
  sheets <- excel_sheets(file)
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    all_species <- union(all_species, df[[1]])
    all_species <- union(all_species, names(df)[-1])
  }
}

all_species <- sort(all_species)

# ---------------------------------------------------------
# STEP 2: COMPUTE SPECIES DISTRIBUTION PER RETRIEVAL DATE
# ---------------------------------------------------------

distribution_table <- data.frame()

for (file in files) {
  sheets <- excel_sheets(file)
  species_temp <- c()
  
  # Collect species for this file
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix for this file
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Actual detections per species
  actual_counts <- rowSums(combined_temp)
  
  # Convert to proportions
  proportions <- actual_counts / sum(actual_counts)
  
  # Align with global species list
  aligned <- rep(0, length(all_species))
  names(aligned) <- all_species
  aligned[names(proportions)] <- proportions
  
  # Add row to table
  distribution_table <- rbind(
    distribution_table,
    cbind(
      Retrieval_Date = file,
      t(aligned)
    )
  )
}

# ---------------------------------------------------------
# STEP 3: CONVERT TO LONG FORMAT FOR PLOTTING
# ---------------------------------------------------------

distribution_long <- distribution_table %>%
  pivot_longer(-Retrieval_Date, names_to = "Species", values_to = "Proportion")

# ---------------------------------------------------------
# STEP 4: ORDER RETRIEVAL DATES CHRONOLOGICALLY
# ---------------------------------------------------------

distribution_long$Retrieval_Date <- factor(
  distribution_long$Retrieval_Date,
  levels = c(
    "OG Pictures Matrix.xlsx",
    "4-15-25 Pictures Matrix.xlsx",
    "6-6-25 Pictures Matrix.xlsx",
    "7-31-25 Pictures Matrix.xlsx",
    "10-10-25 Pictures Matrix.xlsx",
    "1-12-26 Pictures Matrix.xlsx"
  )
)

# ---------------------------------------------------------
# STEP 5: OPTIONAL — REMOVE ZERO-PROPORTION SPECIES
# ---------------------------------------------------------


distribution_long <- distribution_long %>%
  filter(Proportion > 0)

# ---------------------------------------------------------
# STEP 6: FINAL STACKED BAR CHART
# ---------------------------------------------------------

ggplot(distribution_long, aes(x = Retrieval_Date, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Species Composition Across Retrieval Dates",
    x = "Retrieval Date",
    y = "Proportion of Detections"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# LIST OF FILES IN TRUE CHRONOLOGICAL ORDER
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# STEP 1: BUILD GLOBAL SPECIES LIST ACROSS ALL FILES
# ---------------------------------------------------------

all_species <- c()

for (file in files) {
  sheets <- excel_sheets(file)
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    all_species <- union(all_species, df[[1]])
    all_species <- union(all_species, names(df)[-1])
  }
}

all_species <- sort(all_species)

# ---------------------------------------------------------
# STEP 2: COMPUTE SPECIES DISTRIBUTION PER RETRIEVAL DATE
# ---------------------------------------------------------

distribution_list <- list()

for (file in files) {
  sheets <- excel_sheets(file)
  species_temp <- c()
  
  # Collect species for this file
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix for this file
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Actual detections per species
  actual_counts <- rowSums(combined_temp)
  
  # Convert to proportions
  proportions <- actual_counts / sum(actual_counts)
  
  # Align with global species list
  aligned <- rep(0, length(all_species))
  names(aligned) <- all_species
  aligned[names(proportions)] <- proportions
  
  # Store as numeric row
  distribution_list[[file]] <- aligned
}

# Convert list to data frame
distribution_table <- bind_rows(distribution_list, .id = "Retrieval_Date")

# ---------------------------------------------------------
# STEP 3: LONG FORMAT
# ---------------------------------------------------------

distribution_long <- distribution_table %>%
  pivot_longer(-Retrieval_Date, names_to = "Species", values_to = "Proportion")

# ---------------------------------------------------------
# STEP 4: FIX — MAKE PROPORTION NUMERIC + ROUND
# ---------------------------------------------------------

distribution_long$Proportion <- as.numeric(distribution_long$Proportion)
distribution_long$Proportion <- round(distribution_long$Proportion, 3)

# ---------------------------------------------------------
# STEP 5: ORDER RETRIEVAL DATES
# ---------------------------------------------------------

distribution_long$Retrieval_Date <- factor(
  distribution_long$Retrieval_Date,
  levels = files
)

# ---------------------------------------------------------
# STEP 6: REMOVE TINY PROPORTIONS (<1%)
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(Proportion > 0.001)


# ---------------------------------------------------------
# STEP 7: FINAL STACKED BAR CHART
# ---------------------------------------------------------

ggplot(distribution_long, aes(x = Retrieval_Date, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Species Composition Across Retrieval Dates",
    x = "Retrieval Date",
    y = "Proportion of Detections"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(distribution_long, aes(x = Retrieval_Date, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity", width = 0.88, color = "black", size = 0.15) +
  theme_minimal() +
  labs(
    title = "Species Composition Across Retrieval Dates",
    x = "Retrieval Date",
    y = "Proportion of Detections"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )








library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# LIST OF FILES IN TRUE CHRONOLOGICAL ORDER
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# STEP 1: BUILD GLOBAL SPECIES LIST ACROSS ALL FILES
# ---------------------------------------------------------

all_species <- c()

for (file in files) {
  sheets <- excel_sheets(file)
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    all_species <- union(all_species, df[[1]])
    all_species <- union(all_species, names(df)[-1])
  }
}

all_species <- sort(all_species)

# ---------------------------------------------------------
# STEP 2: COMPUTE SPECIES DISTRIBUTION PER RETRIEVAL DATE
# ---------------------------------------------------------

distribution_list <- list()

for (file in files) {
  sheets <- excel_sheets(file)
  species_temp <- c()
  
  # Collect species for this file
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix for this file
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Actual detections per species
  actual_counts <- rowSums(combined_temp)
  
  # Convert to proportions
  proportions <- actual_counts / sum(actual_counts)
  
  # Align with global species list
  aligned <- rep(0, length(all_species))
  names(aligned) <- all_species
  aligned[names(proportions)] <- proportions
  
  # Store as numeric row
  distribution_list[[file]] <- aligned
}

# Convert list to data frame
distribution_table <- bind_rows(distribution_list, .id = "Retrieval_Date")

# ---------------------------------------------------------
# STEP 3: LONG FORMAT
# ---------------------------------------------------------

distribution_long <- distribution_table %>%
  pivot_longer(-Retrieval_Date, names_to = "Species", values_to = "Proportion")

# ---------------------------------------------------------
# STEP 4: FIX — MAKE PROPORTION NUMERIC + ROUND
# ---------------------------------------------------------

distribution_long$Proportion <- as.numeric(distribution_long$Proportion)
distribution_long$Proportion <- round(distribution_long$Proportion, 3)

# ---------------------------------------------------------
# STEP 5: REMOVE Other/Object AND Unidentified
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(!Species %in% c("Other/Object", "Unidentified"))

# ---------------------------------------------------------
# STEP 6: KEEP SPECIES WITH AT LEAST 0.1% PROPORTION
# (Dog + Opossum stay if meaningful; tiny slivers removed)
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(Proportion > 0.001)

# ---------------------------------------------------------
# STEP 7: ORDER RETRIEVAL DATES
# ---------------------------------------------------------

distribution_long$Retrieval_Date <- factor(
  distribution_long$Retrieval_Date,
  levels = files
)

# ---------------------------------------------------------
# STEP 8: FINAL STACKED BAR CHART (cleaner bars)
# ---------------------------------------------------------

ggplot(distribution_long, aes(x = Retrieval_Date, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity", width = 0.88, color = "black", size = 0.15) +
  theme_minimal() +
  labs(
    title = "Species Composition Across Retrieval Dates",
    x = "Retrieval Date",
    y = "Proportion of Detections"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )









library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# LIST OF FILES IN TRUE CHRONOLOGICAL ORDER
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# STEP 1: BUILD GLOBAL SPECIES LIST ACROSS ALL FILES
# ---------------------------------------------------------

all_species <- c()

for (file in files) {
  sheets <- excel_sheets(file)
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    all_species <- union(all_species, df[[1]])
    all_species <- union(all_species, names(df)[-1])
  }
}

all_species <- sort(all_species)

# ---------------------------------------------------------
# STEP 2: COMPUTE SPECIES DISTRIBUTION PER RETRIEVAL DATE
# ---------------------------------------------------------

distribution_list <- list()

for (file in files) {
  sheets <- excel_sheets(file)
  species_temp <- c()
  
  # Collect species for this file
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix for this file
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Actual detections per species
  actual_counts <- rowSums(combined_temp)
  
  # Convert to proportions
  proportions <- actual_counts / sum(actual_counts)
  
  # Align with global species list
  aligned <- rep(0, length(all_species))
  names(aligned) <- all_species
  aligned[names(proportions)] <- proportions
  
  # Store as numeric row
  distribution_list[[file]] <- aligned
}

# Convert list to data frame
distribution_table <- bind_rows(distribution_list, .id = "Retrieval_Date")

# ---------------------------------------------------------
# STEP 3: LONG FORMAT
# ---------------------------------------------------------

distribution_long <- distribution_table %>%
  pivot_longer(-Retrieval_Date, names_to = "Species", values_to = "Proportion")

# ---------------------------------------------------------
# STEP 4: FIX — MAKE PROPORTION NUMERIC + ROUND
# ---------------------------------------------------------

distribution_long$Proportion <- as.numeric(distribution_long$Proportion)
distribution_long$Proportion <- round(distribution_long$Proportion, 3)

# ---------------------------------------------------------
# STEP 5: REMOVE Other/Object AND Unidentified
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(!Species %in% c("Other/Object", "Unidentified"))

# ---------------------------------------------------------
# STEP 6: KEEP SPECIES WITH AT LEAST 0.1% PROPORTION
# (Dog + Opossum stay if meaningful; tiny slivers removed)
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(Proportion > 0.001)

# ---------------------------------------------------------
# STEP 7: RE-NORMALIZE PROPORTIONS SO EACH BAR SUMS TO 1.0
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  group_by(Retrieval_Date) %>%
  mutate(Proportion = Proportion / sum(Proportion)) %>%
  ungroup()

# ---------------------------------------------------------
# STEP 8: ORDER RETRIEVAL DATES
# ---------------------------------------------------------

distribution_long$Retrieval_Date <- factor(
  distribution_long$Retrieval_Date,
  levels = files
)

# ---------------------------------------------------------
# STEP 9: FINAL STACKED BAR CHART (cleaner bars)
# ---------------------------------------------------------

ggplot(distribution_long, aes(x = Retrieval_Date, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity", width = 0.88, color = "black", size = 0.15) +
  theme_minimal() +
  labs(
    title = "Species Composition Across Retrieval Dates",
    x = "Retrieval Date",
    y = "Proportion of Detections"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )



#########updated#########


library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# LIST OF FILES IN TRUE CHRONOLOGICAL ORDER
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# CLEAN LABELS FOR X-AXIS (Option B)
# ---------------------------------------------------------

label_lookup <- c(
  "Original Pictures Matrix.xlsx" = "Original Pictures",
  "4-15-25 Pictures Matrix.xlsx" = "04-15-2025",
  "6-6-25 Pictures Matrix.xlsx"  = "06-06-2025",
  "7-31-25 Pictures Matrix.xlsx" = "07-31-2025",
  "10-10-25 Pictures Matrix.xlsx" = "10-10-2025",
  "1-12-26 Pictures Matrix.xlsx" = "01-12-2026"
)

# ---------------------------------------------------------
# STEP 1: BUILD GLOBAL SPECIES LIST
# ---------------------------------------------------------

all_species <- c()

for (file in files) {
  sheets <- excel_sheets(file)
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    all_species <- union(all_species, df[[1]])
    all_species <- union(all_species, names(df)[-1])
  }
}

all_species <- sort(all_species)

# ---------------------------------------------------------
# STEP 2: SPECIES DISTRIBUTION PER RETRIEVAL DATE
# ---------------------------------------------------------

distribution_list <- list()

for (file in files) {
  sheets <- excel_sheets(file)
  species_temp <- c()
  
  # Collect species for this file
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Actual detections per species
  actual_counts <- rowSums(combined_temp)
  
  # Convert to proportions
  proportions <- actual_counts / sum(actual_counts)
  
  # Align with global species list
  aligned <- rep(0, length(all_species))
  names(aligned) <- all_species
  aligned[names(proportions)] <- proportions
  
  distribution_list[[file]] <- aligned
}

distribution_table <- bind_rows(distribution_list, .id = "Retrieval_Date")

# ---------------------------------------------------------
# STEP 3: LONG FORMAT
# ---------------------------------------------------------

distribution_long <- distribution_table %>%
  pivot_longer(-Retrieval_Date, names_to = "Species", values_to = "Proportion")

# ---------------------------------------------------------
# STEP 4: FIX — MAKE PROPORTION NUMERIC + ROUND
# ---------------------------------------------------------

distribution_long$Proportion <- as.numeric(distribution_long$Proportion)
distribution_long$Proportion <- round(distribution_long$Proportion, 3)

# ---------------------------------------------------------
# STEP 5: REMOVE Other/Object AND Unidentified
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(!Species %in% c("Other/Object", "Unidentified"))

# ---------------------------------------------------------
# STEP 6: REMOVE TINY SPECIES (<0.1%)
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  filter(Proportion > 0.001)

# ---------------------------------------------------------
# STEP 7: RE-NORMALIZE PROPORTIONS
# ---------------------------------------------------------

distribution_long <- distribution_long %>%
  group_by(Retrieval_Date) %>%
  mutate(Proportion = Proportion / sum(Proportion)) %>%
  ungroup()

# ---------------------------------------------------------
# STEP 8: APPLY CLEAN LABELS + ORDERED FACTOR
# ---------------------------------------------------------

distribution_long$Label <- label_lookup[distribution_long$Retrieval_Date]

distribution_long$Label <- factor(
  distribution_long$Label,
  levels = c(
    "Original Pictures",
    "04-15-2025",
    "06-06-2025",
    "07-31-2025",
    "10-10-2025",
    "01-12-2026"
  )
)

# ---------------------------------------------------------
# STEP 9: FINAL STACKED BAR CHART (EVEN SPACING)
# ---------------------------------------------------------

ggplot(distribution_long, aes(x = Label, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity", width = 0.88, color = "black", size = 0.15) +
  theme_minimal() +
  labs(
    title = "Species Composition Across Retrieval Dates",
    x = "Retrieval Date",
    y = "Proportion of Detections"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank()
  )












########updated##########





######chi2#########

library(readxl)
library(tidyverse)

# ---------------------------------------------------------
# SAME FILE LIST
# ---------------------------------------------------------

files <- c(
  "Original Pictures Matrix.xlsx",
  "4-15-25 Pictures Matrix.xlsx",
  "6-6-25 Pictures Matrix.xlsx",
  "7-31-25 Pictures Matrix.xlsx",
  "10-10-25 Pictures Matrix.xlsx",
  "1-12-26 Pictures Matrix.xlsx"
)

# ---------------------------------------------------------
# STEP 1: BUILD SPECIES COUNT TABLE
# ---------------------------------------------------------

count_list <- list()

for (file in files) {
  sheets <- excel_sheets(file)
  species_temp <- c()
  
  # Collect species for this file
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    species_temp <- union(species_temp, df[[1]])
    species_temp <- union(species_temp, names(df)[-1])
  }
  
  species_temp <- sort(species_temp)
  
  # Build combined matrix
  combined_temp <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
  rownames(combined_temp) <- species_temp
  colnames(combined_temp) <- species_temp
  
  for (sheet in sheets) {
    df <- read_excel(file, sheet = sheet)
    if (ncol(df) == 0) next
    if ("Total" %in% names(df)) df <- df %>% select(-Total)
    
    actual <- df[[1]]
    predicted <- names(df)[-1]
    
    df_numeric <- df %>% mutate(across(-1, ~ suppressWarnings(as.numeric(.))))
    df_numeric[is.na(df_numeric)] <- 0
    
    cm <- as.matrix(df_numeric[, -1])
    rownames(cm) <- actual
    colnames(cm) <- predicted
    
    expanded <- matrix(0, nrow = length(species_temp), ncol = length(species_temp))
    rownames(expanded) <- species_temp
    colnames(expanded) <- species_temp
    
    expanded[rownames(cm), colnames(cm)] <- cm
    combined_temp <- combined_temp + expanded
  }
  
  # Actual counts per species
  actual_counts <- rowSums(combined_temp)
  
  count_list[[file]] <- actual_counts
}

# ---------------------------------------------------------
# STEP 2: ALIGN INTO ONE TABLE
# ---------------------------------------------------------

all_species <- sort(unique(unlist(lapply(count_list, names))))

count_table <- matrix(0, nrow = length(all_species), ncol = length(files))
rownames(count_table) <- all_species
colnames(count_table) <- files

for (i in seq_along(files)) {
  vec <- count_list[[files[i]]]
  count_table[names(vec), i] <- vec
}

# ---------------------------------------------------------
# STEP 3: REMOVE NON-BIOLOGICAL CATEGORIES
# ---------------------------------------------------------

count_table <- count_table[!rownames(count_table) %in% c("Other/Object", "Unidentified"), ]

# ---------------------------------------------------------
# STEP 4: REMOVE SPECIES WITH ZERO TOTAL DETECTIONS
# ---------------------------------------------------------

count_table <- count_table[rowSums(count_table) > 0, ]

# ---------------------------------------------------------
# STEP 5: COMBINE SPECIES WITH TOTAL < 50
# ---------------------------------------------------------

rare_species <- rownames(count_table)[rowSums(count_table) < 50]

if (length(rare_species) > 0) {
  rare_sum <- colSums(count_table[rare_species, , drop = FALSE])
  count_table <- count_table[!rownames(count_table) %in% rare_species, ]
  count_table <- rbind(count_table, "Rare_Species" = rare_sum)
}

# ---------------------------------------------------------
# STEP 6: RUN CHI-SQUARE TEST
# ---------------------------------------------------------

chi_result <- chisq.test(count_table)

print("=== Chi-Square Test of Independence ===")
print(chi_result)

print("=== Contingency Table Used in Test ===")
print(count_table)




#####Chi#########





#Code to go along with rest code



combined_cm


# ---------------------------------------------------------
# SPECIES ACCURACY AND ERROR FROM COMBINED MATRIX
# ---------------------------------------------------------

diag_vals  <- diag(combined_cm)
row_totals <- rowSums(combined_cm)
col_totals <- colSums(combined_cm)

# Recall (true → predicted)
species_recall <- diag_vals / row_totals
species_recall[row_totals == 0] <- NA

# Precision (predicted → true)
species_precision <- diag_vals / col_totals
species_precision[col_totals == 0] <- NA

# Force precision to 0 when:
# - diagonal is 0
# - AND column total > 0
# (meaning: AddaxAI predicted the species, but was never correct)
species_precision[diag_vals == 0 & col_totals > 0] <- 0

# Optional: round for readability
species_precision <- round(species_precision, 6)

species_precision


library(ggplot2)

# Convert to data frame
precision_df <- data.frame(
  Species = names(species_precision),
  Precision = species_precision
)

# Plot
ggplot(precision_df, aes(x = reorder(Species, Precision), y = Precision)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Precision by Species (Predicted → True)",
    x = "Species",
    y = "Precision"
  ) +
  geom_text(aes(label = round(Precision, 3)), hjust = -0.1, size = 3) +
  ylim(0, 1.1)


write.csv(precision_df, "species_precision.csv", row.names = FALSE)









library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the Camera 14 sheet
file_path <- "4-15-25 Pictures Matrix.xlsx"
camera14_raw <- read_excel(file_path, sheet = "Camera 14")

# Clean matrix: remove empty rows/columns
camera14_matrix <- camera14_raw %>%
  select(where(~!all(is.na(.)))) %>%
  filter(if_any(everything(), ~ !is.na(.)))

# Explicitly move 'Total' to be the last column (if it exists)
if ("Total" %in% colnames(camera14_matrix)) {
  cols <- colnames(camera14_matrix)
  cols_no_total <- cols[cols != "Total"]
  camera14_matrix <- camera14_matrix[, c(cols_no_total, "Total")]
}

# Convert to long format
camera14_long <- camera14_matrix %>%
  pivot_longer(
    cols = -1,
    names_to = "Predicted",
    values_to = "Count"
  ) %>%
  rename(True = 1)

# Plot confusion matrix: white boxes, black grid, numbers only
ggplot(camera14_long, aes(x = Predicted, y = True)) +
  geom_tile(color = "black", fill = "white") +
  geom_text(aes(label = Count), size = 3.5) +
  labs(
    title = "Confusion Matrix – Camera 14",
    x = "Predicted Category",
    y = "True Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# Plot confusion matrix with no shading, only numbers
ggplot(camera14_long, aes(x = Predicted, y = True)) +
  geom_tile(color = "black", fill = "white") +
  geom_text(aes(label = Count), size = 3.5) +
  labs(
    title = "Confusion Matrix – Camera 14",
    x = "Predicted Species",
    y = "True Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )



library(readxl)
library(dplyr)

# Path to your file
file_path <- "4-15-25 Pictures Matrix.xlsx"

# Read the Camera 14 sheet
camera14_raw <- read_excel(file_path, sheet = "Camera 14")

# Clean the matrix:
# Remove any completely empty rows or columns
camera14_matrix <- camera14_raw %>%
  select(where(~!all(is.na(.)))) %>%   # remove empty columns
  filter(if_any(everything(), ~ !is.na(.)))  # remove empty rows

# Print the matrix as a table
camera14_matrix




library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

file_path <- "4-15-25 Pictures Matrix.xlsx"
camera14_raw <- read_excel(file_path, sheet = "Camera 14")

# Clean matrix: remove empty rows/columns
camera14_matrix <- camera14_raw %>%
  select(where(~!all(is.na(.)))) %>%
  filter(if_any(everything(), ~ !is.na(.)))

# Get original column names (including Total, Unidentified, etc.)
cols <- colnames(camera14_matrix)

# First column is True species; others are predicted categories
pred_cols <- cols[-1]

# Build desired order: all predicted columns except Total,
# but force Unidentified just before Total, and Total last
pred_no_total <- pred_cols[pred_cols != "Total"]

if ("Unidentified" %in% pred_no_total) {
  pred_no_total_no_unid <- pred_no_total[pred_no_total != "Unidentified"]
  pred_order <- c(pred_no_total_no_unid, "Unidentified", "Total")
} else {
  pred_order <- c(pred_no_total, "Total")
}

# Convert to long format
camera14_long <- camera14_matrix %>%
  pivot_longer(
    cols = -1,
    names_to = "Predicted",
    values_to = "Count"
  ) %>%
  rename(True = 1) %>%
  mutate(
    Predicted = factor(Predicted, levels = pred_order)
  )

# Plot: white boxes, black grid, numbers only
ggplot(camera14_long, aes(x = Predicted, y = True)) +
  geom_tile(color = "black", fill = "white") +
  geom_text(aes(label = Count), size = 3.5) +
  labs(
    title = "Confusion Matrix – Camera 14",
    x = "AddaxAI Predicted Category",
    y = "True Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

