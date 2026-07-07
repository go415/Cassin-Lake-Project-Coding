

library(dplyr)
library(ggplot2)
library(stringr)

# ================================
# 1. Load precision and REST results
# ================================
species_precision_df <- read.csv("species_precision.csv", stringsAsFactors = FALSE)

# Convert to named vector
species_precision <- setNames(species_precision_df$Precision,
                              species_precision_df$Species)

overall_results <- read.csv("REST_relative_density_all_species_all_cameras.csv",
                            stringsAsFactors = FALSE)

# ================================
# 2. Clean REST species names
# ================================
clean_map <- c(
  "beaver"              = "Beaver",
  "bird"                = "Bird",
  "boar"                = "Boar",
  "bobcat"              = "Bobcat",
  "cat"                 = "Cat",
  "corvid"              = "Corvid",
  "cougar"              = "Cougar",
  "cow"                 = "Cow",
  "coyote"              = "Coyote",
  "deer"                = "Deer",
  "dog"                 = "Dog",
  "fox"                 = "Fox",
  "human"               = "Person",
  "person"              = "Person",
  "opossum"             = "Opossum",
  "other"               = "Other/Object",
  "rabbit"              = "Rabbit",
  "raccoon"             = "Raccoon",
  "raptor"              = "Raptor",
  "reptile"             = "Reptile",
  "rodent"              = "Rodent",
  "skunk"               = "Skunk",
  "squirrel"            = "Squirrel",
  "unidentified animal" = "Unidentified",
  "weasel"              = "Weasel"
)

overall_results$Species_clean <- clean_map[tolower(overall_results$Species)]

# ================================
# 3. Align species between REST and precision
# ================================
common_species <- intersect(overall_results$Species_clean,
                            names(species_precision))

# Subset REST results to common species
overall_sub <- overall_results[overall_results$Species_clean %in% common_species, ]
overall_sub <- overall_sub[match(common_species, overall_sub$Species_clean), ]

# Subset precision vector to common species (same order)
species_precision_sub <- species_precision[common_species]

# ================================
# 4. Build predicted_events and apply precision
# ================================
# Use TotalEvents from REST as predicted events per species
predicted_events <- overall_sub$TotalEvents
names(predicted_events) <- overall_sub$Species_clean

corrected_events <- predicted_events * species_precision_sub
corrected_events[is.na(corrected_events)] <- 0

lambda_corrected  <- corrected_events / overall_sub$TotalEffort
density_corrected <- lambda_corrected / overall_sub$MeanTau

# ================================
# 5. Build corrected density table
# ================================
corrected_density_table <- data.frame(
  Species          = overall_sub$Species_clean,
  TotalEvents      = overall_sub$TotalEvents,
  Precision        = species_precision_sub[overall_sub$Species_clean],
  CorrectedEvents  = corrected_events[overall_sub$Species_clean],
  TotalEffort      = overall_sub$TotalEffort,
  MeanTau          = overall_sub$MeanTau,
  LambdaOriginal   = overall_sub$LambdaOverall,
  LambdaCorrected  = lambda_corrected,
  DensityOriginal  = overall_sub$RelDensityOverall,
  DensityCorrected = density_corrected
)

print(corrected_density_table)


# ================================
# Plot: ORIGINAL REST density only
# ================================
ggplot(corrected_density_table,
       aes(x = reorder(Species, DensityOriginal), 
           y = DensityOriginal)) +
  geom_col(fill = "gray70") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Original Relative Density (REST Model)",
    x = "Species",
    y = "Relative Density"
  )


# ================================
# Create a consistent species order
# ================================
species_order <- corrected_density_table$Species[order(corrected_density_table$DensityOriginal)]

corrected_density_table$Species <- factor(corrected_density_table$Species,
                                          levels = species_order)


ggplot(corrected_density_table,
       aes(x = Species)) +
  geom_col(aes(y = DensityOriginal),  fill = "gray70") +
  geom_col(aes(y = DensityCorrected), fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "AddaxAI vs Precision-Corrected Density (REST Model)",
    x = "Species",
    y = "Relative Density"
  )




































# ================================
# 6. Plot: original vs corrected density
# ================================
ggplot(corrected_density_table,
       aes(x = reorder(Species, DensityCorrected))) +
  geom_col(aes(y = DensityOriginal),  fill = "gray70") +
  geom_col(aes(y = DensityCorrected), fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "AddaxAI vs Precision-Corrected Density (REST Model)",
    x = "Species",
    y = "Relative Density"
  )


# ================================
# 7. Percent change after correction
# ================================
corrected_density_table$PercentChange <-
  (corrected_density_table$DensityCorrected - corrected_density_table$DensityOriginal) /
  corrected_density_table$DensityOriginal * 100

corrected_density_table

ggplot(corrected_density_table,
       aes(x = reorder(Species, PercentChange), y = PercentChange)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Percent Change After Precision Correction",
    x = "Species",
    y = "Percent Change (%)"
  )

# ================================
# 8. Ranked corrected density
# ================================
ggplot(corrected_density_table,
       aes(x = reorder(Species, DensityCorrected), y = DensityCorrected)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Ranked Precision-Corrected Density (REST Model)",
    x = "Species",
    y = "Corrected Relative Density"
  )

