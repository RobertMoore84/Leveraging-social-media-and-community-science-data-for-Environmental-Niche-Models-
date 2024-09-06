# Load necessary library for raster data manipulation
library(terra)

# Step 1: Load the .asc files as SpatRasters
# Replace these with your own ASC file paths
scenario1_pred <- rast("your/path/to/scenario1.asc")
scenario2_pred <- rast("your/path/to/scenario2.asc")

# Step 2: Set -9999 values to NA to ignore missing values in the comparison
# -9999 typically represents missing data in ASC files, so we replace these with NA
scenario1_pred[scenario1_pred == -9999] <- NA
scenario2_pred[scenario2_pred == -9999] <- NA

# Step 3: Convert the rasters to vectors of values for comparison
# Extract the raster values as vectors for each scenario
vals1 <- values(scenario1_pred)
vals2 <- values(scenario2_pred)

# Remove NA values where either of the rasters has NA
# This ensures we're only comparing valid, non-missing data
valid_idx <- !is.na(vals1) & !is.na(vals2)
vals1 <- vals1[valid_idx]
vals2 <- vals2[valid_idx]

# Step 4: Define the function to calculate Schoener's D and Warren's I
# The function will scale the values, calculate Schoener's D, Warren's I (Hellinger distance), and the Spearman correlation
calculate_overlap <- function(sp1, sp2) {
  # Scale values to relative occupancy (i.e., 0-1 range)
  sp1 <- sp1 / sum(sp1)
  sp2 <- sp2 / sum(sp2)
  
  # Schoener's D: Quantifies niche overlap between two distributions
  D <- 1 - sum(abs(sp1 - sp2)) / 2
  
  # Warren's I: Based on Hellinger distance, used to quantify overlap with a focus on dissimilarity
  I <- 1 - sum((sqrt(sp1) - sqrt(sp2))^2) / 2
  
  # Spearman correlation: Measures how correlated the distributions are in terms of environmental suitability
  cor_value <- cor(sp1, sp2, method = "spearman")
  
  # Return the calculated values as a list
  return(list(D = D, I = I, correlation = cor_value))
}

# Step 5: Calculate Schoener's D, Warren's I, and the Spearman correlation between the two rasters
# Using the extracted values from the two scenarios
overlap_results <- calculate_overlap(vals1, vals2)

# Step 6: Print the results
# Display Schoener's D, Warren's I, and the Spearman correlation with 3 decimal places for clarity
print(paste("Schoener's D: ", round(overlap_results$D, 3)))
print(paste("Warren's I: ", round(overlap_results$I, 3)))
print(paste("Spearman correlation: ", round(overlap_results$correlation, 3)))
