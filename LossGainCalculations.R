# Load the necessary library for handling raster data
library(terra)

# Load ASC files with binary predictions for scenario 1 and scenario 2
# Replace these with your own ASC file paths
scenario1_pred <- rast("your/path/to/scenario1.asc")
scenario2_pred <- rast("your/path/to/scenario2.asc")

# Step 1: Set -9999 values to NA to ignore them in the calculations
# -9999 often represents missing data in rasters, so we replace these with NA
scenario1_pred[scenario1_pred == -9999] <- NA
scenario2_pred[scenario2_pred == -9999] <- NA

# Step 2: Calculate the difference between the two scenarios
# This will highlight the changes between scenario 1 and scenario 2
difference <- scenario1_pred - scenario2_pred

# Step 3: Create masks to categorize the differences
# Mask for areas where scenario 1 has absence and scenario 2 has presence (gain)
mask_gain <- (scenario1_pred == 0) & (scenario2_pred > 0)

# Mask for areas where scenario 1 has presence and scenario 2 has absence (loss)
mask_loss <- (scenario1_pred > 0) & (scenario2_pred == 0)

# Mask for areas where both scenarios have presence (overlap)
mask_overlap <- (scenario1_pred > 0) & (scenario2_pred > 0)

# Mask for areas where both scenarios have absence (absence)
mask_absence <- (scenario1_pred == 0) & (scenario2_pred == 0)

# Step 4: Apply the masks to create the final difference map
# Assign different values to the gain, loss, overlap, and absence categories
masked_difference <- mask_gain * 1 + mask_loss * 2 + mask_overlap * 3 + mask_absence * 4

# Step 5: Define the color palette for plotting
# Colors are assigned for different categories (gain, loss, overlap, absence)
cols <- c("darkgreen", "red", "lightgreen", "gray")

# Step 6: Plot the masked difference map
# Replace the output path with your desired output file path for the plot
output_plot_path <- "your/output/path/plot_name.png"

# Save the plot as a high-resolution PNG file (adjust width, height, and resolution as needed)
png(output_plot_path, width = 8000, height = 6000, res = 300)
plot(masked_difference, col = cols, axes = FALSE, legend = FALSE, box = FALSE)
dev.off()  # Close the PNG device after saving the plot

# Step 7: Calculate the pixel counts for each mask (gain, loss, overlap, absence)
# Use the global function to count the number of pixels for each category
count_gain <- global(mask_gain, "sum", na.rm = TRUE)$sum
count_loss <- global(mask_loss, "sum", na.rm = TRUE)$sum
count_overlap <- global(mask_overlap, "sum", na.rm = TRUE)$sum
count_absence <- global(mask_absence, "sum", na.rm = TRUE)$sum

# Step 8: Calculate total pixels for percentage calculation, excluding absence pixels
# Only include gain, loss, and overlap pixels in the calculation
total_pixels_for_relative_calculation <- count_gain + count_loss + count_overlap

# Step 9: Calculate the relative percentages of gain and loss pixels
relative_percentage_gain <- (count_gain / total_pixels_for_relative_calculation) * 100
relative_percentage_loss <- (count_loss / total_pixels_for_relative_calculation) * 100

# Step 10: Print the relative percentages of gain and loss
print(paste("Relative percentage of gain pixels:", relative_percentage_gain, "%"))
print(paste("Relative percentage of loss pixels:", relative_percentage_loss, "%"))
