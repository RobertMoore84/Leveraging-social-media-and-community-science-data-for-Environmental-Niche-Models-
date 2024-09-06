# Load required libraries for spatial data manipulation, modeling, and parallelization
library(dismo)      # For species distribution modeling (SDM)
library(dplyr)      # For data manipulation
library(tidyr)      # For data tidying
library(mapview)    # For visualizing spatial data
library(usdm)       # For handling collinearity and multicollinearity
library(sdm)        # For building species distribution models
library(parallel)   # For parallel computing
library(rJava)      # For Java integration with R
library(terra)      # For raster data manipulation

# Set working directory (optional)
getwd()  # Prints the current working directory

# Set the path to the MaxEnt software
# Replace with the correct path to your maxent.jar file
maxent_path <- "your/path/to/maxent.jar"

# Allocate more memory to Java if necessary (adjust according to your system)
options(java.parameters = "-Xmx2g")  # Adjust memory allocation (2GB in this case)

# Set Java environment (adjust the path according to your Java installation)
Sys.setenv(JAVA_HOME="your/java/path")  

# Detect the number of cores available on your machine for parallel processing
parallel::detectCores()

# Check if MaxEnt software is available at the specified path
if (file.exists(maxent_path)) {
  cat("Maxent software is available.")
} else {
  stop("Maxent software not found. Please set the correct path to maxent.jar.")
}

# Check Java version to ensure correct installation
system("java -version")

# Load species occurrence data from a CSV file
# Replace with the path to your CSV file containing species occurrences
spg <- read.csv("your_species_occurrences.csv")
head(spg)  # View the first few rows of the data

# Convert coordinates in the CSV to spatial points (ensure your CSV has 'lon' and 'lat' columns)
coordinates(spg) <- c('lon', 'lat')

# Load environmental data (e.g., BioClim rasters)
# Replace with the folder path containing your environmental raster files
bioclim_folder <- "your/bioclim/folder"  

# List all raster files (e.g., .tif) in the environmental data folder
bioclim_files <- list.files(bioclim_folder, pattern = ".tif$", full.names = TRUE)

# Load the environmental rasters into a stack (for use in species distribution modeling)
env_data <- raster::stack(bioclim_files)

# Extract the environmental data for the species occurrence points
extracted_values <- raster::extract(env_data, spg)

# Perform Variance Inflation Factor (VIF) stepwise analysis to detect multicollinearity
# This helps to exclude highly correlated environmental variables
V <- vifstep(extracted_values)

# Exclude environmental variables with high VIF values (reduce multicollinearity)
bioexcluded <- exclude(env_data, V)

# Prepare species distribution model data using species occurrences and the environmental predictors
d <- sdmData(species~., spg, predictors = bioexcluded, bg = list(method= 'gRandom', n = 1000))

# Build species distribution model using MaxEnt with bootstrapped replications
m <- sdm(species ~ ., d, 
         methods = 'maxent',  # Specify MaxEnt as the modeling method
         replications = 'boot',  # Use bootstrapping for model replication
         test.p = 20,  # 20% of the data will be used for testing
         n = 10,  # Number of replications
         parallelSetting = list(ncore = 8, method = 'parallel'),  # Enable parallel processing with 8 cores
         modelSettings = list(  # MaxEnt model settings
           maxent = list(
             reg = 1,              # Regularization multiplier to control model complexity
             maxit = 500,          # Maximum number of iterations for model convergence
             lqphpt = TRUE,        # Enable feature classes (Linear, Quadratic, Product, Hinge, Threshold)
             beta = c(1, 2),       # Beta parameters for feature regularization
             randomTestPoints = 20 # Percentage of random test points
           )
         ))

# Initialize an empty list to store variable importance values from each model replication
var_imp_list <- list()

# Loop through each model replication (e.g., 10 replications)
for (i in 1:10) {
  # Get variable importance for each replication
  var_imp <- getVarImp(m, id = i, wtest = 'test.dep')
  
  # Store variable importance in a list with corresponding replication ID
  var_imp_list[[paste("ID_", i, sep = "")]] <- var_imp
}

# Print variable importance for each replication
for (i in 1:length(var_imp_list)) {
  cat("ID", i, ":\n")
  print(var_imp_list[[i]])
  cat("\n")
}

# Make predictions using the built SDM model and the environmental data
p1 <- predict(m, bioexcluded)

# Calculate the mean prediction across all replications
mean_prediction <- mean(p1, na.rm = TRUE)

# Plot the mean prediction map
plot(mean_prediction)

# Select a specific layer from the prediction results (e.g., first layer)
layer <- mean_prediction

# Define a threshold value for converting predictions into binary (presence/absence)
threshold <- 0.26  # Replace this with your chosen threshold value

# Reclassify the prediction into binary format based on the threshold
binary_prediction <- classify(layer, cbind(c(-Inf, threshold), c(threshold, Inf), c(0, 1)))

# Plot the binary prediction (presence/absence)
plot(binary_prediction)

# Replace NA values in the binary raster with -9999 (for compatibility with certain software)
binary_prediction[is.na(binary_prediction)] <- -9999

# Define output directory and path for saving the binary prediction as an ASCII file
# Replace with your desired output directory
output_dir <- "your/output/directory"
output_path <- file.path(output_dir, "your_output_file_name.asc")

# Check if the output directory exists, if not, create it
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save the binary prediction raster as an ASCII file
writeRaster(binary_prediction, filename = output_path, overwrite=TRUE)

# Specify the path to save the plot as a PNG file
# Replace with your desired output path
plot_output_path <- "your_output_plot_name.png"

# Plot the binary prediction without axes, labels, and a box around the plot
plot.new()  # Initialize a new plot window
plot(binary_prediction, axes = FALSE, box = FALSE, legend = FALSE)  # Plot the binary prediction

# Save the plot as a high-resolution PNG file
png(plot_output_path, width = 1200, height = 1200, units = "px", res = 300)
plot(binary_prediction, axes = FALSE, box = FALSE, legend = FALSE)  # Re-plot the binary prediction
dev.off()  # Close the PNG device
