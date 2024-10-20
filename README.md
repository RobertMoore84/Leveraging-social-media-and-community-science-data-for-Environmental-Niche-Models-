# Leveraging Social Media and Community Science Data for Environmental Niche Models (ENMs): a case study with native Australian bees
Ecological Informatics

## Project Overview

This repository contains the scripts and data used for modeling Environmental Niche Models (ENMs) for Australian native bee species. The project focuses on examining how non-traditional data sources, such as geo-tagged social media (Flickr) and community science (iNaturalist), affect ENMs when combined with traditional museum data (from the Atlas of Living Australia, ALA).

### Abstract

Museum occurrence records have long been used to build ENMs, which map potential species ranges based on environmental variables and species occurrence points. With the increase of non-traditional sources of occurrence data, such as geo-tagged images from community science (iNaturalist) and social media (Flickr), this project aims to explore the impact of adding such data to traditional datasets.

The project compares the ENMs of eight Australian native bee species using MaxEnt models with three datasets:

- Traditional dataset (ALA only).
- ALA + Flickr data.
- ALA + iNaturalist data.

The goal is to determine how non-traditional datasets alter the predicted ecological niche and the importance of environmental variables in the models.

### Keywords

- Citizen science
- Community science
- Environmental Niche Models (ENMs)
- Native bees
- Species Distribution Models (SDMs)
- Social media data (Flickr, iNaturalist)

---

## Repository Structure

This repository is organized into the following folders and files:

### Folders:
- **`Bioclimmasked/`**: Contains masked BioClim `.tif` files used as environmental variables for ENMs.
- **`Presence Points/`**: Contains the occurrence points for each species derived from ALA, Flickr, and iNaturalist datasets.

### Scripts:
- **`LossGainCalculations.R`**: Script to calculate and visualize gains and losses in species' predicted ranges between different scenarios.
- **`SDModelling.R`**: Script to perform Species Distribution Modelling (SDM) using MaxEnt in R. This script processes the occurrence points and environmental data, trains the MaxEnt models, and outputs the predictions.
- **`SchoeD.R`**: Script to calculate Schoener's D, a metric to quantify the overlap of predicted species ranges between two datasets (e.g., ALA vs. ALA + Flickr, ALA vs. ALA + iNaturalist).

---

## Scripts Overview

### 1. `LossGainCalculations.R`
**Purpose**:  
- Compares the predicted ranges of species between different models (ALA vs ALA + Flickr, ALA vs ALA + iNaturalist) and calculates the range gains and losses.

**Outputs**:  
- A map showing areas of gain, loss, and overlap between models.  
- Percentage of range gain and loss.

**Dependencies**:  
- `terra`, `raster`, `ggplot2`.

### 2. `SDModelling.R`
**Purpose**:  
- Performs Species Distribution Modelling (SDM) using MaxEnt.
- Processes occurrence points and environmental data, builds MaxEnt models, and evaluates model performance (AUC, TSS).
- Generates binary predictions and calculates variable importance for each species.

**Inputs**:  
- Presence points from ALA, Flickr, and iNaturalist.  
- Environmental variables (BioClim layers).

**Outputs**:  
- MaxEnt model outputs for each species.  
- Predicted distribution maps.

**Dependencies**:  
- `dismo`, `terra`, `sdm`, `parallel`.

### 3. `SchoeD.R`
**Purpose**:  
- Calculates Schoener's D and Warren's I to measure the niche overlap between different datasets (ALA, Flickr, iNaturalist).
- Provides a quantitative assessment of how much overlap exists between the ecological niches predicted by the models.

**Inputs**:  
- Predicted distribution rasters from different datasets.

**Outputs**:  
- Schoener’s D and Warren's I values.

**Dependencies**:  
- `terra`, `raster`.

---

## Data Files

The occurrence points and environmental data are stored in the respective folders:

### **Bioclimmasked/**:
- Contains `.tif` files for bioclimatic variables masked to the study region (Australia). These are raster files and contain information within the image itself. 

### **Presence Points/**:
- Contains CSV files for the species occurrence points from:
  - ALA (traditional museum records)
  - Flickr (geo-tagged images from social media)
  - iNaturalist (community science data)

---

## Usage Instructions

### Set up environment:
- Install the necessary packages in R: `terra`, `dismo`, `sdm`, `parallel`, `ggplot2`, etc.

### Running the Models:
- **To run the MaxEnt model and predict species distribution**:
  - Update the file paths in `SDModelling.R` to match your local directory.
  - Run the script to generate MaxEnt models for the species.
- **To compare the range gains/losses**:
  - Use `LossGainCalculations.R` to visualize the differences between the models.

### Niche Overlap Calculation:
- Use the `SchoeD.R` script to calculate Schoener's D and Warren's I to assess the niche overlap between the different models.

---

## Results

The results generated by this project show that adding non-traditional data sources (e.g., Flickr, iNaturalist) significantly alters the predicted species ranges and the environmental predictors driving these predictions. Full details of the analysis can be found in the accompanying manuscript.

---

## Citation

If you use this code or data for your own work, please cite the manuscript associated with this project:
R.A. Moore, M.R.E. Symonds and S.R. Howard, Leveraging
social media and community science data for environmental niche models: A case study
with native Australian bees, Ecological Informatics (2024), https://doi.org/10.1016/
j.ecoinf.2024.102857



