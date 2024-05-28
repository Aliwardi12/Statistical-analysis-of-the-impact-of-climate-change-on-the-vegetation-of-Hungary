# Statistical Analysis of the Impact of Climate Change on the Vegetation of Hungary

This repository contains the code used for the thesis titled **"Statistical Analysis of the Impact of Climate Change on the Vegetation of Hungary"**. The thesis investigates the potential impacts of climate change on Hungary's vegetation using statistical and spatial techniques.

## Overview

The study analyzes data from the Multiple Potential Natural Vegetation (MPNV) prediction dataset and employs advanced modeling techniques to assess the survival probability of various vegetation types under different climate scenarios (RCP4.5 and RCP8.5).

## Repository Structure

- **01_DataPreparation_and_Rescaling.R**: Script for preparing and rescaling the dataset.
- **02_stacked_barcharts.R**: Script for creating stacked bar charts for visualizing vegetation data.
- **03_boxplot_and_Tukey_test.R**: Script for generating boxplots and performing Tukey's test for statistical analysis.
- **04-map_of_Kendalls_tau_distance.R**: Script for mapping Kendall's τ distance to show regional variations.
- **05_preparation_for_multivariate_analyses.R**: Script for preparing data for multivariate analysis.
- **06_cluster_analysis.R**: Script for performing cluster analysis on the vegetation data.
- **07_ordination.R**: Script for conducting ordination analysis.
- **Variance.R** This script calculates and analyzes the variance within the dataset, which is crucial for understanding the spread and dispersion of vegetation 
   types under different climate scenarios.

## Scripts Description

1. **01_DataPreparation_and_Rescaling.R**
   - Prepares and rescales the data for analysis.
   - Handles missing values and normalizes the data for subsequent analyses.

2. **02_stacked_barcharts.R**
   - Creates stacked bar charts to visualize the composition and distribution of different vegetation types.
   - Helps in understanding the relative abundance of vegetation categories.

3. **03_boxplot_and_Tukey_test.R**
   - Generates boxplots to illustrate the distribution of vegetation metrics.
   - Performs Tukey's test to identify significant differences between groups.

4. **04-map_of_Kendalls_tau_distance.R**
   - Maps Kendall's τ distance to highlight regional variations in vegetation response to climate change.
   - Visualizes areas of stability and significant shifts in vegetation.

5. **05_preparation_for_multivariate_analyses.R**
   - Prepares the dataset for multivariate statistical analyses.
   - Includes data transformation and selection of relevant variables.

6. **06_cluster_analysis.R**
   - Conducts cluster analysis to group similar vegetation types based on their characteristics.
   - Identifies patterns and relationships within the data.

7. **07_ordination.R**
   - Performs ordination analysis to reduce the dimensionality of the data.
   - Helps in identifying major gradients and patterns in vegetation composition.
     
8. **variance.R**
  - Calculates the variance for various vegetation metrics.

    
## Usage

1. Clone the repository to your local machine:
   ```sh
   git clone https://github.com/Aliwardi12/Statistical-analysis-of-the-impact-of-climate-change-on-the-vegetation-of-Hungary.git
   
2. Navigate to the repository directory:
   ```sh
   cd Statistical-analysis-of-the-impact-of-climate-change-on-the-vegetation-of-Hungary
   
3. Open and run the scripts in R or RStudio in the sequence provided.

## Contact

For any questions or further information, please contact wardi_ali@outlook.com


Feel free to adjust the contact information and any other details as needed!
