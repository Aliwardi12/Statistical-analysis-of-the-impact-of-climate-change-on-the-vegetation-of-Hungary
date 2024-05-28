#Cross-validation project
 # Task 2: Stacked Bar Charts
 # Created by Ali Wardi
 # Date: 06-02-2024
 # This script processes predictions and creates stacked bar charts.


 # Required Libraries
 library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales) # For label_comma

# Set Working Directory
setwd("/Users/aliwardi/Desktop/Research project")

# Load Data
load("output/Task01/info_updated.RData")
load("input/predictions.RData")
load("output/Task01/predictions_rescaled.RData")

# Load habitat names
load("output/Task01/habitat_names.RData")
 
# Update info_updated with habitat long names and climate targets
info_updated$vegetation_long_names <- setNames(habitat_names$name, habitat_names$code)
 info_updated$climate_target_names <- setNames(
  c("CarpatClim-Hu (1971-2000)", "RCP4.5, HadGEM-RegCM (2069-2098)", "RCP4.5, MPI-RegCM (2069-2098)", 
    "RCP8.5, HadGEM-RegCM (2069-2098)", "RCP8.5, MPI-RegCM (2069-2098)"),
   info_updated$climate_targets
 )
 
# Define Climate Targets from the info_updated object
 climate_targets <- info_updated$climate_targets
 
# Create and Save Plots
 output_dir <- "output/Task02"
 dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
 
 # Updated function to return color palette based on the number of prediction levels
 get_colors <- function(n) {
  if (n == 3) {
    # Corrected for 3-level values: Highly probable to Not probable (according to suggestion 10 in feedback 1)
     return(c("darkgreen", "khaki1", "red")) # Corrected order for 3-level values
   } else if (n == 5) {
     # Corrected for 5-level values: Highly probable to Not probable
    return(c("darkgreen", "lightgreen", "khaki1", "sandybrown", "red")) # Corrected order for 5-level values (according to suggestion 10 in feedback 1)
   } else {
    stop("Unexpected number of prediction levels")
  }
 }

for (prediction_type in c("original", "rescaled")) {
   predictions_data <- if (prediction_type == "original") predictions else predictions_rescaled
   
   # Process Each Climate Target
   climate_data_list <- lapply(climate_targets, function(target) {
     prediction_data <- predictions_data[[target]]
     
    # Remove geometry
     prediction_data <- st_drop_geometry(prediction_data)
     
     # Keep only columns that have predictions
     prediction_data <- prediction_data[, info_updated$vegetation_types, drop = FALSE]
     
    # Create a long-format data frame
    long_data <- pivot_longer(prediction_data, cols = names(prediction_data), names_to = "vegetation_types", values_to = "prediction")
    
     # Add a new column for climate target with informative names
     long_data$climate_target <- info_updated$climate_target_names[target]
     
     # Order vegetation types according to the order in info_updated$vegetation_types (based on the suggestion 8 in feedback 3)
   long_data$vegetation_types <- factor(long_data$vegetation_types, 
                                       levels = info_updated$vegetation_types,
                                        labels = info_updated$vegetation_long_names[info_updated$vegetation_types])
   
   return(long_data)
   })
   
   # Combine the data frames into one large data frame
  combined_data <- do.call(rbind, climate_data_list)
  
  # Get the number of prediction levels
  num_levels <- length(unique(combined_data$prediction))
  
   # Adjust values and labels based on prediction type
   values <- get_colors(num_levels)
   if (prediction_type == "original") {
    labels <- c("highly probable", "probable", "moderately probable", "slightly probable", "not probable")
     combined_data$prediction <- factor(combined_data$prediction, levels = 4:0, labels = labels)
   } else {
     labels <- c("highly probable", "moderately probable", "not probable")
     combined_data$prediction <- factor(combined_data$prediction, levels = 3:1, labels = labels)
   }
  
  file_name_suffix <- if (prediction_type == "original") "5" else "3"
   
   # Create ggplot Figure with modified x-axis and vertical axes labels
   plot <- ggplot(data = combined_data, aes(x = climate_target, fill = prediction)) +
     geom_bar(position = "stack") +
     scale_fill_manual(values = values, labels = labels) +
     theme_minimal() +
    labs(fill = "Prediction Rank") +
    facet_wrap(~ vegetation_types, scales = 'fixed', nrow = 7, labeller = label_wrap_gen(multi_line = TRUE)) +
    theme(strip.text.x = element_text(size = 8, angle = 0)) +
     theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
     theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
     scale_y_continuous(labels = label_comma())
   
   # Save Plot with the specified file naming convention
   file_name <- paste0("stacked_barchart_", file_name_suffix, ".png")
   file_path <- file.path(output_dir, file_name)
   ggsave(file_path, plot = plot, width = 20, height = 15, dpi = 300)
 }
