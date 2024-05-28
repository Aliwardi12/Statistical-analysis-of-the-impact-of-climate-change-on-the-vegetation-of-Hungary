 # Task 3: Boxplot and Tukey Test
   # Statistical Analysis and Visualization
   # Written by Ali Wardi
   # 2024.03.21
   # Version: 10
   # Description: Analyzes climate predictions, creates annotated boxplots, and exports them.

 library(sf)
 library(dplyr)
 library(tidyr)
 library(emmeans)
 library(multcomp)
 library(ggplot2)
 library(grid)
 
 # Set the working directory to the project folder
 setwd("/Users/aliwardi/Desktop/Research project")
  # Load the data files
 load("output/Task01/info_updated.RData")
 load("input/predictions_continuous.RData")
 
 # Correct the vegetation_long_names to match the studied vegetation types
 studied_vegetation_long_names <- info_updated$vegetation_long_names[info_updated$vegetation_types]
 names(studied_vegetation_long_names) <- info_updated$vegetation_types
 
 # Create a list of data frames for each climate target
 data_frames_list <- lapply(info_updated$climate_targets, function(target) {
   df <- predictions_continuous[[target]]
   df <- st_drop_geometry(df)  # Remove geometry
   df <- df[, info_updated$vegetation_types]  # Keep columns for vegetation types
   df_long <- pivot_longer(df, cols = everything(), names_to = "vegetation_type", values_to = "prediction")
   
   # Assign vegetation_type_long_name using the corrected subset
   df_long$vegetation_type_long_name <- factor(df_long$vegetation_type,
                                               levels = names(studied_vegetation_long_names),
                                               labels = studied_vegetation_long_names)
   df_long$climate_target <- target
   
   # Correctly assign climate_target_long_name using climate_target_names
   df_long$climate_target_long_name <- info_updated$climate_target_names[target]
   
   return(df_long)
 })
 
 # Combine the individual data frames into a single large data frame
 large_data_frame <- do.call(rbind, data_frames_list)
 # Analyze each vegetation type for significance differences among climate targets
    vegetation_analysis_results <- lapply(info_updated$vegetation_types, function(veg_type) {
      df_subset <- filter(large_data_frame, vegetation_type == veg_type)
      
      # Linear model of prediction by climate target
      lm_model <- lm(prediction ~ climate_target, data = df_subset)
      
      # Post-hoc analysis using emmeans for significance grouping
      emm_res <- emmeans(lm_model, specs = ~ climate_target)
      tukey_groups <- cld(emm_res, Letters = letters, adjust = "tukey")
    
      # Rename '.group' column and trim whitespaces
      colnames(tukey_groups)[colnames(tukey_groups) == ".group"] <- "significance_group"
      tukey_groups$significance_group <- trimws(tukey_groups$significance_group)
    
      # Calculate maximum prediction values for each climate target
     max_predictions <- df_subset %>%
        group_by(climate_target) %>%
        summarise(maximum_prediction = max(prediction), .groups = 'drop')
      
      # Merge Tukey test results with maximum predictions
      final_df <- merge(max_predictions, tukey_groups, by = "climate_target", all.x = TRUE)
      final_df$vegetation_type <- veg_type
      
      # Structure final data frame
      final_df <- final_df[, c("vegetation_type", "climate_target", "significance_group", "maximum_prediction")]
      
     return(final_df)
    })
    # Combine the individual data frames into a single large data frame
       letters_and_maximums <- do.call(rbind, vegetation_analysis_results)
       
       # Convert character-type columns to factor with correct mapping for levels
       letters_and_maximums$vegetation_type <- factor(letters_and_maximums$vegetation_type,
                                                      levels = info_updated$vegetation_types)
       
       letters_and_maximums$climate_target <- factor(letters_and_maximums$climate_target,
                                                     levels = info_updated$climate_targets)
       
       # The path  to save the data frame
       save_path_RData <- "output/Task03/letters_and_maximums.RData"
       
      # Check for NA values in the first three columns
       if (!anyNA(letters_and_maximums[, 1:3])) {
         save(letters_and_maximums, file = save_path_RData)
       } else {
         cat("NA values detected in the first three columns.")
       }
 
 # Extract unique climate targets and their corresponding long names
 unique_climate_targets <- unique(large_data_frame[c("climate_target", "climate_target_long_name")])
 
 # Define the labeller function for wrapping long text in facet labels
 labeller_function <- as_labeller(setNames(
   object = sapply(X = studied_vegetation_long_names, FUN = function(vegetation_long_name) {
     # Increase the width to allow more characters per line
     vegetation_long_name <- strwrap(vegetation_long_name, width = 40, simplify = FALSE) 
     paste(vegetation_long_name[[1]], collapse = "\n")
   }),
   nm = names(studied_vegetation_long_names)
 ))
 
 # Adjusted color palette to reflect related climate targets
 climate_colors <- c(
   "reference_1971" = "#d95f02",        
   "rcp45_hadgem_2069" = "#377eb8",     
   "rcp45_mpi_2069" = "#66c2a5",        
   "rcp85_hadgem_2069" = "#034e7b",     
   "rcp85_mpi_2069" = "#238b45"         
 )
 
 # Mapping climate targets to their long names for legend labels
 climate_target_long_name_map <- unique_climate_targets$climate_target_long_name
 names(climate_target_long_name_map) <- unique_climate_targets$climate_target
 
 # Convert the fill colors to RGB and make them darker for the borders
 border_colors <- col2rgb(climate_colors) / 1.5
 border_colors <- rgb(t(border_colors), maxColorValue = 255)
 
 # Create the plot 
 p <- ggplot(large_data_frame, aes(x = climate_target, y = prediction, fill = climate_target)) +
   geom_boxplot(aes(color = climate_target), outlier.alpha = 0.3, outlier.shape = 1, width = 0.5) +
   geom_text(
     data = letters_and_maximums, 
     aes(label = significance_group, y = maximum_prediction + 0.05, color = climate_target), 
     size = 3.5,  
     vjust = -1   
   ) +
   scale_fill_manual(values = climate_colors, labels = climate_target_long_name_map) +
   scale_color_manual(values = border_colors, labels = climate_target_long_name_map) + 
   facet_wrap(~vegetation_type, nrow = 7, labeller = labeller_function) +
   scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1.4)) +
   theme_minimal() +
   theme(
     strip.text = element_text(face = "bold", size = 8),
     strip.background = element_rect(colour = NA, fill = NA),
     panel.spacing.y = unit(1, "lines"),
     axis.text.x = element_blank(), 
     legend.title = element_text(size = 10),
     legend.text = element_text(size = 8),
     legend.position = "right"
   ) +
   labs(
     fill = "Climate Target", 
     color = "Climate Target", 
     x = "Climate Target"
   ) +
   guides(color = guide_legend(override.aes = list(size=5)))
 
 # Save the plot 
 ggsave("output/Task03/boxplot.png", plot = p, width = 20, height = 15, dpi = 300, bg = "white") 