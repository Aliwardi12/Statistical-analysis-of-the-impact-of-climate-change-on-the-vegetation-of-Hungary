
 # Task 04:map of Kendall's τ distance
  # Ali Wardi
  # 2023.04.08
  # Version: 03
  # Description: Loads and processes various datasets, to create maps
  
 # Load necessary libraries
 library(sf)
 library(dplyr)
 library(tidyr)
 library(ggplot2)
 library(RColorBrewer)
 library(Kendall)
 
 # Set working directory and load data
 setwd("/Users/aliwardi/Desktop/Research project")
 load("output/Task01/info_updated.RData")
 load("input/predictions.RData")
 load("output/Task01/predictions_rescaled.RData")
 
 # Prepare vegetation type names and climate targets
 veg_type_names <- info_updated$vegetation_types
 # Exclude "reference_1971" from climate targets for dissimilarity calculation
 climate_targets <- info_updated$climate_targets[-1]
 
 # Map short climate target names to long names for plotting
 climate_names_map <- setNames(
   info_updated$climate_target_long_names, 
   names(info_updated$climate_target_long_names)
 )
 
 output_dir <- "output/Task04"

 calculate_dissimilarities <- function(predictions_list, veg_type_names, ref_period, climate_targets) {
   ref_data <- st_drop_geometry(predictions_list[[ref_period]])[veg_type_names]
   
   future_dissimilarities <- lapply(climate_targets, function(target) {
     future_data <- st_drop_geometry(predictions_list[[target]])[veg_type_names]
     
     dissimilarity_vector <- sapply(1:nrow(ref_data), function(i) {
       # Ensure the data passed to cor() is numeric and handle potential NAs
       ref_row_vector <- as.numeric(ref_data[i, , drop = TRUE])
       future_row_vector <- as.numeric(future_data[i, , drop = TRUE])
 
       # Compute Kendall's tau using cor() and handle cases with insufficient data
       tau <- if (all(is.na(ref_row_vector)) || all(is.na(future_row_vector))) {
         NA
       } else {
         cor(ref_row_vector, future_row_vector, method = "kendall", use = "complete.obs")
       }
 
       # Calculate dissimilarity
       dissimilarity <- if (is.na(tau)) NA else 0.5 - 0.5 * tau
       return(dissimilarity)
     })
     
     return(dissimilarity_vector)
   })
   
   dissimilarities_df <- do.call(cbind, future_dissimilarities)
   colnames(dissimilarities_df) <- climate_targets
   
   sf_dissimilarity <- st_sf(dissimilarities_df, geometry = st_geometry(predictions_list[[ref_period]]))
   
   return(sf_dissimilarity)
 }
 
 # Calculating dissimilarities for both prediction types
  dissimilarities <- list(
    "5" = calculate_dissimilarities(predictions, veg_type_names, "reference_1971", climate_targets),
    "3" = calculate_dissimilarities(predictions_rescaled, veg_type_names, "reference_1971", climate_targets)
  )
 save(dissimilarities, file = paste0(output_dir, "/dissimilarities.RData"))
 
  # Function to visualize and save maps with adjusted panel spacing
  visualize_and_save_maps <- function(dissimilarity_sf, type, climate_names_map, output_dir) {
    if (!inherits(dissimilarity_sf, "sf")) {
      stop("The input data must be an 'sf' object.")
    }
    
    long_format_sf <- dissimilarity_sf %>%
      pivot_longer(cols = starts_with("rcp"), names_to = "climate_target", values_to = "dissimilarity") %>%
      mutate(climate_target = climate_names_map[climate_target])
    
    p <- ggplot(long_format_sf) +
      geom_sf(aes(color = dissimilarity), show.legend = "point", size = 0.005) +
      scale_color_gradientn(colors = rev(brewer.pal(11, "RdYlGn")), limits = c(0, 1), na.value = "grey50") +
      facet_wrap(~climate_target, ncol = 2, nrow = 2) +
      labs(title = paste("Dissimilarities for Prediction Type", type), color = "Dissimilarity") +
      theme_minimal() +
      theme(panel.spacing = unit(2, "lines"))  # Adjusted spacing
    
    ggsave(filename = paste0(output_dir, "/dissimilarities_", type, ".png"), plot = p, width = 11, height = 8, dpi = 300)
  }
  
 # Execute visualization for both prediction types
  lapply(names(dissimilarities), function(type) {
    visualize_and_save_maps(dissimilarities[[type]], type, climate_names_map, output_dir)
  })
