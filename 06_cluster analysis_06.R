# Task 06: Cluster Analysis
# Ali Wardi
# 2024.05.14
# Version: 04
# Description: This script performs hierarchical clustering on prediction data and generates maps of clustered predictions.

# Seting the working directory 
setwd("/Users/aliwardi/Desktop/Research project")
# Load required packages
library(RColorBrewer)
library(dendextend)
library(ggplot2)
library(sf)
library(tidyr)
library(dplyr)
library(gridExtra)  
library(grid)
# Load data
load("output/Task05/Script 2 output/dissimilarity_matrix.RData")
load("output/Task05/Script 2 output/aggregated_predictions.RData")
load("output/Task01/info_updated.RData")

# Perform hierarchical clustering using UPGMA
hc <- hclust(dissimilarity_matrix, method = "average")

# Save the clustering result in the specified directory
save(hc, file = "output/Task06/clusters.RData")

# Create the integer vector from 2 to 20
cluster_numbers <- 2:20

# Save the vector to the specified directory
save(cluster_numbers, file = "output/Task06/cluster_numbers.RData")

# Create a character vector with 20 different colors by concatenating color palettes
palette <- c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set1"), brewer.pal(4, "Set2"))

# Save the color palette to the specified directory
save(palette, file = "output/Task06/palette.RData")

# Create subfolders within the output/Task06 directory directly
dir.create("output/Task06/dendrograms", recursive = TRUE, showWarnings = FALSE)
dir.create("output/Task06/maps_of_clusters", recursive = TRUE, showWarnings = FALSE)

# Create a copy of the data directly
aggregated_predictions_clustered <- aggregated_predictions

get_branches_heights <- function(hc, k) {
  heights <- sort(hc$height, decreasing = TRUE)
  return(heights[k])
}
# Iterate through the cluster numbers
for (k in cluster_numbers) {
  # Cut the dendrogram to create k clusters
  clusters <- cutree(hc, k)
  
  # Generate a dendrogram object and apply color
  dend_colored <- as.dendrogram(hc)
  dend_colored <- color_branches(dend_colored, k = k, col = palette[1:k])
  
  # Get the height that results in exactly k clusters
  h_cut <- get_branches_heights(hc, k)
  dend_colored <- cut(dend_colored, h = h_cut)$upper
  
  # Remove branch labels
  labels(dend_colored) <- rep(NA, length(labels(dend_colored)))
  
  # Set line width
  dend_colored <- set(dend_colored, "branches_lwd", 4.25)  
  
  # Plot the dendrogram and save to file
  png(filename = sprintf("output/Task06/dendrograms/%02d.png", k), width = 2400, height = 2000)
  plot(dend_colored, main = paste("Dendrogram for", k, "clusters"),
       ylab = "", sub = "", xlab = "",
       cex = 2,        
       cex.main = 4,     
       cex.lab = 1.25,    
       cex.axis = 2,   
       las = 1)
  dev.off()
  
  # Add new column to aggregated_predictions_clustered for the cluster assignments
  column_name <- paste("clusters", k, sep = "_")
  aggregated_predictions_clustered[[column_name]] <- clusters
}

# Save the updated dataframe to the specified file within the Task06 directory
save(aggregated_predictions_clustered, file = "output/Task06/aggregated_predictions_clustered.RData")

# Remove specified columns using habitat_names and convert from wide to long format focusing on cluster columns
final_aggregated_predictions_long <- aggregated_predictions_clustered %>%
  select(-quadrat_ID, -region_name, -one_of(names(habitat_names))) %>% # Remove unnecessary columns based on habitat_names
  pivot_longer(
    cols = starts_with("clusters_"), 
    names_to = "cluster_numbers",
    values_to = "cluster_ID",
    names_prefix = "clusters_"  
  )

# Modify climate target long names to include line breaks
info_updated$climate_target_long_names <- sapply(info_updated$climate_target_long_names, function(name) {
  # Insert a line break at the space
  gsub(" ", "\n", name, fixed = TRUE)
})



# Iterate through the cluster numbers
for (cluster_number in cluster_numbers) {
  # No need to prepend "clusters_", directly use the cluster_number as it appears in the data
  subset_data <- final_aggregated_predictions_long %>%
    filter(cluster_numbers == as.character(cluster_number))
  
  # Convert cluster_ID to factor with levels as specified
  subset_data$cluster_ID <- factor(subset_data$cluster_ID, levels = 1:cluster_number)
  
  # Create the plot
  plot <- ggplot(subset_data, aes(fill = cluster_ID, geometry = geometry)) +
    geom_sf(color = NA) +  
    scale_fill_manual(values = palette[1:cluster_number]) +
    facet_wrap(~climate_target, nrow = 2, ncol = 3, labeller = as_labeller(info_updated$climate_target_long_names)) +
    labs(title = paste(cluster_number, "clusters of the MPNV predictions aggregated to MÉTA quadrats"),
         fill = "Cluster ID") +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, margin = margin(b = 35, unit = "pt"))  
    )
  
  # Adjust the layout to center the second row if the number of facets is less than 3
  if (length(unique(subset_data$climate_target)) %% 3 != 0) {
    g <- ggplotGrob(plot)
    # Find the locations of the empty panels and remove them
    empty_panels <- which(g$layout$name == "panel" & is.na(g$layout$t))
    if (length(empty_panels) > 0) {
      g$layout <- g$layout[-empty_panels, ]
    }
    # Draw the plot without empty panels
    grid.draw(g)
  } else {
    # Print the original plot if no adjustment is needed
    print(plot)
  }
  
  # Save the plot adjusting for possible non-full rows
  ggsave(sprintf("output/Task06/maps_of_clusters/%02d.png", cluster_number), plot, width = 10, height = 8, dpi = 300)
}
