# Task 05: Preparation for Multivariate Analyses
# Ali Wardi
# 2024.05.06
# Version: 03
# Description: This script prepares data for multivariate analyses by loading spatial datasets,
# simplifying them, and calculating dissimilarities for further multivariate statistical methods.


# Seting the working directory to the project folder
setwd("/Users/aliwardi/Desktop/Research project")
# Load required packages
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(prettymapr)
library(ROI)
library(ggspatial)
library(RColorBrewer)
library(proxy)
# Ensuring the system locale can handle UTF-8 characters 
Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Load data
load("output/Task01/info_updated.RData")
load("input/predictions.RData")
load("output/Task01/habitat_names.RData")


# Step 2: Load the shapefile and name it "quadrats"
quadrats <- st_read("input/mtkv_eov.shp")

# Step 3: Keeping only the "KVADRAT" column and rename it
quadrats <- quadrats %>% select(quadrat_ID = KVADRAT)

# Step 4: Creating a character vector of quadrat IDs and save it
quadrat_IDs <- as.character(quadrats$quadrat_ID)
save(quadrat_IDs, file = "output/Task05/quadrat_IDs.RData")

# Step 5: Load another shapefile and name it "landscape_units"
landscape_units <- st_read("input/Mo_kozeptajai_0527.shp")

# Correct field names
names(landscape_units) <- gsub("k<U+00F6>z<U+00E9>pt<U+00E1>j", "középtáj", names(landscape_units), fixed = TRUE)

# Step 6: Creating regions based on "középtáj" and perform union
regions <- landscape_units %>%
  mutate(region_name = case_when(
    str_detect(középtáj, "C.3.3.") ~ "Great Hungarian Plain",
    str_detect(középtáj, "C.1.") ~ "Little Hungarian Plain and Alpokalja",
    str_detect(középtáj, "^C.3.1.|C.3.2.1") ~ "Transdanubian Hills",
    str_detect(középtáj, "C.2.") ~ "Transdanubian Mountains",
    str_detect(középtáj, "B.2.") ~ "North Hungarian Mountains"
  )) %>%
  group_by(region_name) %>%
  summarise(geometry = st_union(geometry), .groups = 'drop')

# Task 7: Creating a new subfolder within the Task05 folder
new_folder_path <- "output/Task05/maps_for_checking"
if (!dir.exists(new_folder_path)) {
  dir.create(new_folder_path, recursive = TRUE)
}

# Save the plot to a file
png(file.path(new_folder_path, "regions.png"), width = 800, height = 800)

# Set margins appropriately
par(mar = c(5, 5, 4, 5) + 0.1)

# Define colors using a color palette suitable for categorical data
region_colors <- RColorBrewer::brewer.pal(n = length(unique(regions$region_name)), name = "Set3")

# Correctly plot the regions 
plot(regions[, "region_name"], main = "Regions", col = region_colors, cex.main = 1.2, axes = TRUE)

# Add north arrow and scale bar
prettymapr::addnortharrow()
prettymapr::addscalebar()

# Placing the legend in the bottom right, with a slight inset to avoid cutting off
legend("bottomright", inset=c(0.1, 0), legend=levels(factor(regions$region_name)), 
       fill=region_colors, title="Region Name", cex=0.8, x.intersp = 0.5)

# Close the graphic device
dev.off()

# Step 9: Creating a new character vector of region names
region_names <- unique(regions$region_name)

# Saving the vector to an RData file in the specified output directory
save(region_names, file = "output/Task05/region_names.RData")
# Assign the CRS from 'regions' to 'quadrats'
st_crs(quadrats) <- st_crs(regions)

# Calculate centroids of the quadrats
quadrats_centroids <- st_centroid(quadrats)

# Spatial join of centroids with regions where centroids are within regions
quadrats_with_regions <- st_join(quadrats_centroids, regions, join = st_within)

# Identify quadrats with no assigned region
missing_regions <- is.na(quadrats_with_regions$region_name)

# For quadrats not within any region, join based on largest intersection
if (any(missing_regions)) {
  quadrats_outside <- quadrats[missing_regions, ]
  quadrats_outside_regions <- st_join(quadrats_outside, regions, join = st_intersects, largest = TRUE)
  # Update the initial dataframe with new region assignments
  quadrats_with_regions[missing_regions, "region_name"] <- quadrats_outside_regions$region_name
}  

# Add the region names back to the original 'quadrats' dataframe
quadrats$region_name <- quadrats_with_regions$region_name


# Identify the missing quadrat
missing_quadrat <- quadrats[is.na(quadrats$region_name), ]

# If there is a missing quadrat, assign the closest region
if (nrow(missing_quadrat) > 0) {
  # Calculate distances to all regions for each missing quadrat
  distances <- st_distance(missing_quadrat, regions)
  
  # Find the index of the closest region for each missing quadrat
  closest_region_indices <- apply(distances, 1, which.min)
  
  # Assign the name of the closest region
  quadrats$region_name[is.na(quadrats$region_name)] <- regions$region_name[closest_region_indices]
  
  # Confirm assignment
  cat("Updated missing region names based on proximity.\n")
} else {
  cat("No quadrats are missing a region name.\n")
}

# Convert 'quadrat_ID' to a factor with specific levels
quadrats$quadrat_ID <- factor(quadrats$quadrat_ID, levels = quadrat_IDs)

# Convert 'region_name' to a factor with specific levels
quadrats$region_name <- factor(quadrats$region_name, levels = region_names)

# Save the 'quadrats' sf object to an RData file in a specific directory
save(quadrats, file = "output/Task05/quadrats.RData")


# Create the plot with ggplot2
map_plot <- ggplot(data = quadrats) +
  geom_sf(aes(fill = region_name), color = NA, alpha = 0.5) +  
  scale_fill_manual(values = brewer.pal(n = length(unique(quadrats$region_name)), name = "Set3")) +
  labs(title = "Map of Quadrats by Region", fill = NULL) +  
  theme_void() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",  
    legend.box.margin = margin(t = 0, b = 20, l = 20, r = 20),  
    legend.margin = margin(t = 0, unit = "pt"),  
    legend.key.size = unit(1, "lines"),  
    plot.background = element_rect(fill = "white", colour = NA),  
    legend.background = element_rect(fill = "white", colour = NA), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  
    axis.text.y = element_text(angle = 45, hjust = 1),  
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    plot.margin = margin(20, 20, 20, 20)  
  ) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.5)

# Save the plot 
ggsave("output/Task05/maps_for_checking/quadrats.png", plot = map_plot, width = 10, height = 10, units = "in", bg = "white")

# Step 14: Iterating through the climate targets and applying the specified operations
results <- lapply(info_updated$climate_targets, function(climate_target) {
  # Step 15: Create a copy of the selected list element
  data_copy <- predictions[[climate_target]]
  
  # Step 16: Drop geometry and remove "hexagon_ID" column
  data_copy <- st_drop_geometry(data_copy) %>%
    select(-hexagon_ID)
  
  # Step 17: Aggregate the data from hexagons to quadrats using the maximum value
  aggregated_data <- data_copy %>%
    group_by(quadrat_ID) %>%
    summarise(across(.cols = everything(), .fns = max, na.rm = TRUE))
  
  # Step 18: Merge with the 'quadrats' data frame
  
  # Merge using dplyr to handle sf objects correctly
  merged_data <- merge(quadrats, aggregated_data, by = "quadrat_ID", all.x = TRUE)
  
  # Step 19: Add a new column for the climate target
  merged_data$climate_target <- climate_target
  
  # Return the merged data frame for this iteration
  merged_data
})

# Step 20: Combine all data frames into one large sf object
aggregated_predictions <- do.call(rbind, results)

# Converting the 'climate_target' column from character type to factor type
aggregated_predictions$climate_target <- factor(aggregated_predictions$climate_target,
                                                levels = info_updated$climate_targets)

# Save the 'aggregated_predictions' sf object to the specified RData file 
save(aggregated_predictions, file = "output/Task05/aggregated_predictions.RData")

# Remove 'quadrat_ID' and 'region_name' columns
aggregated_predictions <- aggregated_predictions %>%
  select(-quadrat_ID, -region_name)
# Step 23: Reshape the data to long format
# First, retrieve the name of the geometry column safely
geometry_col_name <- attr(x = aggregated_predictions, which = "sf_column")

# Reshape using pivot_longer by specifying only the vegetation types from info_updated
long_format <- aggregated_predictions %>%
  tidyr::pivot_longer(
    cols = info_updated$vegetation_types,  
    names_to = "vegetation_type",
    values_to = "prediction"
  ) %>%
  # Ensure the geometry column is kept intact
  sf::st_sf()

# Explicitly setting the geometry column to make sure it is preserved
sf::st_geometry(long_format) <- geometry_col_name

# Step 24: Convert the 'vegetation_type' column to a factor with specified levels
long_format$vegetation_type <- factor(long_format$vegetation_type, levels = info_updated$vegetation_types)

# Use lapply() to iterate through vegetation types
lapply(info_updated$vegetation_types, function(veg_type) {
  # Filter long_format for the current vegetation type
  veg_data <- long_format %>% filter(vegetation_type == veg_type)
  
  # Convert predictions to factor if they are not already
  veg_data$prediction <- factor(veg_data$prediction, levels = 0:4, 
                                labels = c("not probable", "slightly probable", "moderately probable", "probable", "highly probable"))
  
  # Retrieve full vegetation name for the plot title using habitat_names
  full_veg_name <- habitat_names[veg_type]
  
  # Create the plot with wrapped labels for the climate targets
  plot <- ggplot(data = veg_data) +
    geom_sf(aes(fill = prediction), color = NA) +
    facet_wrap(~ climate_target, ncol = 1, scales = 'fixed', labeller = labeller(
      climate_target = as_labeller(info_updated$climate_target_long_names, label_wrap_gen(width = 20)),
      vegetation_type = as_labeller(veg_type)
    )) +
    scale_fill_manual(values = c("red", "sandybrown", "khaki", "lightgreen", "darkgreen")) +
    labs(title = full_veg_name, fill = "Prediction Rank") +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 12),
          strip.text.x = element_text(size = 10, angle = 0, hjust = 0.5))  
  
  # Save the plot
  file_path <- file.path(new_folder_path, paste0(veg_type, ".png"))
  ggsave(file_path, plot = plot, width = 10, height = 8, dpi = 300, units = 'in')
})


# Define the Kendall tau dissimilarity function
kendall_dissimilarity <- function(x, y) {
  tau <- cor(x, y, method = "kendall", use = "complete.obs")
  return(0.5 - 0.5 * tau)
}

# then calculate the dissimilarity
dissimilarity_matrix <- proxy::dist(
  as.matrix(st_drop_geometry(aggregated_predictions)[, info_updated$vegetation_types]),
  method = kendall_dissimilarity
)
# Specify the file path
file_path <- "output/Task05/dissimilarity_matrix.RData"

# Save the dissimilarity matrix to the specified file
save(dissimilarity_matrix, file = file_path)

