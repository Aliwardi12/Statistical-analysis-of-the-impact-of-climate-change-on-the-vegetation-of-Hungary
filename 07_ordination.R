# Task 07: Ordination Analysis
# Ali Wardi
# 2024.05.15
# Version: 03
# Description: This script performs principal coordinate analysis (PCoA) on dissimilarity data 
#and plots ordination with region and climate target annotations.

# Seting the working directory 
setwd("/Users/aliwardi/Desktop/Research project")

# Load required packages
load("output/Task05/Script 2 output/dissimilarity_matrix.RData")
load("output/Task05/Script 2 output/aggregated_predictions.RData")
load("output/Task01/info_updated.RData")
load("output/Task05/Script 2 output/region_names.RData")

# Load required packages
library(vegan)


# Create a 5-element integer vector for point symbols
symbols <- c(0:2, 5:6)
# Create a 5-element vector for line types
line_types <- c(1, 2, 3, 4, 6)
# Create a 5-element character vector for colors representing different climate targets
colors <- c("#FFA500", "#4682B4", "#2E8B57", "#00008B", "#006400")
# Create a copy of 'aggregated_predictions' and remove the 'geometry' column
symbols_and_colors <- aggregated_predictions
symbols_and_colors$geometry <- NULL  # Drop the 'geometry' column
# Keep only specific columns in 'symbols_and_colors'
symbols_and_colors <- symbols_and_colors[, c("quadrat_ID", "region_name", "climate_target")]

# Creating mappings 
symbol_mapping <- setNames(symbols, region_names)
color_mapping <- setNames(colors, info_updated$climate_targets)
line_type_mapping <- setNames(line_types, region_names)

# Add 'symbol' and 'color' columns
symbols_and_colors$symbol <- symbol_mapping[as.character(symbols_and_colors$region_name)]
symbols_and_colors$color <- color_mapping[as.character(symbols_and_colors$climate_target)]
# Save the 'symbols_and_colors' data frame to an RData file
save(symbols_and_colors, file="output/Task07/symbols_and_colors.RData")


# Perform PCoA using the dissimilarity matrix
pcoa <- wcmdscale(dissimilarity_matrix, eig = TRUE, x.ret = FALSE)
# Save the PCoA results
save(pcoa, file="output/Task07/pcoa.RData")

load("/Users/aliwardi/Desktop/Research project/output/Task07/untitled folder/pcoa.RData")

# Combine PCoA results with symbols_and_colors data frame
plot_data <- data.frame(Dim1 = pcoa$points[, 1], Dim2 = pcoa$points[, 2], symbols_and_colors)

# Get long names for climate targets
long_names <- info_updated$climate_target_long_names

# Open a PNG device with larger dimensions to accommodate legends
png("output/Task07/pcoa.png", width = 24, height = 18, units = "in", res = 300)

# Adjust outer margins to provide more space for legends
par(oma = c(4, 4, 4, 12))  

# Set the font size directly
par(mar = c(4, 5, 2, 2), cex.lab = 1.6, cex.axis = 1.6, cex.main = 1.6)  

# Plot the first two axes of the PCoA with extended limits to accommodate legends
plot(pcoa$points[, 1], pcoa$points[, 2], 
     pch = symbols_and_colors$symbol, 
     col = symbols_and_colors$color,
     xlab = "PCoA Axis 1", 
     ylab = "PCoA Axis 2", 
     main = "PCoA with Region and Climate Target Annotations",
     xlim = range(pcoa$points[, 1]) * c(1.4, 1.4), 
     ylim = range(pcoa$points[, 2]) * c(1.3, 1.3))

# Iterate through the regions to draw ordiellipses with a white halo and distinct line types 
for (region in region_names) {
  # White halo
  ordiellipse(pcoa, groups = symbols_and_colors$region_name,
              show.groups = region,
              kind = "sd", draw = "lines",
              col = "white", lty = line_type_mapping[region], lwd = 3)  
  # Colored ellipse
  ordiellipse(pcoa, groups = symbols_and_colors$region_name,
              show.groups = region,
              kind = "sd", draw = "lines",
              col = "black", lty = line_type_mapping[region], lwd = 3)  
}

# Iterate through the climate targets to draw ordiellipses with a white halo
for (target in info_updated$climate_targets) {
  # White halo
  ordiellipse(pcoa, groups = symbols_and_colors$climate_target,
              show.groups = target,
              kind = "sd", draw = "lines",
              col = "white", lty = 1, lwd = 3)
  # Colored ellipse
  ordiellipse(pcoa, groups = symbols_and_colors$climate_target,
              show.groups = target,
              kind = "sd", draw = "lines",
              col = color_mapping[target], lty = 1, lwd = 2)
}

# Add legend for regions and climate targets in the top-left corner
legend(x = "topleft", inset = c(0.02, 0.02), 
       legend = c(region_names, long_names), 
       pch = c(symbols, rep(NA, length(long_names))), 
       col = c(rep("black", length(region_names)), colors), 
       lty = c(line_types, rep(1, length(long_names))), 
       lwd = c(rep(2, length(region_names)), rep(5, length(long_names))), 
       title = "Regions and Climate Targets", bty = "n", cex = 1.6, ncol = 1)

# Close the PNG device
dev.off()
