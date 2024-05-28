
 # Cross-validation project
 # Task 1: Dataset Preparation
 # Ali Wardi
 # 2023.04.03
 # Version: 06
 # Description: Loads and processes various datasets, including habitat names and predictions, for further analysis in the project.
 
 # Initialization of the workspace
 rm(list = ls()) # Clear the workspace
 setwd("/Users/aliwardi/Desktop/Research project") # Set working directory to the research project folder
 
 # Required libraries
 required_packages <- c("readxl", "openxlsx", "rrapply") # Required packages
 installed_packages <- row.names(installed.packages()) # Currently installed packages
 
 # Install and load required packages
 for (package in required_packages) {
     if (!(package %in% installed_packages)) {
         install.packages(package, repos = "http://cran.r-project.org")
     }
     library(package, character.only = TRUE)
 }
 
 # 1) Load the habitat_names.xlsx file
 file_name <- "/Users/aliwardi/Desktop/Research project/input/habitat_names.xlsx" 
 habitat_names <- xlsx::read.xlsx(file = file_name, sheetName = "Munka1") # Load data from the Excel file
 
 # 2) Create a named character vector and save it with the name 'habitat_names'
 habitat_names <- setNames(habitat_names$name, habitat_names$code) # Name elements by habitat codes
 
 # 3) Save the named character vector as 'habitat_names.RData'
 output_file <- "/Users/aliwardi/Desktop/Research project/output/task01/habitat_names.RData" 
 save(habitat_names, file = output_file) # Save the named character vector
 
 # 4) Load the predictions.RData file
 load("/Users/aliwardi/Desktop/Research project/input/predictions.RData") # Load predictions
 
 # 5) Rescale predictions (excluding "hexagon_ID" columns if present)
 predictions_rescaled <- rrapply(predictions, condition = function(x, .xname) .xname %in% habitat_names, 
                                  f = function(x) ifelse(x == 0, 1, ifelse(x == 4, 3, x)),
                                  how = "replace") # Rescale 0 to 1 and 4 to 3

 
 # 6) Save it to predictions_rescaled.RData
 save(predictions_rescaled, file = "/Users/aliwardi/Desktop/Research project/output/task01/predictions_rescaled.RData") # Save rescaled predictions
 
 # 7) Load the info.RData
 load("/Users/aliwardi/Desktop/Research project/input/info.RData") # Load info
 
 # 8) Create a copy called info_updated
 info_updated <- info # Copy info object
 
 # 9) Add a new list element called description_of_ordinal_values_rescaled
 info_updated$description_of_ordinal_values_rescaled <- c("1" = "not probable", 
                                                          "2" = "moderately probable", 
                                                          "3" = "highly probable")
 
 # 9-1) Add the new mapping for climate targets
 info_updated$climate_target_long_names <- c("reference_1971" = "CarpatClim-Hu (1971-2000)", 
                                             "rcp45_hadgem_2069" = "RCP4.5, HadGEM-RegCM (2069-2098)", 
                                             "rcp45_mpi_2069" = "RCP4.5, MPI-RegCM (2069-2098)", 
                                             "rcp85_hadgem_2069" = "RCP8.5, HadGEM-RegCM (2069-2098)", 
                                             "rcp85_mpi_2069" = "RCP8.5, MPI-RegCM (2069-2098)")
 
 # 10) Save it to info_updated.RData
 save(info_updated, file = "/Users/aliwardi/Desktop/Research project/output/task01/info_updated.RData") # Save updated info
 
 # Indicate completion
 cat("Task 1 completed. All files processed and saved.\n")
Task 1 completed. All files processed and saved.
