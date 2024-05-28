
 load("/Users/aliwardi/Desktop/Research project/output/Task07/untitled folder/pcoa.RData")
 # Load necessary libraries
 library(vegan)


 # Calculate the proportion of variance explained by each axis
 eigenvalues <- pcoa$eig
 variance_explained <- eigenvalues / sum(eigenvalues)
 
 # Calculate the cumulative variance explained by the first two axes
 cumulative_variance <- cumsum(variance_explained)
 
 # Print the variance explained by the first two axes
 variance_explained_1_2 <- variance_explained[1:2] * 100
 cat("Variance explained by Axis 1:", variance_explained_1_2[1], "%\n")
Variance explained by Axis 1: 44.30613 %
 cat("Variance explained by Axis 2:", variance_explained_1_2[2], "%\n")
Variance explained by Axis 2: 19.23 %
 cat("Cumulative variance explained by the first two axes:", cumulative_variance[2] * 100, "%\n")
#Cumulative variance explained by the first two axes: 63.53613 %
 
 # Check if the cumulative variance is close to 100%
 if (cumulative_variance[2] > 0.95) {
   cat("The first two axes explain more than 95% of the variance, making it reasonable to interpret Fig5 without showing additional axes.\n")
 } else {
   cat("The first two axes explain less than 95% of the variance, consider examining additional axes for a more comprehensive understanding.\n")
 }
#The first two axes explain less than 95% of the variance, consider examining additional axes for a more comprehensive understanding.

 