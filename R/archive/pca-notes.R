library(gridExtra)  # plot arrangement
pca_cats <- here("prose", "next-steps", "pca-categories.xlsx") %>% 
  read_xlsx
groups <- pca_cats$group %>% unique
# The betas in a linear combination of features
# are the loadings of the first principal component.
# To calculate these loadings, we must find the 
# vector that maximizes the variance. 
# It can be shown using techniques from linear algebra that 
# the eigenvector corresponding to the largest eigenvalue of 
# the covariance matrix is the set of loadings that explains 
# the greatest proportion of the variability.
pca_cat <- pca_cats %>% filter(group == groups[1])

group_values_wide <- df %>% 
  select(id, all_of(pca_cat$label)) %>% 
  ## Remove cases with missing data
  filter(!if_any(everything(), is.na))

group_values_wide <- group_values_wide %>% select(- id) 

map_dbl(group_values_wide, ~ var(.x))
## Scale normalizes
scaled_df <- map_df(group_values_wide, scale)
head(scaled_df)

# Calculate eigenvalues & eigenvectors (principal component loading vector)
groups.cov <- cov(scaled_df)
groups.eigen <- eigen(groups.cov)
str(groups.eigen)

(phi <- groups.eigen$vectors[,1:2])
phi <- -phi
row.names(phi) <- pca_cat$label
colnames(phi) <- c("PC1", "PC2")
phi

# If we project the n data points 
# onto the first eigenvector, the projected values 
# are called the principal component scores for each observation.

# Calculate Principal Components scores
PC1 <- as.matrix(scaled_df) %*% phi[,1]
PC2 <- as.matrix(scaled_df) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(person = row.names(group_values_wide), PC1, PC2)
head(PC)

# Plot Principal Components for each State
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = person), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of USArrests Data")

## The eigenvalues add up to the total number of PCs
PVE <- groups.eigen$values / sum(groups.eigen$values)

# PVE (aka scree) plot
PVEplot <- qplot(c(1:length(PVE)), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Cumulative PVE plot
cumPVE <- qplot(c(1:length(PVE)), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)


# In-built ----------------------------------------------------------------

pca_result <- prcomp(group_values_wide, scale = TRUE)
# The rotation matrix provides the principal 
# component loadings; each column of pca_result$rotation
# contains the corresponding principal component loading vector.

# We can also obtain the principal component scores 
# from our results as these are stored in the x list group of our results.
pca_result$x
pca_result$x <- - pca_result$x
## The points in biplot are the principal component scores
biplot(pca_result, scale = 0)

# The prcomp function also outputs the 
# standard deviation of each principal component.
# The variance explained by each principal component 
# is obtained by squaring these values:
(VE <- pca_result$sdev^2)
PVE <- VE / sum(VE)
round(PVE, 2)




  
