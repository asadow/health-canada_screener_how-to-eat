groups <- pca_cats$group %>% unique

pca_cats <- pca_cats %>% filter(group == groups[1])

item_values_wide <- df %>% 
  select(id, all_of(pca_cats$label)) %>% 
  ## Remove cases with missing data
  filter(!if_any(everything(), is.na))

item_values <- item_values_wide %>% 
  pivot_longer(
    cols = - id
  )

## Using EFA.dimensions:
# res.pca <- PCA(item_values_wide %>% select(- id))

## FactoMineR
res.pca <- FactoMineR::PCA(item_values_wide %>% select(-id),
                           scale.unit = TRUE, 
                           graph = FALSE)
res.pca$eig
pca_result$rotation
pca_result$sdev^2

?FactoMineR::PCA

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(res.pca, col.var = "black")
var$coord

var <- get_pca_var(res.pca)
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)  
# fviz_contrib(res.pca, choice = "var", axes = 3)
?fviz_pca_var
fviz_pca_var(res.pca, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             )

# Silge -------------------------------------------------------------------


item_values %>%
  group_by(name) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  arrange(desc(value)) %>%
  ggplot(aes(name, value, label = name, fill = name)) +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  geom_text(aes(name, 0.001),
            hjust = 0,
            color = "white",
            size = 4,
            family = "IBMPlexSans-Bold") +
  coord_flip() +
  labs(x = NULL, y = "Average Value of Items")

sparse_item_matrix <- item_values %>%
  tidytext::cast_sparse(row = id, column = name, value = value)

items_pca <- prcomp(sparse_item_matrix, scale. = TRUE)


library(broom)
tidied_pca <- bind_cols(name = pca_cats$label,
                        items_pca$rotation) %>%
  pivot_longer(cols = - name,
               names_to = "PC",
               values_to = "Contribution")

tidied_pca %>% 
  ggplot(aes(name, Contribution, fill = name)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  # theme(axis.text.x = element_blank(), 
  #       axis.ticks.x = element_blank(),
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank()) + 
  labs(x = "name",
       y = "Relative importance in each principal component") +
  facet_wrap(~ PC, ncol = 2)


tidied_pca %>%
  filter(PC == "PC1") %>%
  mutate(name = reorder(name, Contribution)) %>%
  ggplot(aes(name, Contribution, fill = name)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, 
                                   vjust = 0.5), 
        axis.ticks.x = element_blank()) + 
  labs(x = "Item PCs",
       y = "Relative importance in principle component")

# 
# percent_variation <- items_pca$sdev^2 / sum(items_pca$sdev^2)
# 
# augmented_pca <- bind_cols(id = rownames(items_pca$x),
#                            items_pca$x)
# 
# augmented_pca %>%
#   ggplot(aes(PC1, PC2)) +
#   geom_point(size = 1.3, color = "midnightblue", alpha = 0.1) +
#   labs(x = paste0("Principal component 1 (",  percent(percent_variation[1]), ")"), 
#        y = paste0("Principal component 2 (",  percent(percent_variation[2]),")"),
#        title = "Projection of Be mindful on the first two principal components")
