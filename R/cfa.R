
library(lavaan)
## Read data and discard re-test
df <- data_path %>% 
  read_rds %>% 
  filter(survey_type == "survey")

##NB 
## Two items means a covariance matrix of 4 values
## 3 of which are unique since
## K(K+1)/2 = unique elements
## Each factor needs at least 3 items
## 3 items results in a saturated model 
## where the number of free parameters equals 
## the number of elements in the variance-covariance matrix

# KMO for FL is 0.05, too low for PCA
fl_factors_1 <- cd %>%
  rename(group = fl_dimension,
         label = labels) %>%
  filter(
    group %in% c(
      # "Nutrition",
                 "Agri-Food Systems",
                 "Cultural"
                 ## 3-item
                 # , "Eco"
                 ## 2-item
                 # , "Food Media", "Civics"
                 ) &
      !label %in% c("agri_exp", "IC_WNsafe") ## removed from analysis
  ) %>%
  select(group, label)

fl_factors_2 <- fl_subgroups %>%
  mutate(
    group = case_when(
      group != "Nutrition" ~ "Combined",
      TRUE ~ group)
  )

specify_factor <- function(df, group){
  factor_title <- to_snake_case(group)
  sum <- df %>% 
    filter(group == !!group) %>% 
    pull(label) %>% 
    glue_collapse(" + ")
  
  glue("{factor_title} =~ {sum}")
}

specify_cfa <- function(df){
  factors <- df$group %>% unique
  map(factors, ~ specify_factor(df, .x)) %>% glue_collapse("\n")
}

cfa_model <- fl_factors_1 %>% specify_cfa
cfa_model_2 <- fl_factors_2 %>% specify_cfa

cfa <- cfa(cfa_model, data = df, std.lv = TRUE)
cfa2 <- cfa(cfa_model_2, data = df, std.lv = TRUE)

summary(cfa, fit.measures = TRUE, standardized = TRUE)
standardizedsolution(cfa)

summary(cfa2, fit.measures = TRUE, standardized = TRUE)

# lavInspect(cfa, "cov.lv")
