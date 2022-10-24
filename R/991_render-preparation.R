# Import and source -------------------------------------------------------
library(here)
source(here("R", "helper_rmd-sources.R"))

df <- read_rds(data_path)

## Non Re-test -------------------------------------------------------------

df_all <- df

df <- df %>% filter(survey_type == "survey")

# Analysis Strata ---------------------------------------------------------

strata <- c(
  "language",
  "gender",
  "education_group",
  "health_literacy",
  "age_group",
  "smoking_group", # Not enough smokers (13)
  "living_situation",
  "income_adequacy"
)

# Output ------------------------------------------------------------------

## Directories -------------------------------------------------------------

figs_demo_path <- here("figs", "demographics")
figs_stats_path <- here("figs", "demographics")
mac_path <- here("results", "mean-and-correlation-analyses")
efa_path <- here("results", "exploratory-factor-analyses") 
other_path <- here("results", "other-analyses") 

output_paths <- list(
  figs_demo_path,
  figs_stats_path,
  mac_path,
  efa_path,
  other_path
)

map(output_paths, dir_create)

## Arguments ---------------------------------------------------------------

names(hte_cols) <- hte_labels
names(fl_cols) <- fl_labels

## NB:
## Object on right contains vector of columns
## Name needs to correspond to prefix of the object name 
## Object name is defined in 00_outcome-vectors.R
## Object needs to be placed in 03_scoring.R to create a sum of columns
## Likewise edit function get_pca_cats() in 00_functions_analysis.R
## if running PCA, to include new group
## See main.R and run steps 2, 3 and 4 to create analysis data
## Then run 991_render-preparation.R to prepare analysis code
## Then use 992_render-analysis.R to create analysis reports
labels <- list(
  "HTE_SUBSET_18" = hte_subset_18_labels
  
  # "HTE" = hte_labels,
  # "FL" = fl_labels
  # "HTE_SUBSET" = hte_subset_labels,
  # "HTE_SUBSET_10" = hte_subset_10_labels
  # ## Literacy Groups
  # "FUNCTIONAL" = functional,
  # "INTERACTIVE" = interactive,
  # "CRITICAL" = critical,
  
  ## Literacy Types
  ## NB MEDIA and CIVIC are 2-item, not suited for EFA
  # "MEDIA" = media,
  # "AGRI" = agri,
  # "NUTRITION" = nutrition,
  # "CULTURAL" = cultural
  # "CIVIC" = civic
  # "ECO" = eco
  
  
  )

args <- crossing(labels, strata) 
args <- args %>% mutate(questionnaire = names(labels))

language <- list(
  c("en", "fr"),
  "en",
  "fr"
  )

efa_args <- crossing(labels, language) %>% mutate(questionnaire = names(labels))
efa_args <- efa_args %>% 
  filter(
    # We omit the french FL analysis
    # due to missing correlations
    # These are likely due to sparseness
    # in IC and food_left variables
    # More info below
    !(language == "fr" & questionnaire %in% c("FL", "INTERACTIVE"))

  )

## Quitting from lines 99-106 (exploratory-factor-analysis.Rmd) 
# Error in .is_cormat(x) : 
#   ⓧ "x" is likely a correlation matrix but contains missing values. Please check the entered data.

