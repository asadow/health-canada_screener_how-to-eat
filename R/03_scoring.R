## Outcome Vectors ---------------------------------------------------------

source(here("R", "00_outcome-vectors.R"))

## Rename ------------------------------------------------------------------

cols <- c(hte_cols, cd$cols) %>% unique
labels <- c(hte_labels, cd$labels) %>% unique

df <- df %>% 
  rename_with(
    ~ labels, 
    all_of(cols)
  )

## Scoring Binary Vars --------------------------------------------------------------

## Make lists of correct and incorrect responses
bvars_df <- bvars_df %>% 
  mutate(
    across(
      c(correct_responses, incorrect_responses),
      ~  .x %>% str_remove_all(" ") %>% str_split(",")
    )
  )

## For duplicate NVS vars that will be scored differently
nvs_dups <- bvars_df %>% 
  filter(nvs_item == "Yes") %>%
  mutate(labels = glue("{labels}_nvs"))

df[nvs_1_0 %>% as.character] <- df[nvs_labels]

### Score -----------------------------------------------------------------

## FL variables for 5, 0 scoring

df <- df %>% 
  ## FL variables for 5, 0 scoring
  score_binary(bvars_df, bvars_df$labels, weight = 5) %>% 
  ## Duplicate NVS variables for 1, 0 scoring
  score_binary(nvs_dups, nvs_1_0, weight = 1)

### Additional requests -----------------------------------------------------

# cd %>% filter(!is.na(additional_request)) %>% select(labels, additional_request)

#### Missing IC_WNsafe -----------------------------------------------------

## Assign 0 points
## A missing value means the previous question was answered incorrectly

df <- df %>%
  mutate(
    across(
      starts_with("IC_WNsafe"),
      ~ case_when(.x %>% is.na ~ 0, TRUE ~ .x)
      )
    )

#### Confidence items  -----------------------------------------------------

## 7 was an additional response option in the fr survey
## It had no label and preceded the 1 (Not at all) option
## Given line below, no 7 was chosen and no fix is needed
# map(df %>% select(starts_with("how_confident")), unique)

## -2 and -4 are responses in the 1st-recruit en data 
## They correspond to 2 and 4 respectively

df <- df %>% 
  mutate(
    across(
      c(Cfollow_recipe,
        Cprep_mealFF,
        Cprep_snackFF,
        Cprep_meal30,
        Cadjust_recipe), 
      ~ case_when(
        .x == "-2" ~ "2", 
        .x == "-4" ~ "4", 
        TRUE ~ .x
      )
    )
  )

#### Multiple choice ---------------------------------------------------

## Elements are strings of numbers, each corresponding to a selection
## 6 is Not Applicable; treat as 0
## The other selections each get 1 point
## Therefore simply change 6 to Not Applicable and count number of digits
## NA stays NA

df <- df %>% 
  mutate(
    across(
      c(know_how, 
        # agri_exp ## removed from analysis
        ),
      ~ .x %>% 
        str_replace("6", "Not Applicable") %>% 
        str_count("\\d")
    )
  )

#### Ranking question  ----------------------------------------------------- 

# cd %>% filter(question %>% str_detect("rank"))

#### Numerics and reversal  -----------------------------------------------------

## fl_labels includes nvs_labels
numerics <- c(hte_labels, fl_labels)

df <- df %>% 
  mutate(
    across(all_of(numerics), ~ as.numeric(.x)),
    across(
      all_of(hte_to_reverse),
      ~ case_when(
        .x == 1 ~ 5,
        .x == 2 ~ 4,
        .x == 3 ~ 3,
        .x == 4 ~ 2,
        .x == 5 ~ 1,
        TRUE ~ NA_real_
      )
    )
  )

## Total scores  -----------------------------------------------------
## For NVS total score, use 1/0 scored variables
nvs_labels <- nvs_1_0

groups <- List(
  hte_subset_labels,
  hte_subset_10_labels,
  hte_subset_18_labels,
  hte_labels, 
  fl_labels, 
  nvs_labels,
  
  nutrition,
  agri,
  media,
  cultural,
  civic,
  eco,
  
  functional,
  interactive,
  critical
)

for (i in seq_along(groups)){
  g <- groups[i]
  col_name <- names(g) %>% 
    str_remove("_labels") %>% 
    str_to_upper %>% 
    glue("_score")
  
  df[[col_name]] <- rowSums(df[, unlist(g)])

}


### Health literacy ---------------------------------------------------------
## Recall each summed element of NVS score is 0 or 5 
df <- df %>% 
  mutate(
    health_literacy = case_when(
      NVS_score < 4 ~ "low", 
      NVS_score > 3 ~ "high",
      TRUE ~ NA_character_
    )
  )


