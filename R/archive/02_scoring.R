### Groups of Items ####


# Literacy Groups
nutrition <- str_subset(names(df), "eating_pattern|budget|plan_meals_ahead|nutrition_information|Confidence|I_(eat_food.*restaurant|(eat_meals|cook).*from_scratch|wash_my_hands|clean|use_a_thermometer|follow.*storage.*instructions)")

agri <- str_subset(names(df), "food_system|i_look_at_food_labels|aware.*in_season")

media <- str_subset(names(df), "information_about_food|food_advertisements")

cultural <- str_subset(names(df), "(where_i_can_get_food|able_to_get_food|eat_food).*cultur")

civic <- str_subset(names(df), "community_programs")

eco <- str_subset(names(df), "best_before|food_left|avoid.*plastic")

# df of only HTE (How To Eat) items ####
hte <- df %>% 
  select(
    matches("i_take_time"):matches("i_am_aware")
  )

# Create subsets of column names
hte_cols <- names(hte)


# Each vector contains columns that should have common responses

agreement_cols <- c(
  str_subset(
    names(df), 
    "pleasure|enjoy|eating_pattern|aware.*in_season|(where_i_can_get_food|able_to_get_food).*cultur"
  ),
  media,
  civic
)

occurrence_range_cols <- str_subset(hte_cols, "i_(eat_food.*restaurant|(eat_meals|cook).*from_scratch)")

occurrence_cols <- c(
  hte_cols[c(1:6, 9, 10, 12:16)],
  cultural[!cultural %in% agreement_cols],
  str_subset(
    names(df), 
    "budget|plan_meals_ahead|food_labels|wash_my_hands|clean_kitchen|thermometer|storage_instructions|look_at_food_labels|avoid.*plastic"
  )
)

common_responses <- list(
  "Agreement" = agreement_cols,
  "Occurrence" = occurrence_cols,
  "Occurrence with Range" = occurrence_range_cols
)


### Items to Reverse ####

# Items to reverse after scoring the above
to_reverse <- c(
  "i_eat_when_i_am_feeling_emotional_even_when_im_not_hungry",
  "i_eat_food_from_restaurants",
  "i_engage_in_other_activities_while_eating_examples_are_driving_working_studying_watching_tv_using_a_computer_phone_or_tablet"
)

to_reverse


### Pre-Scoring Check ####


# Correction

df <- df %>% 
  mutate(
    across(
      starts_with("Confidence") |
        all_of(
          c(agreement_cols, occurrence_cols, occurrence_range_cols)
        ),
      ~ snakecase::to_snake_case(.x)
    ),
    across(
      all_of(agreement_cols),
      ~ str_replace(.x, "neither_agree_nor_disagree", "neither_disagree_nor_agree")
    )
  )



## Check that all unique responses will fit the scoring categories. 


list_df <- map(
  common_responses, 
  ~ show_unique(df, .x)
) 

# imap_xxx(x, ...), an indexed map, is short hand for map2(x, names(x), ...) 
# if x has names, or map2(x, seq_along(x), ...) if it does not.
udf <- imap(
  list_df, 
  ~ cbind(.x, "Response Type" = .y)
) %>% 
  bind_rows %>% 
  mutate(
    across(
      everything(),
      as.factor
    )
  ) %>% 
  select(
    "Response Type", 
    Column, 
    "Unique Responses"
  )



datatable(
  udf,
  filter = 'top',
  extensions = "Buttons",
  options = list(
    columnDefs = list(
      list(width = '150px', targets = 0),
      list(width = '500px', targets = 1),
      list(width = '200px', targets = 2)
    ),
    dom = 'Bfrtip',
    scrollX = TRUE,
    pageLength = 4, 
    buttons = list(
      c(
        'copy', 
        'csv'
      )
    )
  ),
  rownames = F,
  class = 'cell-border stripe'
)



### Scoring ####


df <- df %>% 
  mutate(
    across(
      all_of(agreement_cols),
      ~ score(.x, 
              "strongly_disagree", 
              "disagree", 
              "neither_disagree_nor_agree", 
              "agree", 
              "strongly_agree")
    ),
    across(
      all_of(occurrence_cols),
      ~ score(.x, "never", "rarely", "sometimes", "often", "always")
    ),
    across(
      all_of(occurrence_range_cols),
      ~ score(.x, 
              "never", 
              "1_3_times_month", 
              "1_3_times_week", 
              "4_6 _times_Week", 
              "7_or_more_times_week")
    ),
    across(
      starts_with("Confidence"),
      ~ score(.x, 
              "not_at_all_confident_1", 
              "2", 
              "somewhat_confident_3", 
              "4", 
              "very_confident_5")
    ),
    across(
      str_subset(names(.), "best before"),
      ~ case_when(
        str_detect(.x, "^(Not sure|Should be thrown out after that date)") ~ 0,
        str_detect(.x, "^Can still be consumed") ~ 1,
        TRUE ~ NA_real_
      )
    ),
    across(
      str_subset(names(.), "food left"),
      ~ case_when(
        str_detect(.x, "None of the above") ~ 0,
        str_detect(.x, "^(I keep it|Give leftovers)") ~ 1,
        TRUE ~ NA_real_
      )
    )
  )




# Reverse scoring ####
df <- df %>% 
  mutate(
    across(
      to_reverse,
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

# Total scoring
df <- df %>% 
  mutate(
    score_HTE = rowSums(
      across(all_of(hte_cols))
    ),
    score_LIT = rowSums(
      across(
        all_of(
          c(
            nutrition,
            agri,
            media,
            cultural,
            civic,
            eco
          )
        )
      )
    )
  )

lit_subcat = List(
  nutrition,
  agri,
  media,
  cultural,
  civic,
  eco
)

for (i in seq_along(lit_subcat)){
  li <- lit_subcat[i]
  df[, paste0("score_", names(li))] <- rowSums(df[, unlist(li)])
  
}


