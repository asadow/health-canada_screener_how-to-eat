# Coding Dictionary ------------------------------------------------------------------

cd <- read_excel(
  here("prose", "scoring", "response_scoring.xlsx")
) %>% 
  clean_names %>% 
  mutate(cols = to_snake_case(qualtrics)) %>% 
  rename(labels = abbreviation) %>% 
  filter(
    ## Rank question is represented as multiple columns in data
    labels != "rank_Ffactors"
    )

# Item Groups -----------------------------------------------------

### HTE ---------------------------------------------------------------------

## How To Eat

## These items are not in the coding dictionary (except for 1)
## NB these must remain unlabelled until after calculating the total score
## as tidyselect across(all_of(hte_cols)) will select
## labels inside hte_cols rather than the elements themselves

hte_cols <- c(
  "i_take_time_to_eat_my_meals", 
  "i_eat_slowly", 
  "i_engage_in_other_activities_while_eating_examples_are_driving_working_studying_watching_tv_using_a_computer_phone_or_tablet", 
  "i_rely_on_my_hunger_signals_to_tell_me_when_to_eat", 
  "i_rely_on_my_fullness_signals_to_tell_me_when_to_stop_eating", 
  "i_eat_when_i_am_feeling_emotional_even_when_im_not_hungry", 
  "i_eat_food_from_restaurants_this_includes_dine_in_restaurants_take_out_delivery_or_fast_food", 
  "i_eat_meals_that_are_cooked_at_home_from_scratch_this_means_meals_that_include_at_least_3_basic_ingredients_examples_of_basic_ingredients_include_canned_beans_meats_vegetables_rice_spices_etc", 
  "i_eat_foods_that_are_part_of_my_family_food_traditions_and_or_culture", 
  "i_eat_foods_from_a_variety_of_cultures_and_or_food_traditions", 
  "i_cook_meals_from_scratch_this_means_meals_that_include_at_least_3_basic_ingredients_examples_of_basic_ingredients_include_canned_beans_meats_vegetables_rice_spices_etc", 
  "i_cook_meals_with_others", 
  "i_plan_meals_ahead_of_time", 
  "i_plan_meals_with_others", 
  "when_i_have_the_opportunity_i_eat_my_meals_with_others",
  "i_use_nutrition_information_on_food_labels_when_buying_foods_and_or_drinks_for_the_first_time", 
  "eating_is_a_pleasure_for_me", 
  "i_enjoy_trying_new_foods",
  "i_enjoy_cooking_food", 
  "i_enjoy_eating_with_others", 
  "i_am_aware_that_food_advertisements_can_make_me_want_to_eat_or_drink"
)

hte_to_reverse <- c(
  "eat_emo",
  "eat_out",
  "eat_engage"
)

hte_to_reverse_cols <- c(
  "i_eat_when_i_am_feeling_emotional_even_when_im_not_hungry",
  "i_eat_food_from_restaurants_this_includes_dine_in_restaurants_take_out_delivery_or_fast_food",
  "i_engage_in_other_activities_while_eating_examples_are_driving_working_studying_watching_tv_using_a_computer_phone_or_tablet"
)

## Hypothetical domains in comments
hte_labels <- c( 
  "eat_Ttime", "eat_slow", "eat_engage", # Slow/distracted eating
  "rely_start", "rely_stop", # Signal-reliance eating
  "eat_emo", # Food-impulsiveness
  "eat_out", "eat_Hcook", "eat_Ftrad", "eat_Vtrad", "Hcook", 
  "WO_cook", # Food with others
  "plan_Mahead", # Food-impulsiveness
  "WO_plan", "WO_eat", # Food with others
  "use_Ninfo", # Information-aware
  "joy_eat", "joy_newfoods", "joy_cook", "joy_eat_WO", # Food-enjoyment
  "aware_adverts" # Information-aware
)

hte_subset_labels <- c("eat_Ttime", 
                       "WO_eat",
                       "Hcook",
                       "joy_eat_WO",
                       "joy_eat",
                       "eat_Hcook",
                       "plan_Mahead",
                       "joy_cook",
                       "WO_cook",
                       "eat_emo",
                       "eat_out",
                       "WO_plan")

hte_subset_10_labels <- c("eat_Ttime", 
                           "WO_eat",
                           "Hcook",
                           "joy_eat_WO",
                           "joy_eat",
                           "eat_Hcook",
                           "plan_Mahead",
                           "joy_cook",
                           "eat_emo",
                           "eat_out")

hte_subset_18_labels <- c(
  "eat_Ttime",
  "rely_start",
  "WO_eat",
  "Hcook",
  "joy_newfoods",
  "joy_eat_WO",
  "joy_eat",
  "eat_Vtrad",
  "eat_Hcook",
  "eat_Ftrad",
  "plan_Mahead",
  "use_Ninfo",
  "joy_cook",
  "aware_adverts",
  "WO_plan",
  "WO_cook",
  "eat_emo",
  "eat_out")

### NVS -----------------------------------------------------------------
## Newest Vital Signs: health literacy measure

## Includes IC_WNsafe
nvs_labels <- cd %>% 
  filter(nvs_item == "Yes") %>% 
  pull(labels) 

## Duplicate NVS variables for 1, 0 scoring
nvs_1_0 <- glue("{nvs_labels}_nvs")


### FL ----------------------------------------------------------------------
## Food Literacy

## Exclude rank question as it is not scored
## Exclude agri_exp as it was not forced response and there are 26 missing
## From NVS, Only ice cream food questions are included in FL

fl_cd <- cd %>% 
  filter(
    fl_item == "Yes"
    & !labels %in% c("agri_exp", "IC_WNsafe")
  ) 

fl_cols <- fl_cd %>% pull(cols)
fl_labels <- fl_cd %>% pull(labels)

# Literacy Groups
grab <- function(variable, string){
  fl_cd %>% 
    filter({{variable}} == string) %>% 
    pull(labels)
}

nutrition <- grab(fl_dimension, "Nutrition")
agri <- grab(fl_dimension, "Agri-Food Systems")
media <- grab(fl_dimension, "Food Media")
cultural <- grab(fl_dimension, "Cultural")
civic <- grab(fl_dimension, "Civics")
eco <- grab(fl_dimension, "Eco")

functional <- grab(fl_level, "Functional")
interactive <- grab(fl_level,  "Interactive") 
critical <- grab(fl_level, "Critical")

outcomes <- c("HTE_score", "FL_score", hte_labels, fl_labels, nvs_labels) %>% unique

bvars_df <- cd %>% filter(!correct_responses %>% is.na)
