marshalled_path <- here(
  "data", "processed", "r_exports",
  glue("marshalled_data", ".rds")
)

df <- read_rds(marshalled_path)

# Rename ------------------------------------------------------------------

df <- df %>% 
  rename_with(
    ~ .x %>% str_replace("how.*ethnicity.*apply", "ethnicity"),
    matches("ethnicity")
  ) %>% 
  rename(education = education_level)


# Pre-mutation -----------------------------------------------------------

df <- df %>% 
  mutate(
    ## NB
    ## gender_identity_my_gender_identity_is_not_listed_above_please_specify_text
    ## is all missing for non re-test
    gender = gender_identity_selected_choice,
    ## age_other_please_specify_text
    ## is all missing
    age = age_selected_choice,
    ethnicity = apply(
      cbind(
        ethnicity_selected_choice, 
        ethnicity_other_please_specify_text
        ),
      1, 
      function(x) paste(x[!is.na(x)], collapse = ", ")
      )
    )


# Levels from Qualtrics------------------------------------------------------------------

## lvls are copy-pasted from Qualtrics Data Dictionary 
## Shows as "choice text (numeric)"
## Used to convert from numeric to text

ethnic_lvls <- c(
  "Black (African, Afro-Caribbean, African-Canadian descent)  (1)", 
  "East/Southeast Asian (Chinese, Korean, Japanese, Taiwanese descent; Filipino, Vietnamese, Cambodian, Thai, Indonesian, other Southeast Asian descent)  (2)", 
  "Indigenous (First Nations, Métis, Inuit descent)  (3)", 
  "Latino (Latin American, Hispanic descent)  (4)", 
  "Middle Eastern (Arab, Persian, West Asian descent, e.g., Afghan, Egyptian, Iranian, Lebanese, Turkish, Kurdish, etc.)  (5)", 
  "South Asian (South Asian descent, e.g., East Indian, Pakistani, Bangladeshi, Sri Lankan, Indo-Caribbean etc.)  (6)",
  "White (European descent)  (7)", 
  "Other, please specify:  (8)",
  "Prefer not to respond  (9)", 
  "I don't know  (10)"
)

age_lvls <- c(
  "18-24 years  (1)",  
  "25-34 years  (2)",  
  "35-44 years  (3)",  
  "45-54 years  (4)",  
  "55-65 years  (5)",  
  "Other, please specify:  (6)", 
  "Prefer not to respond  (7)",  
  "I don't know  (8)"
)

edu_lvls <- c(
  "8th grade/école élémentaire or less  (1)",  
  "Some high school  (2)",  
  "High school graduate  (3)",  
  "Some college  (4)",  
  "College graduate/CÉGEP  (5)",  
  "Apprenticeship training and/or trade program  (6)",  
  "Some university  (7)",  
  "University graduate  (8)",  
  "Postgraduate training or degree  (9)",  
  "Professional degree (medical, law, etc.)  (10)",  
  "Prefer not to respond  (11)",  
  "I don't know  (12)"
)

# Response Coding ----------------------------------------------------------

## Manual Case When's -------------------------------------------------------------

df <- df %>% 
  mutate(
    gender = case_when(
      gender == "2" ~ "Woman",
      gender == "1" ~ "Man",
      gender == "3" ~ "Gender-fluid, non-binary, and/or Two-Spirit",
      TRUE ~ as.character(gender)
    ),
    education_group = case_when(
      education %in% 8:10 ~ "uni_graduate",
      education %in% 1:7 ~ "non_uni_graduate",
      TRUE ~ NA_character_
    ),
    age_group = case_when(
      age %in% 3:5 ~ "35_and_over",
      age %in% 1:2 ~ "34_and_under",
      TRUE ~ NA_character_
    ),
    smoking_group = case_when(
      tabacco_use %in% c(1, 2, 6) ~ "smoker",
      tabacco_use %in% 3 ~ "non_smoker",
      TRUE ~ NA_character_
    ),
    income_adequacy = case_when(
      financial_security %in% c("1", "2") ~ "Difficult or very difficult",
      financial_security %in% c("3") ~ "Neither easy nor difficult",
      financial_security %in% c("4", "5") ~ "Easy or very easy",
      TRUE ~ NA_character_
    ),
    ## NB 
    ## living_situation_other_please_specify_text is all missing
    living_situation = case_when(
      living_situation_selected_choice == "1" ~ "Live alone",
      living_situation_selected_choice == "2" ~ "Live with others",
      TRUE ~ NA_character_
    )
  )

## Using Qualtrics' Dict. ------------------------------------------------------------------

df <- df %>% 
  mutate(
    education = education %>% code_as(edu_lvls),
    age = age %>% code_as(age_lvls),
    ethnicity = ethnicity %>% code_as(ethnic_lvls),
    ethnicity = ethnicity %>% str_replace(" \\(.*\\)", "")
    )
