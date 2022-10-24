eligible_ids <- read_csv(here("data",  "eligible-ids.csv")) %>% clean_names

# Import (manually) recruit-one EN data -------------------------------

flist <- list.files(here("data"), "1st-recruit.*.csv")
m <- str_split_fixed(flist, n = 4, "[_]")
colnames(m) <- c("study_language", "survey_type", "wave", "date")
recruit_one <- m %>% 
  as_tibble %>% 
  separate(study_language, c("study", "language"), "-") %>% 
  mutate(path = here("data", flist)) %>% 
  mutate(
    response_format = "text",
    data = map(
      path, 
      ~ read_csv(
        .x,
        col_types = cols(.default = "c"), 
        skip = 1
      ) %>% 
        clean_names %>% 
        filter(!str_detect(progress, "ImportId"))
    )
  )


# Import from Qualtrics ------------------------------------------

surveys <- all_surveys()
surveys <- surveys %>% filter(name %>% str_detect("cfs")) 
surveys <- surveys %>%
  separate(
    name,
    c("study_language", "survey_type"),
    "_"
  ) %>%
  separate(study_language, c("study", "language")) %>% 
  mutate(
    wave = case_when(
      survey_type == "survey" & language == "en" ~ "2nd-recruit", 
      TRUE ~ NA_character_
      )
    )

nested_surveys_numeric <- surveys %>% 
  filter(survey_type %in% c("survey", "re-test")) %>% 
  mutate(
    response_format = "numeric",
    data = map2(id, response_format, ~ fetch_survey_format(.x, .y))
  )

## for warning see https://github.com/ropensci/qualtRics/issues/210

## We need the EN text version 
## As we will use the EN text and numeric versions to turn
## the 1st-recruit text into numeric

nested_surveys_text <- surveys %>% 
  filter(survey_type == "survey" & language == "en") %>% 
  mutate(
    response_format = "text",
    data = map2(id, response_format, ~ fetch_survey_format(.x, .y))
  )

nested_surveys <- nested_surveys_numeric %>% bind_rows(nested_surveys_text)

nested_surveys <- nested_surveys %>%
  mutate(
    data = map(
      data,
      . %>% # All as character so we can unnest and avoid class conflicts
        mutate(
          across(everything(), as.character)
      ) %>% # Fix typos in one column
        rename_with(
          ~ .x %>% str_replace("plans", "plan"),
          any_of("i_plans_meals_ahead_of_time")
        )
    )
  )

# Cleaning ----------------------------------------------------------------

df <- nested_surveys %>% 
  fix_french_misaligned_cols %>% 
  use_english_names("survey") %>% 
  use_english_names("re-test")

df_en_both_formats <- df %>% 
  filter(language == "en" & survey_type == "survey") %>%
  unnest(data)

df <- df %>% 
  bind_rows(recruit_one) %>% 
  fix_recruit_one_response_type(reference = df_en_both_formats) %>% 
  filter(response_format == "numeric") %>% 
  unnest(data) %>% 
  fix_ids

# Check for duplicates --------------------------------------------------------------

df %>% 
  get_dupes(id, survey_type)

# Deidentify --------------------------------------------------------------

df <- df %>% 
  select(
    - matches("ip_address|recipient.*name|^location|email")
  )

marshalled_path <- here(
  "data", "processed", "r_exports",
  glue("marshalled_data", ".rds")
)

df %>% write_rds(marshalled_path)
