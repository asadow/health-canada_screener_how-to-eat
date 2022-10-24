marshalled_path <- here(
  "data", "processed", "r_exports",
  glue("marshalled_data", ".rds")
)

df <- read_rds(marshalled_path)
df <- df %>% filter(survey_type == "survey")

df <- df %>% 
  mutate(
    gardener_know_how = case_when(
      i_know_how_to_check_all_that_apply %>% str_detect("5") ~ "5 or 5 and more",
      TRUE ~ i_know_how_to_check_all_that_apply
    ),
    gardener_agri_exp = case_when(
      over_the_past_year_i_have_check_all_that_apply %>% str_detect("5") ~ "5 or 5 and more",
      TRUE ~ over_the_past_year_i_have_check_all_that_apply
    ),
  )

df %>% group_by(gardener_know_how) %>% summarize(n())
df %>% group_by(gardener_agri_exp) %>% summarize(n())

df %>% group_by(gardener_know_how, gardener_agri_exp) %>% 
  summarize(n()) %>%
  ungroup() %>% 
  filter(if_any(everything(), is.na))
