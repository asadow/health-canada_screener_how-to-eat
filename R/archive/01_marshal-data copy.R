eligible_ids <- read_csv(here("data",  "eligible-ids.csv")) %>% clean_names

df <- read_raw_data()

## We only have 1st-recruit data in choice text
## By using 2nd-recruit's choice text's identity to its numeric version
## We change the 1st-recruit to be numeric

df <- df %>% 
  fix_responses_of_recruit_one(reference = df_en_both_formats) %>% 
  fix_french_misaligned_cols %>% 
  assign_english_names %>% 
  fix_ids

ready_fpath <- here(
  "data", "processed", "r_exports",
  glue("data", ".csv")
)

write_csv(df, ready_fpath)
