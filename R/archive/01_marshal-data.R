# source(here::here("R", "00_packages.R"))
# ids <- read_clean_ids()

# path <- here("data", "processed")
# flist <- list.files(path, pattern = "*.xlsx")
# 
# bad_ids <- map_df(
#   flist, 
#   ~ here(path, .x) %>% 
#     readxl::read_excel() %>% 
#     clean_names
#   )
# 
# bad_ids <- bad_ids %>% pull(1)


eligible_ids <- read_csv(here("data",  "eligible-ids.csv")) %>% clean_names

## Nice-to-have: rewrite to use qualtRics package 
df <- read_raw_data()

## We only have 1st-recruit data in choice text
## By using 2nd-recruit's choice text's identity to its numeric version
## We change the 1st-recruit to be numeric

## Nice-to-have: Write a stopifnot 
## as the two wave == 2nd-recruit must be numeric and choice text format

both_formats <- df %>% filter(wave == "2nd-recruit") %>% unnest(data)

df <- df %>% 
  fix_responses_of_recruit_one %>% 
  fix_french_misaligned_cols %>% 
  assign_english_names %>% 
  fix_ids

# df <- df %>% filter(!id %in% bad_ids)

# ## Duplicate ids
# dupes <- df %>% get_dupes(id) %>% suppressMessages()
# setdiff(ids$external_data_reference, df$id) 
# df %>% group_by(language) %>% summarise(n = sum(!is.na(id)))

ready_fpath <- here(
  "data", "processed", "r_exports",
  glue("data", ".csv")
)
# 
# write_csv(df, ready_fpath)
