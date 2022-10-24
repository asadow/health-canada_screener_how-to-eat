# library(qualtRics)
# library(sjlabelled)
# 
# ## Copied ---
# surveys <- all_surveys()
# surveys <- surveys %>% filter(name %>% str_detect("cfs"))
# surveys <- surveys %>%
#   separate(
#     name,
#     c("study_language", "survey_type"),
#     "_"
#   ) %>%
#   separate(study_language, c("study", "language"))
# ##
# 
# nested_surveys <- surveys %>%
#   filter(survey_type == "re-test") %>%
#   mutate(
#     response_format = "numeric",
#     data = map2(id, response_format, ~ fetch_survey_format(.x, .y))
#   )
# 
# ## for warning see https://github.com/ropensci/qualtRics/issues/210
# 
# ## survey data
# ready_fpath <- here(
#   "data", "processed", "r_exports",
#   glue("data", ".csv")
# )
# 
# df <- read_csv(ready_fpath)
# 
# rt <- nested_surveys %>%
#   filter(survey_type == "re-test") %>%
#   unnest(data) %>%
#   clean_names %>%
#   rename(survey_id = id, id = external_data_reference)
# ## for the re-test, id's are all in external_data_reference
# 
# ## Fixing the duplicate id in the re-test
# ## Found response_id by comparing email against the case in original survey
# ## as seen here:
# # df %>% filter(id %in% c("CFS14", "CFS014")) %>% select(id, recipient_email, response_id)
# ## Checked CFS02 which has no identifying info in re-test and
# ## ip_address does not match original survey
# 
# rt <- rt %>%
#   mutate(
#     id = id %>% replace(id == "CFS14" & response_id == "R_1BLZCFIhJIOH7Gx", "CFS014")
#   ) %>%
#   filter(id %in% df$id)
# 
# ## No duplicates
# # rt %>%
# #   get_dupes(id)
# ## Counts
# # rt %>%
# #   group_by(language) %>%
# #   summarize(n = n())
# ## List of ids
# # rt$id %>% .[order(.)]
