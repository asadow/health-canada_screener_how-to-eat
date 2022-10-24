
# Re-test -----------------------------------------------------------------

surveys <- surveys %>%
  filter(name %>% str_detect("cfs.*test"))

sv <- surveys %>%
  mutate(
    data = map(
      id,
      ~ fetch_survey(.x, convert = FALSE) %>%
        label_to_colnames
    )
  )

sv <- sv %>%
  unnest(data) %>% 
  clean_names %>% 
  rename(survey_id = id, id = external_data_reference)

df %>% inner_join(sv, by = c("id", "recipient_email")) %>% select(id) %>% arrange(id)

