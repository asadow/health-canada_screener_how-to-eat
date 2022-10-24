
common_responses <- list(
  "Agreement" = agreement_cols,
  "Occurrence" = occurrence_cols,
  "Occurrence with Range" = occurrence_range_cols
)


### Pre-Scoring Check ####

# ## Check that all unique responses will fit the scoring categories. 

# list_df <- map(
#   common_responses,
#   ~ show_unique(df, .x)
# )
# 
# # imap_xxx(x, ...), an indexed map, is short hand for map2(x, names(x), ...) 
# # if x has names, or map2(x, seq_along(x), ...) if it does not.
# udf <- imap(
#   list_df, 
#   ~ cbind(.x, "Response Type" = .y)
# ) %>% 
#   bind_rows %>% 
#   mutate(
#     across(
#       everything(),
#       as.factor
#     )
#   ) %>% 
#   select(
#     "Response Type", 
#     Column, 
#     "Unique Responses"
#   )


# 
# datatable(
#   udf,
#   filter = 'top',
#   extensions = "Buttons",
#   options = list(
#     columnDefs = list(
#       list(width = '150px', targets = 0),
#       list(width = '500px', targets = 1),
#       list(width = '200px', targets = 2)
#     ),
#     dom = 'Bfrtip',
#     scrollX = TRUE,
#     pageLength = 4, 
#     buttons = list(
#       c(
#         'copy', 
#         'csv'
#       )
#     )
#   ),
#   rownames = F,
#   class = 'cell-border stripe'
# )
# 

