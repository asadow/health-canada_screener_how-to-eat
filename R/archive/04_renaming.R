
names(hte_cols) <- hte_labels
names(fl_cols) <- fl_labels

fl_cols_no_hte <- fl_cols[!fl_cols %in% hte_cols]
 
df <- df %>% 
  rename_with(
    ~ names(hte_cols),
    all_of(hte_cols)
  ) %>% 
  rename_with(
    ~ names(fl_cols_no_hte),
    all_of(fl_cols_no_hte)
  )


