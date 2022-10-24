# Write to file -----------------------------------------------------

data_path <- here(
  "data", "processed", "r_exports",
  glue("data", ".rds")
)

write_rds(df, data_path)