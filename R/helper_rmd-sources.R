source(here("R", "00_packages.R"))
source(here("R", "00_functions_analysis.R"))
source(here("R", "00_functions_data.R"))
source(here("R", "00_outcome-vectors.R"))

data_path <- here(
  "data", "processed", "r_exports",
  glue("data", ".rds")
)
