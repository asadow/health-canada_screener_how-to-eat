
# Primary focus -----------------------------------------------------------

## PCA ---------------------------------------------------------------------

rmarkdown::render(
  here("Rmd", "pca.Rmd"),
  output_file = here("results",
                     paste0('pca_',
                            Sys.Date(),
                            ".html")
  )
)

## Mean and Correlations ---------------------------------------------------

pmap(
  list(
    args$questionnaire,
    args$labels,
    args$strata
  ), 
  function(q, l, s){
    s_kebab <- s %>% str_replace_all("_", "-")
    rmarkdown::render(
      here("Rmd", 
           glue('mean-and-correlation-analysis.Rmd')),
      output_file = here(
        mac_path,
        glue("{stl(q)}-mean-and-corr-by-{s_kebab}_{Sys.Date()}.html")),
      params = list(questionnaire = q,
                    labels = l,
                    by = s)
    )
  }
) %>% 
  invisible

## HEFI --------------------------------------------------------------------

hefi_args <- args %>% 
  filter(strata == "language")

pmap(
  list(
    hefi_args$questionnaire,
    hefi_args$labels,
    hefi_args$strata
  ),
  function(q, l, s){
    rmarkdown::render(
      here("Rmd", glue('hefi-analysis.Rmd')),
      output_file = here(
        other_path,
        glue("{stl(q)}-hefi-analysis_{Sys.Date()}")
      ),
      params = list(
        questionnaire = q,
        labels = l,
        by = s
      )
    )
  }
)

# Secondary focus ---------------------------------------------------------

## EFA ---------------------------------------------------------------------

pmap(
  list(
    efa_args$questionnaire,
    efa_args$labels,
    efa_args$language
  ), 
  function(q, l, lg){
    ## NB This allows "en-fr" in output file
    ## ff is for file
    lgff <- lg %>% glue_collapse("-")
    
    rmarkdown::render(
      here("Rmd", 'exploratory-factor-analysis.Rmd'),
      output_file = here(
        efa_path,
        '{stl(q)}-{lgff}-exploratory-factor_{Sys.Date()}.html' %>% glue
      ),
      params = list(
        questionnaire = q,
        labels = l,
        language = lg
      )
    )
  }
) %>% 
  invisible

## Test and Re-test: ICC ---------------------------------------------------------------------

rmarkdown::render(
  here("Rmd", glue('test-re-test-analysis.Rmd')),
  output_file = here(
    other_path,
    "test-re-test-analysis_{Sys.Date()}" %>% glue
  )
)



# Appendix ----------------------------------------------------------------


## Item Legend --------------------------------------------------------------------

rmarkdown::render(
  here("Rmd", glue('item-legend.Rmd')),
  output_file = here(
    "results",
    "item-legend"
  )
)

## Theory --------------------------------------------------------------------

rmarkdown::render(
  here("Rmd", glue('theory.Rmd')),
  output_file = here(
    "results",
    "theory-on-reliability"
  )
)




# Figures -----------------------------------------------------------------


## Demographics ------------------------------------------------------------
# 
# map(
#   strata, 
#   ~ df %>%
#     demo_by(.x) %>%
#     as_gt() %>%
#     gtsave(
#       glue("demographics-by-{.x}_{Sys.Date()}.html"),
#       here("figs", sub_d, "demographics")
#     )
# ) %>% invisible

## Item Statistics  --------------------------------------------------------

pmap(
  list(
    args$questionnaire,
    args$labels,
    args$strata
  ), 
  function(q, l, s){ 
    df %>% 
      summ_by(q, l, s) %>%
      as_gt() %>%
      gtsave(
        glue("{q  %>% str_to_lower}-summary-by-{s}_{Sys.Date()}.html"),
        here("figs", sub_d, "statistics")
      )
  }
) %>% invisible


### Quantiles ---------------------------------------------------------------

other_args <- args %>% 
  filter(!duplicated(questionnaire)) %>% 
  mutate(strata = "age") # age for summary across all age groups

export_where <- function(x){
  map2(
    other_args$questionnaire,
    other_args$labels,
    function(q, l) df %>% 
      quantile_summary(q, l) %>% 
      gtsave(
        glue("{q %>% str_to_lower}-quantiles_{Sys.Date()}.html"),
        x
      )
  ) %>% invisible
  
}

export_where(here("results", "other-analyses"))
export_where(here("figs", "final", "statistics"))

### All age groups summary ---------------------------------------------------

pmap(
  list(
    other_args$questionnaire,
    other_args$labels,
    other_args$strata
  ),
  function(q, l, s) {
    df %>% 
      summ_by(q, l, s) %>% 
      as_gt() %>% 
      gtsave(
        glue("{q %>% str_to_lower}-summary-by-age-all-groups_{Sys.Date()}.html"),
        here("figs", "final", "statistics")
      )
  }
) %>% invisible

