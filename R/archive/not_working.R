summ_by <- function(df, y, by){
  
  by_header <- glue("**{by}**") %>%
    str_replace("_", " ") %>% 
    str_to_title
  
  y_header <- enexpr(y) %>%
    str_extract("[^_]+") %>%
    str_to_upper

  y_score <- y_header %>% glue("_score")

  df <- if(by == "gender")
  {df %>% filter(gender %in% c("Man", "Woman"))} 
  else {df}
  
  df %>%
    select(
      y_score,
      all_of(y),
      all_of(by)
    ) %>% 
    tbl_summary(
      by = by, 
      type = list(
        c(y_score, all_of(y)) ~ "continuous2"
      ), 
      statistic = all_continuous() ~ c("{mean} ({sd})",
                                       "{median} ({p25}, {p75})", 
                                       "{min}, {max}"),
      missing = "no"
    ) %>% 
    add_p %>% 
    add_overall %>% 
    bold_labels() %>% 
    modify_header(label = "") %>% 
    modify_spanning_header(c("stat_1", "stat_2") ~ by_header) %>%
    modify_caption(
      glue("**{y_header} Score and Item Statistics by {by_header}**")
    )
}

save_stat_figs <- function(df, cols, by){
  abbr <- enexpr(cols) %>% str_extract("[^_]+")
  cols <- enexpr(cols)
  df %>% 
    summ_by(!!cols, by) %>%
    as_gt() %>%
    gtsave(
      glue("{abbr}-summary-by-{by}_{Sys.Date()}.html"),
      here("figs", Sys.Date(), "statistics")
    )
}

library(reprex)
reprex(
  {
    library(tidyverse)
    
    z <- c("a", "b")
    df <- data.frame(Z = 1:3, a = 1:3, b = 1:3)
    
    select_this <- function(df, x){
      
      x_upper <- enexpr(x) %>% str_to_upper

      df %>%
        select(
          all_of(x_upper), all_of(x)
        )
    }
    
    wrapper_select_this <- function(df, x){
      df %>% select_this(x)
      
    }
    
    df %>% select_this(z)
    
    df %>% wrapper_select_this(z)
  }
)

up <- function(y){
  enexpr(y) %>% str_to_upper
}

wrapper_up <- function(x){
  x <- enexpr(x)
  x_upper <- up(!!x)
  df %>% select(all_of(x_upper), !!all_of(x))
}

wrapper_up(z)
