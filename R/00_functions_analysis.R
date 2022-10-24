stl <- function(x){x %>% str_to_lower}

as_header <- function(x){
  glue("**{to_title_case(x)}**")
}

quantiles <- c(.01, .05, .1, .25, .5, .75, .9, .95, .99)

quantile_summary <- function(df, questionnaire, y){
  
  item_sum <- questionnaire %>% glue("_score")
  y <- c(item_sum, y)
  
  quantile_summary <- df %>% 
    select(all_of(y)) %>% 
    summarize(
      across(
        everything(),
        list( 
          mean = ~ mean(., na.rm = TRUE),
          sd = ~ sd(., na.rm = TRUE),
          quantiles = ~ quantile(., quantiles, type = 2, na.rm = TRUE) %>% list
        ),
        .names = "{.fn}_separator_{.col}"
      )
    ) %>% 
    pivot_longer(
      everything(),
      names_pattern = c("(.*)_separator_(.*)"),
      names_to = c(".value", "item")
    ) %>% 
    mutate(
      across(
        where(is.numeric),
        ~ round(., 2)
      )
    ) %>% 
    unnest_wider(quantiles)
  
  quantile_summary %>% 
    rename_with(str_to_title) %>% 
    rename(SD = Sd) %>% 
    gt %>%
    tab_header(
      title = glue("{questionnaire} Item Statistics")
    ) %>%
    tab_spanner(
      label = "Percentiles",
      columns = glue("{quantiles * 100}%")
    ) %>%
    tab_source_note(
      source_note = "Percentiles were calculated using R function quantiles() with argument type = 2 (SAS default). See Wicklin (2017)."
    ) %>%
    tab_source_note(
      source_note = "Wicklin, R. (2017) Sample quantiles: A comparison of 9 definitions; SAS Blog. https://blogs.sas.com/content/iml/2017/05/24/definitions-sample-quantiles.html"
    )
}

demo_by <- function(df, by){
  
  df <- if(by == "gender")
  {df %>% filter(gender %in% c("Man", "Woman"))} 
  else {df}
  
  by_header <- by %>% as_header
  
  df %>% 
    select(
      all_of(strata), 
      education, 
      age,
      ethnicity
      ) %>% 
    tbl_summary(by = all_of(by)) %>% 
    # add_p(test.args = all_tests("fisher.test") ~ list(workspace = 2e9)) %>% 
    add_p(
      test.args = all_tests("fisher.test") ~ list(
        simulate.p.value = TRUE, 
        B = 2000
        )
      ) %>% 
    add_overall() %>% 
    bold_labels() %>% 
    modify_header(label = "") %>% 
    modify_spanning_header(update = all_stat_cols() ~ by_header) %>%
    modify_caption(glue("**Demographics by {by_header}**"))
}

summ <- function(df, x) {
  df %>% 
    summarize(
      across(
        all_of(x),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE),
          n = ~ sum(!is.na(.x))
        ),
        .names = "{col}_{fn}"
      )
    ) 
}

round_two <- function(df){
  df %>% mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )
}

summ_by <- function(df, questionnaire, y, by){
  
  item_sum <- questionnaire %>% glue("_score")
  y <- c(item_sum, y)
  
  by_header <- by %>% as_header
  
  df <- if(by == "gender")
  {df %>% filter(gender %in% c("Man", "Woman"))} 
  else {df}
  
  df %>%
    select(
      all_of(y),
      all_of(by)
      ) %>% 
    tbl_summary(
      by = by, 
      type = list(
        c(all_of(y)) ~ "continuous2"
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
    modify_spanning_header(update = all_stat_cols() ~ by_header) %>%
    modify_caption(
      glue("**{questionnaire} Score and Item Statistics by {by_header}**")
      )
}


# CFA ---------------------------------------------------------------------

specify_factor <- function(df, group){
  factor_title <- to_snake_case(group)
  sum <- df %>% 
    filter(group == !!group) %>% 
    pull(label) %>% 
    glue_collapse(" + ")
  
  glue("{factor_title} =~ {sum}")
}

specify_cfa <- function(df){
  factors <- df$group %>% unique
  map(factors, ~ specify_factor(df, .x)) %>% glue_collapse("\n")
}


# PCA ---------------------------------------------------------------------
prepare_and_hetcor <- function(df, labels){
  df %>% 
    select(all_of(labels)) %>%
    ## Remove cases with missing data
    filter(!if_any(everything(), is.na)) %>%   
    mutate(
      across(
        everything(), 
        ~ factor(.x, ordered = TRUE)
      )
    ) %>% 
    as.data.frame %>% 
    # prepare_for_hetcor %>% 
    hetcor(parallel = TRUE, std.error = FALSE) %>%  
    .[["correlations"]]
}

get_pca_cats <- function(cd){
  
  hte_subgroups <- here("prose", "scoring", "pca_hte-subgroups.xlsx") %>%
    read_xlsx
  
  ## KMO for FL is 0.05, too low for PCA 
  # fl_subgroups <- cd %>% 
  #   rename(group = fl_dimension,
  #          label = labels) %>% 
  #   filter(
  #     group %in% c("Nutrition",
  #                  "Agri-Food Systems", 
  #                  "Cultural", 
  #                  "Eco") &
  #       !label %in% c("agri_exp", "IC_WNsafe") ## removed from analysis
  #   )
  # 
  # fl_groups <- fl_subgroups %>% 
  #   select(group, label) %>% 
  #   bind_rows(
  #     tibble(group = "Food literacy",
  #            label = fl_labels)
  #   )
  
  ## Bind HTE, FL groups
  pca_cats <- hte_subgroups %>% select(group, label) %>% 
    ## FL has KMO of 0.06 (see pca results); not suitable for PCA
    # bind_rows(fl_groups) %>% 
    bind_rows(
      tibble(group = "How to eat", 
             label = hte_labels)
    ) %>% 
    bind_rows(
      tibble(group = "How to eat (subset)",
             label = hte_subset_labels)
    ) %>% 
    bind_rows(
      tibble(group = "How to eat (subset 10)",
             label = hte_subset_10_labels)
      ) %>% 
    bind_rows(
      tibble(group = "How to eat (subset 18)",
             label = hte_subset_18_labels)) 
  
  
}


scree_plot <- function(df, x, y, cumulative = FALSE){
  
  if(cumulative == TRUE){
    y_addition <- .1
    title_addition <- "Cumulative "
  }else{
    y_addition <- 0
    title_addition <- ""
  }
  
  df %>% 
    ggplot(aes(x = {{ x }},
                y = {{ y }}, 
                label = round({{ y }}, 2))) + 
    geom_point() +
    geom_line() + 
    xlab("Principal Component") + 
    ylab(NULL) + 
    ggtitle(glue("{title_addition}Scree Plot")) +
    ylim(0, 1 + y_addition) +  
    ggrepel::geom_text_repel(nudge_y = .1,
                             segment.size = 0)
}

pca_interpretation <- function(df){
  df %>% 
    mutate(
      text = case_when(
        group == "Be mindful of your eating habits" ~ 
          
          glue("The first 2 components explain 56% of variation in the 6 items (within group: Be mindful of your eating habits). 
    
  1. explains 34%. All items load in the same direction, showing that what explains the most variation in these items is whether someone is generally mindful of eating habits: that 1) they are not eating when emotional and not hungry (eat_emo), 2) they are not engaging in other activites while eating (eat_engage), 3) they do not rush to to eat, and 4) they rely on hunger and fullness signals to start and stop eating respectively.
    
  2. explains 21%. Two sets of items load in one direction (rely_start ~= rely_start), two in the other (eat_emo < eat_engage), and two load very weakly. This component represents a constrast between hunger/fullness reliance and unemotional/undistracted eating. In other words, paying attention to physical appetite or to emotions and distractions.
   
  3. explains a further 15% but is difficult to interpret."),
        
        group == "Enjoy your food" ~ 
          
          glue("The first three components explain 74% of variation in the five items (within group: Enjoy your food).
  
  1. explains 36%. All items load in the same direction, and the weakest loading comes from eating foods that are part of family traditions (eat_Ftrad). What explains the most variation in these five items is whether someone enjoys a variety of foods (eat_Vtrad, joy_newfoods, joy_eat) as well as the act of cooking.
  
  2. explains 21%. Two items load in one direction (eat_Vtrad < joy_newfoods), and, in the other direction, one item loads strongly (eat_Ftrad) and the last two weakly. This component represents whether someone eats foods from within their family's tradition or enjoys exploring foods from outside traditions.
   
  3. explains 17%. Two items load in one direction (eat_Vtrad > eat_Ftrad), and, in the other direction, one item loads strongly (joy_eat) and the last two weakly. This component represents whether someone eats foods from traditions or simply enjoys eating."),
        
        group == "Cook more often" ~ 
          
          glue("The first four components explain almost 90% of variation in the six items (within group: Cook more often).
  
  1. explains 36%. All items load in the same direction, showing that what explains the most variation is the degree to which someone cooks and eats at (someone's) home, and plans and works with others on meals.
    
  2. explains 26%. Two items load in one direction (WO_cook ~= WO_plan), three in the other (eat_Hcook ~= Hcook > eat_out), and the last very weakly. This component represents whether someone works with others on meals or cooks alone and eats at home. 
   
  3. explains 13%. Three items load in one direction (eat_Hcook > WO_cook >> Hcook), twos item load in the other (plan_Mahead >>> eat_out), and the last practically does not load. This component represents whether someone eats cooked food and helps in cooking it or plans the meals.
   
  4. explains 12%. One item loads strongly in one direction (eat_out), three items load in the other direction (plan_Mahead > eat_Hcook ~= Hcook), and the remaining two load weakly. This component repesents whether someone eats at home without planning or working with others."),
        
        group == "Nutrition" ~ 
          
          glue("The first four components explain roughly half of variation in the 18 Nutrition items. 
  
  1. explains 26%. All items load in the same direction, with very weak loadings from two items (budget and use_therm). Hence budgetting one's spending on food and checking internal temperatures of food for safety are not related to the most explanatory set of items on nutrition literacy. The groups of items contributing the most to this principal component are, in order, 1) food-skill confidence; and 2) understanding healthy eating patterns, and use of home-cooking and good food practices.
  
  2. explains 10%. Two items load very strongly in one direction (eat_out > Hcook), and many in the other (follow_storage > wash20 > IC_safe > clean_kitchen). This component represents whether someone eats and cooks at home or is able to read/follow nutritional instructions, labels and good practices.
  
  3. explains 9% and is difficult to interpret (eat_out is not together with eat_Hcook and Hcook)."),
        
        group == "Agri-Food Systems" ~ 
          
          glue("The first two components explain 81% of variation in the three Agri-Food Systems items. 
  
  1. explains 58%. All items load in the same direction (aware_Fseason ~= know_how ~= look_Flabels). Being aware of food seasons, having agricultural-food know-how, and looking at food labels for food origin form this component.
  
  2. explains 23%. One item loads strongly in one direction (look_Flabels) and one in another (know_how). This component represents a contrast between looking at labels and agricultural know-how."),
        
        group == "Cultural" ~ 
          
          glue("The first two components explain ~70% of variation in the four Cultural items.
  
  1. explains 45%. All items load in the same direction (get_food_Ftrad ~= loc_food_Ftrad > eat_Ftrad > eat_Vtrad). Most of the contribution to this component comes from location awareness of and ability to acquire food from one's family tradition. 
  
  2. explains 24%. One item loads strongly in one direction (eat_Vtrad) and one moderately in another (eat_Vtrad). This component represents eating foods from a variety of traditions vs. only the family's tradition."),
        
        group == "Eco" ~ 
          
          glue("The first two components explain ~70% of variation in the three Eco items.
  
  1. explains 43%. All items load in the same direction and similarly in strength. Avoiding plastic, understanding best-before dates and what to do with leftover food are equally important to this component.
  
  2. explains 31%. One item loads strongly in one direction (eat_plastic) and one in another (best_before). This component represents whether one avoids plastic or understands best-before dates."),
        
        group == "How to eat" ~ 
          
          glue("The first two components explain a third of the variation in the 21 'How to eat' items.
     
  1. explains 21%. All items load in the same direction, with one (aware_adverts) being very weak. What explains the most variation in our participants is whether or not they follow good how-to-eat practices (without knowing about the influence of adverts on one's desire to eat or drink).
  
  2. explains 10%. Many load in one direction (joy_newfoods > rely_start > eat_Vtrad ...), and many in the other (WO_plan > WO_cook > eat_emo...). This component represents whether someone enjoys foods from a variety of traditions, or works with others on meals and is not otherwise engaged while eating."),
        
        
        group == "How to eat (subset)" ~ 
          
          glue("The first two components explain 43% of variation in the 'How to eat' subset items.
     
  1. explains 29%. All items load in the same direction,. What explains the most variation in this subset is whether or not they enjoy taking time to eat, eating with others, and home cooking.
  
  2. explains 14%. Four items load in one direction (WO_cook = WO_plan > eat_emo > WO_eat), and 5 in the other (Hcook > eat_Hcook > joy_eat > eat_out > joy_cook). This component represents whether someone 1) works with others to cook and plan meals, and does not eat only due to feeling emotional; or 2) enjoys home cooking."),
        
        group == "How to eat (subset 10)" ~ 
          
          glue("The first two components explain almost half of variation in the 'How to eat' subset of 10 items.
     
  1. explains 32%. All items load in the same direction. What explains the most variation in this subset is whether or not they enjoy taking time to eat, eating with others, and home cooking.
  
  2. explains 13%. Four items load in one direction (eat_emo > WO_eat > joy_eat_WO >> eat_Ttime), and 5 in the other (Hcook > eat_Hcook > eat_out > joy_eat >> joy_cook). This component represents whether someone 1) does not eat out of emotion and eats meals with others when possible; or 2) eats at home and finds joy in eating."),
        
        group == "Food literacy" ~ 
          
          glue("The first five components explain almost half of variation in the 'Food literacy' items.
     
  1. explains 18%. All but the budget item load in the same direction. What explains the most variation in this subset are the confidence items.
  
  +. Components two to five each explain around 7%."),
        
        TRUE ~ NA_character_)
    )
}

### Correlations ####


corr_round <- function(df){
  df %>% correlate() %>% round_two
}


## DT for correlation matrix
corr_dt <- function(df) {
  datatable(
    df,
    extensions = 'FixedColumns',
    options = list(
      dom = 'rt',
      scrollX = TRUE,
      fixedColumns = TRUE,
      pageLength = nrow(df),
      scrollY = 500,
      scroller = TRUE
    ),
    rownames = F
  )
}

#### Outputs ####
corr_matrix <- function(df){
  df %>% 
    corr_round %>% 
    rearrange() %>%
    shave() %>% 
    rename(item = term)
}

corr_means <- function(df){
  means <- df %>% 
    corr_round %>% 
    select(- term) %>% 
    map_dbl(~ mean(., na.rm = TRUE)) %>% 
    round(2) %>% 
    .[order(.)]
  
  tibble(item = names(means), corr_means = means)
}

corr_plot <- function(df){
  df %>%
    correlate %>% 
    shave() %>% 
    rplot
}

data_by <- function(df, y, x){
  if(x == "gender")
  {df <- df %>% filter(gender %in% c("Man", "Woman"))} 
  if(x == "smoking_group")
  {df <- df %>% filter(smoking_group %in% c("smoker", "non_smoker"))} 
  
  df <- df %>% 
    select({{ x }}, all_of({{ y }})) %>% 
    rename_with(~ str_replace(.x, "hte_", ""))
  
  df_split <- df %>% nest(data = !{{ x }})
  
  df <- tibble(
    data = list(df %>% select(- {{ x }}))
  )
  df[, x] <- "overall"
  
  df %>% bind_rows(df_split)
}

hush <- function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}

add_corr <- function(df){
  df %>% 
    mutate(
      #corr_plot = map(data, corr_plot),
      cmatrix = map(data, corr_matrix),
      cmeans = map(data, corr_means)
    )
}

evaluate_alpha <- function(x) evaluate("psych::alpha(x)")

add_warning_alpha <- function(df){
  df %>% 
    mutate(
      eval_alpha = map(data, evaluate_alpha),
      warning_alpha = map(
        eval_alpha, # ^ is needed below due to an extra list element when knitting
        ~ .x[str_detect(.x, "^Some items.*were negatively correlated")]
      ),
      warning_items = map(
        warning_alpha, 
        ~ .x %>% 
          str_extract("\\(.*\\)") %>% 
          str_replace_all("(\\( | \\))", "") %>% 
          str_split(" ") %>% unlist
      ),
      data_subset = map2(
        data, 
        warning_items, 
        ~ select(.x, - any_of(.y))
      )
    )
}

tidy_psych_alpha <- function(df){
  df %>%
    mutate(item = rownames(df)) %>%
    select(item, everything()) %>%
    round_two
}

tidy_dropped_alpha <- function(df){
  df[["alpha.drop"]] %>%
    tidy_psych_alpha %>% 
    select(item, raw_alpha) %>%
    rename(dropped_alpha = raw_alpha)
}

tidy_item_stats <- function(df) {
  df[["item.stats"]] %>%
    tidy_psych_alpha %>% 
    select(item, r.drop, raw.r) %>%
    rename(itc = raw.r, itcc = r.drop)
}

add_alpha <- function(df, data){
  df %>% 
    mutate(
      pa := map({{ data }}, ~ psych::alpha(.x)),
      alpha := map_dbl(pa, ~ .[["total"]] %>% pull(raw_alpha) %>% round(2)),
      dropped_alpha := map(pa, tidy_dropped_alpha),
      itc := map(pa, tidy_item_stats),
      itcc := map(itc, . %>% select(item, itcc)),
      itc := map(itc, . %>% select(item, itc))
    ) %>% 
    rename(
      "alpha_of_{{ data }}" := alpha,
      "dropped_alpha_of_{{ data }}" := dropped_alpha,
      "itc_of_{{ data }}" := itc,
      "itcc_of_{{ data }}" := itcc
    )
}

## Pivot-wider names values
pw_nv <- function(df, group, y){
  group <- as.name(group)
  df %>% 
    select(set, {{ group }}, {{ y }}) %>% 
    unnest({{ y }}) %>% 
    pivot_wider(names_from = {{ group }}, values_from = {{ y }})
}

# HEFI --------------------------------------------------------------------

hefi_corr <- function(df, x, questionnaire, y){
  
  item_sum <- questionnaire %>% glue("_score")
  y <- c(item_sum, y)
  
  df %>% 
    select({{ x }}, all_of(y)) %>% 
    corr_round %>%
    rename(variable = term) %>% 
    select(variable, {{ x }}) %>% 
    arrange(desc({{ x }})) 
}

hefi_dt <- function(df){
  df %>% 
    datatable(
      extensions = 'FixedColumns',
      options = list(
        columnDefs = list(
          list(className = 'dt-center', targets = 0:3)
        ),
        dom = 'rt',
        scrollX = TRUE,
        fixedColumns = TRUE,
        pageLength = nrow(df),
        scrollY = 500,
        scroller = TRUE
      ),
      rownames = F
    )
}

summarize_tiles <- function(df, tiles, name_tiles, y, x){
  df %>% 
    mutate(
      "{name_tiles}" := ntile({{ y }}, tiles)
    ) %>% 
    group_by(across(all_of(name_tiles))) %>% 
    summarize(
      "{{ y }}" := max({{ y }}),
      "{x}_mean" := mean(
        !!sym(glue("{x}_score")), 
        na.rm = TRUE
      ),
      n = n()
    )
}

### efa ####

N_FACTORS_no_hull <- function(corr, x){
  N_FACTORS(
    corr, 
    N = x, 
    criteria = c(
      # "CD", # CD needs raw data but we use corr matrix
      "EKC", "KGC", "PARALLEL", 
      "SCREE", "SMT"
      ),
    eigen_type_other = c("SMC", "PCA")
  )
}

# archive #####
# 
# ## Total
# tab <- function(df, x, y){
#   df %>% 
#     tabyl({{ x }}, {{ y }}) %>%
#     adorn_totals(c("col")) %>%
#     adorn_percentages("col") %>% 
#     adorn_pct_formatting(rounding = "half up", digits = 0) %>%
#     adorn_ns(position = "front") %>% 
#     select(1, Total, everything()) %>% 
#     knitr::kable()
#   
# }
# 
# 
# categorize <- function(df){
#   df %>% 
#     mutate(
#       i = as.factor(i),
#       s = as.factor(s),
#       Category = as.factor(
#         case_when(
#           i %in% scores ~ "Scores",
#           i %in% hte_cols ~ "HTE",
#           i %in% nutrition ~ "Nutrition Literacy",
#           i %in% agri ~ "Agri-Foods System Literacy",
#           i %in% media ~ "Food Media Literacy",
#           i %in% civic ~ "Civics Literacy",
#           i %in% cultural ~ "Cultural Literacy",
#           i %in% eco ~ "Eco Literacy"
#         )
#       )
#     ) %>%
#     rename(
#       Column = i,
#       Statistic = s
#     ) %>% 
#     select(
#       Category, 
#       everything()
#     )
# }
# 
# # To create df of column names and column unique values
# show_unique <- function(df, cols) {
#   map(
#     df %>% 
#       select(
#         all_of(cols)
#       ) %>% 
#       mutate(# Replace NA with character string as NA in a vector x will cause a collapse of vector x to be NA
#         across(
#           everything(), # z is used to place at end when ordered
#           ~ replace_na(.x, "z_NA")
#         )
#       ), 
#     ~ str_c(
#       unique(.x)[order(unique(.x))], 
#       collapse = ", "
#     )
#   ) %>% 
#     bind_rows %>% 
#     pivot_longer(
#       cols = everything(),
#       names_to = "Column",
#       values_to = "Unique Responses"
#     )
# }
# 
# option_freq <- function(df, y){
#   df %>% 
#     select(y) %>% 
#     pull() %>% 
#     str_split(",(?!\\s)") %>% 
#     unlist() %>% 
#     table() %>% 
#     as.data.frame()  %>% 
#     rename(
#       Choice = ".",
#       n = Freq
#     ) %>%
#     mutate(
#       Column = {{ y }},
#       "%" = 100*n / nrow(df)
#     ) %>% 
#     select(
#       Column,
#       everything()
#     ) %>%
#     arrange(desc(n)) %>% 
#     round_two
# }
# 
# score <- function(x, a, b, c, d, e){
#   case_when(
#     x == a ~ 1,
#     x == b ~ 2,
#     x == c ~ 3,
#     x == d ~ 4,
#     x == e ~ 5,
#     TRUE ~ NA_real_
#   )
# }
# 


# 
# # `name<-` <- function(x, value) {
# #   names(x) <- rep(value, length(x))
# #   x
# # }