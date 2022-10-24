fetch_survey_format <- function(id, response_format){
  fetch_survey(
    id, 
    label = ifelse(response_format == "numeric", FALSE, TRUE), 
    force = TRUE, 
    convert = FALSE,
    breakout_sets = FALSE 
    ## en and fr surveys end up with different ncols if breakout_sets = TRUE
    ## fr, survey has 151 vs 149 ncols
    ## unaware why
  ) %>%
    label_to_colnames %>% 
    clean_names
}

read_raw_data <- function(){
  
  path <- here("data", "raw")
  flist <- list.files(path, pattern = "*.csv")
  nms <- c("data_type", "study", "language", 
           "date_downloaded", "response_format", 
           "wave", "file_type")
  m <- str_split_fixed(flist, "[_\\.]", length(nms))
  colnames(m) <- nms
  
  df <- as_tibble(m) %>% add_column(file = flist)
  
  df %>% 
    mutate(
      data = map(
        file, 
        ~ read_csv(
          here(path, .x), 
          col_types = cols(.default = "c"),
          skip = 1
        ) %>% 
          clean_names %>% 
          filter(!str_detect(progress, "ImportId"))
      ) 
    )
}


recode_var <- function(df, x, reference = df_en_both_formats){
  ## Get unique and non-missing responses
  x_both_formats <- df_en_both_formats %>% 
    select(response_format, {{ x }}) %>% 
    filter(
      !is.na({{ x }})
    )
  
  ## Create df of the unique values of numeric and text
  if(nrow(x_both_formats) > 0 
     & c("text", "numeric") %in% x_both_formats$response_format %>% all){
    numeric <- x_both_formats %>% 
      filter(response_format == "numeric") %>% 
      select(numeric = {{ x }})
    
    text <- x_both_formats %>% 
      filter(response_format == "text") %>% 
      select(text = {{ x }})
    
    text_numeric <- text %>% cbind(numeric)
    ## Create levels and recode
    level_key <- text_numeric$numeric %>% unique
    names(level_key) <- text_numeric$text %>% unique
    
    df %>% mutate("{{ x }}" := recode({{ x }}, !!!level_key)) 
  } else {df}
  
}


fix_recruit_one_response_type <- function(df, reference = df_en_both_formats){
  ## Pull 1st-recruit data
  i <- which(df$wave == "1st-recruit")
  one_df <- df$data[[i]]
  
  ## Recode common names
  common_names <- intersect(
    names(one_df), 
    names(df_en_both_formats)
    )
  
  for (i in seq_along(common_names)){
    name <- sym(common_names[i])
    one_df <- one_df %>% recode_var(!!name)
    
  }
  
  df %>% 
    mutate(
      data = replace(data, wave == "1st-recruit", list(one_df)),
      response_format = replace(
        response_format, 
        wave == "1st-recruit",
        "numeric"
        )
    )
  
}

fix_french_misaligned_cols <- function(df){
  ## Fixing misaligned columns between FR and EN surveys 
  ## Maude: question on budget in FR version should be moved
  ## to after the question on confidence in modifying recipe
  df %>% 
    mutate(
      data = map2(
        data,
        language,
        function(data, language){ 
          if(language == "fr")
            data %>% 
            relocate(
              je_budgetise_combien_je_depense_pour_la_nourriture,
              .after = dans_quelle_mesure_etes_vous_sur_e_de_pouvoir_faire_ce_qui_suit_modifier_une_recette_par_exemple_pour_la_rendre_plus_sante_pour_respecter_des_restrictions_alimentaires_pour_reduire_les_couts_ou_pour_tenir_compte_de_vos_propres_pratiques_culturelles
            )
          else 
            data
        }
      )
    )
}

use_english_names <- function(df, x){
  
  en_df <- df %>% 
    filter(
      language == "en" & 
        response_format == "numeric" &
        survey_type == x
    ) %>% 
    pull(data) %>% 
    reduce(data.frame)
  
  df %>% 
    mutate(
      data = map2(
        data,
        survey_type,
        function(data, survey_type){ 
          if(survey_type == x)
            data %>% set_names(names(en_df))
          else
            data
        }
      )
    )

}

assign_english_names <- function(df){
  ## To all but 1st-recruit
  ## since 1st-recruit does not have similar columns
  ## Then bind 1st-recruit
  
  first_recruit <- df %>% filter(wave == "1st-recruit") %>% unnest(data)
  
  ## Keep only numeric format
  df <- df %>% filter(response_format == "numeric")
  
  i <- with(df, which(language == "en" & response_format == "numeric")) 
  
  en_df <- df$data[[i]]
  
  df <- df %>% 
    mutate(
      data = map(
        data,
        ~ .x %>% set_names(names(en_df))
      )
    )
  
  df <- df %>% unnest(data)
  df <- df %>% bind_rows(first_recruit)
  
  df
}

fix_ids <- function(df){
  ### id re-assignment 
  ## Re-Assign id's that Qualtrics had repeated
  ## See email by Joy Hutchinson 24-01-22 3:29 pm
  
  ## For CFS14 in re-test:
  ## Found response_id by comparing email against the case in original survey
  ## as seen here:
  # df %>% filter(id %in% c("CFS14", "CFS014")) %>% select(id, survey_type, recipient_email, response_id)
  ## Checked CFS02 which has no identifying info in re-test and 
  ## ip_address does not match original survey
  
  df <- df %>% 
    mutate(
      across(c(external_data_reference, participant_id), ~ na_if(.x, "")),
      id = coalesce(external_data_reference, participant_id),
      id = id %>% 
        replace(id == "CFS14" & wave == "2nd-recruit", "CFS014"),
      id = id %>% 
        replace(id == "CFS14" & response_id == "R_1BLZCFIhJIOH7Gx", "CFS014"),
      id = id %>% 
        replace(response_id == "R_eXWqTbHzMljssQV", "CFS02")
    ) %>%
    filter(
      (survey_type == "survey" & response_id %in% eligible_ids$response_id) |
        (survey_type == "re-test" & id %in% eligible_ids$study_id)
        )
  df
}

# ## Compare small random sample of questions from EN and FR surveys
# ## confirming Jess's statement:
# ## The surveys were designed to be replicates of eachother. 
# ## Same questions, same order, same response options.
# 
# i <- which(df$language == "fr" & df$data_type == "survey")
# 
# x <- df$data[[i]] %>% names
# x <- x[50:length(x)]
# srs <- sample(length(x), 5)
# 
# e <- which(df$language == "en" & df$data_type == "survey")
# y <- df$data[[e]] %>% names
# y <- y[50:length(y)]
# 
# clipr::write_clip(x[srs])
# clipr::write_clip(y[srs])

# ### CASE
# 
# ct <- both_formats %>% 
#   select(response_format, start_date) %>% 
#   filter(
#     !duplicated(start_date) 
#     & !is.na(start_date)
#     )
# 
# c <- ct %>% 
#   filter(response_format == "numeric") %>% 
#   select(numeric = start_date)
# 
# t <- ct %>% 
#   filter(response_format == "text") %>% 
#   select(text = start_date)
# 
# ct <- t %>% cbind(c)
# 
# level_key <- ct$numeric  
# names(level_key) <- ct$text
# 
# i <- which(df$wave == "1st-recruit")
# one_df <- df$data[[i]]
# one_df <- one_df %>% 
#   mutate(
#     "age_selected_choice" = recode(age_selected_choice, !!!level_key)
#     )


code_as <- function(x, lvls){
  names(lvls) <- seq(lvls)
  y <- factor(recode(x, !!!lvls), lvls)
  str_replace_all(y, "  \\(\\d{1,2}\\)", "")
}

List <- function(...) {
  names <- as.list(substitute(list(...)))[-1L]
  setNames(list(...), names)
}

snake_to_sentence <- function(x){
  str_to_sentence(
    str_replace_all(x, "_", " ")
  )
}


## Silences warnings
`%W>%` <- function(lhs,rhs){
  w <- options()$warn
  on.exit(options(warn=w))
  options(warn=-1)
  eval.parent(substitute(lhs %>% rhs))
}

score_binary <- function(df, scoring, vars, weight){
  df %>% 
    mutate(
      across(
        all_of(vars),
        ~ case_when(
          .x %in% unlist(
            scoring %>% 
              filter(labels == cur_column()) %>% 
              pull(correct_responses)
          ) ~ weight,
          .x %in% unlist(
            scoring %>% 
              filter(labels == cur_column()) %>% 
              pull(incorrect_responses)
          ) ~ 0,
          TRUE ~ NA_real_
        )    
      )
    )
}
