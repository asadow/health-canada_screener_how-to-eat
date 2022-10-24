# read_clean_ids <- function(){
#   path <- here("data", "processed")
#   flist <- list.files(path, pattern = "*.csv")
# 
#   ids <- map_df(
#     flist,
#     ~ here(path, .x) %W>%
#       data.table::fread(., skip = 1, header = TRUE,
#                         select = c("External Data Reference",
#                                    "Response ID",
#                                    "FLAG for exclusion")
#       ) %>%
#       clean_names
#   )
# 
#   ids %>% filter(flag_for_exclusion %in% "ok" | is.na(flag_for_exclusion))
# 
# }

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


recode_var <- function(df, x){
  numeric_text <- df_choice_text %>% 
    select(response_format, {{ x }}) %>% 
    filter(
      !duplicated({{ x }}) 
      & !is.na({{ x }})
    )
  
  if(nrow(numeric_text) > 0 
     & c("text", "numeric") %in% numeric_text$response_format %>% all){
    numeric <- numeric_text %>% 
      filter(response_format == "numeric") %>% 
      select(numeric = {{ x }})
    
    text <- numeric_text %>% 
      filter(response_format == "text") %>% 
      select(text = {{ x }})
    
    text_numeric <- text %>% cbind(numeric)
    
    level_key <- text_numeric$numeric  
    names(level_key) <- text_numeric$text
    
    df %>% mutate("{{ x }}" := recode({{ x }}, !!!level_key)) 
  } else {df}
  
}


fix_responses_of_recruit_one <- function(df){
  ## 1st-recruit data to recode
  i <- which(df$wave == "1st-recruit")
  one_df <- df$data[[i]]
  
  ## Fix improper names
  one_df <- one_df %>% 
    rename(i_plan_meals_ahead_of_time = i_plans_meals_ahead_of_time)
  
  ## Recode common names
  common_names <- intersect(names(one_df), names(df_choice_text))
  for (i in seq_along(common_names)){
    name <- sym(common_names[i])
    one_df <- one_df %>% recode_var(!!name)
    
  }
  
  df %>% 
    mutate(
      data = replace(data, wave == "1st-recruit", list(one_df))
    )
  
}

fix_french_misaligned_cols <- function(df){
  ## Fixing misaligned columns between FR and EN surveys 
  ## Maude: question on budget in FR version should be moved
  ## to after the question on confidence in modifying recipe
  i <- which(df$language == "fr")
  fr_df <- df$data[[i]]
  df$data[[i]] <- fr_df %>% 
    relocate(
      je_budgetise_combien_je_depense_pour_la_nourriture,
      .after = dans_quelle_mesure_etes_vous_sur_e_de_pouvoir_faire_ce_qui_suit_modifier_une_recette_par_exemple_pour_la_rendre_plus_sante_pour_respecter_des_restrictions_alimentaires_pour_reduire_les_couts_ou_pour_tenir_compte_de_vos_propres_pratiques_culturelles
    )
  
  df
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
  
  df <- df %>% 
    mutate(
      across(c(external_data_reference, participant_id), ~ na_if(.x, "")),
      id = coalesce(external_data_reference, participant_id),
      id = id %>% replace(id == "CFS14" & wave == "2nd-recruit", "CFS014"),
      id = id %>% replace(response_id == "R_eXWqTbHzMljssQV", "CFS02")
    ) %>%
    filter(
      response_id %in% eligible_ids$response_id
      # !str_detect(id, "ImportId") &
      #   response_id %in% ids$response_id |
      #   # clean fr data uses external_data_reference instead of response_id;
      #   language == "fr" & id %in% ids$external_data_reference
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
# ct <- df_choice_text %>% 
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

