
## Need to combine NVS_score 1, 2, and 3 into low since:
# df %>% group_by(NVS_score) %>% summarize(n())

## Counts of re-test

# df %>% filter(survey_type == "re-test") %>% pull(id) %>% .[order(.)]
# df %>% 
#   filter(survey_type == "re-test") %>%
#   group_by(language) %>%
#   summarize(n())


hte_n <- glue("hte_{1:21}")

## Hypothetical domains in comments
content_labels <- c( 
  "eat_Ttime", "eat_slow", "eat_engage", # Slow/distracted eating
  "rely_start", "rely_stop", # Signal-reliance eating
  "eat_emo", # Food-impulsiveness
  "eat_out", "eat_Hcook", "eat_Ftrad", "eat_Vtrad", "Hcook", 
  "WO_cook", # Food with others
  "plan_Mahead", # Food-impulsiveness
  "WO_plan", "WO_eat", # Food with others
  "use_Ninfo", # Information-aware
  "joy_eat", "joy_newfoods", "joy_cook", "joy_eat_WO", # Food-enjoyment
  "aware_adverts" # Information-aware
  )


efa_df <- df %>%  
  filter(language == "en") %>% 
  select(all_of(hte_labels)) %>% 
  filter(!if_any(everything(), is.na))

# ## Remove items that psych::alpha suggested to reverse-score
# efa_df <- efa_df %>%
#   select(!matches("^hte_(2|3|12|14)$"))

names(efa_df) <- content_labels

pcor <- efa_df %>% POLYCHORIC_R()
pcor <- efa_df %>% polychoric


library(polycor)
library(psych)

efa_ordered <- efa_df %>%   
  mutate(
    across(
      everything(), 
      ~ factor(.x, ordered = TRUE)
      )
    ) 

pcor_het <- efa_ordered %>%
  as.data.frame %>%
  hetcor

pcor_het$correlations
pcor

# N_FACTORS(pcor, N = nrow(efa_df), eigen_type_other = c("SMC", "PCA"))

## Using promax below as oblimin gives error at SL(efa_hte)
efa_hte <- EFA(pcor, n_factors = 4, rotation = "promax")
# efa_hte
# 
# dev.new(noRStudioGD = TRUE)
# 
# COMPARE(efa_hte$rot_loadings, wefa_hte$rot_loadings)
# 




## Archive ####



# sl_hte <- SL(efa_hte)
# OMEGA(sl_hte, type = "psych")




# PA_FA(efa_df, corkind= "polychoric", Nfactors = 3,
#       Ncases=nrow(efa_df), iterpaf = 50,
#       rotate='PROMAX', ppower = 3, verbose=TRUE)
# 
#        
# BARTLETT(df_hte)
# KMO(df_hte)
# library(psych)
# my.vss <- psych::VSS(pcor,n=8,rotate="none",diagonal=FALSE, n.obs = 323)   #compares up to 8 factors
# op <- par(mfrow=c(1,2))  #make a two panel graph
# VSS.plot(my.vss)  #shows a simple summary of VSS
# VSS.scree(pcor,main="scree plot of principal components of mydata")

# COMPARE(
#   EFA(pcor, n_factors = 3, rotation = "oblimin")$rot_loadings,
#   EFA(pcor, n_factors = 3, rotation = "oblimin", method = "ULS")$rot_loadings,
#   x_labels = c("PAF", "ULS")
# )

# x is total score?
alpha.scale=function (x,y)   #create a reusable function to find coefficient alpha
  #input to the function are a scale and a data.frame of the items in the scale
{
  Vi=sum(diag(var(y,na.rm=TRUE)))     #sum of item variance
  Vt=var(x,na.rm=TRUE)                #total test variance
  n=dim(y)[2]     #how many items are in the scale?  (calculated dynamically)
  ((Vt-Vi)/Vt)*(n/(n-1))}             #alpha


# EXTENSION_FA(pcor, Ncore=1, Next=6, higherorder=TRUE, roottest='MAP',
#              
#              corkind='pearson',  Nfactors=4,
#              
#              NfactorsHO=1, Ndatasets=100, percentile=95, salvalue=.40, numsals=3,
#              
#              iterpaf=200, ppower=4, verbose=TRUE)
#              


# 
# EFA_AVERAGE(pcor, n_factors = 3, N = 500,
#             method = c("PAF", "ML", "ULS"), rotation = "oblique",
#             show_progress = FALSE)
# 







## Correlations ####

## Archive ####

# ### Mean, SD, Range#
# 
# summary <- df %>% 
#   summ(c("hte_score", hte_n))
# 
# summary_l <- df %>% 
#   group_by(language) %>% 
#   summ(c("hte_score", hte_n))
# 
# sump <- summary %>% 
#   pivot_longer(
#     cols = everything(),
#     names_to = c("item", "stat"),
#     names_pattern = c("(.*)_(.*)"),
#     values_to = "total"
#   )
# 
# sump_l <- summary_l %>% 
#   pivot_longer(
#     cols = - language,
#     names_to = c("item", "stat"),
#     names_pattern = c("(.*)_(.*)"),
#     values_to = "Value"
#   ) %>% 
#   pivot_wider(
#     names_from = language,
#     values_from = Value
#   ) %>% 
#   mutate(
#     en_minus_fr = en - fr
#   )
# 
# sump <- sump %>% full_join(sump_l, by = c("item", "stat"))
# 
# sump <- sump %>% 
#   round_two %>%
#   mutate(stat = as.factor(stat))
# 
# datatable(sump, rownames = F, filter = "top", options = list(pageLength = 6, scrollY = 700))



