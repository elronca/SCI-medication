## Associations which opioid intake

library(tidyverse)
library(lme4)
library(broom.mixed)
library(cowplot)
library(brms)
library(dotwhisker)

load(file.path("workspace", "opioid_info.RData"))
load(file.path("data", "data_andrea.RData"))

all_vars <- read.csv2(file.path("data", "all_data.csv"), stringsAsFactors = F)



soc_dem <- pain_andrea2 %>% 
  
  filter(id_swisci %in% unique(pull(all_vars, id_swisci))) %>% 
  
  rename(
    sex = ts1_sex, 
    age = ts1_age_quest,
    months_since_SCI = ts1_time_since_sci,
    degree_of_SCI = ts1_sci_degree,
    cause_of_SCI = ts1_sci_cause_type,
    pain_past_week = ts1_pain,
    pain_intensity = ts1_pain_intensity,
    lesion_level = ts1_sci_type) %>% 
  
  mutate(
    sex = factor(sex, levels = c(1,2), c("male", "female")),
    lesion_level = factor(lesion_level, levels = c(3,6), c("paraplegia", "tetraplegia")),
    degree_of_SCI = factor(degree_of_SCI, levels = c(1,2), c("complete", "incomplete")), 
    cause_of_SCI = factor(cause_of_SCI, levels = c(1,2,3), c("traumatic", "non-traumatic", "other")),
    pain_past_week = factor(pain_past_week, levels = c(0,1), c("no", "yes"))) %>% 
  
  mutate(pain_type = if_else(ts1_pain_type_4 == 1 | ts1_pain_type_5 == 1, "neuropathic", NA_character_),
         years_since_SCI = round(months_since_SCI / 12, 0),
         id_swisci = as.character(id_swisci)) %>% 
  
  select(id_swisci:lesion_level, degree_of_SCI, cause_of_SCI, years_since_SCI, pain_type)


my_vars <- all_vars %>% 
  
  select(id_swisci, 
         contains("qual_life"), 
         contains("ppi1_pain_intensity"),
         contains("SUM_HADS"),
         contains("COMPLETE"),
         contains("_3SedHypn")
         ) %>% 
  
  rename(ppi1_ltfu = Baseline_COMPLETE, ppi3_ltfu = Posttreatm_COMPLETE, ppi4_ltfu = Followup_COMPLETE) %>% 
  
  rename_all(tolower) %>% 
  
  gather(the_vars, the_values, -id_swisci) %>% 
  
  mutate(tp = str_split(the_vars, "_") %>% map(1) %>% unlist(),
         the_vars = str_replace_all(the_vars, c("ppi1_" = "", "ppi3_" = "", "ppi4_" = ""))) %>% 
  
  group_by(the_vars) %>%  
  
  mutate(unique_identifier = row_number()) %>% 
  
  spread(the_vars, the_values) %>%
  
  ungroup() %>% 
  
  select(-unique_identifier) %>% 
  
  mutate(id_swisci = as.character(id_swisci))


anDs <- right_join(soc_dem, my_vars, by = "id_swisci")

opioids <- opioids %>% select(id_swisci, tp, ther_freq_2)

anDs <- left_join(anDs, opioids, by = c("id_swisci", "tp"))


rm(all_vars, my_vars, opioids, pain_andrea2, soc_dem)


anDs <- anDs %>% 
  
  filter(ltfu != 0) %>% 
  
  mutate(ther_freq_2 = as.character(ther_freq_2),
    
    is_opioid = case_when(
      ther_freq_2 %in% "no opioid" ~ 0L,
      ther_freq_2 %in% "pro re nata" ~ 1L,
      ther_freq_2 %in% "regular therapy" ~ 1L,
      is.na(ther_freq_2) ~ 0L,
      TRUE ~ NA_integer_))

anDs <- anDs %>% 
  select(id_swisci, tp, sex, age, qual_life, sum_hads, "3sedhypn", is_opioid)

anDs_ppi1 <- anDs %>% 
  filter(tp == "ppi1") %>% 
  rename(sedhypn = "3sedhypn")

fit <- glm(is_opioid ~ sex + age + sum_hads + sedhypn, data = anDs_ppi1, family = binomial(link = "logit"))

tidy_fit <- tidy(fit)

as_tibble(confint(fit)) %>% 
  set_names("ll", "ul") %>% 
  add_column(estim = fit$coefficients, .before = 1) %>% 
  mutate_all(exp) %>% 
  mutate_all(round, 2) %>% 
  add_column(term = tidy_fit$term, .before = 1) %>% 
  add_column(p_val = round(tidy_fit$p.value, 2))
