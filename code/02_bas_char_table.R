
###### 
##
##
##
######

library(tidyverse)

load(file.path("data", "data_andrea.RData"))

all_vars <- read.csv2(file.path("data", "all_data.csv"), stringsAsFactors = F)




# Prepare data for baseline table ------------------------------------------

soc_dem <- pain_andrea2 %>% 
  
  
  filter( # We chose SwiSCI participants that also participated in the pain study
    
    id_swisci %in% unique(pull(all_vars, id_swisci))) %>% 
  
  
  select( # We select and rename the variables we use
    
    id_swisci,
    sex = ts1_sex, 
    age = ts1_age_quest,
    months_since_SCI = ts1_time_since_sci,
    degree_of_SCI = ts1_sci_degree,
    cause_of_SCI = ts1_sci_cause_type,
    pain_intensity = ts1_pain_intensity,
    lesion_level = ts1_sci_type) %>% 
  
  
  mutate( # We modify some variables
    
    years_since_SCI = months_since_SCI / 12,
    id_swisci = as.character(id_swisci)) %>% 
  
  mutate( # We rename the variable levels/categories
    
    sex = factor(sex, levels = c(1, 2), c("male", "female")),
    lesion_level = factor(lesion_level, levels = c(3, 6), c("paraplegia", "tetraplegia")),
    degree_of_SCI = factor(degree_of_SCI, levels = c(1, 2), c("complete", "incomplete")), 
    cause_of_SCI = factor(cause_of_SCI, levels = c(1, 2, 3), c("traumatic", "non-traumatic", "other"))
    
  ) %>% 
  
  
  mutate( # We rearrange the factor levels/categories
    
    sex = fct_relevel(sex, c("male", "female")),
    cause_of_SCI = factor(cause_of_SCI, c("traumatic", "non-traumatic", "other")),
    
    seveity_of_SCI = case_when(
      
      lesion_level == "paraplegia" & degree_of_SCI == "complete" ~ "complete_paraplegia",
      lesion_level == "paraplegia" & degree_of_SCI == "incomplete" ~ "incomplete_paraplegia",
      lesion_level == "tetraplegia" & degree_of_SCI == "complete" ~ "complete_tetraplegia",
      lesion_level == "tetraplegia" & degree_of_SCI == "incomplete" ~ "incomplete_tetraplegia",
      TRUE ~ NA_character_),
    
    seveity_of_SCI = as.factor(seveity_of_SCI),
    seveity_of_SCI = fct_relevel(seveity_of_SCI, c("complete_paraplegia", "incomplete_paraplegia", 
                                                   "complete_tetraplegia", "incomplete_tetraplegia"))
    
  )


map(soc_dem, class)



# Summarize discrete variables --------------------------------------------


# Calculations

discrete_vars <- soc_dem %>% 
  
  select_if(is.factor) %>% 
  
  map(function(category) table(category, useNA = "ifany")) %>% 
  
  map_dfr(~as_tibble(.), .id = "variable") %>% 
  
  group_by(variable) %>% 
  
  
  mutate( # We calculate the number [n, (%)] of participants with specific characteristics
    
    prop = n / sum(n) * 100,
    prop = formatC(prop, digits = 0, format = "f"),
    n_perc = str_c(n, " ", "(", prop, ")")) %>% 
  
  ungroup()


# Cosmetics

discrete_vars <- discrete_vars %>% 
  
  replace_na(list(category = "missing")) %>% 
  
  filter(n != 0) %>% 
  
  filter(!variable %in% c("degree_of_SCI", "lesion_level")) %>% 
  
  mutate(variable = replace(variable, duplicated(variable), "")) %>% 
  
  select(-n, -prop)



# Summarize continuous variables --------------------------------------------

continuous_vars <- soc_dem %>% 
  
  mutate_if(is.integer, as.double) %>% 
  
  select_if(is_double) %>% 
  
  map(~quantile(., c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
  
  map(~formatC(., digits = 0, format = "f")) %>% 
  
  map(~str_c(.["50%"], " (", .["25%"], "\u2013", .["75%"], ")")) %>% 
  
  map_dfr(~enframe(., name = NULL), .id = "variable")


# Cosmetics

continuous_vars <- filter(continuous_vars, variable != "months_since_SCI")


# For pain table ----------------------------------------------------------


# Calculations

pain_vars <- pain_andrea2 %>% 
  
  filter(id_swisci %in% unique(pull(all_vars, id_swisci))) %>% 
  
  select(id_swisci, ts1_pain, contains("pain_type"), ts1_pain_intensity) %>% 
  
  rename(
    
    pain_at_all = ts1_pain,
    musculoskeletal_system = ts1_pain_type_1,
    internal_organs = ts1_pain_type_2,
    due_to_spasms = ts1_pain_type_3,
    neuropathic_above_lesion_level = ts1_pain_type_4,
    neuropathic_at_lesion_level = ts1_pain_type_5,
    neuropathic_beneath_lesion_level = ts1_pain_type_6,
    other = ts1_pain_type_7,
    other_specified = ts1_pain_type_other,
    pain_intensity = ts1_pain_intensity
    
  ) %>% 
  
  mutate_at(vars(pain_at_all:neuropathic_beneath_lesion_level), ~replace_na(., 0)) %>% 
  
  mutate(n_participants = 1) %>% 
  
  mutate(
    
    neuropathic_pain = rowSums(select(., contains("neuropathic"))),
    neuropathic_pain = if_else(neuropathic_pain >= 1, 1, 0)
    
  )

pain_vars_cat <- pain_vars %>% 
  
  summarize_at(vars(musculoskeletal_system:neuropathic_beneath_lesion_level, n_participants, neuropathic_pain), sum) %>% 
  
  pivot_longer(cols = c(musculoskeletal_system:neuropathic_beneath_lesion_level, neuropathic_pain), names_to = "variable", values_to = "n") %>% 
  
  filter(variable != "n_participants") %>% 
  
  mutate(rel_freq = (n / n_participants) * 100)


pain_vars_mean <- pain_vars %>% 
  
  pivot_longer(
    cols = c(musculoskeletal_system:neuropathic_beneath_lesion_level, neuropathic_pain), 
    values_to = "pain_yes", 
    names_to = "pain_types") %>% 
  
  filter(pain_yes == 1) %>% 
  
  select(id_swisci, pain_types, pain_intensity) %>% 
  
  group_by(pain_types) %>% 
  
  summarize(
    mean_pain_intensity = mean(pain_intensity, na.rm = TRUE), 
    sd_pain_intensity = sd(pain_intensity, na.rm = TRUE)
    
  )


round(mean(pain_vars$pain_intensity, na.rm = TRUE), 1)


# Cosmetics

pain_vars_table <- pain_vars_cat %>% 
  
  left_join(pain_vars_mean, by = c("variable" = "pain_types")) %>% 
  
  mutate(n_rel_freq = str_c(n, " (", formatC(rel_freq, digits = 0, format = "f"), ")")) %>% 
  
  mutate(
    
    variable = as.factor(variable), 
    
    variable = fct_relevel(variable, 
                           "musculoskeletal_system", 
                           "neuropathic_pain",
                           "neuropathic_above_lesion_level",
                           "neuropathic_at_lesion_level",
                           "neuropathic_beneath_lesion_level",
                           "internal_organs",
                           "due_to_spasms")) %>% 
  
  mutate(
    
    pain_intensity = str_c(
      
      formatC(mean_pain_intensity, digits = 1, format = "f"),
      
      " (",
      
      formatC(sd_pain_intensity, digits = 1, format = "f"),
      
      ")"
    )
    
  ) %>% 
  
  select(-n, -n_participants, -rel_freq, -mean_pain_intensity, -sd_pain_intensity) %>% 
  
  arrange(variable)



  
  



# Tables ------------------------------------------------------------------

write.csv2(discrete_vars, file.path("output", "table_1_discrete_vars.csv"), row.names = FALSE)
write.csv2(continuous_vars, file.path("output", "table_1_continuous_vars.csv"), row.names = FALSE)
write.csv2(pain_vars_table, file.path("output", "table_1_pain_vars.csv"), row.names = FALSE)

discrete_vars

continuous_vars

pain_vars_table
