

####################################################################################################################
##
##  Pain-Related Medication Use in Community-Dwelling Persons with Spinal Cord Injury Suffering from Chronic Pain:
##  Misuse, Guideline Adherence, Medication Interactions
##
###################################################################################################################


library(tidyverse)



# Get med data of different time points -----------------------------------

get_data <- function(x) {
  
  file.path("data", "med_agents_r_ER.csv") %>% 
    
    read.csv2(stringsAsFactors = FALSE) %>% 
    
    select(
      id_swisci, starts_with(x), 
      -c(starts_with(str_c(x, "medication_")), str_c(str_to_upper(x), "1Opioid"):str_c(str_to_upper(x), "9Baclofen"))) %>% 
    
    mutate(id_swisci = as.character(id_swisci)) %>% 
    
    as_tibble()
  
}

time_points <- c("ppi1_", "ppi3_", "ppi4_")

drugs <- map(time_points, get_data)

names(drugs) <- time_points

map_dbl(drugs, function(x) length(unique(x$id_swisci)))



# Change names of drug names and doses variables --------------------------

colnames(drugs[["ppi1_"]]) <- str_replace_all(colnames(drugs[["ppi1_"]]), "pp1", "ppi1")


renameCols <- function(x) {
  
  
# Correct wrong column name

colnames(x) <- str_replace_all(colnames(x), "_add_", "_prn_")


# Regular therapy: drug name

rt_dn <- str_detect(colnames(x), "_daily_dosage_mg|_add_", negate = TRUE)

colnames(x)[rt_dn] <- str_replace_all(colnames(x)[rt_dn], "_agent_", "_drug_")


# Regular therapy: drug dose

rt_dd <- str_detect(colnames(x), "_daily_dosage_mg") & str_detect(colnames(x), "_prn_", negate = TRUE)

colnames(x)[rt_dd] <- str_replace_all(colnames(x)[rt_dd], c("_agent_" = "_", "daily_dosage_mg" = "dose"))


# Pro re nata therapy: drug name

prn_dn <- str_detect(colnames(x), "_daily_dosage_mg", negate = TRUE) & str_detect(colnames(x), "_prn_")

colnames(x)[prn_dn] <- str_replace_all(colnames(x)[prn_dn], "_agent_", "_drug_")


# Pro re nata therapy: drug dose

prn_dd <- str_detect(colnames(x), "_daily_dosage_mg") & str_detect(colnames(x), "_prn_")

colnames(x)[prn_dd] <- str_replace_all(colnames(x)[prn_dd], c("_agent_" = "_", "_prn_" = "_", "daily_dosage_mg" = "dose_prn"))

return(x)


}

drugs <- map(drugs, renameCols)

map(drugs, function(x) length(unique(x$id_swisci)))




# Bring dataset from wide to long format ----------------------------------


drugsToLong <- function(x, time_point) {
  
  my_drugs <- x %>% 
    
    gather(key, val, -id_swisci) %>% 
    
    mutate(key = str_remove_all(key, time_point)) %>% 
    mutate(key = str_remove_all(key, "\\.")) %>% 
    mutate(key = str_remove_all(key, "[[:digit:]]")) %>%
    mutate(key = str_remove_all(key, "_$")) %>% 
    mutate(key = str_remove_all(key, "^_")) %>% 
    
    group_by(key) %>%  
    
    mutate(unique_identifier = row_number()) %>% 
    
    spread(key, val) %>%
    
    ungroup() %>% 
    
    select(-unique_identifier) %>%
    
    mutate_if(is_character, list(~na_if(., ""))) %>% 
    
    select(id_swisci, drug, dose, drug_prn, dose_prn) %>%
    
    mutate(dose = ifelse(is.na(drug), NA_real_, dose)) %>% 
    
    mutate(dose_prn = ifelse(is.na(drug_prn), NA_real_, dose_prn)) %>% 
    
    distinct(.keep_all = TRUE)
  
    single_records <- table(my_drugs$id_swisci)[table(my_drugs$id_swisci) == 1] %>% names()
  multiple_records <- table(my_drugs$id_swisci)[table(my_drugs$id_swisci) > 1] %>% names()
  
  drugs_single_records <- my_drugs %>% 
    filter(id_swisci %in% single_records)
  
  drugs_multiple_records <- my_drugs %>% 
    filter(id_swisci %in% multiple_records) %>% 
    filter(!(is.na(drug) & is.na(drug_prn)))
  
  my_drugs <- bind_rows(drugs_single_records, drugs_multiple_records) %>% 
    arrange(id_swisci)
  
  return(my_drugs)
  
}



drugs[["ppi1_"]] <- drugsToLong(x = drugs[["ppi1_"]], time_point = "ppi1_")
drugs[["ppi3_"]] <- drugsToLong(x = drugs[["ppi3_"]], time_point = "ppi3_")
drugs[["ppi4_"]] <- drugsToLong(x = drugs[["ppi4_"]], time_point = "ppi4_")

map(drugs, function(x) length(unique(x$id_swisci)))



# removing white spaces at the end

removeWS <- function(x) {
  
  x %>% 
    
    mutate(
      
      drug = str_trim(drug),
      drug = str_to_lower(drug),
      drug_prn = str_trim(drug_prn),
      drug_prn = str_to_lower(drug_prn)
      
    )
  
}

drugs <- map(drugs, removeWS)


# Find names of all drugs

findAllDrugNames <- function(x) {
  
  x %>% 
    select(drug, drug_prn) %>% 
    unlist(use.names = FALSE) %>% 
    .[complete.cases(.)] %>% 
    unique() %>% 
    sort()
  
}

if(F) map(drugs, findAllDrugNames)


# Save dataset and clear workspace ----------------------------------------

save(drugs, file = file.path("workspace", "drugs_clean.RData"))

rm("drugs", "drugsToLong", "findAllDrugNames", "get_data", "removeWS", "renameCols", "time_points")

