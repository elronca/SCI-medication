
library(tidyverse)

guess_encoding(file.path("data", "med_agents_r.csv"), n_max = 10000, threshold = 0.2)

ppi1 <- read_csv2(file.path("data", "med_agents_r.csv"), locale = locale(encoding = 'ISO-8859-1')) %>% 
  select(id_swisci, starts_with("ppi1_"), -starts_with("ppi1_medication_"), -c(PPI1_1Opioid:PPI1_9Baclofen))


# Wide to long format  
                          
# Replace  "_agent_" with "_drug_" in all variable names that do not contain (!grepl) the substring "_daily_dosage_mg".

# Regular therapy: drug name and dose

cols_drug_names <- !str_detect(colnames(ppi1), "_daily_dosage_mg|_add_")
colnames(ppi1)[cols_drug_names]

cols_drug_dose <- str_detect(colnames(ppi1), "_daily_dosage_mg") & !str_detect(colnames(ppi1), "_add_")
colnames(ppi1)[cols_drug_dose]

# Pro re nata therapy: drug name and dose

cols_drug_names_add <- !str_detect(colnames(ppi1), "_daily_dosage_mg") & str_detect(colnames(ppi1), "_add_")
colnames(ppi1)[cols_drug_names_add]

cols_drug_dose_add <- str_detect(colnames(ppi1), "_daily_dosage_mg") & str_detect(colnames(ppi1), "_add_")
colnames(ppi1)[cols_drug_dose_add]


## Rename columns

# Regular therapy: drug name and dose

colnames(ppi1)[cols_drug_names] <- str_replace_all(colnames(ppi1)[cols_drug_names], pattern = "_agent_", replacement = "_drug_")

colnames(ppi1)[cols_drug_dose] <- str_replace_all(colnames(ppi1)[cols_drug_dose], c("_agent_" = "_", "daily_dosage_mg" = "dose"))

# Pro re nata therapy: drug name and dose

colnames(ppi1)[cols_drug_names_add] <- str_replace_all(colnames(ppi1)[cols_drug_names_add], "_agent_", "_drug_")

colnames(ppi1)[cols_drug_dose_add] <- str_replace_all(colnames(ppi1)[cols_drug_dose_add], c("_agent_" = "_", "_add_" = "_", "daily_dosage_mg" = "dose_add"))

colnames(ppi1) <- str_replace_all(colnames(ppi1), "pp1", "ppi1")

rm("cols_drug_dose", "cols_drug_dose_add", "cols_drug_names", "cols_drug_names_add")


# wide to long format

ppi1_long <- ppi1 %>% 
  
  gather(key, val, -id_swisci) %>% 
  mutate(key = str_replace_all(key, c(
    "ppi1_" = "", 
    "\\." = "", 
    "[[:digit:]]" = "", 
    "^_" = "", 
    "_$" = ""))) %>% 
  
  group_by(key) %>%  
  
  mutate(row = row_number()) %>% 
  
  spread(key, val) %>%
  
  select(id_swisci, drug, dose, drug_add, dose_add, -row) %>% 
  
  filter(!is.na(drug) | !is.na(drug_add))

ppi1 <- ppi1_long; rm(ppi1_long)


# removing white spaces at the end

ppi1 <- ppi1 %>% 
  
  mutate(
    
    drug = str_trim(drug),
    drug = tolower(drug),
    drug_add = str_trim(drug_add),
    drug_add = tolower(drug_add)
    
  )

# Find names of all drugs


ppi1 %>% 
  select(drug, drug_add) %>% 
  unlist(use.names = FALSE) %>% 
  .[complete.cases(.)] %>% 
  unique() %>% 
  sort()



# save dataset

save(ppi1, file = file.path("workspace", "ppi1_clean.RData"))


# Clear workspace

rm(ppi1)
