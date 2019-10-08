# Create table with dosages

library(tidyverse)

load(file.path("workspace", "OME_added.RData"))



# Get the total number of study participants ------------------------------

tot_patients <- ppi1 %>% distinct(id_swisci) %>% nrow()


# Calculate the number of drugs taken in regular therapy ------------------

drugs_rt_count <- ppi1 %>% 
  group_by(id_swisci) %>% 
  distinct(drug, .keep_all = TRUE) %>% 
  ungroup()

n_obs_pp <- count(drugs_rt_count, id_swisci, name = "n_obs_pp")

drugs_rt_count <- drugs_rt_count %>% 
  left_join(n_obs_pp, by = "id_swisci") %>% 
  mutate(drug = if_else(n_obs_pp == 1 & is.na(drug), "no_pain_medication", drug)) %>% 
  count(drug, name = "reg_therapy_n") %>% 
  mutate("reg_therapy_rf" = round(100 * (reg_therapy_n / tot_patients), 0)) %>% 
  drop_na(drug)

rm(n_obs_pp)


# Calculate the number of drugs taken prn ---------------------------------

drugs_prn_count <- ppi1 %>% 
  group_by(id_swisci) %>% 
  distinct(drug_prn, .keep_all = TRUE) %>% 
  ungroup()

n_obs_pp <- count(drugs_prn_count, id_swisci, name = "n_obs_pp")

drugs_prn_count <- drugs_prn_count %>% 
  left_join(n_obs_pp, by = "id_swisci") %>% 
  mutate(drug_prn = if_else(n_obs_pp == 1 & is.na(drug_prn), "no_pain_medication", drug_prn)) %>% 
  count(drug_prn, name = "pro_re_nata_n") %>% 
  mutate(pro_re_nata_rf = round(100 * (pro_re_nata_n / tot_patients), 0)) %>% 
  drop_na(drug_prn)

rm(n_obs_pp)


# Calculate the number of drugs taken in total-------------------------------

drugs_total_count <- ppi1 %>%
  gather(key = "medication_type", value = "drug", -id_swisci, -dose, -dose_prn, -mo_eq_dose_rt, -mo_eq_dose_prn) %>% 
  select(id_swisci, medication_type, drug, dose, dose_prn, mo_eq_dose_rt, mo_eq_dose_prn) %>% 
  group_by(id_swisci) %>% 
  distinct(drug, .keep_all = TRUE) %>% 
  ungroup()

n_obs_pp <- count(drugs_total_count, id_swisci, name = "n_obs_pp")

drugs_total_count <- drugs_total_count %>% 
  left_join(n_obs_pp, by = "id_swisci") %>% 
  mutate(drug = if_else(n_obs_pp == 1 & is.na(drug), "no_pain_medication", drug)) %>% 
  count(drug, name = "total_n") %>% 
  mutate(total_rf = round(100 * (total_n / tot_patients), 0)) %>% 
  drop_na(drug)

rm(n_obs_pp)



# Create table of drugs taken in total ------------------------------------

drug_count <- drugs_total_count %>% 
  full_join(drugs_rt_count, by = "drug") %>% 
  full_join(drugs_prn_count, by = c("drug" = "drug_prn"))


# Add mean daily drug doses in regular therapy ----------------------------


roundFC <- function(x, d) {formatC(x, digits = d, format = "f")}



mean_dd_RT <- ppi1 %>% 
  
  group_by(id_swisci, drug) %>% 
  mutate(dose = if_else(drug %in% c("fentanyl", "buprenorphine"), (dose * 24) / 1000, dose)) %>%  
  summarize(sum_dd_pp_RT = sum(dose, na.rm = TRUE)) %>% 
  drop_na(drug) %>% 
  filter(sum_dd_pp_RT != 0) %>% 
  ungroup() %>% 
  group_by(drug) %>% 
  summarize(
    mean_dd_RT = mean(sum_dd_pp_RT), 
    sd_dd_RT = sd(sum_dd_pp_RT)) %>% 
  mutate(mean_daily_dose_RT = str_c(roundFC(mean_dd_RT, 1), " (", roundFC(sd_dd_RT, 1), ")")) %>% 
  select(-mean_dd_RT, -sd_dd_RT)

drug_info <- left_join(drug_count, mean_dd_RT, by = "drug")



# Add information about drug class ----------------------------------------

anticonvulsants <- c("pregabalin", "gabapentin", "carbamazepine", "lamotrigine", "valproate")
other_analgesics <- c("paracetamol", "metamizole")
opioids <- c("buprenorphine", "codeine", "fentanyl", "hydromorphone", "methadone", "morphine", "oxycodone", "pethidine", "tapentadol", "tramadol")
nsaid <- c("acetylsalicylic acid", "celecoxib", "diclofenac", "etodolac", "etoricoxib", "ibuprofen", "mefenamic acid", "naproxen")
ssri <- c("citalopram", "escitalopram", "paroxetine", "fluoxetine")
snri <- c("duloxetine", "venlafaxine")
sari <- "trazodone"
tca <- c("amitriptyline", "melitracen", "nortriptyline", "opipramol", "trimipramine")
teca <- c("mirtazapine", "mianserin")
muscle_relaxants <- c("baclofen", "dantrolene", "tizanidine")
benzodiazepines <- c("alprazolam", "bromazepam", "clonazepam", "clorazepate", "diazepam", "flurazepam", "lorazepam", "midazolam")
cannabinoids <- c("cannabidiol", "tetrahydrocannabinol")
others <- c("clonidine", "ketamine", "naloxone", "pramipexole", "quetiapine",  "ziconotide", "zolpidem")
no_pain_medication <- c("no_pain_medication")


classify_drug <- function(drug_var) {
  
  case_when(
    
    drug_var %in% anticonvulsants ~ "anticonvulsants",
    drug_var %in% other_analgesics ~ "other_analgesics",
    drug_var %in% opioids ~ "opioids",
    drug_var %in% nsaid ~ "nsaid",
    drug_var %in% ssri ~ "ssri",
    drug_var %in% snri ~ "snri",
    drug_var %in% tca ~ "tca",
    drug_var %in% teca ~ "teca",
    drug_var %in% sari ~ "sari",
    drug_var %in% muscle_relaxants ~ "muscle_relaxants",
    drug_var %in% benzodiazepines ~ "benzodiazepines",
    drug_var %in% cannabinoids ~ "cannabinoids",
    drug_var %in% others ~ "others",
    drug_var %in% no_pain_medication ~ "no_pain_medication",
    TRUE ~ NA_character_)
  
}

drug_info <- mutate(drug_info, drug_class = classify_drug(drug)) %>% arrange(drug_class)

rm(drug_count, drugs_total_count, drugs_rt_count, drugs_prn_count)



# Calculate the total number of drug classes "taken" -------------------------------


# regular therapy

drug_class_rt <- ppi1 %>% 
  mutate(drug_class_rt = classify_drug(drug)) %>% 
  select(id_swisci, drug_class_rt) %>% 
  group_by(id_swisci) %>% 
  distinct(drug_class_rt, .keep_all = TRUE)

drug_class_rt_count <- drug_class_rt %>%
  ungroup() %>% 
  count(drug_class_rt, name = "reg_therapy_n") %>% 
  drop_na(drug_class_rt) %>% 
  mutate(reg_therapy_rf = round(100 * (reg_therapy_n / tot_patients), 0))


# pro re nata

drug_class_prn <- ppi1 %>% 
  mutate(drug_class_prn = classify_drug(drug_prn)) %>% 
  select(id_swisci, drug_class_prn) %>% 
  group_by(id_swisci) %>% 
  distinct(drug_class_prn, .keep_all = TRUE)

drug_class_prn_count <- drug_class_prn %>%
  ungroup() %>% 
  count(drug_class_prn, name = "pro_re_nata_n") %>% 
  drop_na(drug_class_prn) %>% 
  mutate(pro_re_nata_rf = round(100 * (pro_re_nata_n / tot_patients), 0))


# all 

drug_class_tot <- bind_rows(
  
  rename(drug_class_rt, drug_class = drug_class_rt), 
  rename(drug_class_prn, drug_class = drug_class_prn)
  
  ) %>% 
  
  group_by(id_swisci) %>% 
  
  distinct(drug_class, .keep_all = TRUE) %>% 
  
  ungroup() %>% 
  
  count(drug_class, name = "total_n") %>% 
  
  drop_na(drug_class) %>% 
  
  mutate(total_rf = round(100 * (total_n / tot_patients), 0))


drug_class_count <- drug_class_tot %>% 
  full_join(drug_class_rt_count, by = c("drug_class" = "drug_class_rt")) %>% 
  full_join(drug_class_prn_count, by = c("drug_class" = "drug_class_prn"))

rm(drug_class_prn, drug_class_prn_count, drug_class_rt, drug_class_rt_count, 
   drug_class_tot)


# Construct drug frequency table ---------------------------------------

drug_class_count <- mutate(drug_class_count, drug = "", mean_daily_dose_RT = "")

drug_table <- bind_rows(drug_info, drug_class_count) %>% 
  select(drug_class, everything())


# Create table with frequencies of drugs taken ----------------------------

drug_table <- drug_table %>% 
  
  mutate(drug_class = as.factor(drug_class)) %>% 
  mutate(drug_class = fct_relevel(drug_class, c("no_pain_medication",
                                                "nsaid", 
                                                "opioids", 
                                                "other_analgesics", 
                                                "anticonvulsants", 
                                                "muscle_relaxants", 
                                                "benzodiazepines",
                                                "tca", "ssri", "snri", "sari", "teca", 
                                                "cannabinoids", "others"))) %>% 

  arrange(drug_class, desc(total_n)) %>% 

  mutate(total = str_c(total_n, " (", total_rf, ")")) %>% 
  mutate(reg_therapy = str_c(reg_therapy_n, " (", reg_therapy_rf, ")")) %>% 
  mutate(pro_re_nata = str_c(pro_re_nata_n, " (", pro_re_nata_rf, ")")) %>% 
  
  select(drug_class, drug, total, reg_therapy, pro_re_nata, mean_daily_dose_RT)


# Pimp table --------------------------------------------------------------

drug_table$drug_class <- as.character(drug_table$drug_class)
drug_table$drug_class[duplicated(drug_table$drug_class)] <- ""
drug_table[is.na(drug_table$mean_daily_dose_RT), ]$mean_daily_dose_RT <- ""


# Add morphin equivalent --------------------------------------------------

mo_eq_avg <- ppi1 %>% 
  
  group_by(id_swisci) %>% 
  
  summarize(mo_eq_pp = sum(mo_eq_dose_rt, na.rm = TRUE)) %>% 
  
  ungroup() %>% 
  
  filter(mo_eq_pp != 0) %>% 
  
  summarize(mo_eq_avg = mean(mo_eq_pp, na.rm = TRUE), 
            mo_eq_sd = sd(mo_eq_pp, na.rm = TRUE)) %>% 
  
  mutate(
    
    daily_mo_eq_dose = str_c(
      
      roundFC(mo_eq_avg, 1),
            " (", 
            roundFC(mo_eq_sd, 1), 
      ")"
    )
    
  ) %>% 
  
  pull()

drug_table[drug_table$drug_class == "opioids", "mean_daily_dose_RT"] <- mo_eq_avg

write.csv2(drug_table, file.path("output", "drug_table.csv"), row.names = FALSE)


# Clear workspace ---------------------------------------------------------

rm("anticonvulsants", "benzodiazepines", "cannabinoids", "classify_drug", 
  "drug_class_count", "drug_info", "drug_table", "mean_dd_RT", 
  "mo_eq_avg", "muscle_relaxants", "no_pain_medication", "nsaid", 
  "opioids", "other_analgesics", "others", "ppi1", "roundFC", "sari", 
  "snri", "ssri", "tca", "teca", "tot_patients")

