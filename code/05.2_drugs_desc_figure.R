
# Create figure of drugs intake frequency

library(tidyverse)
library(wesanderson)
library(extrafont)

load(file.path("workspace", "OME_added.RData"))


# Create no medication variable ------------------------------------------------

ppi1 <- ppi1 %>% 
  group_by(id_swisci) %>% 
  distinct(drug, drug_prn, .keep_all = TRUE) %>% 
  ungroup()

n_obs_pp <- count(ppi1, id_swisci, name = "n_obs_pp")

ppi1 <- ppi1 %>% 
  left_join(n_obs_pp, by = "id_swisci") %>% 
  mutate(drug = if_else(n_obs_pp == 1 & is.na(drug) & is.na(drug_prn), "No_pain_medication", drug)) %>% 
  select(id_swisci, drug, drug_prn) %>% 
  filter(!(is.na(drug) & is.na(drug_prn)))

rm(n_obs_pp)


# Classify agents, those are the variables that will displayed in the final plot in the end ----------------------------------------

drug_classes_list <- list(
  
  Anticonvulsants = c("pregabalin", "gabapentin"), # "carbamazepine", "lamotrigine", "valproate"
  Paracetamol = "paracetamol",
  Metamizole = "metamizole", 
  Opioids = c("buprenorphine", "codeine", "fentanyl", "hydromorphone", "methadone", "morphine", "oxycodone", "pethidine", "tapentadol", "tramadol"),
  NSAID = c("acetylsalicylic acid", "celecoxib", "diclofenac", "etodolac", "etoricoxib", "ibuprofen", "mefenamic acid", "naproxen"),
  SSRI = c("citalopram", "escitalopram", "paroxetine", "fluoxetine"),
  SNRI = c("duloxetine", "venlafaxine"),
  SARI = c("trazodone"),
  TCA = c("amitriptyline", "melitracen", "nortriptyline", "opipramol", "trimipramine"),
  TECA = c("mirtazapine", "mianserin"),
  Muscle_relaxants = c("baclofen", "dantrolene", "tizanidine"),
  Benzodiazepines = c("alprazolam", "bromazepam", "clonazepam", "clorazepate", "diazepam", "flurazepam", "lorazepam", "midazolam"),
  Cannabinoids = c("cannabidiol", "tetrahydrocannabinol"),
  # others = c("clonidine", "ketamine", "naloxone", "pramipexole", "quetiapine",  "ziconotide", "zolpidem"),
  No_pain_medication = "No_pain_medication"
  
)

ad <- list(Antidepressants = unlist(drug_classes_list[c("SSRI", "SNRI", "SARI", "TCA", "TECA")], use.names = FALSE))

drug_classes_list <- c(drug_classes_list, ad)

names(drug_classes_list)


# Classify drug taken into drug classes -----------------------------------


# Every drug class will get a 1 if a drug of a specific drug class is taken at least once

classify_drug <- function(drug_cl_nms, drug_var, data_set, var_suffix) {
  
  mutate(data_set, !!drug_cl_nms := if_else(!!sym(drug_var) %in% drug_classes_list[[drug_cl_nms]], 1L, 0L)) %>% 
    
    select(id_swisci, drug_cl_nms) %>%
    
    rename_if(is.integer, list( ~str_c(., var_suffix, sep = "_")))
  
}


# Regular therapy

rt <- map(.x = names(drug_classes_list), .f = classify_drug, data_set = ppi1, drug_var = "drug", var_suffix = "rt") %>% 
  
  bind_cols() %>% 
  
  select(-num_range("id_swisci", seq_len(length(drug_classes_list) - 1))) %>% 
  
  group_by(id_swisci) %>% 
  
  summarize_all(sum) %>% 
  
  ungroup()



# Pro re nata

prn <- map(.x = names(drug_classes_list), .f = classify_drug, data_set = ppi1, drug_var = "drug_prn", var_suffix = "prn") %>% 
  
  bind_cols() %>% 
  
  select(-num_range("id_swisci", seq_len(length(drug_classes_list) - 1))) %>% 
  
  group_by(id_swisci) %>% 
  
  summarize_all(sum) %>% 
  
  ungroup()


# Make a nice data frame

drug_freq <- left_join(rt, prn, by = "id_swisci") %>% 
  
  pivot_longer(-id_swisci, names_to = "drug_class", values_to = "intake") %>% 
  
  mutate(intake_type = if_else(str_detect(drug_class, "_rt"), "rt", "prn")) %>% 
  
  mutate(drug_class = str_remove_all(drug_class, c("_rt|_prn"))) %>% 
  
  pivot_wider(names_from = intake_type, values_from = intake) %>% 
  
  mutate(rt_prn = rt + prn,
         rt_prn = if_else(rt_prn == 2L, 1L, 0L)) %>% 
  
  mutate(rt = if_else(rt_prn == 1L, 0L, rt),
         prn = if_else(rt_prn == 1L, 0L, prn)) %>% 
  
  pivot_longer(cols = c(prn, rt, rt_prn), names_to = "intake_type", values_to = "intake") %>% 
  
  mutate(intake_type = if_else(drug_class == "No_pain_medication", "No_pain_medication", intake_type)) %>% 
  
  mutate(intake_type = as.factor(intake_type),
         intake_type = fct_relevel(intake_type, c("prn", "rt_prn", "rt", "No_pain_medication")))


# Do some cosmetics for plotting ------------------------------------------


# Calculate proportions of drug intake

drug_freq_prop <- drug_freq %>% 
  group_by(drug_class, intake_type) %>% 
  summarize(n_per_group = sum(intake),
            prop = n_per_group / (length(unique(drug_freq$id_swisci)))) %>% 
  
  ungroup()


# Remove drug categories I don't want to plot

drug_freq_prop <- drug_freq_prop %>% 
  filter(!drug_class %in% c("SSRI", "SNRI", "SARI", "TCA", "TECA"))


# Rename and reorder levels of drug class variable

drug_freq_prop <- drug_freq_prop %>% 
  mutate(drug_class = as.factor(drug_class)) %>% 
  mutate(drug_class = fct_relabel(drug_class, ~str_replace_all(., "_", " "))) %>% 
  
  group_by(drug_class) %>% 
  mutate(tot_prop = sum(prop)) %>% 
  ungroup() %>% 
  mutate(drug_class = fct_reorder(drug_class, tot_prop))


# Rename categories of drug intake

drug_freq_prop <- drug_freq_prop %>%

  mutate(intake_type = fct_recode(intake_type,
                                  "Pro re nata" = "prn",
                                  "Regular therapy &\npro re nata" = "rt_prn",
                                  "Regular therapy" = "rt",
                                  "No pain medication" = "No_pain_medication"))

# Produce a barplot -------------------------------------------------------


# Wes Anderson colors

# https://wesandersonpalettes.tumblr.com/

if(F) map(names(wes_palettes), function(x) wes_palette(n = 4, name = x))


p <- ggplot(data = drug_freq_prop, mapping = aes(y = prop, x = drug_class, fill = intake_type)) + 
  
  geom_bar(stat = "identity") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  scale_fill_manual(breaks = c("Pro re nata", "Regular therapy &\npro re nata", "Regular therapy"), 
                    
                    values = wes_palettes[["Chevalier1"]][c(4, 2, 1, 3)], 
                    
                    name = "Type of intake") +
  
  labs(x = "Drug (class)", y = "Relative frequency of intake") +
  
  coord_flip() +
  
  theme_classic() +
  
  theme(legend.position = c(0.8, 0.3)) +
  
  theme(text = element_text(family = "Arial", size = 18))


ggsave(file.path("output", "boxplot_drug_freq.pdf"), device = cairo_pdf, width = 20, height = 12, units = "cm")
ggsave(file.path("output", "boxplot_drug_freq.emf"), device = "emf", width = 20, height = 12, units = "cm")



# Clear workspace ---------------------------------------------------------

rm("ad", "classify_drug", "drug_classes_list", "drug_freq", "drug_freq_prop", "p", "ppi1", "prn", "rt")
