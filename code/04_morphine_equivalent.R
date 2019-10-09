
################################################
#                                              #
#  Estimating oral morphine equivalents (OME)  #
#                                              #
################################################

library(tidyverse)
library(pdftools)
library(extrafont)
library(RColorBrewer)

windowsFonts(Arial = windowsFont("Arial"))

load(file.path("workspace", "names_harmonized.RData"))

ppi1 <- drugs[["ppi1_"]]



# Correct non-numeric characters in dose columns ------------------------------------------------


# [^abe] -> Anything but

find_non_numerals <- function(x) str_detect(x, "[^[:digit:].]")


# Regular therapy

condition_dose <- (find_non_numerals(ppi1$dose) | ppi1$dose == ".") & !is.na(ppi1$dose)

ppi1[condition_dose, ]$dose <- c("101", "1.20E-02", NA_character_, 
                                 "11", NA_character_, "4", 
                                 NA_character_, NA_character_, "50",
                                 NA_character_)

ppi1$dose <- as.numeric(ppi1$dose)

# Pro re nata

condition_dose_prn <- (find_non_numerals(ppi1$dose_prn) | ppi1$dose_prn == ".") & !is.na(ppi1$dose_prn)

ppi1[condition_dose_prn, ]$dose_prn <- c("37.5", "2.5")

ppi1$dose_prn <- as.numeric(ppi1$dose_prn)

rm("condition_dose", "condition_dose_prn", "find_non_numerals")



# Get opioid equivalent conversion factors --------------------------------


# Downlaod file and load pdf

if(FALSE) {
  
  file_url <- "https://ndarc.med.unsw.edu.au/sites/default/files/ndarc/resources/TR.329.pdf"
  
  download.file(file_url, file.path("data", "Sidney.pdf"), mode = "wb")
  
}

OME_table_pdf <- file.path("data", "Sidney.pdf") %>% pdf_text()


# Get title of table with opioid equivalent conversion factors

title <- OME_table_pdf %>% 
  pluck(5) %>% 
  str_split("\r\n") %>% 
  pluck(1, 1)


# Considerations for research from the authors

file_conn <- file(file.path("output", "OME_research_notes.txt"))

publication_notes <- OME_table_pdf %>% 
  pluck(7) %>% 
  str_remove_all("\uf0b7")

writeLines(publication_notes, file_conn)

close(file_conn)


# Get table content of pdf

OME_table <- OME_table_pdf %>% 
  pluck(5) %>% 
  str_split("\r\n") %>% 
  pluck(1) %>% 
  str_split(" ") %>% 
  .[c(7:12, 14, 16, 18:20, 22, 24)] %>% 
  map(~ .[. != ""]) %>% 
  map(~ { c(head(., 2), tail(., 1)) }) %>% 
  unlist() %>%
  matrix(ncol = 3, byrow = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  set_names(c("opioid", "dose_unit", "conv_factor")) %>% 
  mutate(conv_factor = as.numeric(conv_factor)) %>% 
  filter(!(opioid %in% c("buprenorphine", "fentanyl") & str_detect(dose_unit, "mcg/day|mg/day")))



# Calculate oral morphine equivalents for each opioid taken in the study ------------------------

calcMoEqDose <- function(drug_var, dose_var, conv_table) {
  
  
  getConvFactor <- function(x) {
    
    opioid_conv_f <- filter(conv_table, opioid == x) %>% pull(conv_factor)
    
    if (length(opioid_conv_f) == 1) { 
      
      return(opioid_conv_f)
      
    } else if (length(opioid_conv_f) == 0) {
      
      warning("conversion factor for ", x, " is not available.") 
      
      return(NA_real_)
      
    } else {
      
      warning("There are multiple conversion factors for ", x, ". Please specify. Returning NA.")
      
      return(NA_real_)
      
      
    }
    
  }
  
  case_when(
    
    drug_var == "oxycodone" ~ dose_var * getConvFactor("oxycodone"),
    drug_var == "morphine" ~ dose_var * getConvFactor("morphine"),
    drug_var == "methadone" ~ dose_var * getConvFactor("methadone"),
    drug_var == "buprenorphine" ~ dose_var * getConvFactor("buprenorphine"),
    drug_var == "codeine" ~ dose_var * getConvFactor("codeine"),
    drug_var == "fentanyl" ~ dose_var * getConvFactor("fentanyl"),
    drug_var == "tramadol" ~ dose_var * getConvFactor("tramadol"),
    drug_var == "hydromorphone" ~ dose_var * getConvFactor("hydromorphone"),
    drug_var == "tapentadol" ~ dose_var * getConvFactor("tapentadol"),
    
    TRUE ~ NA_real_
    
  )
  
}


ppi1 <- ppi1 %>% 
  
  mutate(
    
    mo_eq_dose_rt = calcMoEqDose(drug, dose, OME_table),
    mo_eq_dose_prn = calcMoEqDose(drug_prn, dose_prn, OME_table)
    
  ) %>% 
  
  mutate_at(vars(mo_eq_dose_rt, mo_eq_dose_prn), ~ round(., 1))


# Calculate average doses -------------------------------------------------

#  By opioid

to_plot <- ppi1 %>% 
  filter(drug %in% (unique(OME_table$opioid))) %>% 
  mutate_at(vars(drug, drug_prn), as.factor) %>% 
  group_by(drug) %>% 
  mutate(median_dose = median(mo_eq_dose_rt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(drug = str_to_sentence(drug)) %>% 
  mutate(drug = as.factor(drug)) %>% 
  mutate(drug = fct_reorder(drug, desc(median_dose))) %>% 
  mutate(group = if_else(!is.na(drug), "Dose of each person", NA_character_))


ggplot(to_plot) +
  
  geom_boxplot(aes(x = drug, y = mo_eq_dose_rt), outlier.shape = NA) +
  
  geom_jitter(aes(x = drug, y = mo_eq_dose_rt, colour = drug), 
              width = 0.2, alpha = 0.5, size = 2) +
  
  geom_hline(yintercept = 50, linetype = "dashed", color = "orange", size = 1, alpha = 0.5) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red", size = 1, alpha = 0.5) +
  
  coord_flip() +
  
  labs(x = "Opioid", y = "Daily opioid dose in morphin equivalents") +
  
  theme_classic() +
  
  theme(
    legend.position = "none",
    text = element_text(family = "Arial", size = 18)
)

ggsave(file.path("output", "boxplot_mo_eq.pdf"), device = cairo_pdf, width = 22, height = 15, units = "cm")
ggsave(file.path("output", "boxplot_mo_eq.svg"), device = "svg", width = 22, height = 15, units = "cm")

sum(is.na(to_plot$mo_eq_dose_rt))



# Save file and clear workspace -------------------------------------------

save(ppi1, file = file.path("workspace", "OME_added.RData"))