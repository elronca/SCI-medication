############################################################################
##
##  Pain-related drugs in persons with SCI suffering from chronic pain 
##
############################################################################

load(file = file.path("workspace", "drugs_clean.RData"))



# Clean agent names -------------------------------------------------------


if(F) table(ppi1$drug)

drugs[[3]] %>% filter(drug == "buprenorphin")

map(drugs, function(x) length(unique(x$id_swisci)))



harmonizeDrugNames <- function(x) {
  
  x %>% 
    
    mutate_at(
      
      vars(matches("drug", "drug_prn")), ~ case_when(
        
        str_detect(., "acetylsalicyl") ~ "acetylsalicylic acid",
        str_detect(., "valp") ~ "valproate",
        str_detect(., "alpraz") ~ "alprazolam",
        str_detect(., "amitripty") ~ "amitriptyline",
        str_detect(., "baclo") ~ "baclofen",
        str_detect(., "bromazepam") ~ "bromazepam",
        str_detect(., "bupren") ~ "buprenorphine",
        str_detect(., "cannabidiol") ~ "cannabidiol",
        str_detect(., "tetrahydrocannabinol") ~ "tetrahydrocannabinol",
        str_detect(., "carbamaze") ~ "carbamazepine",
        str_detect(., "celecox") ~ "celecoxib",
        str_detect(., "etoricox") ~ "etoricoxib",
        str_detect(., "^citalopram") ~ "citalopram",
        str_detect(., "escitalopram") ~ "escitalopram",
        str_detect(., "clonaz") ~ "clonazepam",
        str_detect(., "clonid") ~ "clonidine",
        str_detect(., "code") ~ "codeine",
        str_detect(., "dantro") ~ "dantrolene",
        str_detect(., "diaz") ~ "diazepam",
        str_detect(., "diclofenac") ~ "diclofenac",
        str_detect(., "diclofenac_topical") ~ "diclofenac_topical",
        str_detect(., "clora") ~ "clorazepate",
        str_detect(., "dulox") ~ "duloxetine",
        str_detect(., "etodol") ~ "etodolac",
        str_detect(., "etoricoxib") ~ "etoricoxib",
        str_detect(., "fenta") ~ "fentanyl",
        str_detect(., "fluo") ~ "fluoxetine",
        str_detect(., "fluraz") ~ "flurazepam",
        str_detect(., "gabap") ~ "gabapentin",
        str_detect(., "hydromorph") ~ "hydromorphone",
        str_detect(., "ibup") ~ "ibuprofen",
        str_detect(., "keta") ~ "ketamine",
        str_detect(., "lamo") ~ "lamotrigine",
        str_detect(., "levetir") ~ "levetiracetam",
        str_detect(., "lorazepam") ~ "lorazepam",
        str_detect(., "mefen") ~ "mefenamic acid",
        str_detect(., "melitr") ~ "melitracen",
        str_detect(., "metam") ~ "metamizole",
        str_detect(., "metha") ~ "methadone",
        str_detect(., "mian") ~ "mianserin",
        str_detect(., "mida") ~ "midazolam",
        str_detect(., "zapin") ~ "mirtazapine",
        str_detect(., "^morph") ~ "morphine",
        str_detect(., "naloxo") ~ "naloxone",
        str_detect(., "naprox") ~ "naproxen",
        str_detect(., "nortripty") ~ "nortriptyline",
        str_detect(., "opipram") ~ "opipramol",
        str_detect(., "^oxycod") ~ "oxycodone",
        str_detect(., "paracetamol") ~ "paracetamol",
        str_detect(., "parox") ~ "paroxetine",
        str_detect(., "pethi") ~ "pethidine",
        str_detect(., "pramipex") ~ "pramipexole",
        str_detect(., "prega") ~ "pregabalin",
        str_detect(., "queti") ~ "quetiapine",
        str_detect(., "tapen") ~ "tapentadol",
        str_detect(., "tizani") ~ "tizanidine",
        str_detect(., "trama") ~ "tramadol",
        str_detect(., "trazo") ~ "trazodone",
        str_detect(., "trimi") ~ "trimipramine",
        str_detect(., "venla") ~ "venlafaxine",
        str_detect(., "zicon") ~ "ziconotide",
        str_detect(., "zolpi") ~ "zolpidem",
        
        TRUE ~ as.character(.)
        
      ))
  
}

drugs <- map(drugs, harmonizeDrugNames)

save(drugs, file = file.path("workspace", "all_drugs.RData"))


# Remove all non-painkillers ----------------------------------------------

agents <- c("acetylsalicylic acid", "valproate", "alprazolam", "amitriptyline", 
            "baclofen", "bromazepam", "buprenorphine", "cannabidiol", "tetrahydrocannabinol", 
            "carbamazepine", "celecoxib", "etoricoxib", "citalopram", "escitalopram", 
            "clonazepam", "clonidine", "codeine", "dantrolene", "diazepam", 
            "diclofenac", "clorazepate", "duloxetine", "etodolac", "fentanyl", 
            "fluoxetine", "flurazepam", "gabapentin", "hydromorphone", "ibuprofen", 
            "ketamine", "lamotrigine", "lorazepam", "mefenamic acid", 
            "melitracen", "metamizole", "methadone", "mianserin", "midazolam", 
            "mirtazapine", "morphine", "naloxone", "naproxen", "nortriptyline", 
            "opipramol", "oxycodone", "paracetamol", "paroxetine", "pethidine", 
            "pramipexole", "pregabalin", "quetiapine", "tapentadol", "tizanidine", 
            "tramadol", "trazodone", "trimipramine", "venlafaxine", "ziconotide", 
            "zolpidem")

removeNonPainKillers <- function(x) {
  
  x %>% 
    mutate_at(vars(drug, drug_prn), ~if_else(. %in% agents, ., NA_character_)) %>% 
    arrange(id_swisci)
  
}

drugs <- map(drugs, removeNonPainKillers)




if(F) table(drugs[[3]]$drug)


# Save data and clear workspace -------------------------------------------

save(drugs, file = file.path("workspace", "names_harmonized.RData"))
save(agents, file = file.path("workspace", "all_agents.RData"))

rm("agents", "drugs", "harmonizeDrugNames", "removeNonPainKillers")
