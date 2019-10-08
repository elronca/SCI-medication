
library(tidyverse)
library(rvest)


load(file = file.path("workspace", "ppi1_clean.RData"))

drugs <- ppi1 %>% 
  select(id_swisci, drug, drug_prn) %>% 
  split(.$id_swisci) %>% 
  map(unlist) %>%
  map(~ .[str_detect(names(.), "drug")]) %>% 
  map(~ .[!is.na(.)]) %>% 
  map(unname) %>% 
  map(unique)

all_drugs <- sapply(drugs, '[', seq(max(lengths(drugs)))) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  rownames_to_column(var = "id_swisci")

write.csv2(all_drugs, file = file.path("output", "drugs_for_interaction.csv"), row.names = FALSE)



## Test drug interactions for all patients using uptodate



interactions <- list()

interactions[[names(drugs)[1]]] <- "no_interaction"

drugs[2]

interactions[[names(drugs)[2]]] <- list("C" = "Insulin Glargine AND MetFORMIN")

interactions[[names(drugs)[3]]] <- "no_interaction"

interactions[[names(drugs)[4]]] <- "no_medication"

interactions[[names(drugs)[5]]] <- list("D" = c("Ferrous Sulfate AND Calcium Carbonate", 
                                                "Oxycodone and Naloxone AND Clorazepate",
                                                "Oxycodone and Naloxone AND Pregabalin"),
                                        "C" = c("Cholecalciferol AND Calcium Carbonate",
                                                "Clorazepate AND Pregabalin",
                                                "Pramipexole AND Clorazepate",
                                                "Pramipexole AND Oxycodon and Naloxone",
                                                "Pramipexole AND Pregabalin"))

interactions[[names(drugs)[6]]] <- "no_interaction"

interactions[[names(drugs)[7]]] <- "no_interaction"

interactions[[names(drugs)[8]]] <- "no_medication"

interactions[[names(drugs)[9]]] <- list("D" = c("Acenocoumarol AND Acetylsalicylic Acid", 
                                                "Acenocoumarol AND Diclofenac",
                                                "Acetylsalicylic Acid AND Dicofenac",
                                                "Morphine AND Pregabalin",
                                                "Morphine AND Trazodone",
                                                "Torasemide AND Diclofenac",
                                                "Trazodone AND Fluoxetine"),
                                        "C" = c("Acenocoumarol AND Fluoxetine",
                                                "Acetylsalicylic Acid AND Fluoxetine",
                                                "Fluoxetine AND Morphine",
                                                "Fluoxetine AND Pregabalin",
                                                "Morphine AND Lercanidipine",
                                                "Morphine AND Torasemide",
                                                "Pregabalin AND Trazodone",
                                                "Torasemide AND Acetylsalicylic Acid"))

drugs %>% pluck(9)

sapply(interactions, "[", 1) %>% names()
