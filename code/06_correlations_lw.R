# ## Show which drugs/ drug classes are most often taken together

library(tidyverse)
library(cluster)
library(ggrepel)
library(corrplot)
library(plotly)

load(file.path("workspace", "OME_added.RData"))

drugs <- ppi1 %>% 
  select(id_swisci, drug, drug_prn) %>% 
  split(.$id_swisci) %>% 
  map(unlist) %>%
  map(~ .[str_detect(names(.), "drug")]) %>% 
  map(~ .[!is.na(.)]) %>% 
  map(unname)
  
drugs <- drugs[map(drugs, length) != 0]

drugClassPP <- function(x) {


Anticonvulsants <- c("pregabalin", "gabapentin") # "carbamazepine", "lamotrigine", "valproate"
Paracetamol <- "paracetamol"
Metamizole <- "metamizole"
Opioids <- c("buprenorphine", "codeine", "fentanyl", "hydromorphone", "methadone", "morphine", "oxycodone", "pethidine", "tapentadol", "tramadol")
NSAID <- c("celecoxib", "diclofenac", "etodolac", "etoricoxib", "ibuprofen", "mefenamic acid", "naproxen") # "acetylsalicylic acid", 

ssri <- c("citalopram", "escitalopram", "paroxetine", "fluoxetine")
snri <- c("duloxetine", "venlafaxine")
sari <- "trazodone"
tca <- c("amitriptyline", "melitracen", "nortriptyline", "opipramol", "trimipramine")
teca <- c("mirtazapine", "mianserin")

Antidepressants <- c(ssri, snri, sari, tca, teca)
  
Muscle_relaxants <- c("baclofen", "dantrolene", "tizanidine")
Benzodiazepines <- c("alprazolam", "bromazepam", "clonazepam", "clorazepate", "diazepam", "flurazepam", "lorazepam", "midazolam", "zolpidem")
cannabinoids <- c("cannabidiol", "tetrahydrocannabinol")
others <- c("ketamine", "ziconotide") # "naloxone", "clonidine", "pramipexole", "quetiapine",  
# no_pain_medication <- c("no_pain_medication")


dc_l <- list()

dc_l["Anticonvulsants"] <- if_else(any(x %in% Anticonvulsants), 1, 0)
dc_l["Paracetamol"] <- if_else(any(x %in% Paracetamol), 1, 0)
dc_l["Metamizole"] <- if_else(any(x %in% Metamizole), 1, 0)
dc_l["Opioids"] <- if_else(any(x %in% Opioids), 1, 0)
dc_l["NSAID"] <- if_else(any(x %in% NSAID), 1, 0)
dc_l["ssri"] <- if_else(any(x %in% ssri), 1, 0)
dc_l["snri"] <- if_else(any(x %in% snri), 1, 0)
dc_l["sari"] <- if_else(any(x %in% sari), 1, 0)
dc_l["tca"] <- if_else(any(x %in% tca), 1, 0)
dc_l["teca"] <- if_else(any(x %in% teca), 1, 0)
dc_l["Antidepressants"] <- if_else(any(x %in% Antidepressants), 1, 0)
dc_l["Muscle_relaxants"] <- if_else(any(x %in% Muscle_relaxants), 1, 0)
dc_l["Benzodiazepines"] <- if_else(any(x %in% Benzodiazepines), 1, 0)
dc_l["cannabinoids"] <- if_else(any(x %in% cannabinoids), 1, 0)
dc_l["others"] <- if_else(any(x %in% others), 1, 0)

return(dc_l)

}


drugs <- map_dfr(drugs, drugClassPP)

drugs_freq <- colSums(drugs)



# Lance and Williams (dissimilarity measures for binary data)

# https://www.ibm.com/support/knowledgecenter/en/SSLVMB_24.0.0/spss/base/cmd_cluster_measure_binary.html


#  Lance and Williams is computed from a fourfold table as (b+c)/(2a+b+c), where 

# a represents the cell corresponding to cases present on both items, 
# and b and c represent the diagonal cells corresponding to cases present on one item but absent on the other. 
# This measure has a range of 0 to 1. (Also known as the Bray-Curtis nonmetric coefficient.)


getLanceWillliams <- function(x) {
  
  taken_together <- drugs[[x[1]]] + drugs[[x[2]]]
  
  
  # If drugs from both drug classes are taken togehter we get a value of 2 and set an "a"
  
  taken_together[taken_together %in% 2] <- "a"  
  
  
  # If drugs from only one category are taken we set a "bc"
  
  taken_together[taken_together %in% 1] <- "bc"
  
  
  # We check how many times we found that drugs from both classes were taken
  
  a <- sum(taken_together %in% "a")
  
  
  # We check how many times drugs from one category were taken alone
  
  bc <- sum(taken_together %in% "bc")
  
  # we calculate the Bray-Curtis nonmetric coefficient
  bc / (2 * a + bc)
  
}


# We search for all possible combination of drug classes

drug_combos <- combn(colnames(drugs), 2, simplify = FALSE)


# We calculate the Bray-Curtis nonmetric coefficient for all combinations

drug_dist <- map_dbl(drug_combos, getLanceWillliams)


# We put those values back into a matrix using as.matrix.dist() to set up a "dist" object by hand. 

class(drug_dist) <- "dist"
attr(drug_dist, "Labels") <- colnames(drugs)
attr(drug_dist, "Size") <- length(colnames(drugs))
attr(drug_dist, "Upper") <- FALSE
attr(drug_dist, "Diag") <- FALSE

drug_dist <- as.matrix(drug_dist)



# Multidimensional Scaling

mds <- cmdscale(drug_dist, eig = TRUE, k = 2) %>% 
  .$points %>% 
  as.data.frame %>% 
  rownames_to_column(var = "drug_class") %>% 
  rename(x = V1, y = V2) %>% 
  mutate(n = drugs_freq) %>% 
  filter(!drug_class %in% c("ssri", "sari", "snri", "tca", "teca", "others", "cannabinoids"))



# 2D Plot with ggplot2 ----------------------------------------------------


p <- ggplot(mds)

## Check drug interactions using uptodate

p <- p + geom_point(aes(x = x, y = y, size = n), shape = 21, color = "black", show.legend = FALSE)

p <- p + geom_label_repel(aes(x = x, y = y, label = drug_class),
                          box.padding   = 0.35, 
                          point.padding = 0.5,
                          segment.color = 'grey50')

p <- p + theme_classic()

p <- p + labs(x = '', y = '')

p <- p + scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)

plot(p)




# 3D Plot with plotly ------------------------------------------------------

mds_3d <- cmdscale(drug_dist, eig = TRUE, k = 3) %>% 
  .$points %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "drug_class") %>% 
  rename(x = V1, y = V2, z = V3) %>% 
  mutate(n = drugs_freq) %>% 
  filter(!drug_class %in% c("ssri", "sari", "snri", "tca", "teca", "others", "cannabinoids"))


plot_ly(mds_3d, 
        x = ~x, 
        y = ~y, 
        z = ~z, 
        color = ~drug_class, 
        size = ~n, 
        mode = 'markers', 
        type = 'scatter3d', 
        marker = list(sizeref = 0.05))




# Clear workspace ---------------------------------------------------------

rm("drug_combos", "drug_dist", "drugClassPP", "drugs", "drugs_freq", 
  "getLanceWillliams", "mds", "mds_3d", "p", "ppi1")
