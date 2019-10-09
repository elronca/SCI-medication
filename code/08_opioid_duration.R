
################################################
#                                              #
#  Comparing drug intake longitudinally        #
#                                              #
################################################

library(tidyverse)
library(pBrackets)  
library(grid)
library(extrafont)
windowsFonts(Arial = windowsFont("Arial"))



# Load drugs

load(file.path("workspace", "all_drugs.RData"))

# Check whether we have data about all 3 time points from all study participants

map_int(drugs, function(x) length(unique(x$id_swisci)))

drugs_all <- bind_rows(drugs, .id = "tp") %>% 
  select(id_swisci, drug, drug_prn, tp) %>% 
  mutate(tp = str_remove(tp, "_$")) %>% 
  arrange(id_swisci, tp)


# Get "lost to follow-up" status for every study participant at any time point

# ltfu = 1 means that participant was not lost to follow up

follow_up <- read.csv2(file.path("data", "all_data.csv"), stringsAsFactors = F) %>% 
  select(id_swisci, ppi1 = Baseline_COMPLETE, ppi3 = Posttreatm_COMPLETE, ppi4 = Followup_COMPLETE) %>% 
  gather("tp", "ltfu", -id_swisci) %>% 
  mutate(id_swisci = as.character(id_swisci))


# Merge ltfu information into dataset

drugs_all <- left_join(drugs_all, follow_up, by = c("id_swisci", "tp"))



# Reduce dataset to opioids -----------------------------------------------

opioids_string <- c("oxycodone", "morphine", "methadone", "buprenorphine", 
             "codeine", "fentanyl", "tramadol", "hydromorphone", "tapentadol")

drugs_opi <- drugs_all %>% 
  mutate(opioid_rt = if_else(drug %in% opioids_string, 1, 0),
         opioid_prn = if_else(drug_prn %in% opioids_string, 1, 0))


# Find number of opioids taken per timepoint per regular therapy/pro re nata and participant

opioids <- drugs_opi %>% 
  group_by(id_swisci, tp) %>% 
  summarize(opioids_rt = sum(opioid_rt),
            opioids_prn = sum(opioid_prn)) %>% 
  ungroup()


# Merge numbers of opioids taken into original dataset
ltfu <- drugs_opi %>% 
  select(id_swisci, tp, ltfu) %>% 
  distinct()

opioids <- left_join(opioids, ltfu, by = c("id_swisci", "tp"))


# Participants who consumed an opioid at any of the timepoints

opioid_consumers <- opioids %>% 
  group_by(id_swisci) %>% 
  mutate(is_opioid = sum(opioids_rt) + sum(opioids_prn)) %>% 
  filter(is_opioid > 0) %>% 
  pull(id_swisci) %>% 
  unique()

length(opioid_consumers)

# Reduce dataset to opioid users.
# Define for every time point whether participants took no opioid or whether they took an opioid as a 
# regular therapy or prn. If somebody took an opioid as rt and prn then it was coded as rt.

opioids <- opioids %>% 
  filter(id_swisci %in% opioid_consumers) %>% 
  mutate(is_opioid = if_else(opioids_rt + opioids_prn > 0, 1, 0)) %>% 
  mutate(ther_freq = case_when(
    is_opioid == 0 ~ "no_opioid",
    opioids_rt >= 1 ~ "rt",
    opioids_rt == 0 & opioids_prn >= 1 ~ "prn",
    TRUE ~ NA_character_))


# Remove the one person who did not take an opioid at ppi1. In whole study we looked only at persons at time point 1

id_no_opioid_TP_1 <- opioids %>% 
  filter(tp == "ppi1" & is_opioid == 0) %>% 
  pull(id_swisci)

opioids <- filter(opioids, !id_swisci %in% id_no_opioid_TP_1)

save(opioids, file = file.path("workspace", "opioid_info.RData"))


# Plot opioid intake ------------------------------------------------------

# Prepare dataset

# - jitter points and lines (we need to do that because otherwise all lines and points would lie on of each other
# - "lost to follow-up is added to the points

opioids_plot <- opioids %>% 
  
  mutate(is_opioid_jit = jitter(is_opioid, 1.2)) %>% 
  
  mutate(
    
    tp = factor(tp,
                levels = c("ppi1", "ppi3", "ppi4"),
                # labels = c("t_1 (0 month)", "t_2 (2 month)", "t_3 (5 month)"))) %>% 
                labels = c("0 month", "2 month", "5 month"))) %>% 
  
  mutate(
    
    ther_freq = if_else(ltfu == 0, "ltfu", ther_freq),
    ther_freq = factor(ther_freq,
                       levels = c("ltfu", "no_opioid", "prn", "rt"),
                       labels = c("Lost to follow-up", "No opioid", "Pro re nata", "Regular therapy")))



# Make brackets plotable

bracketsGrob <- function(...) {
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

# Define location of brackets on x axis

x_axis_loc <- 0.16

b1 <- bracketsGrob(x1 = x_axis_loc, y1 = 0.55, x2 = x_axis_loc, y2 = 0.9, h = 0.04, lwd = 2, col = "black")
b2 <- bracketsGrob(x1 = x_axis_loc, y1 = 0.12, x2 = x_axis_loc, y2 = 0.4, h = 0.04, lwd = 2, col = "black")


opioids_plot <- opioids_plot %>% 
  group_by(id_swisci) %>% 
  mutate(ltfu_tot = sum(ltfu)) %>% 
  ungroup() %>% 
  mutate(ltfu_tot = as.factor(if_else(ltfu_tot == 1, "ltfu", "not_ltfu"))) %>% 
  mutate(is_opioid = as.factor(is_opioid))



# Plot including lost to follow ups ---------------------------------------

pj <- position_jitter(width = 0.05, height = 0.3)

ggplot(data = opioids_plot, mapping = aes(tp, is_opioid, group = id_swisci)) +
  
  geom_point(aes(fill = ther_freq), alpha = 0.5, size = 4, shape = 21, position = pj) +
  geom_line(aes(linetype = ltfu_tot, color = ltfu_tot), alpha = 0.4, show.legend = FALSE, position = pj) + 
  
  scale_fill_manual(name = "Opioid intake status",
                    labels = c("Lost to follow-up", "No opioid", "Pro re nata", "Regular therapy"),
                    values = c("grey10", "green", "blue", "red")) +
  
  scale_colour_manual(values = c("grey2", "grey10")) +
  scale_linetype_manual(values = c("blank", "dotted")) +
  
  theme_classic() +
  
  theme(axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.line.x = element_blank(), axis.line.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Arial", size = 18)
        ) +
  
  guides(fill = guide_legend(nrow = 2, title.theme = element_text(size = 15))) +
  

  annotate(geom = "text", x = 0.6, y = 2, label = "Opioids\ntaken", color = "black") +
  annotate(geom = "text", x = 0.6, y = 0.95, label = "No\nopioids\ntaken\nor\nlost to\nfollow-up", color = "black") +
  
  annotation_custom(b1) + annotation_custom(b2)

ggsave(file.path("output", "opioid_duration.svg"), device = "svg", width = 18, height = 20, units = "cm")





ltfu_excl <- opioids_plot %>% 
  
  filter(ltfu_tot == "not_ltfu") %>% 
  mutate(ther_freq = fct_drop(ther_freq)) %>%
  group_by(id_swisci) %>% 
  mutate(numb_opioid = sum(is_opioid)) %>% 
  ungroup() %>% 
  mutate(numb_opioid = factor(numb_opioid, levels = c(3, 2, 1), labels = c("always", "twice", "once")))

levels(ltfu_excl$numb_opioid)

ltfu_excl %>% pull(id_swisci) %>% unique %>% length




x_axis_loc <- 0.16

b1 <- bracketsGrob(x1 = x_axis_loc, y1 = 0.65, x2 = x_axis_loc, y2 = 0.95, h = 0.04, lwd = 2, col = "black")
b2 <- bracketsGrob(x1 = x_axis_loc, y1 = 0.00, x2 = x_axis_loc, y2 = 0.30, h = 0.04, lwd = 2, col = "black")

ggplot(ltfu_excl, aes(tp, is_opioid_jit, group = id_swisci, color = numb_opioid)) +
  geom_line(alpha = 0.3, size = 1) + 
  geom_point(alpha = 0.3, size = 3, shape = 1, stroke = 1) +
  scale_color_manual(name = "opioid intake", 
                     labels = c("always", "twice", "once"), 
                     values = c("red", "orange", "green")) +
 
  labs(x = "time point", y = "opioid_taken", color = "Lost to follow-up") +
  
  theme_classic() +
  
  theme(axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank()) +
  
  coord_fixed(ratio = 2) +
  
  annotate(geom = "text", x = 0.6, y = 1, label = "opioids\ntaken", color = "black") +
  annotate(geom = "text", x = 0.6, y = 0, label = "no\nopioids\ntaken", color = "black") +
  
  annotation_custom(b1) +
  annotation_custom(b2)
  


# How many of the opioid consumers did consume opioids at different time points --------

opioids <- opioids %>% 
  mutate(ther_freq_2 = if_else(ltfu == 0, "ltfu", ther_freq),
         ther_freq_2 = factor(ther_freq_2,
                              levels = c("ltfu", "no_opioid", "prn", "rt"),
                              labels = c("lost to follow-up", "no opioid", "pro re nata", "regular therapy")))

opioids %>% filter(tp != "ppi1") %>% 
  pull(ther_freq_2) %>% 
  table()

opioid_freq_data <- opioids %>% 
  mutate(opioids_rt = if_else(ltfu == 0, NA_integer_, as.integer(opioids_rt))) %>% 
  mutate(opioids_prn = if_else(ltfu == 0, NA_integer_, as.integer(opioids_prn))) %>% 
  mutate(is_opioid = if_else(ltfu == 0, NA_integer_, as.integer(is_opioid))) 
  

opioid_freq_data <- opioid_freq_data %>% select(id_swisci, ther_freq_2) %>% 
gather(key, value, -id_swisci) %>%
  group_by(id_swisci, key, value) %>%
  tally %>% 
  spread(value, n, fill = 0) %>% 
  ungroup() %>% 
  select(-key)

n_opioid_consumers <- opioids %>% pull(id_swisci) %>% unique() %>% length()

ltfu_id <- opioids %>% 
  filter(ltfu == 0) %>% 
  pull(id_swisci) %>% 
  unique()

n_ltfu <- length(ltfu_id)

n_nltfu <- n_opioid_consumers - n_ltfu

all_opi <- opioid_freq_data %>% nrow()

rt_3_prn_0 <- opioid_freq_data %>% filter(`regular therapy` == 3) %>% nrow()

rt_2_prn_1 <- opioid_freq_data %>% filter(`pro re nata` == 1 & `regular therapy` == 2) %>% nrow()

rt_1_prn_2 <- opioid_freq_data %>% filter(`pro re nata` == 2 & `regular therapy` == 1) %>% nrow()

rt_0_prn_3 <- opioid_freq_data %>% filter(`pro re nata` == 3) %>% nrow()


rt_2_prn_0 <- opioid_freq_data %>% filter(`pro re nata` == 0 & `regular therapy` == 2) %>% nrow()
rt_1_prn_1 <- opioid_freq_data %>% filter(`pro re nata` == 1 & `regular therapy` == 1) %>% nrow()
rt_0_prn_2 <- opioid_freq_data %>% filter(`pro re nata` == 2 & `regular therapy` == 0) %>% nrow()

opi_3 <- sum(rt_3_prn_0, rt_2_prn_1, rt_1_prn_2, rt_0_prn_3)

opi_2 <- sum(rt_2_prn_0, rt_1_prn_1, rt_0_prn_2)

opi_1 <- opioid_freq_data %>% filter(`no opioid` == 2) %>% nrow()

ltfu_n <- opioid_freq_data %>% filter(`lost to follow-up` >= 1) %>% nrow()

# One person was lost to follow-up only at one time point (t2/t3) This person was lost to follow up but had
# also taken an opioid at two time points

sum(opi_3, opi_2, opi_1, ltfu_n) 

cat(opi_3, "of", all_opi, "opioid-consuming participants took opioids at all 3 time points.", 
    "\nThat is", round(opi_3/all_opi * 100, 1), "% of all opioid-consuming participants.",
    "\nExcluding lost to follow-up cases (", ltfu_n, ") that is", round(opi_3/ (all_opi -  ltfu_n) * 100, 1), 
    "% of cases.")

cat(opi_2, "of", all_opi, "opioid-consuming participants took opioids at 2 time points.", 
    "\nThat is", round(opi_2/all_opi * 100, 1), "% of all opioid-consuming participants.",
    "\nExcluding lost to follow-up cases (", ltfu_n - 1, ") that is", round(opi_2/ (all_opi - (ltfu_n + 1)) * 100, 1), 
    "% of cases.")

cat(opi_3 + opi_2, "of", all_opi, "opioid-consuming participants took opioids at 2 or 3 time points.", 
    "\nThat is", round(opi_2 + opi_3/all_opi * 100, 1), "% of all opioid-consuming participants.",
    "\nExcluding lost to follow-up cases (", ltfu_n, ") that is", round((opi_2 + opi_3)/ (all_opi -  ltfu_n) * 100, 1), 
    "% of cases.")


# Clear workspace ---------------------------------------------------------

rm("all_opi", "b1", "b2", "bracketsGrob", "drugs", "drugs_all", 
     "drugs_opi", "follow_up", "id_no_opioid_TP_1", "ltfu", "ltfu_excl", 
     "ltfu_id", "ltfu_n", "n_ltfu", "n_nltfu", "n_opioid_consumers", 
     "opi_1", "opi_2", "opi_3", "opioid_consumers", "opioid_freq_data", 
     "opioids", "opioids_plot", "opioids_string", "rt_0_prn_2", "rt_0_prn_3", 
     "rt_1_prn_1", "rt_1_prn_2", "rt_2_prn_0", "rt_2_prn_1", "rt_3_prn_0", 
     "x_axis_loc")
