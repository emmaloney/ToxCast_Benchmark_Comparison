#Comparative Figures - ToxCast vs. ECOTOX PODs

library(tidyverse)
library(ggpubr)
library(readxl)
library(writexl)
library(viridis)
library(gridExtra)
library(readr)
library(openxlsx)

#read in data####
ecotox_acc <- read.xlsx("ECOTOX_PODs_ACC_04_12_2022.xlsx")
ecotox_acc$min_EC <- as.numeric(ecotox_acc$min_EC)
ecotox_acc$`acc5_linear.(mg/L)` <- as.numeric(ecotox_acc$`acc5_linear.(mg/L)`)
ecotox_cb <- read_excel("ECOTOX_PODs_CB_04_12_2022.xlsx")
qsar_toxcast <- read_excel("ToxCast_QSAR.xlsx")
qsar_CB <- read_excel("CB_QSAR.xlsx")
qsar_ECOTOX <- read_csv("QSAR_ECOTOX_04_12_2022.csv")
View(qsar_ECOTOX)

#ToxCast vs. ECOTOX####
names(ecotox_acc)
#evaluate data distributions####
#ToxCast ACC####
#overall####
ecotox_acc_overall <- ecotox_acc %>% filter(pod_type == "overall")
summary(ecotox_acc_overall)
hist(ecotox_acc_overall$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_overall$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_overall$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_overall$log_acc <- log10(ecotox_acc_overall$`acc5_linear.(mg/L)`)
hist(ecotox_acc_overall$log_acc)
qqnorm(ecotox_acc_overall$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_overall$log_acc, col = "steelblue", lwd = 2)

#tier 1 vs. tier 2####
list(unique(ecotox_acc$EFFECT_TYPE))
ecotox_acc_t1 <- ecotox_acc %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_1")
ecotox_acc_t2 <- ecotox_acc %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_2")

hist(ecotox_acc_t1$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_t1$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_t1$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

hist(ecotox_acc_t2$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_t2$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_t2$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_t1$log_acc <- log10(ecotox_acc_t1$`acc5_linear.(mg/L)`)
hist(ecotox_acc_t1$log_acc)
qqnorm(ecotox_acc_t1$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_t1$log_acc, col = "steelblue", lwd = 2)

ecotox_acc_t2$log_acc <- log10(ecotox_acc_t2$`acc5_linear.(mg/L)`)
hist(ecotox_acc_t2$log_acc)
qqnorm(ecotox_acc_t2$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_t2$log_acc, col = "steelblue", lwd = 2)

#acute vs. chronic####
list(unique(ecotox_acc$STUDY_LENGTH))
list(unique(ecotox_acc$pod_type))

ecotox_acc_acute <- ecotox_acc %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Acute")
ecotox_acc_chronic <- ecotox_acc %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Chronic")

hist(ecotox_acc_acute$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_acute$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_acute$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

hist(ecotox_acc_chronic$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_chronic$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_chronic$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_acute$log_acc <- log10(ecotox_acc_acute$`acc5_linear.(mg/L)`)
hist(ecotox_acc_acute$log_acc)
qqnorm(ecotox_acc_acute$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_acute$log_acc, col = "steelblue", lwd = 2)

ecotox_acc_chronic$log_acc <- log10(ecotox_acc_chronic$`acc5_linear.(mg/L)`)
hist(ecotox_acc_chronic$log_acc)
qqnorm(ecotox_acc_chronic$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_chronic$log_acc, col = "steelblue", lwd = 2)

#spp####
list(unique(ecotox_acc$ORGANISM_TYPE))
list(unique(ecotox_acc$pod_type))

ecotox_acc_fish <- ecotox_acc %>% filter(pod_type == "species", ORGANISM_TYPE == "Fish/Frogs")
ecotox_acc_inverts <- ecotox_acc %>% filter(pod_type == "species", ORGANISM_TYPE == "Invertebrates")
ecotox_acc_plants <- ecotox_acc %>% filter(pod_type == "species", ORGANISM_TYPE == "Plants")

hist(ecotox_acc_fish$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_fish$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_fish$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

hist(ecotox_acc_inverts$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_inverts$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_inverts$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

hist(ecotox_acc_plants$`acc5_linear.(mg/L)`)
qqnorm(ecotox_acc_plants$`acc5_linear.(mg/L)`, pch = 1, frame = FALSE)
qqline(ecotox_acc_plants$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_fish$log_acc <- log10(ecotox_acc_fish$`acc5_linear.(mg/L)`)
hist(ecotox_acc_fish$log_acc)
qqnorm(ecotox_acc_fish$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_fish$log_acc, col = "steelblue", lwd = 2)

ecotox_acc_inverts$log_acc <- log10(ecotox_acc_inverts$`acc5_linear.(mg/L)`)
hist(ecotox_acc_inverts$log_acc)
qqnorm(ecotox_acc_inverts$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_inverts$log_acc, col = "steelblue", lwd = 2)

ecotox_acc_plants$log_acc <- log10(ecotox_acc_plants$`acc5_linear.(mg/L)`)
hist(ecotox_acc_plants$log_acc)
qqnorm(ecotox_acc_plants$log_acc, pch = 1, frame = FALSE)
qqline(ecotox_acc_plants$log_acc, col = "steelblue", lwd = 2)

#kow####
ecotox_acc$log10_minEC <- log10(ecotox_acc$min_EC)
ecotox_acc$log10_acc <- log10(ecotox_acc$`acc5_linear.(mg/L)`)

acc_ecotox_kow3 <- ecotox_acc %>% filter(OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED <= 3)
acc_ecotox_kow3plus <- ecotox_acc %>% filter(OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED > 3)

hist(acc_ecotox_kow3$`acc5_linear.(mg/L)`)
qqnorm(acc_ecotox_kow3$`acc5_linear.(mg/L)`)
qqline(acc_ecotox_kow3$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

hist(acc_ecotox_kow3$log10_acc)
qqnorm(acc_ecotox_kow3$log10_acc)
qqline(acc_ecotox_kow3$log10_acc, col = "steelblue", lwd = 2)

hist(acc_ecotox_kow3plus$`acc5_linear.(mg/L)`)
qqnorm(acc_ecotox_kow3plus$`acc5_linear.(mg/L)`)
qqline(acc_ecotox_kow3plus$`acc5_linear.(mg/L)`, col = "steelblue", lwd = 2)

hist(acc_ecotox_kow3plus$log10_acc)
qqnorm(acc_ecotox_kow3plus$log10_acc)
qqline(acc_ecotox_kow3plus$log10_acc, col = "steelblue", lwd = 2)


#ToxCast ECOTOX####
#overall####
hist(ecotox_acc_overall$min_EC)
ecotox_acc_overall$log_min_eco_POD <- log10(ecotox_acc_overall$min_EC)
hist(ecotox_acc_overall$log_min_eco_POD)
qqnorm(ecotox_acc_overall$log_min_eco_POD, pch = 1, frame = FALSE)
qqline(ecotox_acc_overall$log_min_eco_POD, col = "steelblue", lwd = 2)

#tier1tier2####
hist(ecotox_acc_t1$min_EC)
qqnorm(ecotox_acc_t1$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_t1$min_EC, col = "steelblue", lwd = 2)

hist(ecotox_acc_t2$min_EC)
qqnorm(ecotox_acc_t2$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_t2$min_EC, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_t1$log10_minEC <- log10(ecotox_acc_t1$min_EC)
ecotox_acc_t1 <- ecotox_acc_t1 %>% filter(log10_minEC != -Inf)
hist(ecotox_acc_t1$log10_minEC)
qqnorm(ecotox_acc_t1$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_t1$log10_minEC, col = "steelblue", lwd = 2)

ecotox_acc_t2$log10_minEC <- log10(ecotox_acc_t2$min_EC)
hist(ecotox_acc_t2$log10_minEC)
qqnorm(ecotox_acc_t2$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_t2$log10_minEC, col = "steelblue", lwd = 2)

#acute vs. chronic####
hist(ecotox_acc_acute$min_EC)
qqnorm(ecotox_acc_acute$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_acute$min_EC, col = "steelblue", lwd = 2)

hist(ecotox_acc_chronic$min_EC)
qqnorm(ecotox_acc_chronic$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_chronic$min_EC, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_acute$log10_minEC <- log10(ecotox_acc_acute$min_EC)
ecotox_acc_acute <- ecotox_acc_acute %>% filter(log10_minEC != -Inf)
hist(ecotox_acc_acute$log10_minEC)
qqnorm(ecotox_acc_acute$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_acute$log10_minEC, col = "steelblue", lwd = 2)

ecotox_acc_chronic$log10_minEC <- log10(ecotox_acc_chronic$min_EC)
hist(ecotox_acc_chronic$log10_minEC)
qqnorm(ecotox_acc_chronic$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_chronic$log10_minEC, col = "steelblue", lwd = 2)

#spp####
hist(ecotox_acc_fish$min_EC)
qqnorm(ecotox_acc_fish$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_fish$min_EC, col = "steelblue", lwd = 2)

hist(ecotox_acc_inverts$min_EC)
qqnorm(ecotox_acc_inverts$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_inverts$min_EC, col = "steelblue", lwd = 2)

hist(ecotox_acc_plants$min_EC)
qqnorm(ecotox_acc_plants$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_acc_plants$min_EC, col = "steelblue", lwd = 2)

#log transform
ecotox_acc_fish$log10_minEC <- log10(ecotox_acc_fish$min_EC)
ecotox_acc_fish <- ecotox_acc_fish %>% filter(log10_minEC != -Inf)
hist(ecotox_acc_fish$log10_minEC)
qqnorm(ecotox_acc_fish$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_fish$log10_minEC, col = "steelblue", lwd = 2)

ecotox_acc_inverts$log10_minEC <- log10(ecotox_acc_inverts$min_EC)
hist(ecotox_acc_inverts$log10_minEC)
qqnorm(ecotox_acc_inverts$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_inverts$log10_minEC, col = "steelblue", lwd = 2)

ecotox_acc_plants$log10_minEC <- log10(ecotox_acc_plants$min_EC)
hist(ecotox_acc_plants$log10_minEC)
qqnorm(ecotox_acc_plants$log10_minEC, pch = 1, frame = FALSE)
qqline(ecotox_acc_plants$log10_minEC, col = "steelblue", lwd = 2)

#kow####
hist(acc_ecotox_kow3$min_EC)
qqnorm(acc_ecotox_kow3$min_EC)
qqline(acc_ecotox_kow3$min_EC, col = "steelblue", lwd = 2)

hist(acc_ecotox_kow3$log10_minEC)
acc_ecotox_kow3 <- acc_ecotox_kow3 %>% filter(log10_minEC != -Inf)
qqnorm(acc_ecotox_kow3$log10_minEC)
qqline(acc_ecotox_kow3$log10_minEC, col = "steelblue", lwd = 2)

hist(acc_ecotox_kow3plus$min_EC)
qqnorm(acc_ecotox_kow3plus$min_EC)
qqline(acc_ecotox_kow3plus$min_EC, col = "steelblue", lwd = 2)

hist(acc_ecotox_kow3plus$log10_minEC)
acc_ecotox_kow3plus <- acc_ecotox_kow3plus %>% filter(log10_minEC != -Inf)
qqnorm(acc_ecotox_kow3plus$log10_minEC)
qqline(acc_ecotox_kow3plus$log10_minEC, col = "steelblue", lwd = 2)

#ToxCast-ECOTOX correlations####
names(ecotox_acc_overall)
#overall####
#drop potential outliers#
toxcast_ecotox_corplot <- ecotox_acc_overall %>% filter(`acc5_linear.(mg/L)` < 60, min_EC < 20000) %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_corplot

toxcast_ecotox_logcorplot <- ecotox_acc_overall %>%
  ggscatter("log_min_eco_POD", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A")
toxcast_ecotox_logcorplot

cor(ecotox_acc_overall$log_min_eco_POD, ecotox_acc_overall$log_acc)
test <- cor.test(ecotox_acc_overall$log_min_eco_POD, ecotox_acc_overall$log_acc)
test

toxcast_ecotox_logcorplot_extended <- ecotox_acc_overall %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log_min_eco_POD", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 2), cor.method = "pearson", color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification",
            title = "B") +rremove("legend")
toxcast_ecotox_logcorplot_extended

toxcast_ecotox_plots <- grid.arrange(toxcast_ecotox_logcorplot, toxcast_ecotox_logcorplot_extended)
ggsave("toxcast_ecotox_plots_04_12_2022.jpeg", toxcast_ecotox_plots, height = 15, width = 10)  

#tier1tier2####
#t1
names(ecotox_acc_t1)
toxcast_ecotox_t1_corplot <- ecotox_acc_t1 %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman", title = "A (Tier 1)")
toxcast_ecotox_t1_corplot

toxcast_ecotox_t1_logcorplot <- ecotox_acc_t1 %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Tier 1)")
toxcast_ecotox_t1_logcorplot

cor(ecotox_acc_t1$log10_minEC, ecotox_acc_t1$log_acc)
test <- cor.test(ecotox_acc_t1$log10_minEC, ecotox_acc_t1$log_acc)
test

toxcast_ecotox_t1_logcorplot_ext <- ecotox_acc_t1 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 2), cor.method = "pearson", color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "C (Tier 1)") +
  rremove("legend")
toxcast_ecotox_t1_logcorplot_ext

#t2
names(ecotox_acc_t2)
ecotox_acc_t2$log_acc <- log10(ecotox_acc_t2$`acc5_linear.(mg/L)`)
toxcast_ecotox_t2_corplot <- ecotox_acc_t2 %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_t2_corplot

toxcast_ecotox_t2_logcorplot <- ecotox_acc_t2 %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Tier 2)")
toxcast_ecotox_t2_logcorplot

cor(ecotox_acc_t2$log10_minEC, ecotox_acc_t2$log_acc)
test <- cor.test(ecotox_acc_t2$log10_minEC, ecotox_acc_t2$log_acc)
test

toxcast_ecotox_t2_logcorplot_ext <- ecotox_acc_t2 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 1.5), cor.method = "pearson", color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "D (Tier 2)") + ylim(-6,2) +
  rremove("legend")
toxcast_ecotox_t2_logcorplot_ext

tier1_tier2_ecotox_toxcast <- grid.arrange(toxcast_ecotox_t1_logcorplot, toxcast_ecotox_t2_logcorplot, toxcast_ecotox_t1_logcorplot_ext, toxcast_ecotox_t2_logcorplot_ext)
ggsave("tier1_tier2_ecotox_toxcast_04_12_2022.jpeg", tier1_tier2_ecotox_toxcast, height = 15, width = 15)

#acute_chronic####
names(ecotox_acc_acute)
names(ecotox_acc_chronic)

#acute
toxcast_ecotox_acute_corplot <- ecotox_acc_acute %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_acute_corplot

toxcast_ecotox_acute_logcorplot <- ecotox_acc_acute %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (acute)")
toxcast_ecotox_acute_logcorplot

cor(ecotox_acc_acute$log10_minEC, ecotox_acc_acute$log_acc)
test <- cor.test(ecotox_acc_acute$log10_minEC, ecotox_acc_acute$log_acc)
test

toxcast_ecotox_acute_logcorplot_ext <- ecotox_acc_acute %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 2), cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "C (acute)") + rremove("legend")
toxcast_ecotox_acute_logcorplot_ext

#chronic
toxcast_ecotox_chronic_corplot <- ecotox_acc_chronic %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_chronic_corplot

toxcast_ecotox_chronic_logcorplot <- ecotox_acc_chronic %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (chronic)")
toxcast_ecotox_chronic_logcorplot

cor(ecotox_acc_chronic$log10_minEC, ecotox_acc_chronic$log_acc)
test <- cor.test(ecotox_acc_chronic$log10_minEC, ecotox_acc_chronic$log_acc)
test

toxcast_ecotox_chronic_logcorplot_ext <- ecotox_acc_chronic %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 2), cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "D (chronic)")+
ylim(-6, 3) + rremove("legend")
toxcast_ecotox_chronic_logcorplot_ext

toxcast_ecotox_acute_chronic <- grid.arrange(toxcast_ecotox_acute_logcorplot, toxcast_ecotox_chronic_logcorplot, toxcast_ecotox_acute_logcorplot_ext, toxcast_ecotox_chronic_logcorplot_ext)
ggsave("toxcast_ecotox_acute_chronic_04_12_2022.jpeg", toxcast_ecotox_acute_chronic, width = 15, height = 15)

#species####
names(ecotox_acc_fish)
names(ecotox_acc_inverts)
names(ecotox_acc_plants)

#fish
toxcast_ecotox_fish_corplot <- ecotox_acc_fish  %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_fish_corplot

toxcast_ecotox_fish_logcorplot <- ecotox_acc_fish %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Aquatic Vertebrate)")
toxcast_ecotox_fish_logcorplot

cor(ecotox_acc_fish$log10_minEC, ecotox_acc_fish$log_acc)
test <- cor.test(ecotox_acc_fish$log10_minEC, ecotox_acc_fish$log_acc)
test

toxcast_ecotox_fish_logcorplot_ext <- ecotox_acc_fish %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 3), cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = ("D (Aquatic Vertebrate)")) + rremove("legend")
toxcast_ecotox_fish_logcorplot_ext

#inverts
toxcast_ecotox_invert_corplot <- ecotox_acc_inverts  %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_invert_corplot

toxcast_ecotox_invert_logcorplot <- ecotox_acc_inverts %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Invertebrate)")
toxcast_ecotox_invert_logcorplot

cor(ecotox_acc_inverts$log10_minEC, ecotox_acc_inverts$log_acc)
test <- cor.test(ecotox_acc_inverts$log10_minEC, ecotox_acc_inverts$log_acc)
test

toxcast_ecotox_invert_logcorplot_ext <- ecotox_acc_inverts %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5), cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "E (Invertebrate)") + rremove("legend")
toxcast_ecotox_invert_logcorplot_ext

#plants
toxcast_ecotox_plants_corplot <- ecotox_acc_plants  %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
toxcast_ecotox_plants_corplot

toxcast_ecotox_plants_logcorplot <- ecotox_acc_plants %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "C (Plant)")
toxcast_ecotox_plants_logcorplot

cor(ecotox_acc_plants$log10_minEC, ecotox_acc_plants$log_acc)
test <- cor.test(ecotox_acc_plants$log10_minEC, ecotox_acc_plants$log_acc)
test

toxcast_ecotox_plants_logcorplot_ext <- ecotox_acc_plants %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log_acc",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 3), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "F (Plant)") + ylim(-6, 4) + rremove("legend")
toxcast_ecotox_plants_logcorplot_ext

toxcast_ecotox_spp <- grid.arrange(toxcast_ecotox_fish_logcorplot, toxcast_ecotox_invert_logcorplot, toxcast_ecotox_plants_logcorplot, toxcast_ecotox_fish_logcorplot_ext,
                                   toxcast_ecotox_invert_logcorplot_ext, toxcast_ecotox_plants_logcorplot_ext, nrow = 2)
  

ggsave("toxcast_ecotox_spp_04_12_2022.jpeg", toxcast_ecotox_spp, height = 17, width = 25)

#kow####
names(acc_ecotox_kow3)
names(acc_ecotox_kow3plus)

#kow < 3
acc_ecotox_kow3_corplot <- acc_ecotox_kow3  %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
acc_ecotox_kow3_corplot

acc_ecotox_kow3_logcorplot <- acc_ecotox_kow3 %>%
  ggscatter("log10_acc", "log10_minEC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (logKow <=3)")
acc_ecotox_kow3_logcorplot

acc_ecotox_kow3$log10_acc <- as.numeric(acc_ecotox_kow3$log10_acc)
acc_ecotox_kow3$log10_minEC <- as.numeric(acc_ecotox_kow3$log10_minEC)

cor(acc_ecotox_kow3$log10_minEC, acc_ecotox_kow3$log10_acc)
test <- cor.test(acc_ecotox_kow3$log10_minEC, acc_ecotox_kow3$log10_acc)
test

acc_ecotox_logcorplot_ext <- acc_ecotox_kow3 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_acc", "log10_minEC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 3), cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification",  title = "C (logKow <=3)") + rremove("legend")
acc_ecotox_logcorplot_ext

#kow > 3
acc_ecotox_kow3plus_corplot <- acc_ecotox_kow3plus  %>%
  ggscatter("min_EC", "acc5_linear.(mg/L)",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
acc_ecotox_kow3plus_corplot

acc_ecotox_kow3plus_logcorplot <- acc_ecotox_kow3plus %>%
  ggscatter("log10_acc", "log10_minEC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (logKow >3)")
acc_ecotox_kow3plus_logcorplot

acc_ecotox_kow3plus$log10_acc <- as.numeric(acc_ecotox_kow3plus$log10_acc)
acc_ecotox_kow3plus$log10_minEC <- as.numeric(acc_ecotox_kow3plus$log10_minEC)

cor(acc_ecotox_kow3plus$log10_minEC, acc_ecotox_kow3plus$log10_acc)
test <- cor.test(acc_ecotox_kow3plus$log10_minEC, acc_ecotox_kow3plus$log10_acc)
test

acc_ecotox_kow3plus_logcorplot_ext <- acc_ecotox_kow3plus %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_acc", "log10_minEC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 3), cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "D (logKow >3)") + ylim(-7.5, 4) + rremove("legend")
acc_ecotox_kow3plus_logcorplot_ext

toxcast_ecotox_kow <- grid.arrange(acc_ecotox_kow3_logcorplot, acc_ecotox_kow3plus_logcorplot, acc_ecotox_logcorplot_ext, acc_ecotox_kow3plus_logcorplot_ext)

ggsave("toxcast_ecotox_kow_04_12_2022.jpeg", toxcast_ecotox_kow, width = 10, height = 10)

#ECOTOX vs. Cytotoxic Burst####
names(ecotox_cb)
#evaluate data distributions####
#Cytotoxic Burst####
#overall####
ecotox_cb_overall <- ecotox_cb %>% filter(pod_type == "overall")
hist(ecotox_cb_overall$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_overall$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_overall$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_overall$log10_cbb <- log10(ecotox_cb_overall$`Cytotox_low bnd_mg_L` + 0.000001)
hist(ecotox_cb_overall$log10_cbb)
qqnorm(ecotox_cb_overall$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_overall$log10_cbb, col = "steelblue", lwd = 2)

#tier1tier2####
ecotox_cb_t1 <- ecotox_cb %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_1")
hist(ecotox_cb_t1$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_t1$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_t1$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_t1$log10_cbb <- log10(ecotox_cb_t1$`Cytotox_low bnd_mg_L` + 0.000001)
hist(ecotox_cb_t1$log10_cbb)
qqnorm(ecotox_cb_t1$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_t1$log10_cbb, col = "steelblue", lwd = 2)

ecotox_cb_t2 <- ecotox_cb %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_2")
hist(ecotox_cb_t2$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_t2$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_t2$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_t2$log10_cbb <- log10(ecotox_cb_t2$`Cytotox_low bnd_mg_L` + 0.01)
hist(ecotox_cb_t2$log10_cbb)
qqnorm(ecotox_cb_t2$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_t2$log10_cbb, col = "steelblue", lwd = 2)

#acute vs. chronic####
ecotox_cb_acute <- ecotox_cb %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Acute")
hist(ecotox_cb_acute$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_acute$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_acute$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_acute$log10_cbb <- log10(ecotox_cb_acute$`Cytotox_low bnd_mg_L` + 0.00001)
hist(ecotox_cb_acute$log10_cbb)
qqnorm(ecotox_cb_acute$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_acute$log10_cbb, col = "steelblue", lwd = 2)

ecotox_cb_chronic <- ecotox_cb %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Chronic")
hist(ecotox_cb_chronic$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_chronic$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_chronic$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_chronic$log10_cbb <- log10(ecotox_cb_chronic$`Cytotox_low bnd_mg_L` + 0.00001)
hist(ecotox_cb_chronic$log10_cbb)
qqnorm(ecotox_cb_chronic$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_chronic$log10_cbb, col = "steelblue", lwd = 2)

#spp####
list(unique(ecotox_cb$ORGANISM_TYPE))
ecotox_cb_fish <- ecotox_cb %>% filter(pod_type == "species", ORGANISM_TYPE == "Fish/Frogs")
ecotox_cb_fish$`Cytotox_low bnd_mg_L` <- as.numeric(ecotox_cb_fish$`Cytotox_low bnd_mg_L`)
hist(ecotox_cb_fish$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_fish$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_fish$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_fish$log10_cbb <- log10(ecotox_cb_fish$`Cytotox_low bnd_mg_L` + 0.00001)
hist(ecotox_cb_fish$log10_cbb)
qqnorm(ecotox_cb_fish$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_fish$log10_cbb, col = "steelblue", lwd = 2)

ecotox_cb_inverts <- ecotox_cb %>% filter(pod_type == "species", ORGANISM_TYPE == "Invertebrates")
ecotox_cb_inverts$`Cytotox_low bnd_mg_L` <- as.numeric(ecotox_cb_inverts$`Cytotox_low bnd_mg_L`)
hist(ecotox_cb_inverts$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_inverts$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_inverts$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_inverts$log10_cbb <- log10(ecotox_cb_inverts$`Cytotox_low bnd_mg_L` + 0.00001)
hist(ecotox_cb_inverts$log10_cbb)
qqnorm(ecotox_cb_inverts$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_inverts$log10_cbb, col = "steelblue", lwd = 2)

ecotox_cb_plants <- ecotox_cb %>% filter(pod_type == "species", ORGANISM_TYPE == "Plants")
ecotox_cb_plants$`Cytotox_low bnd_mg_L` <- as.numeric(ecotox_cb_plants$`Cytotox_low bnd_mg_L`)
hist(ecotox_cb_plants$`Cytotox_low bnd_mg_L`)
qqnorm(ecotox_cb_plants$`Cytotox_low bnd_mg_L`, pch = 1, frame = FALSE)
qqline(ecotox_cb_plants$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

ecotox_cb_plants$log10_cbb <- log10(ecotox_cb_plants$`Cytotox_low bnd_mg_L` + 0.00001)
hist(ecotox_cb_plants$log10_cbb)
qqnorm(ecotox_cb_plants$log10_cbb, pch = 1, frame = FALSE)
qqline(ecotox_cb_plants$log10_cbb, col = "steelblue", lwd = 2)

#logKow####
cb_ecotox_logkow3 <- ecotox_cb_overall %>% filter(OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED <= 3)
cb_ecotox_logkowplus3 <- ecotox_cb_overall %>% filter(OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED > 3)

hist(cb_ecotox_logkow3$`Cytotox_low bnd_mg_L`)
qqnorm(cb_ecotox_logkow3$`Cytotox_low bnd_mg_L`)
qqline(cb_ecotox_logkow3$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

hist(cb_ecotox_logkow3$log10_cytotox)
qqnorm(cb_ecotox_logkow3$log10_cytotox)
qqline(cb_ecotox_logkow3$log10_cytotox, col = "steelblue", lwd = 2)

hist(cb_ecotox_logkowplus3$`Cytotox_low bnd_mg_L`)
qqnorm(cb_ecotox_logkowplus3$`Cytotox_low bnd_mg_L`)
qqline(cb_ecotox_logkowplus3$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

hist(cb_ecotox_logkowplus3$log10_cytotox)
qqnorm(cb_ecotox_logkowplus3$log10_cytotox)
qqline(cb_ecotox_logkowplus3$log10_cytotox, col = "steelblue", lwd = 2)

#CB-ECOTOX####
#overall####
ecotox_cb_overall <- ecotox_cb %>% filter(pod_type == "overall")
hist(ecotox_cb_overall$min_EC)
qqnorm(ecotox_cb_overall$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_overall$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_overall$log10min_EC <- log10(ecotox_cb_overall$min_EC)
hist(ecotox_cb_overall$log10min_EC)
qqnorm(ecotox_cb_overall$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_overall$log10min_EC, col = "steelblue", lwd = 2)

#tier1tier2####
names(ecotox_cb)
ecotox_cb_tier1 <- ecotox_cb %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_1")
hist(ecotox_cb_tier1$min_EC)
qqnorm(ecotox_cb_tier1$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_tier1$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_tier1$log10min_EC <- log10(ecotox_cb_tier1$min_EC)
hist(ecotox_cb_tier1$log10min_EC)
View(ecotox_cb_tier1)
ecotox_cb_tier1 <- ecotox_cb_tier1 %>% filter(log10min_EC != -Inf)
qqnorm(ecotox_cb_tier1$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_tier1$log10min_EC, col = "steelblue", lwd = 2)

ecotox_cb_tier2 <- ecotox_cb %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_2")
hist(ecotox_cb_tier2$min_EC)
qqnorm(ecotox_cb_tier2$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_tier2$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_tier2$log10min_EC <- log10(ecotox_cb_tier2$min_EC)
hist(ecotox_cb_tier2$log10min_EC)
View(ecotox_cb_tier2)
ecotox_cb_tier2 <- ecotox_cb_tier2 %>% filter(log10min_EC != -Inf)
qqnorm(ecotox_cb_tier2$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_tier2$log10min_EC, col = "steelblue", lwd = 2)

#acute_chronic####
ecotox_cb_acute <- ecotox_cb %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Acute")
hist(ecotox_cb_acute$min_EC)
qqnorm(ecotox_cb_acute$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_acute$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_acute$log10min_EC <- log10(ecotox_cb_acute$min_EC)
hist(ecotox_cb_acute$log10min_EC)
qqnorm(ecotox_cb_acute$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_acute$log10min_EC, col = "steelblue", lwd = 2)

ecotox_cb_chronic <- ecotox_cb %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Chronic")
hist(ecotox_cb_chronic$min_EC)
qqnorm(ecotox_cb_chronic$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_chronic$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_chronic$log10min_EC <- log10(ecotox_cb_chronic$min_EC)
hist(ecotox_cb_chronic$log10min_EC)
qqnorm(ecotox_cb_chronic$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_chronic$log10min_EC, col = "steelblue", lwd = 2)

#species#####
ecotox_cb_fish <- ecotox_cb %>% filter(pod_type == "species", ORGANISM_TYPE == "Fish/Frogs")
hist(ecotox_cb_fish$min_EC)
qqnorm(ecotox_cb_fish$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_fish$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_fish$log10min_EC <- log10(ecotox_cb_fish$min_EC)
hist(ecotox_cb_fish$log10min_EC)
qqnorm(ecotox_cb_fish$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_fish$log10min_EC, col = "steelblue", lwd = 2)

ecotox_cb_inverts <- ecotox_cb %>% filter(pod_type == "species", ORGANISM_TYPE == "Invertebrates")
hist(ecotox_cb_inverts$min_EC)
qqnorm(ecotox_cb_inverts$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_inverts$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_inverts$log10min_EC <- log10(ecotox_cb_inverts$min_EC)
hist(ecotox_cb_inverts$log10min_EC)
qqnorm(ecotox_cb_inverts$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_inverts$log10min_EC, col = "steelblue", lwd = 2)

ecotox_cb_plants <- ecotox_cb %>% filter(pod_type == "species", ORGANISM_TYPE == "Plants")
hist(ecotox_cb_plants$min_EC)
qqnorm(ecotox_cb_plants$min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_plants$min_EC, col = "steelblue", lwd = 2)

ecotox_cb_plants$log10min_EC <- log10(ecotox_cb_plants$min_EC)
hist(ecotox_cb_plants$log10min_EC)
qqnorm(ecotox_cb_plants$log10min_EC, pch = 1, frame = FALSE)
qqline(ecotox_cb_plants$log10min_EC, col = "steelblue", lwd = 2)


#logKow####
hist(cb_ecotox_logkow3$min_EC)
qqnorm(cb_ecotox_logkow3$min_EC)
qqline(cb_ecotox_logkow3$min_EC, col = "steelblue", lwd = 2)

hist(cb_ecotox_logkow3$log10min_EC)
qqnorm(cb_ecotox_logkow3$log10min_EC)
qqline(cb_ecotox_logkow3$log10min_EC, col = "steelblue", lwd = 2)

hist(cb_ecotox_logkowplus3$min_EC)
qqnorm(cb_ecotox_logkowplus3$min_EC)
qqline(cb_ecotox_logkowplus3$min_EC, col = "steelblue", lwd = 2)

hist(cb_ecotox_logkowplus3$log10min_EC)
qqnorm(cb_ecotox_logkowplus3$log10min_EC)
qqline(cb_ecotox_logkowplus3$log10min_EC, col = "steelblue", lwd = 2)

#Cytotox-ECOTOX correlations####
#overall####
names(ecotox_cb_overall)
ecotox_cb_overall$log10_cytotox <- log10(ecotox_cb_overall$`Cytotox_low bnd_mg_L`)

cytotox_ecotox_corplot <- ecotox_cb_overall  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_corplot

cytotox_ecotox_logcorplot <- ecotox_cb_overall %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A")
cytotox_ecotox_logcorplot

cor(ecotox_cb_overall$log10min_EC, ecotox_cb_overall$log10_cytotox)
test <- cor.test(ecotox_cb_overall$log10min_EC, ecotox_cb_overall$log10_cytotox)
test

cytotox_ecotox_logcorplot_ext <- ecotox_cb_overall %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "B") + ylim(-6, -1) + rremove("legend")
cytotox_ecotox_logcorplot_ext

cytotox_ecotox <- grid.arrange(cytotox_ecotox_logcorplot, cytotox_ecotox_logcorplot_ext)
ggsave("cytotox_ecotox_04_12_2022.jpeg", cytotox_ecotox, height = 15, width = 10)

#tier1_tier2####
ecotox_cb_tier1$log10_cytotox <- log10(ecotox_cb_tier1$`Cytotox_low bnd_mg_L`)
ecotox_cb_tier2$log10_cytotox <- log10(ecotox_cb_tier2$`Cytotox_low bnd_mg_L`)

names(ecotox_cb_tier1)
names(ecotox_cb_tier2)

#tier 1
cytotox_ecotox_t1_corplot <- ecotox_cb_tier1  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_t1_corplot

cytotox_ecotox_t1_logcorplot <- ecotox_cb_tier1 %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Tier 1)")
cytotox_ecotox_t1_logcorplot

cor(ecotox_cb_tier1$log10min_EC, ecotox_cb_tier1$log10_cytotox)
test <- cor.test(ecotox_cb_tier1$log10min_EC, ecotox_cb_tier1$log10_cytotox)
test

cytotox_ecotox_t1_logcorplot_ext <- ecotox_cb_tier1 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "C (Tier 1)") +
  ylim(-6, -1) + rremove("legend")
cytotox_ecotox_t1_logcorplot_ext

#tier 2
cytotox_ecotox_t2_corplot <- ecotox_cb_tier2  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_t2_corplot

cytotox_ecotox_t2_logcorplot <- ecotox_cb_tier2 %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Tier 2)")
cytotox_ecotox_t2_logcorplot

cor(ecotox_cb_tier2$log10min_EC, ecotox_cb_tier2$log10_cytotox)
test <- cor.test(ecotox_cb_tier2$log10min_EC, ecotox_cb_tier2$log10_cytotox)
test

cytotox_ecotox_t2_logcorplot_ext <- ecotox_cb_tier2 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "D (Tier 2)") +
  ylim(-6, -1) + rremove("legend")

cytotox_ecotox_t2_logcorplot_ext

cytotox_ecotox_tier12 <- grid.arrange(cytotox_ecotox_t1_logcorplot, cytotox_ecotox_t2_logcorplot, cytotox_ecotox_t1_logcorplot_ext, cytotox_ecotox_t2_logcorplot_ext)
ggsave("cytotox_ecotox_tier12.jpeg", cytotox_ecotox_tier12, height = 15, width = 15)

#acute_chronic####
names(ecotox_cb_acute)
names(ecotox_cb_chronic)

#acute
ecotox_cb_acute$log10_cytotox <- log10(ecotox_cb_acute$`Cytotox_low bnd_mg_L`)
cytotox_ecotox_acute_corplot <- ecotox_cb_acute  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_acute_corplot

cytotox_ecotox_acute_logcorplot <- ecotox_cb_acute %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (acute)")
cytotox_ecotox_acute_logcorplot

cor(ecotox_cb_acute$log10min_EC, ecotox_cb_acute$log10_cytotox)
test <- cor.test(ecotox_cb_acute$log10min_EC, ecotox_cb_acute$log10_cytotox)
test

cytotox_ecotox_acute_logcorplot_ext <- ecotox_cb_acute %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "C (acute)") +
  ylim(-6, -1) + rremove("legend")
cytotox_ecotox_acute_logcorplot_ext

#chronic
ecotox_cb_chronic$log10_cytotox <- log10(ecotox_cb_chronic$`Cytotox_low bnd_mg_L`)
cytotox_ecotox_chronic_corplot <- ecotox_cb_chronic  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_chronic_corplot

cytotox_ecotox_chronic_logcorplot <- ecotox_cb_chronic %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (chronic)")
cytotox_ecotox_chronic_logcorplot

cor(ecotox_cb_chronic$log10min_EC, ecotox_cb_chronic$log10_cytotox)
test <- cor.test(ecotox_cb_chronic$log10min_EC, ecotox_cb_chronic$log10_cytotox)
test

cytotox_ecotox_chronic_logcorplot_ext <- ecotox_cb_chronic %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = ("D (chronic)")) +
              ylim(-6, -1) + rremove("legend")
cytotox_ecotox_chronic_logcorplot_ext

cytotox_ecotox_acutechronic <- grid.arrange(cytotox_ecotox_acute_logcorplot,cytotox_ecotox_chronic_logcorplot, cytotox_ecotox_acute_logcorplot_ext, cytotox_ecotox_chronic_logcorplot_ext)
ggsave("cytotox_ecotox_acutechronic_04_12_2022.jpeg", cytotox_ecotox_acutechronic, height = 15, width = 15)

#species####
names(ecotox_cb_fish)
names(ecotox_cb_inverts)
names(ecotox_cb_plants)

#fish
ecotox_cb_fish$log10_cytotox <- log10(ecotox_cb_fish$`Cytotox_low bnd_mg_L`)

cytotox_ecotox_fish_corplot <- ecotox_cb_fish  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_fish_corplot

cytotox_ecotox_fish_logcorplot <- ecotox_cb_fish %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Aquatic Vertebrate)")

cytotox_ecotox_fish_logcorplot

cor(ecotox_cb_fish$log10min_EC, ecotox_cb_fish$log10_cytotox)
test <- cor.test(ecotox_cb_fish$log10min_EC, ecotox_cb_fish$log10_cytotox)
test

cytotox_ecotox_fish_logcorplot_ext <- ecotox_cb_fish %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "D (Aquatic Vertebrate)") + ylim(-6, -1) + rremove("legend")
cytotox_ecotox_fish_logcorplot_ext

#inverts
ecotox_cb_inverts$log10_cytotox <- log10(ecotox_cb_inverts$`Cytotox_low bnd_mg_L`)

cytotox_ecotox_inverts_corplot <- ecotox_cb_inverts  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_inverts_corplot

cytotox_ecotox_inverts_logcorplot <- ecotox_cb_inverts %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Invertebrate)")
cytotox_ecotox_inverts_logcorplot

cor(ecotox_cb_inverts$log10min_EC, ecotox_cb_inverts$log10_cytotox)
test <- cor.test(ecotox_cb_inverts$log10min_EC, ecotox_cb_inverts$log10_cytotox)
test

cytotox_ecotox_inverts_logcorplot_ext <- ecotox_cb_inverts %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "E (Invertebrate)") + ylim(-6, -1) + rremove("legend")
cytotox_ecotox_inverts_logcorplot_ext

#plants
ecotox_cb_plants$log10_cytotox <- log10(ecotox_cb_plants$`Cytotox_low bnd_mg_L`)

cytotox_ecotox_plants_corplot <- ecotox_cb_plants  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_plants_corplot

cytotox_ecotox_plants_logcorplot <- ecotox_cb_plants %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "C (Plant)")
cytotox_ecotox_plants_logcorplot

cor(ecotox_cb_plants$log10min_EC, ecotox_cb_plants$log10_cytotox)
test <- cor.test(ecotox_cb_plants$log10min_EC, ecotox_cb_plants$log10_cytotox)
test

cytotox_ecotox_plants_logcorplot_ext <- ecotox_cb_plants %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "F (Plant)") + ylim(-6, -1) + rremove("legend")
cytotox_ecotox_plants_logcorplot_ext

cytotox_ecotox_spp <- grid.arrange(cytotox_ecotox_fish_logcorplot, cytotox_ecotox_inverts_logcorplot, cytotox_ecotox_plants_logcorplot, cytotox_ecotox_fish_logcorplot_ext, 
                                   cytotox_ecotox_inverts_logcorplot_ext, cytotox_ecotox_plants_logcorplot_ext, nrow = 2)

ggsave("cytotox_ecotox_spp_04_12_2022.jpeg", cytotox_ecotox_spp, height = 15, width = 20)

#logKow####
names(cb_ecotox_logkow3)
names(cb_ecotox_logkowplus3)

#kow <3
cytotox_ecotox_kow3_corplot <- cb_ecotox_logkow3  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_kow3_corplot

cytotox_ecotox_kow3_logcorplot <- cb_ecotox_logkow3 %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (logKow <= 3)")
cytotox_ecotox_kow3_logcorplot

cor(cb_ecotox_logkow3$log10min_EC, cb_ecotox_logkow3$log10_cytotox)
test <- cor.test(cb_ecotox_logkow3$log10min_EC, cb_ecotox_logkow3$log10_cytotox)
test

cytotox_ecotox_kow3_logcorplot_ext <- cb_ecotox_logkow3 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "C (logKow <= 3)") + ylim(-6, -1) + rremove("legend")
cytotox_ecotox_kow3_logcorplot_ext

#kow > 3
cytotox_ecotox_pluskow3_corplot <- cb_ecotox_logkowplus3  %>%
  ggscatter("min_EC", "Cytotox_low bnd_mg_L",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
cytotox_ecotox_pluskow3_corplot

cytotox_ecotox_pluskow3_logcorplot <- cb_ecotox_logkowplus3 %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (logKow <= 3)")
cytotox_ecotox_pluskow3_logcorplot

cor(cb_ecotox_logkowplus3$log10min_EC, cb_ecotox_logkowplus3$log10_cytotox)
test <- cor.test(cb_ecotox_logkowplus3$log10min_EC, cb_ecotox_logkowplus3$log10_cytotox)
test

cytotox_ecotox_pluskow3_logcorplot_ext <- cb_ecotox_logkowplus3 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10min_EC", "log10_cytotox",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, -2), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = ("D (logKow > 3)")) + 
  ylim(-6, -1) + rremove("legend")
cytotox_ecotox_pluskow3_logcorplot_ext

cytotox_ecotox_logKow <- grid.arrange(cytotox_ecotox_kow3_logcorplot, cytotox_ecotox_pluskow3_logcorplot, cytotox_ecotox_kow3_logcorplot_ext, cytotox_ecotox_pluskow3_logcorplot_ext)
ggsave("cytotox_ecotox_logKow.jpeg", cytotox_ecotox_logKow, height = 15, width = 15)

#QSAR vs. ToxCast####
classifications
qsar_toxcast <- left_join(qsar_toxcast, classifications)
#evaluate data distributions
#overall####
names(qsar_toxcast)
View(qsar_toxcast)
hist(qsar_toxcast$`acc5_linear (mg/L)`)
qqnorm(qsar_toxcast$`acc5_linear (mg/L)`)
qqline(qsar_toxcast$`acc5_linear (mg/L)`, col = "steelblue", lwd = 2)

qsar_toxcast$log10_acc <- log10(qsar_toxcast$`acc5_linear (mg/L)`)
hist(qsar_toxcast$log10_acc)
qqnorm(qsar_toxcast$log10_acc)
qqline(qsar_toxcast$log10_acc, col = "steelblue", lwd = 2)

qsar_toxcast$min_aquatic_qsar_acute_pod_mg_l <- as.numeric(qsar_toxcast$min_aquatic_qsar_acute_pod_mg_l)
hist(qsar_toxcast$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_toxcast$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_toxcast$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

qsar_toxcast$log10_qsar <- log10(qsar_toxcast$min_aquatic_qsar_acute_pod_mg_l + 0.000001)
hist(qsar_toxcast$log10_qsar)
qqnorm(qsar_toxcast$log10_qsar)
qqline(qsar_toxcast$log10_qsar, col = "steelblue", lwd = 2)

#fish####
qsar_fish <- qsar_toxcast %>% filter(min_qsar_acute_pod_taxa == "fish")
hist(qsar_fish$`acc5_linear (mg/L)`)
qqnorm(qsar_fish$`acc5_linear (mg/L)`)
qqline(qsar_fish$`acc5_linear (mg/L)`, col = "steelblue", lwd = 2)

qsar_fish$log10_acc <- log10(qsar_fish$`acc5_linear (mg/L)`)
hist(qsar_fish$log10_acc)
qqnorm(qsar_fish$log10_acc)
qqline(qsar_fish$log10_acc, col = "steelblue", lwd = 2)

qsar_fish <- qsar_toxcast %>% filter(min_qsar_acute_pod_taxa == "fish")
hist(qsar_fish$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_fish$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_fish$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

qsar_fish$log10_qsar <- log10(qsar_fish$min_aquatic_qsar_acute_pod_mg_l)
hist(qsar_fish$log10_qsar)
qqnorm(qsar_fish$log10_qsar)
qqline(qsar_fish$log10_qsar, col = "steelblue", lwd = 2)

#inverts####
qsar_inverts <- qsar_toxcast %>% filter(min_qsar_acute_pod_taxa == "daphnia")
hist(qsar_inverts$`acc5_linear (mg/L)`)
qqnorm(qsar_inverts$`acc5_linear (mg/L)`)
qqline(qsar_inverts$`acc5_linear (mg/L)`, col = "steelblue", lwd = 2)

qsar_inverts$log10_acc <- log10(qsar_inverts$`acc5_linear (mg/L)`)
hist(qsar_inverts$log10_acc)
qqnorm(qsar_inverts$log10_acc)
qqline(qsar_inverts$log10_acc, col = "steelblue", lwd = 2)

qsar_inverts <- qsar_toxcast %>% filter(min_qsar_acute_pod_taxa == "daphnia")
hist(qsar_inverts$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_inverts$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_inverts$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

qsar_inverts$log10_qsar <- log10(qsar_inverts$min_aquatic_qsar_acute_pod_mg_l)
hist(qsar_inverts$log10_qsar)
qqnorm(qsar_inverts$log10_qsar)
qqline(qsar_inverts$log10_qsar, col = "steelblue", lwd = 2)

#plants####
qsar_plants <- qsar_toxcast %>% filter(min_qsar_acute_pod_taxa == "algae")
hist(qsar_plants$`acc5_linear (mg/L)`)
qqnorm(qsar_plants$`acc5_linear (mg/L)`)
qqline(qsar_plants$`acc5_linear (mg/L)`, col = "steelblue", lwd = 2)

qsar_plants$log10_acc <- log10(qsar_plants$`acc5_linear (mg/L)`)
hist(qsar_plants$log10_acc)
qqnorm(qsar_plants$log10_acc)
qqline(qsar_plants$log10_acc, col = "steelblue", lwd = 2)

hist(qsar_plants$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_plants$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_plants$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

qsar_plants$log_qsar <- log10(qsar_plants$min_aquatic_qsar_acute_pod_mg_l)
hist(qsar_plants$log_qsar)
qqnorm(qsar_plants$log_qsar)
qqline(qsar_plants$log_qsar, col = "steelblue", lwd = 2)

#QSAR-ToxCast Comparative Analyses####
#overall####
names(qsar_toxcast)

qsar_toxcast_corplot <- qsar_toxcast  %>%
  ggscatter("acc5_linear (mg/L)", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_toxcast_corplot

qsar_toxcast_logcorplot <- qsar_toxcast %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A")
qsar_toxcast_logcorplot

cor(qsar_toxcast$log10_acc, qsar_toxcast$log10_qsar)
test <- cor.test(qsar_toxcast$log10_acc, qsar_toxcast$log10_qsar)
test

qsar_toxcast_logcorplot_ext <- qsar_toxcast %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 10), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "B") + rremove("legend")

qsar_toxcast_logcorplot_ext

qsar_toxcast <- grid.arrange(qsar_toxcast_logcorplot,qsar_toxcast_logcorplot_ext)
ggsave("qsar_toxcast_04_12_2022.jpeg",qsar_toxcast, height = 15, width = 15)

#fish####
names(qsar_fish)

qsar_toxcast_fish_corplot <- qsar_fish  %>%
  ggscatter("acc5_linear (mg/L)", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_toxcast_fish_corplot

qsar_toxcast_fish_logcorplot <- qsar_fish %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Aquatic Vertebrate)")
qsar_toxcast_fish_logcorplot

cor(qsar_fish$log10_acc, qsar_fish$log10_qsar)
test <- cor.test(qsar_fish$log10_acc, qsar_fish$log10_qsar)
test

qsar_toxcast_fish_logcorplot_ext <- qsar_fish %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "D (Aquatic Vertebrate)") + rremove("legend")

qsar_toxcast_fish_logcorplot_ext

#invertebrates####
names(qsar_inverts)

qsar_toxcast_inverts_corplot <- qsar_inverts  %>%
  ggscatter("acc5_linear (mg/L)", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_toxcast_inverts_corplot

qsar_toxcast_inverts_logcorplot <- qsar_inverts %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Invertebrate)")
qsar_toxcast_inverts_logcorplot

cor(qsar_inverts$log10_acc, qsar_inverts$log10_qsar)
test <- cor.test(qsar_inverts$log10_acc, qsar_inverts$log10_qsar)
test

qsar_toxcast_inverts_logcorplot_ext <- qsar_inverts %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 10), cor.method = "pearson", 
            color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "E (Invertebrate)") + rremove("legend")

qsar_toxcast_inverts_logcorplot_ext

#plants####
names(qsar_plants)

qsar_toxcast_plants_corplot <- qsar_plants  %>%
  ggscatter("acc5_linear (mg/L)", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_toxcast_plants_corplot

qsar_toxcast_plants_logcorplot <- qsar_plants %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "C (Plant)")
qsar_toxcast_plants_logcorplot

cor(qsar_plants$log10_acc, qsar_plants$log10_qsar)
test <- cor.test(qsar_plants$log10_acc, qsar_plants$log10_qsar)
test

qsar_toxcast_plants_logcorplot_ext <- qsar_plants %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_acc", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 10), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", facet.by = "subclass_list_newest_for_classification", title = "F (Plant)") + rremove("legend")

qsar_toxcast_plants_logcorplot_ext

qsar_toxcast_spp <- grid.arrange(qsar_toxcast_fish_logcorplot, qsar_toxcast_inverts_logcorplot, qsar_toxcast_plants_logcorplot, 
                                 qsar_toxcast_fish_logcorplot_ext, qsar_toxcast_inverts_logcorplot_ext, qsar_toxcast_plants_logcorplot_ext, 
                                 nrow = 2)

ggsave("qsar_toxcast_spp_04_12_2022.jpeg", qsar_toxcast_spp, height = 15, width = 20)

#QSAR vs. Cytotox####
names(qsar_CB)
qsar_CB <- left_join(qsar_CB, classifications)
#data distributions####
#cytotoxic burst - overall####
hist(qsar_CB$`Cytotox_low bnd_mg_L`)
qqnorm(qsar_CB$`Cytotox_low bnd_mg_L`)
qqline(qsar_CB$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

qsar_CB$log10_cb <- log10(qsar_CB$`Cytotox_low bnd_mg_L` + 0.00001)
hist(qsar_CB$log10_cb)
qqnorm(qsar_CB$log10_cb)
qqline(qsar_CB$log10_cb, col = "steelblue", lwd = 2)

#qsar - overall####
qsar_CB$min_aquatic_qsar_acute_pod_mg_l <- as.numeric(qsar_CB$min_aquatic_qsar_acute_pod_mg_l)
hist(qsar_CB$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_CB$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_CB$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

qsar_CB$log10_qsar <- log10(qsar_CB$min_aquatic_qsar_acute_pod_mg_l)
hist(qsar_CB$log10_qsar)
qqnorm(qsar_CB$log10_qsar)
qqline(qsar_CB$log10_qsar, col = "steelblue", lwd = 2)

#fish####
qsar_CB_fish <- qsar_CB %>% filter(min_qsar_acute_pod_taxa == "fish")

#cytotox
hist(qsar_CB_fish$`Cytotox_low bnd_mg_L`)
qqnorm(qsar_CB_fish$`Cytotox_low bnd_mg_L`)
qqline(qsar_CB_fish$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

qsar_CB_fish$log10_cb <- log10(qsar_CB_fish$`Cytotox_low bnd_mg_L` + 0.00001)
hist(qsar_CB_fish$log10_cb)
qqnorm(qsar_CB_fish$log10_cb)
qqline(qsar_CB_fish$log10_cb, col = "steelblue", lwd = 2)

#qsar
hist(qsar_CB_fish$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_CB_fish$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_CB_fish$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_CB_fish$log10_qsar)
qqnorm(qsar_CB_fish$log10_qsar)
qqline(qsar_CB_fish$log10_qsar, col="steelblue", lwd = 2)


#invertebrates####
qsar_CB_inverts <- qsar_CB %>% filter(min_qsar_acute_pod_taxa == "daphnia")

#cytotox 
hist(qsar_CB_inverts$`Cytotox_low bnd_mg_L`)
qqnorm(qsar_CB_inverts$`Cytotox_low bnd_mg_L`)
qqline(qsar_CB_inverts$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

hist(qsar_CB_inverts$log10_cb)
qqnorm(qsar_CB_inverts$log10_cb)
qqline(qsar_CB_inverts$log10_cb, col = "steelblue", lwd = 2)

#qsar 
hist(qsar_CB_inverts$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_CB_inverts$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_CB_inverts$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_CB_inverts$log10_qsar)
qqnorm(qsar_CB_inverts$log10_qsar)
qqline(qsar_CB_inverts$log10_qsar, col = "steelblue", lwd = 2)


#plants####
qsar_CB_plants <- qsar_CB %>% filter(min_qsar_acute_pod_taxa == "algae")

#qsar
hist(qsar_CB_plants$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_CB$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_CB$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_CB_plants$log10_qsar)
qqnorm(qsar_CB$log10_qsar)
qqline(qsar_CB$log10_qsar, col = "steelblue", lwd = 2)

#cytotoxic burst
hist(qsar_CB_plants$`Cytotox_low bnd_mg_L`)
qqnorm(qsar_CB$`Cytotox_low bnd_mg_L`)
qqline(qsar_CB$`Cytotox_low bnd_mg_L`, col = "steelblue", lwd = 2)

hist(qsar_CB_plants$log10_cb)
qqnorm(qsar_CB$log10_cb)
qqline(qsar_CB$log10_cb, col = "steelblue", lwd = 2)

#QSAR_Cytotox_Correlations####
#overall####
names(qsar_CB)
qsar_CB_corplot <- qsar_CB  %>%
  ggscatter("Cytotox_low bnd_mg_L", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_CB_corplot

qsar_CB_logcorplot <- qsar_CB %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A")
qsar_CB_logcorplot

cor(qsar_CB$log10_cb, qsar_CB$log10_qsar)
test <- cor.test(qsar_CB$log10_cb, qsar_CB$log10_qsar)
test

qsar_CB_logcorplot_ext <- qsar_CB %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "B")+ rremove("legend")


qsar_CB_logcorplot_ext

qsar_CB <- grid.arrange(qsar_CB_logcorplot, qsar_CB_logcorplot_ext)
ggsave("qsar_CB_05_12_2022.jpeg",qsar_CB, height = 15, width=15)

#fish####
names(qsar_CB_fish)
qsar_CB_fish_corplot <- qsar_CB_fish  %>%
  ggscatter("Cytotox_low bnd_mg_L", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_CB_fish_corplot

qsar_CB_fish_logcorplot <- qsar_CB_fish %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Aquatic Vertebrate)")
qsar_CB_fish_logcorplot

cor(qsar_CB_fish$log10_cb, qsar_CB_fish$log10_qsar)
test <- cor.test(qsar_CB_fish$log10_cb, qsar_CB_fish$log10_qsar)
test

qsar_CB_fish_logcorplot_ext <- qsar_CB_fish %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "D (Aquatic Vertebrate)") + rremove("legend")

qsar_CB_fish_logcorplot_ext

#inverts####
names(qsar_CB_inverts)
qsar_CB_inverts_corplot <- qsar_CB_inverts  %>%
  ggscatter("Cytotox_low bnd_mg_L", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_CB_inverts_corplot

qsar_CB_inverts_logcorplot <- qsar_CB_inverts %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Invertebrate)")
qsar_CB_inverts_logcorplot

cor(qsar_CB_inverts$log10_cb, qsar_CB_inverts$log10_qsar)
test <- cor.test(qsar_CB_inverts$log10_cb, qsar_CB_inverts$log10_qsar)
test

qsar_CB_inverts_logcorplot_ext <- qsar_CB_inverts %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "E (Invertebrate)") + rremove("legend")

qsar_CB_inverts_logcorplot_ext

#plants####
names(qsar_CB_plants)
qsar_CB_plants_corplot <- qsar_CB_plants  %>%
  ggscatter("Cytotox_low bnd_mg_L", "min_aquatic_qsar_acute_pod_mg_l",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_CB_plants_corplot

qsar_CB_plants_logcorplot <- qsar_CB_plants %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "C (Plant)")
qsar_CB_plants_logcorplot

cor(qsar_CB_plants$log10_cb, qsar_CB_plants$log10_qsar)
test <- cor.test(qsar_CB_plants$log10_cb, qsar_CB_plants$log10_qsar)
test

qsar_CB_plants_logcorplot_ext <- qsar_CB_plants %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_cb", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "F (Plant)") +rremove("legend")

qsar_CB_plants_logcorplot_ext

qsar_CB_spp <- grid.arrange(qsar_CB_fish_logcorplot, qsar_CB_inverts_logcorplot,
                            qsar_CB_plants_logcorplot, 
                            qsar_CB_fish_logcorplot_ext, qsar_CB_inverts_logcorplot_ext, 
                            qsar_CB_plants_logcorplot_ext, ncol = 3)

ggsave("qsar_CB_spp_05_12_2022.jpeg",qsar_CB_spp, height = 15, width = 20)

#QSAR vs. ECOTOX####
names(qsar_ECOTOX)
qsar_ECOTOX <- left_join(qsar_ECOTOX, classifications)
qsar_ECOTOX$log10_minEC <- log10(qsar_ECOTOX$min_EC)
qsar_ECOTOX$min_aquatic_qsar_acute_pod_mg_l <- as.numeric(qsar_ECOTOX$min_aquatic_qsar_acute_pod_mg_l)
qsar_ECOTOX$log10_qsar <- log10(qsar_ECOTOX$min_aquatic_qsar_acute_pod_mg_l)
View(qsar_ECOTOX)

#data distributions####
#overall####
qsar_ECOTOX_overall <- qsar_ECOTOX %>% filter(pod_type == "overall")

#qsar
hist(qsar_ECOTOX_overall$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_ECOTOX_overall$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_ECOTOX_overall$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_overall$log10_qsar)
qqnorm(qsar_ECOTOX_overall$log10_qsar)
qqline(qsar_ECOTOX_overall$log10_qsar, col = "steelblue", lwd =2)

#ecotox
hist(qsar_ECOTOX_overall$min_EC)
qqnorm(qsar_ECOTOX_overall$min_EC)
qqline(qsar_ECOTOX_overall$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_overall$log10_minEC)
qqnorm(qsar_ECOTOX_overall$log10_minEC)
qqline(qsar_ECOTOX_overall$log10_minEC, col = "steelblue", lwd = 2)

#tier1tier2####
qsar_ECOTOX_t1 <- qsar_ECOTOX %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_1")
qsar_ECOTOX_t2 <- qsar_ECOTOX %>% filter(pod_type == "tier1_tier2", EFFECT_TYPE == "TIER_2")

#qsar
hist(qsar_ECOTOX_t1$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_ECOTOX_t1$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_ECOTOX_t1$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_t1$log10_qsar)
qqnorm(qsar_ECOTOX_t1$log10_qsar)
qqline(qsar_ECOTOX_t1$log10_qsar, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_t2$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_ECOTOX_t2$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_ECOTOX_t2$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_t2$log10_qsar)
qqnorm(qsar_ECOTOX_t2$log10_qsar)
qqline(qsar_ECOTOX_t2$log10_qsar, col = "steelblue", lwd = 2)

#ecotox
qsar_ECOTOX_t1 <- qsar_ECOTOX_t1 %>% filter(log10_minEC != -Inf)
View(qsar_ECOTOX_t1$log10_minEC)
hist(qsar_ECOTOX_t1$min_EC)
qqnorm(qsar_ECOTOX_t1$min_EC)
qqline(qsar_ECOTOX_t1$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_t1$log10_minEC)
qqnorm(qsar_ECOTOX_t1$log10_minEC)
qqline(qsar_ECOTOX_t1$log10_minEC, col = "steelblue", lwd = 2)

qsar_ECOTOX_t2 <- qsar_ECOTOX_t2 %>% filter(log10_minEC != -Inf)
hist(qsar_ECOTOX_t2$min_EC)
qqnorm(qsar_ECOTOX_t2$min_EC)
qqline(qsar_ECOTOX_t2$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_t2$log10_minEC)
qqnorm(qsar_ECOTOX_t2$log10_minEC)
qqline(qsar_ECOTOX_t2$log10_minEC, col = "steelblue", lwd = 2)


#acute_chronic####
names(qsar_ECOTOX)
list(unique(qsar_ECOTOX$pod_type))
list(unique(qsar_ECOTOX$min_qsar_acute_pod_taxa))
list(unique(qsar_ECOTOX$STUDY_LENGTH))
#acute
qsar_ECOTOX_acute <- qsar_ECOTOX %>% filter(STUDY_LENGTH == "Acute", !min_qsar_acute_pod_taxa %in% c("rodent", NA, "-"))
hist(qsar_ECOTOX_acute$min_EC)
qqnorm(qsar_ECOTOX_acute$min_EC)
qqline(qsar_ECOTOX_acute$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_acute$log10_qsar)
qqnorm(qsar_ECOTOX_acute$log10_qsar)
qqline(qsar_ECOTOX_acute$log10_qsar, col = "steelblue", lwd = 2)

#chronic
qsar_ECOTOX_chronic <- qsar_ECOTOX %>% filter(pod_type == "acute_chronic", STUDY_LENGTH == "Chronic", !min_qsar_acute_pod_taxa %in% c("rodent", NA, "-"))
hist(qsar_ECOTOX_chronic$min_EC)
qqnorm(qsar_ECOTOX_chronic$min_EC)
qqline(qsar_ECOTOX_chronic$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_chronic$log10_qsar)
qqnorm(qsar_ECOTOX_chronic$log10_qsar)
qqline(qsar_ECOTOX_chronic$log10_qsar, col = "steelblue", lwd = 2)

#species####
names(qsar_ECOTOX)
list(unique(qsar_ECOTOX$pod_type))
qsar_ECOTOX_fish <- qsar_ECOTOX %>% filter(pod_type == "species", ORGANISM_TYPE == "Fish/Frogs", min_qsar_acute_pod_taxa == "fish")  
qsar_ECOTOX_inverts <- qsar_ECOTOX %>% filter(pod_type == "species", ORGANISM_TYPE == "Invertebrates", min_qsar_acute_pod_taxa == "daphnia")  
qsar_ECOTOX_plants <- qsar_ECOTOX %>% filter(pod_type == "species", ORGANISM_TYPE == "Plants", min_qsar_acute_pod_taxa == "algae")  

#fish
hist(qsar_ECOTOX_fish$min_EC)
qqnorm(qsar_ECOTOX_fish$min_EC)
qqline(qsar_ECOTOX_fish$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_fish$log10_minEC)
qqnorm(qsar_ECOTOX_fish$log10_minEC)
qqline(qsar_ECOTOX_fish$log10_minEC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_fish$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_ECOTOX_fish$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_ECOTOX_fish$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_fish$log10_qsar)
qqnorm(qsar_ECOTOX_fish$log10_qsar)
qqline(qsar_ECOTOX_fish$log10_qsar, col = "steelblue", lwd = 2)

#inverts
hist(qsar_ECOTOX_inverts$min_EC)
qqnorm(qsar_ECOTOX_inverts$min_EC)
qqline(qsar_ECOTOX_inverts$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_inverts$log10_minEC)
qqnorm(qsar_ECOTOX_inverts$log10_minEC)
qqline(qsar_ECOTOX_inverts$log10_minEC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_inverts$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_ECOTOX_inverts$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_ECOTOX_inverts$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_inverts$log10_qsar)
qqnorm(qsar_ECOTOX_inverts$log10_qsar)
qqline(qsar_ECOTOX_inverts$log10_qsar, col = "steelblue", lwd = 2)

#plants
hist(qsar_ECOTOX_plants$min_EC)
qqnorm(qsar_ECOTOX_plants$min_EC)
qqline(qsar_ECOTOX_plants$min_EC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_plants$log10_minEC)
qqnorm(qsar_ECOTOX_plants$log10_minEC)
qqline(qsar_ECOTOX_plants$log10_minEC, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_plants$min_aquatic_qsar_acute_pod_mg_l)
qqnorm(qsar_ECOTOX_plants$min_aquatic_qsar_acute_pod_mg_l)
qqline(qsar_ECOTOX_plants$min_aquatic_qsar_acute_pod_mg_l, col = "steelblue", lwd = 2)

hist(qsar_ECOTOX_plants$log10_qsar)
qqnorm(qsar_ECOTOX_plants$log10_qsar)
qqline(qsar_ECOTOX_plants$log10_qsar, col = "steelblue", lwd = 2)

#correlation####
#overall####
names(qsar_ECOTOX_overall)
qsar_ECOTOX_corplot <- qsar_ECOTOX_overall  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_corplot

qsar_ECOTOX_logcorplot <- qsar_ECOTOX_overall %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A")
qsar_ECOTOX_logcorplot

cor(qsar_ECOTOX_overall$log10_minEC, qsar_ECOTOX_overall$log10_qsar)
test <- cor.test(qsar_ECOTOX_overall$log10_minEC, qsar_ECOTOX_overall$log10_qsar)
test

qsar_ECOTOX_logcorplot_ext <- qsar_ECOTOX_overall %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 7.5),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "B") + rremove("legend")

qsar_ECOTOX_logcorplot_ext

qsar_ECOTOX <- grid.arrange(qsar_ECOTOX_logcorplot, qsar_ECOTOX_logcorplot_ext)
ggsave("qsar_ECOTOX_05_12_2022.jpeg", qsar_ECOTOX, height = 15, width = 15)

#tier1tier2####
#tier 1
names(qsar_ECOTOX_t1)
qsar_ECOTOX_t1_corplot <- qsar_ECOTOX_t1  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_t1_corplot

qsar_ECOTOX_t1_logcorplot <- qsar_ECOTOX_t1 %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Tier 1)")
qsar_ECOTOX_t1_logcorplot

cor(qsar_ECOTOX_t1$log10_minEC, qsar_ECOTOX_t1$log10_qsar)
test <- cor.test(qsar_ECOTOX_t1$log10_minEC, qsar_ECOTOX_t1$log10_qsar)
test

qsar_ECOTOX_t1_logcorplot_ext <- qsar_ECOTOX_t1 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 10),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "C (Tier 1)") + ylim(-5, 12) + rremove("legend")

qsar_ECOTOX_t1_logcorplot_ext

#tier 2
names(qsar_ECOTOX_t2)
qsar_ECOTOX_t2_corplot <- qsar_ECOTOX_t2  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_t2_corplot

qsar_ECOTOX_t2_logcorplot <- qsar_ECOTOX_t2 %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Tier 2)")
qsar_ECOTOX_t2_logcorplot

cor(qsar_ECOTOX_t2$log10_minEC, qsar_ECOTOX_t2$log10_qsar)
test <- cor.test(qsar_ECOTOX_t2$log10_minEC, qsar_ECOTOX_t2$log10_qsar)
test

qsar_t2_ECOTOX_logcorplot_ext <- qsar_ECOTOX_t2 %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "D (Tier 2)") + rremove("legend")

qsar_t2_ECOTOX_logcorplot_ext

qsar_ecotox_t1t2 <- grid.arrange(qsar_ECOTOX_t1_logcorplot, qsar_ECOTOX_t2_logcorplot, qsar_ECOTOX_t1_logcorplot_ext,qsar_t2_ECOTOX_logcorplot_ext)
ggsave("qsar_ecotox_t1t2_06_12_2022.jpeg", qsar_ecotox_t1t2, height = 15, width = 15)

#acute_chronic####
names(qsar_ECOTOX_acute)
names(qsar_ECOTOX_chronic)

qsar_ECOTOX_acute$log10_qsar <- log10(qsar_ECOTOX_acute$min_aquatic_qsar_acute_pod_mg_l)
qsar_ECOTOX_chronic$log10_qsar <- log10(qsar_ECOTOX_chronic$min_aquatic_qsar_acute_pod_mg_l)
qsar_ECOTOX_acute$log10_minEC <- log10(qsar_ECOTOX_acute$min_EC)
qsar_ECOTOX_chronic$log10_minEC <- log10(qsar_ECOTOX_chronic$min_EC)

#acute
qsar_ECOTOX_acute_corplot <- qsar_ECOTOX_acute  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            qsar_ECOTOX_acute_corplot = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_corplot

qsar_ECOTOX_acute_logcorplot <- qsar_ECOTOX_acute %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (acute)")
qsar_ECOTOX_acute_logcorplot

cor(qsar_ECOTOX_acute$log10_minEC, qsar_ECOTOX_acute$log10_qsar)
test <- cor.test(qsar_ECOTOX_acute$log10_minEC, qsar_ECOTOX_acute$log10_qsar)
test

qsar_ECOTOX_acute_logcorplot_ext <- qsar_ECOTOX_acute %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 10), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "C (acute)") + rremove("legend")

qsar_ECOTOX_acute_logcorplot_ext

#chronic
qsar_ECOTOX_chronic_corplot <- qsar_ECOTOX_chronic  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            qsar_ECOTOX_acute_corplot = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_chronic_corplot

qsar_ECOTOX_chronic_logcorplot <- qsar_ECOTOX_chronic %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (chronic)")
qsar_ECOTOX_chronic_logcorplot

cor(qsar_ECOTOX_chronic$log10_minEC, qsar_ECOTOX_chronic$log10_qsar)
test <- cor.test(qsar_ECOTOX_chronic$log10_minEC, qsar_ECOTOX_chronic$log10_qsar)
test

qsar_ECOTOX_chronic_logcorplot_ext <- qsar_ECOTOX_chronic %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 10),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "D (chronic)") + rremove("legend")

qsar_ECOTOX_chronic_logcorplot_ext

qsar_ECOTOX_acutechronic <- grid.arrange(qsar_ECOTOX_acute_logcorplot, qsar_ECOTOX_chronic_logcorplot,
                                         qsar_ECOTOX_acute_logcorplot_ext, qsar_ECOTOX_chronic_logcorplot_ext)

ggsave("qsar_ECOTOX_acutechronic_06_12_2022.jpeg", qsar_ECOTOX_acutechronic, height = 15, width = 15)

#species####
names(qsar_ECOTOX_fish)
names(qsar_ECOTOX_inverts)
names(qsar_ECOTOX_plants)

#fish
qsar_ECOTOX_fish_corplot <- qsar_ECOTOX_fish  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_fish_corplot

qsar_ECOTOX_fish_logcorplot <- qsar_ECOTOX_fish %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "A (Aquatic Vertebrate)")
qsar_ECOTOX_fish_logcorplot

cor(qsar_ECOTOX_fish$log10_minEC, qsar_ECOTOX_fish$log10_qsar)
test <- cor.test(qsar_ECOTOX_fish$log10_minEC, qsar_ECOTOX_fish$log10_qsar)
test

qsar_ECOTOX_fish_logcorplot_ext <- qsar_ECOTOX_fish %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 5), 
            cor.method = "pearson", color = "subclass_list_newest_for_classification", 
            facet.by = "subclass_list_newest_for_classification", title = "D (Aquatic Vertebrate)")

qsar_ECOTOX_fish_logcorplot_ext

#inverts
qsar_ECOTOX_inverts_corplot <- qsar_ECOTOX_inverts  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_inverts_corplot

qsar_ECOTOX_inverts_logcorplot <- qsar_ECOTOX_inverts %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "B (Invertebrate)")
qsar_ECOTOX_inverts_logcorplot

cor(qsar_ECOTOX_inverts$log10_minEC, qsar_ECOTOX_inverts$log10_qsar)
test <- cor.test(qsar_ECOTOX_inverts$log10_minEC, qsar_ECOTOX_inverts$log10_qsar)
test

qsar_ECOTOX_inverts_logcorplot_ext <- qsar_ECOTOX_inverts %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 8.5),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "E (Invertebrate)") + rremove("legend")

qsar_ECOTOX_inverts_logcorplot_ext

#plants
qsar_ECOTOX_plants_corplot <- qsar_ECOTOX_plants  %>%
  ggscatter("min_aquatic_qsar_acute_pod_mg_l", "min_EC",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "spearman")
qsar_ECOTOX_plants_corplot

qsar_ECOTOX_plants_logcorplot <- qsar_ECOTOX_plants %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", title = "C (Plant)")
qsar_ECOTOX_plants_logcorplot

cor(qsar_ECOTOX_plants$log10_minEC, qsar_ECOTOX_plants$log10_qsar)
test <- cor.test(qsar_ECOTOX_plants$log10_minEC, qsar_ECOTOX_plants$log10_qsar, method = "pearson")
test

qsar_ECOTOX_plants_logcorplot_ext <- qsar_ECOTOX_plants %>% filter(subclass_list_newest_for_classification != "NA") %>%
  ggscatter("log10_minEC", "log10_qsar",  add = "reg.line",
            conf.int = TRUE, cor.coef = TRUE, cor.coef.coord = c(-7.5, 8.5),
            cor.method = "pearson", color = "subclass_list_newest_for_classification",
            facet.by = "subclass_list_newest_for_classification", title = "F (Plant)") + rremove("legend")


qsar_ECOTOX_plants_logcorplot_ext


qsar_ECOTOX_spp <- grid.arrange(qsar_ECOTOX_fish_logcorplot, qsar_ECOTOX_inverts_logcorplot,
                                qsar_ECOTOX_plants_logcorplot,
                                qsar_ECOTOX_fish_logcorplot_ext,
                                 qsar_ECOTOX_inverts_logcorplot_ext,
                                 qsar_ECOTOX_plants_logcorplot_ext, 
                                ncol = 3)

ggsave("qsar_ECOTOX_spp_06_12_2022.jpeg", qsar_ECOTOX_spp, height = 15, width = 20)
