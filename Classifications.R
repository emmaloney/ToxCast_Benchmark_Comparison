#Adding Chemical Classifications to ECOTOX-TOXCAST POD

library(dplyr)
library(tidyverse)
library(readxl)
library(writexl)

master_cas_list <- read_excel("Chemical Classifications\\Master Workbook_ECOTOX_ToxCast Comparisons.xlsx", "Comparisons") %>%
  filter(`acc5_linear (mg/L)` != 0, `ECOTOX 5th centile (mg/L)` != 0, !is.na(`ECOTOX 5th centile (mg/L)`)) %>%
  select("dtxsid", "CASRN", "PREFERRED_NAME") %>% rename("casrn" = "CASRN") 
View(master_cas_list)

#Bind in GLRI Classifications####
GLRI_classifications <- read_excel("Chemical Classifications\\chemical_vw_out.xlsx") %>% select("dtxsid", "casrn",
                                                                                                "chemical_class_t_name",
                                                                                                "subclass_list")
names(GLRI_classifications)

master_cas_list_w_descriptors <- left_join(master_cas_list, GLRI_classifications) %>% distinct()
master_cas_list_w_descriptors$chemical_class_t_name <- as.factor(master_cas_list_w_descriptors$chemical_class_t_name)
summary(master_cas_list_w_descriptors$chemical_class_t_name) #435 NA's 
master_cas_list_w_descriptors$class_source <- "GLRI_DB"

#SPLIT INTO DL and non_DL
master_cas_list_w_descriptors_noNA <- master_cas_list_w_descriptors %>% filter(!is.na(chemical_class_t_name))
View(master_cas_list_w_descriptors_noNA)
master_cas_list_w_descriptors_NA <- master_cas_list_w_descriptors %>% filter(is.na(chemical_class_t_name)) %>%
  select(-class_source, -chemical_class_t_name, -subclass_list)


#Bind in COMPTOX Classifications####
COMPTOX_classifications <- read_excel("Chemical Classifications\\CCD-Batch-Phase III Chemicals_wAssayHits.xlsx", "Main Data") %>%
  select(-c(INPUT, FOUND_BY, PREFERRED_NAME, QC_LEVEL, "TOXCAST_NUMBER_OF_ASSAYS/TOTAL", "TOXCAST_PERCENT_ACTIVE")) %>%
  rename("dtxsid"="DTXSID", "casrn" = "CASRN")
names(COMPTOX_classifications)

list(unique(COMPTOX_classifications$ANTIBIOTICS))
list(unique(master_cas_list_w_descriptors$chemical_class_t_name))

COMPTOX_classifications$ANTIBIOTICS <- ifelse(COMPTOX_classifications$ANTIBIOTICS == "Y", "PPCP", "NA")
COMPTOX_classifications$ANTIMICROBIALS <- ifelse(COMPTOX_classifications$ANTIMICROBIALS == "Y", "PPCP", "NA")
COMPTOX_classifications$COSMOSDB <- ifelse(COMPTOX_classifications$COSMOSDB == "Y", "PPCP", "NA")
COMPTOX_classifications$DRUGBANK <- ifelse(COMPTOX_classifications$DRUGBANK == "Y", "PPCP", "NA")
COMPTOX_classifications$FLAMERETARD <- ifelse(COMPTOX_classifications$FLAMERETARD == "Y", "Flame Retardant", "NA")
COMPTOX_classifications$HUMANHORMONES <- ifelse(COMPTOX_classifications$HUMANHORMONES == "Y", "Hormone", "NA")
COMPTOX_classifications$PAHLIST <- ifelse(COMPTOX_classifications$PAHLIST == "Y", "PAH", "NA")
COMPTOX_classifications$PBDES <- ifelse(COMPTOX_classifications$PBDES == "Y", "PBDE", "NA")
COMPTOX_classifications$PCBCHEMICALS <- ifelse(COMPTOX_classifications$PCBCHEMICALS == "Y", "PCB", "NA")
COMPTOX_classifications$PYRETHROIDS <- ifelse(COMPTOX_classifications$PYRETHROIDS == "Y", "Pesticide", "NA")
COMPTOX_classifications$WIKIANTIFUNGALS <- ifelse(COMPTOX_classifications$WIKIANTIFUNGALS == "Y", "PPCP", "NA")
COMPTOX_classifications$WIKIANTISEPTICS <- ifelse(COMPTOX_classifications$WIKIANTISEPTICS == "Y", "PPCP", "NA")
COMPTOX_classifications$WIKIANTIVIRALS <- ifelse(COMPTOX_classifications$WIKIANTIVIRALS == "Y", "PPCP", "NA")
COMPTOX_classifications$WIKIHERBICIDES <- ifelse(COMPTOX_classifications$WIKIHERBICIDES == "Y", "Pesticide", "NA")
COMPTOX_classifications$WIKIINSECTICIDES <- ifelse(COMPTOX_classifications$WIKIINSECTICIDES == "Y", "Pesticide", "NA")
COMPTOX_classifications$WIKINSAIDS <- ifelse(COMPTOX_classifications$WIKINSAIDS == "Y", "PPCP", "NA")
COMPTOX_classifications$WIKIRODENTICIDES <- ifelse(COMPTOX_classifications$WIKIRODENTICIDES == "Y", "Pesticide", "NA")
COMPTOX_classifications$ZINC15PHARMA <- ifelse(COMPTOX_classifications$ZINC15PHARMA == "Y", "PPCP", "NA")
COMPTOX_classifications$WIKIFLAVORS <- ifelse(COMPTOX_classifications$WIKIFLAVORS == "Y", "Flavours", "NA")
COMPTOX_classifications$ALLSURFACTANTS <- ifelse(COMPTOX_classifications$ALLSURFACTANTS == "Y", "Surfactants", "NA")
COMPTOX_classifications$FDAFOODSUBS <- ifelse(COMPTOX_classifications$FDAFOODSUBS == "Y", "Food Substituents", "NA")
COMPTOX_classifications$WIKISOLVENTS <- ifelse(COMPTOX_classifications$WIKISOLVENTS == "Y", "Solvents", "NA")
COMPTOX_classifications$FLAVORNET <- ifelse(COMPTOX_classifications$FLAVORNET == "Y", "Flavours", "NA")

COMPTOX_classifications_1 <- COMPTOX_classifications %>% select(-c(AMINOACIDS, OPPIN, VITAMINS)) %>% 
  gather(ALLSURFACTANTS:ZINC15PHARMA, key = "class_source", value = "chemical_class_t_name") %>%
  filter(chemical_class_t_name != "NA")

#gather chemicals with remaining NA classifications in master_list & bind in new classes based on COMPTOX
master_cas_list_w_descriptors_NA_1 <- left_join(master_cas_list_w_descriptors_NA, COMPTOX_classifications_1)
master_cas_list_w_descriptors_NA_1$chemical_class_t_name <- as.factor(master_cas_list_w_descriptors_NA_1$chemical_class_t_name)
summary(master_cas_list_w_descriptors_NA_1$chemical_class_t_name) #258 NAs
View(master_cas_list_w_descriptors_NA_1)

#summarize chemicals with multiple classifications & pull out####
names(master_cas_list_w_descriptors_NA_1)
multiple_class_chemicals <- master_cas_list_w_descriptors_NA_1 %>% group_by(dtxsid, casrn, PREFERRED_NAME) %>%
  summarize(n_class = n_distinct(chemical_class_t_name)) %>% filter(n_class > 1)
multiple_class_chemicals$FLAG <- "multiple class"

master_cas_list_w_descriptors_NA_2 <- left_join(master_cas_list_w_descriptors_NA_1, (multiple_class_chemicals %>% select(-n_class)))
View(master_cas_list_w_descriptors_NA_2)
master_cas_list_w_descriptors_NA_2_multiclass <- master_cas_list_w_descriptors_NA_2 %>% filter(FLAG == "multiple class")
master_cas_list_w_descriptors_NA_2_nomulticlass <- master_cas_list_w_descriptors_NA_2 %>% filter(is.na(FLAG)) %>% select(-class_source) %>% distinct()
master_cas_list_w_descriptors_NA_2_nomulticlass$class_source <- "COMPTOX"
View(master_cas_list_w_descriptors_NA_2_nomulticlass)

master_cas_list_w_descriptors_NA_2_multiclass_annotated <- master_cas_list_w_descriptors_NA_2_multiclass %>% 
  select(casrn, dtxsid, PREFERRED_NAME) %>% distinct()

master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- "Unknown"
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Acifluorfen",
                                                                                        "Pesticide", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "2,4-Dinitrophenol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Sodium nitrite",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "1,2-Propylene glycol",
                                                                                        "Multi-Use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Boric acid (H3BO3)",
                                                                                        "Multi-Use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Salicylaldehyde",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Cyclohexylamine",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "3-Phenylprop-2-enal",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Sodium dodecyl sulfate",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Calcium propionate",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "2-Phenylphenol",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Resorcinol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Dimethyl sulfoxide",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "1-Propanol",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "2-Undecanone",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Morpholine",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Limonene",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Zinc chloride",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "4-Hydroxybenzoic acid butyl ester",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Nonanoic acid",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Diethanolamine",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Nerolidol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Diethylene glycol monomethyl ether",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "4-Hydroxybenzoic acid",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Urea",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Potassium nitrate",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Ammonium sulfamate",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Acetic acid",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "2,2-Dibromo-3-nitrilopropionamide",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "1,2-Benzisothiazolin-3-one",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Benzoic acid",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "N-Methyl-2-pyrrolidone",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "4-Aminobenzoic acid",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Glutaraldehyde",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Methyl 2-aminobenzoate",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "m-Xylene",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Octylbicycloheptenedicarboximide",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Tetramethrin",
                                                                                        "Pesticide", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Thymol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Chlorpropham",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Nitrofen",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Benzonitrile",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "1-Octanol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "1-Decanol",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Sodium propionate",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Salicylic acid",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Deltamethrin",
                                                                                        "Pesticide", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Diethylene glycol",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Ethylene glycol",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Ferric chloride",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "o-Cresol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Levythyroxine",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "3,5,3'-Triiodothyronine",
                                                                                        "Hormone", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "1-Methyl-3-phenyl-5-(3-(trifluoromethyl)phenyl)-4-pyridone",
                                                                                        "Pesticide", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Bronopol",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Propionic acid",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Benzyl benzoate",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Allethrin",
                                                                                        "Pesticide", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Zinc acetate",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "2,6-Dimethylphenol",
                                                                                        "PPCP", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)
master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name <- ifelse(master_cas_list_w_descriptors_NA_2_multiclass_annotated$PREFERRED_NAME == "Butylene carbonate",
                                                                                        "Multi-use", master_cas_list_w_descriptors_NA_2_multiclass_annotated$chemical_class_t_name)

master_cas_list_w_descriptors_NA_2_multiclass_annotated$class_source <- "COMPTOX EM annotated"


#Pull together what we have so far####
master_cas_list_w_descriptors_noNA
master_cas_list_w_descriptors_NA_2_nomulticlass
master_cas_list_w_descriptors_NA_2_multiclass_annotated

master_class_list_for_annotation <- bind_rows(master_cas_list_w_descriptors_noNA, master_cas_list_w_descriptors_NA_2_nomulticlass, master_cas_list_w_descriptors_NA_2_multiclass_annotated) %>% select(-FLAG) %>% distinct()
master_class_list_for_annotation$class_source <- ifelse(is.na(master_class_list_for_annotation$chemical_class_t_name), NA, master_class_list_for_annotation$class_source)
View(master_class_list_for_annotation)
master_class_list_for_annotation$chemical_class_t_name <- as.factor(master_class_list_for_annotation$chemical_class_t_name)
summary(master_class_list_for_annotation$chemical_class_t_name) # still 258 chemicals have NA 

write_xlsx(master_class_list_for_annotation, "master_class_list_for_annotation.xlsx")

#read in annotated chemical list from CS & EM ####

EM_annotated <- read_excel("Chemical Classifications\\master_class_list_for_annotation_EM_annotated.xlsx") %>% filter(!is.na(chemical_class_t_name))
names(EM_annotated)
CS_annotated <- read_excel("Chemical Classifications\\Master_class_list_for_annotation_EM_CMS_annotated.xlsx") %>% filter(class_source == "CMS")
list(unique(CS_annotated$class_source))

all_annotated <- bind_rows(EM_annotated, CS_annotated) %>% rename("CASRN" = "casrn")
View(all_annotated)

#bind classifications into workbook###
master_comparison_list <- read_excel("Chemical Classifications\\Master Workbook_ECOTOX_ToxCast Comparisons.xlsx", "Comparisons") 
names(master_comparison_list)
names(all_annotated)

annotated_master_comparison <- left_join(master_comparison_list, all_annotated)
write_xlsx(annotated_master_comparison, "Comparison_list_with_Classifications.xlsx")
