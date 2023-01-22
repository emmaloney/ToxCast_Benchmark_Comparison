#ECOTOX POD for comparison to ToxCast ####

library(tidyverse)
library(dplyr)
library(ggpubr)
library(readxl)
library(writexl)

#read in ECOTOX data pull####
ecotox_data <- read_csv("Background\\toxcast-aquatic.csv")

names(ecotox_data)

#filter out formulations, co-exposures, fungi+bacteria, & approximate values####
list(unique(ecotox_data_1$TEST_GRADE_DESC))
list(unique(ecotox_data$EXPOSURE_TYPE_DESC))
list(unique(ecotox_data$CONC1_MEAN_STD_OP))
list(unique(ecotox_data$TEST_PURITY_MEAN))
list(unique(ecotox_data$KINGDOM))
list(unique(ecotox_data_1$EXPOSURE_TYPE_DESC))

ecotox_data$TEST_PURITY_MEAN <- gsub("/", "", ecotox_data$TEST_PURITY_MEAN)
ecotox_data$TEST_PURITY_MEAN <- as.numeric(ecotox_data$TEST_PURITY_MEAN)
names(ecotox_data)

ecotox_data_1 <- ecotox_data %>%
  filter(!CONC1_MEAN_STD_OP %in% c(">", "<", "<=", "~", ">="),
                                        !TEST_GRADE_DESC %in% c("Formulated grade", "Not reported"),
                                        TEST_PURITY_MEAN >= 85.00 | is.na(TEST_PURITY_MEAN),
                                        KINGDOM %in% c("Animalia", "Chromista", "Plantae", "Community"),
                                        !EXPOSURE_TYPE_DESC %in% c("Not reported"),
         !CONC1_TYPE_STD_DESC %in% c("Formulation"), 
         !TEST_FORMULATION_DESC %in% c("Emulsifiable concentrate", "Suspension concentrate (also known as Flowable concentrate)", "Emulsion", "Encapsulated",
                                       "Formulated", "Wettable Powder or Water Dispersible Powder", "Water Dispersible Granule (also known as wettable granule)",
                                       "Granule, granular", "Flowable formulation", "Volume per volume", "Powder", "Soluble powder", "Weight per volume",
                                       "Weight per weight", "Finely ground", "Liquid", "Crystal", "Commercial", "Aqueous suspension", "Concentrate",
                                       "Water soluble"),
         !EECOMMENT %in% c("MSMT/ORGANISMS EXPOSED AT A UVB INTENSITY OF 70.0 U W/CM^2.//", "MSMT/ORGANISMS EXPOSED AT A UVB INTENSITY OF 170.0 U W/CM^2.//", "MSMT/ORGANISMS EXPOSED AT A UVB INTENSITY OF 14.8 U W/CM^2.//",
                           "MSMT/CUMULATIVE UV LT50, ESTIMATED FROM GRAPH//", "MSMT/CUMULATIVE UV LT50, //", "MSMT/NO CLEAR DOSE RESPONSE//",
                           "ENDPT/BMF//", "MSMT/ACETYLCHOLINESTERASE ACTIVITY, NO CLEAR DOSE RESPONSE//", "MSMT/CATALASE ACTIVITY; NO CLEAR DOSE RESPONSE; ALSO REPORTED AOX //",
                           "MSMT/MEAN STAGE. NO CLEAR DOSE RESPONSE.//", "MSMT/NO CLEAR DOSE RESPONSE FOR TURNING RATE; SPEED, DISTANCE TORTUOSITY ALSO REPORTED//",
                           "MSMT/NO CLEAR DOSE RESPONSE, E2, VITELLOGENIN, FSH, LH ALSO REPORTED//", "MSMT/NO CLEAR DOSE RESPONSE, HATCH ALSO REPORTED//",
                           "MSMT/NO CLEAR DOSE RESPONSE, ONLY 10 AND 20 PPM RESULTS ANALYZED, AMP, PCR, ADP, GDP, GTP ALSO REPORTED//",
                           "MSMT/NO CLEAR DOSE RESPONSE. 2 HIGHEST CONCS NOT REPORTED DUE TO MORTALITY//", 
                           "MSMT/NO CLEAR DOSE RESPONSE. HIGHEST CONC NOT REPORTED DUE TO MORTALITY//",
                           "MSMT/NO CLEAR DOSE RESPONSE.//",
                           "MSMT/PERCENT OF SURVIVING FEMALES, NO CLEAR DOSE RESPONSE//",
                           "MSMT/RATIO OF OVIGEROUS FEMALES TO NON-OVIGEROUS FEMALES, NO CLEAR DOSE RESPONSE//",
                           "MSMT/GONADOSOMATIC INDEX.  HEPATOSOMATIC INDEX ALSO REPORTED.  NO CLEAR DOSE RESPONSE.//"),
         !ENDPOINT %in% c("BCF", "LT50", "BAF","BCFD","ET50","ATCN", "BCF/", "MSMT/HIGH CONTROL MORTALITY 42%//"),
         !SIGNIFICANT_DESC %in% c("Not significant at all concentrations"),
         !EXPDESIGN %in% c("SUBAMBIENT UV-B LIGHT DURING 10 D EXPOSURE, 4 REPLICATES", "7 UW PER SQUARE CM UV-B LIGHT", "ADDED SUBAMBIENT UV-B LIGHT, 4 REPLICATES",
                           "EXPOSED TO UV A LIGHT", "25 UW PER SQUARE CM UV-B LIGHT", "NON-IRRADIATED CARBARYL, UVB-RADIATION"),
         !STUDY_CONTROL_DESC %in% c("Insufficient", "Unsatisfactory", "Historical control"),
         !grepl("MIXTURE", OTHEREFFECT), !grepl("Mixture", OTHEREFFECT)) %>% 
  select(-c("TEST_PURITY":"TEST_PURITY_MEAN_OP", "TEST_PURITY_MIN_OP":"TEST_PURITY_MAX", SUPERCLASS, PHYLUM_DIVISION, CLASS, FAMILY, GENUS, VARIETY,
            "AGE_DESC", "ORGANISM_AGE_MIN_OP":"ORGANISM_AGE_MAX", ORGANISM_SOURCE_DESC:ORGANISM_INIT_WT_UNITS,ORGANISM_CHAR, "ORGANISM_AGE_MEAN_OP":"ORGANISM_AGE_MEAN",
            "DOSE_NUMBER", CONC1_TYPE_DESC:CONC3_UNIT, BCF1_MEAN_OP:BCF3_UNIT,CONC1_MEAN_STD_OP, CONC2_TYPE_STD_DESC:CONC3_UNITS_STD,
            TEMPERATURE:SULFUR_UNITS, TEST_FORMULATION_DESC, ION1:ION3, EECOMMENT, EFFECT_PCT:EFFECT_PERCENT_MAX, TISSUE_DESC, SIGNIFICANT_DESC,
            STATISTICAL_SIG:SIGNIFICANCE_LEVEL_MAX,ENDPOINT_ASSIGNED_DESC, HALFLIFE_MEAN_OP:HALFLIFE_TIME_UNIT, LATITUDE:SUBSTRATE_DESC,TEST_DURATION_STD_CONCAT_LONG, STUDY_DURATION_CONCAT_LONG,
            TEST_RADIOLABEL,TEST_CHAR, APP_FREQUENCY_VALUE_UNIT:APP_FREQUENCY_UNIT_DESC, 
            EXPOSURE_DUR_STD_CONCAT_LONG, STUDY_TYPE_DESC, ORGANISM_NUMBER,
            EXPDESIGN, FS_TEST_LOCATION, STEADY_STATE, HABITAT_CODE:DEPTH_UNITS, SAMPLE_SIZE_MEAN_OP:SAMPLE_UNIT,TREND_DESC, ORGANISM_FINAL_WT:DRY_WET_PCT_MAX, CHEMICAL_CARRIER_CONCAT,
            NCBI_TAXID:ORGANISM_HABITAT, RESULT_COMMENT, GENERALCOMMENT, TEST_DURATION_STD_OP:TEST_DURATION_OP, EXPOSURE_DURATION_CONCAT_LONG, STUDY_CONTROL_DESC,
            TEST_METHOD_DESC, TEST_METHOD_NEW_DESC, TEST_GRADE_DESC, TEST_PURITY_MEAN, AGE_UNIT_DESC, SPECIES, TEST_LOCATION_DESC, STUDY_DURATION_STD_CONCAT_LONG,
            TEST_DURATION:STUDY_DURATION_STD_CONCAT_LONG, EXPOSURE_DURATION_MEAN_OP:EXPOSURE_DURATION_UNITS_DESC))

names(ecotox_data_1)

#replace na concentration mean with average of min and max ####
ecotox_data_1$CONC1_MEAN_STD <- as.numeric(ecotox_data_1$CONC1_MEAN_STD)
list(unique(is.na(ecotox_data_1$CONC1_MEAN_STD)))

ecotox_data_1$CONC1_MEAN_STD <- as.numeric(ecotox_data_1$CONC1_MEAN_STD)
ecotox_data_1$CONC1_MIN_STD <- as.numeric(ecotox_data_1$CONC1_MIN_STD)
ecotox_data_1$CONC1_MAX_STD <- as.numeric(ecotox_data_1$CONC1_MAX_STD)

list(unique(ecotox_data_1$CONC1_MIN_STD_OP))
list(unique(ecotox_data_1$CONC1_MAX_STD_OP))

ecotox_data_1$CONC_MEAN <- ifelse(!is.na(ecotox_data_1$CONC1_MEAN_STD), ecotox_data_1$CONC1_MEAN_STD,
                                  ifelse(!is.na(ecotox_data_1$CONC1_MIN_STD) & !is.na(ecotox_data_1$CONC1_MAX_STD) & 
                                           !ecotox_data_1$CONC1_MIN_STD_OP %in%c(">", "<", "~") & !ecotox_data_1$CONC1_MIN_STD_OP %in%c(">", "<", "~"),
                                         (ecotox_data_1$CONC1_MIN_STD + ecotox_data_1$CONC1_MAX_STD)/2, NA))

#limit to units that can be standardized & used together####
names(ecotox_data_1)
list(unique(ecotox_data_1$CONC1_UNITS_STD))

ecotox_data_2 <- ecotox_data_1 %>% select(-c(CONC1_MEAN_STD:CONC1_MAX_STD)) %>%
  filter(!is.na(CONC_MEAN), CONC_MEAN != 0, CONC1_UNITS_STD %in% c("mg/L", "AI mg/L", "ppt", "ppb", "ug/L", "ug/3.5 L", "ng/ml", "ppm", "ug/100 ml", "mol/ml", 
                                                                 "mM"))

#final QAQC step - eliminate values that are NOECs at highest concentration tested####
list(unique(ecotox_data_2_doses$ENDPOINT))
names(ecotox_data_2_doses)

ecotox_data_2_doses <- ecotox_data_2 %>% filter(!DOSES == "NA") %>% separate_rows(DOSES)
ecotox_data_2_doses$DOSES <- as.numeric(ecotox_data_2_doses$DOSES)
ecotox_data_2_doses_1 <- ecotox_data_2_doses %>% filter(!is.na(DOSES), ENDPOINT %in% c("NOEC", "NR-ZERO", "NOEL"))
ecotox_data_2_doses_2 <- ecotox_data_2_doses_1 %>% group_by(AQUIRE_LOCATION, TEST_CAS, SPECIES_LIST_DESC, REFERENCE_NUMBER, TEST_NUMBER, RESULT_NUMBER, CHEM_PRIMARY_NAME,
                                                            LATIN_NAME, COMMON_NAME, KINGDOM, SUBPHYLUM_DIV, TAX_ORDER, LIFESTAGE_DESC, GENDER, EXPOSURE_TYPE_DESC,
                                                            WATER_TYPE_DESC, CONC1_TYPE_STD_DESC, CONC1_UNITS_STD, STUDY_DURATION_MIN_STD,
                                                            STUDY_DURATION_MAX_STD_OP, STUDY_DURATION_MAX_STD, STUDY_DURATION_UNITS_STD_LONG, EXPOSURE_DURATION_MEAN_STD_OP, EXPOSURE_DUR_MEAN_STD,
                                                            EXPOSURE_DURATION_MIN_STD_OP, EXPOSURE_DUR_MIN_STD, EXPOSURE_DURATION_MAX_STD_OP, EXPOSURE_DUR_MAX_STD, EXPOSURE_DUR_UNITS_STD_LONG, TEST_TYPE_DESC,
                                                            OTHEREFFECT, AUTHOR, TITLE, SOURCE, PUBLICATION_YEAR, CONC_MEAN) %>% summarize(max_DOSE = max(DOSES))

ecotox_data_2_doses_3 <- left_join(ecotox_data_2_doses_1, ecotox_data_2_doses_2) %>% filter(CONC_MEAN == max_DOSE) %>% select(-DOSES, -max_DOSE) %>% distinct()
ecotox_data_2_doses_3$FLAG <- "NOEC == highest dose"

ecotox_data_2 <- anti_join((ecotox_data_2 %>% select(-DOSES)), (ecotox_data_2_doses_3 %>% select(-FLAG)))

#convert everything to mg/L
list(unique(ecotox_data_2$CONC1_UNITS_STD))
ecotox_data_2$EFFECT_CON <- NA
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("mg/L", "AI mg/L", "ppm"), ecotox_data_2$CONC_MEAN, ecotox_data_2$EFFECT_CON)
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("ppb", "ug/L", "ng/ml"), ecotox_data_2$CONC_MEAN / 1000, ecotox_data_2$EFFECT_CON)
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("ppt"), ecotox_data_2$CONC_MEAN / 1000000, ecotox_data_2$EFFECT_CON)
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("ug/3.5 L"), (ecotox_data_2$CONC_MEAN / 3.5)/1000, ecotox_data_2$EFFECT_CON)
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("ug/3.5 L"), (ecotox_data_2$CONC_MEAN / 3.5)/1000, ecotox_data_2$EFFECT_CON)
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("ug/100 ml"), (ecotox_data_2$CONC_MEAN / 0.1)/1000, ecotox_data_2$EFFECT_CON)

EC_in_molL <- ecotox_data_2 %>% filter(CONC1_UNITS_STD %in% c("mol/ml", "mM"))
list(unique(EC_in_molL$TEST_CAS))
#63252 341695
#MW from comptox:
#63252 == 201.225 g/mol
#341695 == 305.85 g/mol

ecotox_data_2$MW <- ifelse(ecotox_data_2$TEST_CAS == "63252", 201.225,
                           ifelse(ecotox_data_2$TEST_CAS == "341695", 305.85,
                                  NA))

ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("mM"), (ecotox_data_2$CONC_MEAN * ecotox_data_2$MW), ecotox_data_2$EFFECT_CON)
ecotox_data_2$EFFECT_CON <- ifelse(ecotox_data_2$CONC1_UNITS_STD %in% c("mol/ml"), (ecotox_data_2$CONC_MEAN * ecotox_data_2$MW * 10^6), ecotox_data_2$EFFECT_CON)

ecotox_data_3 <- ecotox_data_2 %>% select(-CONC_MEAN) %>% filter(!is.na(ecotox_data_2$EFFECT_CON), ecotox_data_2$EFFECT_CON != 0)

#pull together standardized exposure duration column####
names(ecotox_data_3)

ecotox_data_3$STUDY_LENGTH <- NA

#we are keeping approximate (~) exposure lengths 

ecotox_data_3$EXPOSURE_DUR_MEAN_STD <- as.numeric(ecotox_data_3$EXPOSURE_DUR_MEAN_STD)
ecotox_data_3$EXPOSURE_DUR_MIN_STD <- as.numeric(ecotox_data_3$EXPOSURE_DUR_MIN_STD)
ecotox_data_3$EXPOSURE_DUR_MAX_STD <- as.numeric(ecotox_data_3$EXPOSURE_DUR_MAX_STD)

ecotox_data_3$STUDY_LENGTH <- ifelse(!is.na(ecotox_data_3$EXPOSURE_DUR_MEAN_STD) & !ecotox_data_3$EXPOSURE_DUR_MEAN_STD %in% c(">", "<", "<="), ecotox_data_3$EXPOSURE_DUR_MEAN_STD,ecotox_data_3$STUDY_LENGTH) 
ecotox_data_3$STUDY_LENGTH <- ifelse(is.na(ecotox_data_3$EXPOSURE_DUR_MEAN_STD) & 
                                       !is.na(ecotox_data_3$EXPOSURE_DUR_MIN_STD) & !is.na(ecotox_data_3$EXPOSURE_DUR_MAX_STD) &
                                                !ecotox_data_3$EXPOSURE_DURATION_MIN_STD_OP %in% c(">", "<", "<=") & !ecotox_data_3$EXPOSURE_DURATION_MAX_STD_OP %in% c(">", "<", "<=") ,
                                              (ecotox_data_3$EXPOSURE_DUR_MIN_STD + ecotox_data_3$EXPOSURE_DUR_MAX_STD)/2, 
                                              ecotox_data_3$STUDY_LENGTH)


ecotox_data_3$STUDY_DURATION_MEAN_STD <- as.numeric(ecotox_data_3$STUDY_DURATION_MEAN_STD)
ecotox_data_3$STUDY_DURATION_MIN_STD <- as.numeric(ecotox_data_3$STUDY_DURATION_MIN_STD)
ecotox_data_3$STUDY_DURATION_MAX_STD <- as.numeric(ecotox_data_3$STUDY_DURATION_MAX_STD)

ecotox_data_3$STUDY_LENGTH <- ifelse(is.na(ecotox_data_3$STUDY_LENGTH) & !is.na(ecotox_data_3$STUDY_DURATION_MEAN_STD) & !ecotox_data_3$STUDY_DURATION_MEAN_STD_OP %in% c(">", "<", "<="),
                                     ecotox_data_3$STUDY_DURATION_MEAN_STD, ecotox_data_3$STUDY_LENGTH)

ecotox_data_3$STUDY_LENGTH <- ifelse(is.na(ecotox_data_3$STUDY_LENGTH) & is.na(ecotox_data_3$STUDY_DURATION_MEAN_STD) &
                                       !is.na(ecotox_data_3$STUDY_DURATION_MIN_STD) & !is.na(ecotox_data_3$STUDY_DURATION_MAX_STD) &
                                       !ecotox_data_3$STUDY_DURATION_MIN_STD_OP %in% c(">", "<", "<=") & !ecotox_data_3$STUDY_DURATION_MAX_STD_OP %in% c(">", "<", "<="),
                                     (ecotox_data_3$STUDY_DURATION_MIN_STD + ecotox_data_3$STUDY_DURATION_MAX_STD)/2, ecotox_data_3$STUDY_LENGTH)

names(ecotox_data_3)
ecotox_data_4 <- ecotox_data_3 %>% filter(!is.na(STUDY_LENGTH)) %>% select(-c(STUDY_DURATION_MEAN_STD_OP:STUDY_DURATION_MAX_STD,EXPOSURE_DURATION_MIN_STD_OP:EXPOSURE_DUR_MAX_STD, MW, OTHEREFFECT, CONC1_TYPE_STD_DESC, CONC1_UNITS_STD))
ecotox_data_4$EFFECT_CON_UNITS <- "mg/L"
ecotox_data_4$STUDY_LENGTH_UNITS <- ifelse(ecotox_data_4$STUDY_DURATION_UNITS_STD_LONG == ecotox_data_4$EXPOSURE_DUR_UNITS_STD_LONG, ecotox_data_4$EXPOSURE_DUR_UNITS_STD_LONG,
                                           paste(ecotox_data_4$EXPOSURE_DUR_UNITS_STD_LONG, "; ", ecotox_data_4$EXPOSURE_DUR_UNITS_STD_LONG))
names(ecotox_data_4)
list(unique(ecotox_data_4$STUDY_DURATION_UNITS_STD_LONG))
list(unique(ecotox_data_4$STUDY_LENGTH_UNITS))

ecotox_data_5 <- ecotox_data_4 %>% select(-c(STUDY_DURATION_UNITS_STD_LONG,EXPOSURE_DUR_UNITS_STD_LONG))
ecotox_data_5$STUDY_LENGTH_UNITS <- gsub("Generation ;  Generation", "Generation", ecotox_data_5$STUDY_LENGTH_UNITS)
ecotox_data_5$STUDY_LENGTH_UNITS <- gsub("Day(s) ;  Day(s)", "Day(s)", ecotox_data_5$STUDY_LENGTH_UNITS)
ecotox_data_5$STUDY_LENGTH_UNITS <- gsub("Day(s) ;  Day(s)", "Day(s)", ecotox_data_5$STUDY_LENGTH_UNITS)

list(unique(ecotox_data_5$STUDY_LENGTH_UNITS))

#annotate results to help categorize endpoints####
#ENDPOINT TYPE####
names(ecotox_data_5)
list(unique(ecotox_data_5$ENDPOINT))
ecotox_data_5$ENDPOINT_TYPE <- ifelse(ecotox_data_5$ENDPOINT %in% c("LOEC", "EC25", "LOEC/", "LC10", "IC10", "IC20", "MATC/", "EC10", "LC15", "IC25", "MATC", "LOEL", "LC30",
                                                                  "LC20", "LC40", "BMC20", "LOEL/", "LC25", "EC20", "EC22","EC41", "EC18"), "Low Effect",
                                    ifelse(ecotox_data_5$ENDPOINT %in% c("LC50", "NR-LETH", "EC50/", "EC50", "IC50", "LC90", "LC95", "LC99", "LC85","EC100", "IC50/", "LC50/", "LC70",
                                                                         "EC70", "EC60", "LC60", "EC80", "LC80", "LC100","LD50", "LD95", "EC90", "IC80","LETC", "NR-LETH/",
                                                                         "EC95/", "LC75", "ED90"), "Effect",
                                                                        ifelse(ecotox_data_5$ENDPOINT %in% c("NOEL", "NOEC", "NR-ZERO", "LC01", "NOEC/", "LC05", "LC0", "EC01",
                                                                                                             "EC04", "EC05"), "No Effect",
                                                                               "Misc")))
list(unique(ecotox_data_5$ENDPOINT_TYPE ))                                    
list(unique((ecotox_data_5 %>% filter(ENDPOINT_TYPE == "Misc"))$ENDPOINT))

ecotox_data_6 <- ecotox_data_5 %>% filter(ENDPOINT_TYPE != "Misc")

#EFFECT TYPE####
names(ecotox_data_6)
list(unique(ecotox_data_6$EFFECT_DESC))

ecotox_data_6$EFFECT_TYPE <- ifelse(ecotox_data_6$EFFECT_DESC %in% c("Mortality", "Population", "Growth", "Reproduction","Development","Injury", "Physiology", "Intoxication", "Morphology",
                                                                     "Behavior", "Avoidance", "Feeding behavior"), "TIER_1",
                                    ifelse(ecotox_data_6$EFFECT_DESC %in% c("Biochemistry", "Histology", "Genetics", "Enzyme(s)","Hormone(s)", "Cell(s)", "Immunological", "Multiple", "Ecosystem process"), "TIER_2",
                                           "Misc"))
list(unique(ecotox_data_6$EFFECT_TYPE))
list(unique((ecotox_data_6 %>% filter(EFFECT_TYPE == "Misc"))$EFFECT_DESC))

ecotox_data_7 <- ecotox_data_6 %>% filter(EFFECT_TYPE != "Misc")

#SPECIES TYPE####
names(ecotox_data_7)
list(unique(ecotox_data_7$SPECIES_LIST_DESC))

ecotox_data_7$ORGANISM_TYPE <- ifelse(ecotox_data_7$SPECIES_LIST_DESC %in% c("Fish", "Fish; Standard Test Species; U.S. Threatened and Endangered Species", "Amphibians; Standard Test Species",
                                                                             "Amphibians; U.S. Threatened and Endangered Species", "Fish; U.S. Invasive Species","Amphibians","Amphibians; U.S. Invasive Species",
                                                                             "Fish; U.S. Threatened and Endangered Species", "Fish; Standard Test Species"), "Fish/Frogs",
                                      ifelse(ecotox_data_7$SPECIES_LIST_DESC %in% c("Crustaceans; Standard Test Species", "Crustaceans","Invertebrates; Standard Test Species",
                                                                                    "Insects/Spiders", "Worms; Standard Test Species", "Molluscs; U.S. Invasive Species", "Molluscs", 
                                                                                    "Molluscs; Standard Test Species", "Invertebrates", "Worms", "Insects/Spiders; Standard Test Species",
                                                                                    "Molluscs; Standard Test Species; U.S. Invasive Species", "Insects/Spiders; U.S. Invasive Species", "Molluscs; U.S. Threatened and Endangered Species"), "Invertebrates",
                                             ifelse(ecotox_data_7$SPECIES_LIST_DESC %in% c("Algae", "Flowers, Trees, Shrubs, Ferns; Standard Test Species; U.S. Invasive Species","Algae; Standard Test Species",
                                                                                           "Flowers, Trees, Shrubs, Ferns; Standard Test Species","Flowers, Trees, Shrubs, Ferns; U.S. Invasive Species",
                                                                                           "Flowers, Trees, Shrubs, Ferns"), "Plants",
                                                    "Misc")))
list(unique(ecotox_data_7$ORGANISM_TYPE))
list(unique((ecotox_data_7 %>% filter(ORGANISM_TYPE == "Misc"))$SPECIES_LIST_DESC ))
ecotox_data_7 <- ecotox_data_7 %>% filter(ORGANISM_TYPE != "Misc")

#STUDY LENGTH####
list(unique(ecotox_data_7$TEST_TYPE_DESC))
names(ecotox_data_7)


ecotox_data_7$STUDY_LENGTH1 <- ifelse(ecotox_data_7$TEST_TYPE_DESC %in% c("Acute", "Subacute"), "Acute",
                                     ifelse(ecotox_data_7$TEST_TYPE_DESC %in% c("Chronic", "Full life cycle", "Subchronic", "Generational", "Partial life cycle"), "Chronic",
                                            "Misc"))
ecotox_data_7$STUDY_LENGTH1 <- ifelse(ecotox_data_7$STUDY_LENGTH1 == "Misc" & ecotox_data_7$STUDY_LENGTH < 7, "Acute", ecotox_data_7$STUDY_LENGTH1)
ecotox_data_7$STUDY_LENGTH1 <- ifelse(ecotox_data_7$STUDY_LENGTH1 == "Misc" & ecotox_data_7$STUDY_LENGTH >= 7, "Chronic",ecotox_data_7$STUDY_LENGTH1)


list(unique(ecotox_data_7$STUDY_LENGTH1))
View(ecotox_data_7)
list(unique((ecotox_data_7 %>% filter(is.na(STUDY_LENGTH1)))$TEST_TYPE_DESC))

#Data Distribution QAQC####
names(ecotox_data_7)

iqr <- ecotox_data_7 %>% group_by(TEST_CAS) %>% summarize(q1 = quantile(EFFECT_CON, 0.25), q3 = quantile(EFFECT_CON, 0.75))
iqr$iqr <- iqr$q3 - iqr$q1                                                           
iqr$iqr1.5 <- 1.5 * iqr$iqr


ecotox_data_8 <- left_join(ecotox_data_7, iqr)

ecotox_data_8$potential_outlier <- ifelse(ecotox_data_8$EFFECT_CON < (ecotox_data_8$q1 - ecotox_data_8$iqr1.5), "potential_outlier",
                                          ifelse(ecotox_data_8$EFFECT_CON > (ecotox_data_8$q3 + ecotox_data_8$iqr1.5), "potential_outlier",
                                                 "not_outlier"))

ecotox_data_9 <- ecotox_data_8 %>% filter(potential_outlier == "not_outlier")
View(ecotox_data_9)


##bind in data from Chris to pull in CAS numbers (normal, not ECOTOX version)####
ECOTOX_CAS_list <- read_excel("ECOTOX_POD\\220123_TOXCAST_PhIII_ECOTOX_Dec2021.xlsx") %>% select(CASRN, PREFERRED_NAME, CAS_NUMBER) %>% rename("TEST_CAS" = "CAS_NUMBER")
names(ECOTOX_CAS_list)

ecotox_data_10 <- left_join(ecotox_data_9,ECOTOX_CAS_list)

write_csv(ecotox_data_10, "raw_filtered_ECOTOX_data_04_12_2022.csv")

#pull together potential PODs####
names(ecotox_data_10)
gmean <- function(x) exp(mean(log(x)))

ecotox_data_PODs <- ecotox_data_10 %>% group_by(TEST_CAS, PREFERRED_NAME, CASRN) %>% summarize(min_EC = min(EFFECT_CON), median_EC = median(EFFECT_CON), max_EC = max(EFFECT_CON),
                                                                        centile_5 = quantile(EFFECT_CON, 0.05), centile_10 = quantile(EFFECT_CON, 0.1),
                                                                        centile_25 = quantile(EFFECT_CON, 0.25), centile_75 = quantile(EFFECT_CON, 0.75),
                                                                        centile_95 = quantile(EFFECT_CON, 0.95), n_distinct_EC = n_distinct(EFFECT_CON),
                                                                        geomean = gmean((EFFECT_CON)))
View(ecotox_data_PODs)

ecotox_data_PODs$pod_5centile_min <- ifelse(ecotox_data_PODs$n_distinct_EC < 5, ecotox_data_PODs$min_EC,
                                            ecotox_data_PODs$centile_5)
  
View(ecotox_data_PODs$pod_5centile_min)
write_xlsx(ecotox_data_PODs, "ecotox_data_PODs_04_12_2022.xlsx")

#break up tier 1 and tier 2 and acute vs. chronic & calculate new 5th centiles#### 
ecotox_data_t1t2_PODs <- ecotox_data_10 %>% filter(EFFECT_TYPE != "Misc") %>% group_by(TEST_CAS, PREFERRED_NAME, CASRN,EFFECT_TYPE)  %>% summarize(min_EC = min(EFFECT_CON), median_EC = median(EFFECT_CON), max_EC = max(EFFECT_CON),
                                                                                                                                                  centile_5 = quantile(EFFECT_CON, 0.05), centile_10 = quantile(EFFECT_CON, 0.1),
                                                                                                                                                  centile_25 = quantile(EFFECT_CON, 0.25), centile_75 = quantile(EFFECT_CON, 0.75),
                                                                                                                                                  centile_95 = quantile(EFFECT_CON, 0.95), n_distinct_EC = n_distinct(EFFECT_CON),
                                                                                                                                                  geomean = gmean((EFFECT_CON)))
ecotox_data_t1t2_PODs$pod_5centile_min <- ifelse(ecotox_data_t1t2_PODs$n_distinct_EC < 5, ecotox_data_t1t2_PODs$min_EC,
                                                 ecotox_data_t1t2_PODs$centile_5)

write_xlsx(ecotox_data_t1t2_PODs, "ecotox_data_tier1vtier2_PODs_04_12_2022.xlsx")


ecotox_data_studylength_PODs <- ecotox_data_10 %>% filter(STUDY_LENGTH1 != "Misc") %>% group_by(TEST_CAS, PREFERRED_NAME, CASRN,STUDY_LENGTH1) %>% summarize(min_EC = min(EFFECT_CON), median_EC = median(EFFECT_CON), max_EC = max(EFFECT_CON),
                                                                                                                                                           centile_5 = quantile(EFFECT_CON, 0.05), centile_10 = quantile(EFFECT_CON, 0.1),
                                                                                                                                                           centile_25 = quantile(EFFECT_CON, 0.25), centile_75 = quantile(EFFECT_CON, 0.75),
                                                                                                                                                           centile_95 = quantile(EFFECT_CON, 0.95), n_distinct_EC = n_distinct(EFFECT_CON),
                                                                                                                                                           geomean = gmean((EFFECT_CON)))
ecotox_data_studylength_PODs$pod_5centile_min <- ifelse(ecotox_data_studylength_PODs$n_distinct_EC < 5, ecotox_data_studylength_PODs$min_EC,
                                                        ecotox_data_studylength_PODs$centile_5)

write_xlsx(ecotox_data_studylength_PODs, "ecotox_data_studylength_PODs_04_12_2022.xlsx")


#fish, insect, plant 
species_PODs <- ecotox_data_10 %>% group_by(TEST_CAS, PREFERRED_NAME, CASRN, ORGANISM_TYPE) %>% summarize(min_EC = min(EFFECT_CON), median_EC = median(EFFECT_CON), max_EC = max(EFFECT_CON),
                                                                                                          centile_5 = quantile(EFFECT_CON, 0.05), centile_10 = quantile(EFFECT_CON, 0.1),
                                                                                                          centile_25 = quantile(EFFECT_CON, 0.25), centile_75 = quantile(EFFECT_CON, 0.75),
                                                                                                          centile_95 = quantile(EFFECT_CON, 0.95), n_distinct_EC = n_distinct(EFFECT_CON),
                                                                                                          geomean = gmean((EFFECT_CON)))

species_PODs$pod_5centile_min <- ifelse(species_PODs$n_distinct_EC < 5, species_PODs$min_EC,
                                        species_PODs$centile_5)

write_xlsx(species_PODs, "ecotox_species_PODs_04_12_2022.xlsx")


#pull together classification, acc, logP, and ecotox pod files for generated pod files####
acc <- read_excel("ToxCast_ECOTOX_Comparison_list_with_Classifications_02_05_2022.xlsx", "Main comparison")

annotated_classification <- read_excel("ToxCast_ECOTOX_Comparison_list_with_Classifications_02_05_2022.xlsx", "Divided by chemical type")
EDC_classification <- read_excel("ToxCast_ECOTOX_Comparison_list_with_Classifications_02_05_2022.xlsx", "EDCs")

cytotoxicburst_logP <- read_excel("LogP and Cytotox burst 636 ECOTOX chemicals.xlsx")

#collate pods and then annotate with classification etc.####
ecotox_PODs <- read_excel("ecotox_data_PODs_04_12_2022.xlsx")
ecotox_PODs$pod_type <- "overall"

ecotox_data_t1t2_PODs <- read_excel("ecotox_data_tier1vtier2_PODs_04_12_2022.xlsx")
ecotox_data_t1t2_PODs$pod_type <- "tier1_tier2"

ecotox_data_studylength_PODs <- read_excel("ecotox_data_studylength_PODs_04_12_2022.xlsx")
ecotox_data_studylength_PODs$pod_type <- "acute_chronic"

species_PODs <- read_excel("ecotox_species_PODs_04_12_2022.xlsx")
species_PODs$pod_type <- "species"

ecotox_pods_all <- bind_rows(ecotox_PODs, ecotox_data_t1t2_PODs, ecotox_data_studylength_PODs, species_PODs) %>% rename("STUDY_LENGTH" = "STUDY_LENGTH1")
View(ecotox_pods_all)

#join ACC, logP, and cytotoxic burst####
names(acc)
acc_1 <- acc %>% select(CASRN, "acc5_linear (mg/L)") %>% filter(!is.na(`acc5_linear (mg/L)`), `acc5_linear (mg/L)` != 0)

names(cytotoxicburst_logP)
cytburst_logp_dtxsid_for_caspull <- cytotoxicburst_logP %>% select(dtxsid)
write_xlsx(cytburst_logp_dtxsid_for_caspull, "cytburst_logp_dtxsid_for_caspull.xlsx")

CAS_forcytburst <- read_excel("CAS_for_logP_cytburst.xlsx") %>% select(CASRN, DTXSID) %>% rename("dtxsid" = "DTXSID")

cytotoxicburst_logP1 <- left_join(cytotoxicburst_logP, CAS_forcytburst)
names(cytotoxicburst_logP1)
cytburst <- cytotoxicburst_logP1 %>% select(CASRN, `Cytotox_low bnd_mg_L`, cytotox_low_bnd_um)
logP <- cytotoxicburst_logP1 %>% select(CASRN, OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED)

ecotox_pods_acc <- left_join(ecotox_pods_all, acc_1) %>% filter(!is.na(`acc5_linear (mg/L)`))
ecotox_pods_acc_1 <- left_join(ecotox_pods_acc, logP)

ecotox_pods_cytburst <- left_join(ecotox_pods_all, cytburst) %>% filter(!is.na(`Cytotox_low bnd_mg_L`), cytotox_low_bnd_um != 1000)
ecotox_pods_cytburst_1 <- left_join(ecotox_pods_cytburst, logP)

#join classifications####
names(annotated_classification)
classifications <- annotated_classification %>% select(CASRN, chemical_class_t_name, general_subclass_list, subclass_list_newest_for_classification)

ecotox_pods_acc_2 <- left_join(ecotox_pods_acc_1, classifications)
ecotox_pods_cytburst_2 <- left_join(ecotox_pods_cytburst_1, classifications)

#write out pods####
write_csv(ecotox_pods_acc_2, "ECOTOX_PODs_ACC_04_12_2022.csv")
write_csv(ecotox_pods_cytburst_2, "ECOTOX_PODs_CB_04_12_2022.csv")

View(ecotox_pods_acc_2)


#QSAR data####
QSAR_data <- read_excel("QSAR Master List for ECOTOX comparisons w_CAS.xlsx") %>% rename("CASRN" = "cas_rn") %>% select(CASRN, min_aquatic_qsar_acute_pod_mg_l, min_qsar_acute_pod_taxa)
names(QSAR_data)

ToxCast_QSAR <- left_join(acc_1, QSAR_data) %>% filter(!is.na(`acc5_linear (mg/L)`), !is.na(min_aquatic_qsar_acute_pod_mg_l))
write_xlsx(ToxCast_QSAR, "ToxCast_QSAR.xlsx")

CB_QSAR <- left_join(cytburst, QSAR_data) %>% filter(!is.na(`Cytotox_low bnd_mg_L`), !is.na(min_aquatic_qsar_acute_pod_mg_l), cytotox_low_bnd_um != 1000)
names(CB_QSAR)
write_xlsx(CB_QSAR, "CB_QSAR.xlsx")

QSAR_ECOTOX <- left_join(ecotox_pods_all, QSAR_data)
write_csv(QSAR_ECOTOX, "QSAR_ECOTOX_04_12_2022.csv")
View(QSAR_ECOTOX)
