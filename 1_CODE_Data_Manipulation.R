#### Required packages ####
require(haven)      # To read data from SPSS
require(survey)     # To use survey weighted analysis
require(tidyverse)  # To data manipulation
require(ggplot2)    # To graphs
require(ggpubr)     ## Complements ggplot2, to create multiple panels plots
require(ggbeeswarm) ## Figure 1, to use geom_quasirandom()
require(patchwork)  ## Figure 3, to create the panel layout
require(segmented)  # To calculate cut-off in a Piecewise Regression

#### Importing data form SPSS ####
data <- read_sav("C:/Users/dietov/OneDrive - Helse Vest/Dokumenter/HELSEVEST PROJECT/DATASET/ORIGINAL DATA/SPSS FILES/HUNTFULLDATA.sav", encoding = "Latin1")
##### To replace '@' for '_' in SPSS data names #####
colnames(data) <- iconv(colnames(data), from = "Latin1", to = "ASCII//TRANSLIT")
colnames(data) <- gsub("@", "_", colnames(data))

#### Data corrections ####
##### Missing age in HUNT4 #####
data$PartAg_NT4BLM[data$BirthYear == 1943 & is.na(data$PartAg_NT4BLM)] <- 75
data$PartAg_NT4BLM[data$BirthYear == 1929 & is.na(data$PartAg_NT4BLM)] <- 88

#### Data manipulation ####
##### Transform Sex in a factor variable #####
data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Female", "Male"))

##### Filter conditions for pTau217 #####
cond0 <- !is.na(data$PlaPTau217NT3_113229)    # People with pTau217 in HUNT3
cond1 <- !is.na(data$PlaPTau217NT4_113229)    # People with pTau217 in HUNT4 70+
cond2 <- !is.na(data$PlaPTau217NT4Cov_113229) # People with pTau217 in HUNT AiT

##### ApoE variables #####
ApoE <- read.delim(file = "C:/Users/dietov/OneDrive - Helse Vest/Dokumenter/HELSEVEST PROJECT/DATASET/ORIGINAL DATA/APOE FILES/dose_PID113229_varSubset.txt") # change into your data file comprising ApoE SNPs here
ApoE <- ApoE[,1:3]

names(ApoE)[2] <- "rs429358"
names(ApoE)[3] <- "rs7412"

# Rounding the quantities
ApoE$r_rs429358 <- round(ApoE$rs429358)
ApoE$r_rs7412 <- round(ApoE$rs7412)

## recodes the rs7412 (a) and rs429358 (b) values to ApoE genotype
ApoErecode <- function(a,b) {
  if (a == 2 &  b == 0)     { return(1)  }
  else if (a == 1 & b == 0) { return(2)  }
  else if (a == 1 & b == 1) { return(3)  }
  else if (a == 0 & b == 0) { return(4)  }
  else if (a == 0 & b == 1) { return(5)  }
  else if (a == 0 & b == 2) { return(6)  }
  else if (a == 2 & b == 2) { return(7)  }
  else if (a == 2 & b == 1) { return(8)  }
  else if (a == 1 & b == 2) { return(9)  }
}

for (i in 1:length(ApoE$r_rs429358)) {
  ApoE$gtype_[i] <- ApoErecode(ApoE$r_rs7412[i],ApoE$r_rs429358[i])
}
ApoE$gtype__ <- factor(ApoE$gtype_, levels=c(1:9), labels=c("ApoE ε2/ε2", "ApoE ε2/ε3","ApoE ε2/ε4","ApoE ε3/ε3","ApoE ε3/ε4", "ApoE ε4/ε4", "ApoE ε1/ε1", "ApoE ε1/ε2", "ApoE ε1/ε4"))
ApoE$gtype <- factor(ApoE$gtype_, levels=c(1:6), labels=c("ApoE ε2/ε2", "ApoE ε2/ε3","ApoE ε2/ε4","ApoE ε3/ε3","ApoE ε3/ε4", "ApoE ε4/ε4"))

## Recodes a variable for E4 positivity
ApoE$E4 <- ifelse(ApoE$gtype_==1 | ApoE$gtype_==2 | ApoE$gtype_==4, 0, 
                  ifelse(ApoE$gtype_==3 | ApoE$gtype_==5 | ApoE$gtype_==6, 1, NA))

## Recodes a variable for the number of E4 positivity
ApoE$E4_num <- ifelse(ApoE$gtype_==1 | ApoE$gtype_==2 | ApoE$gtype_==4, 0, 
                      ifelse(ApoE$gtype_==3 | ApoE$gtype_==5, 1,
                             ifelse(ApoE$gtype_==6, 2, NA)))

data <- merge(data, ApoE[,c("PID","gtype","E4","E4_num")], by.x = "PID_113229", by.y = "PID", all = TRUE)
rm(i, ApoErecode, ApoE)

data$E4_num <- ifelse(is.na(data$E4_num), 99, data$E4_num)
data$E4_num <- factor(data$E4_num)

##### eGFR calculation #####
StdCreat = data$SeCrea_NT4BLM/88.42
a = ifelse(data$Sex == "Female", -0.241, ifelse(data$Sex == "Male", -0.302, NA))
k = ifelse(data$Sex == "Female", 0.7, ifelse(data$Sex == "Male", 0.9, NA))
data$eGFR_H4 = ifelse(data$Sex == "Female", 142*(pmin(StdCreat/k, 1)^a)*(pmax(StdCreat/k, 1)^-1.2)*(0.9938^data$PartAg_NT4BLM)*1.012,
                   ifelse(data$Sex == "Male", 142*(pmin(StdCreat/k, 1)^a)*(pmax(StdCreat/k, 1)^-1.2)*(0.9938^data$PartAg_NT4BLM), NA))
rm(StdCreat,a,k)

##### Cognitive categories in each wave ##### 
# HUNT4 70+: only CU - MCI - Dementia
data <- data %>% mutate(Cognitive_HUNT4_70 = case_when(DiagCog_NT4Eld1MIX == 0 ~ 0,  # NCI
                                                       DiagCog_NT4Eld1MIX %in% c(1, 2) ~ 1,  # MCI
                                                       DiagCog_NT4Eld1MIX == 3 ~ 2,  # Dementia
                                                       TRUE ~ NA_real_  # Si hay otros valores los dejamos como NA
))
data$Cognitive_HUNT4_70 <- factor(data$Cognitive_HUNT4_70, levels = 0:2, labels = c("CU", "MCI", "Dementia"))

# HUNT4 70+: CU  MCI - Dementia - O-CI - NSI
data <- data %>% mutate(Cognitive_groupsHUNT4 = case_when(DiagCog_NT4Eld1MIX == 0 ~ 0,  # NCI
                                                    DiagCog_NT4Eld1MIX %in% c(1, 2) ~ 1,  # MCI
                                                    DiagCog_NT4Eld1MIX == 3 ~ 2,  # Dementia
                                                    DiagCog_NT4Eld1MIX == 4 ~ 3,  # Other causes of Cognitive Impairment
                                                    DiagCog_NT4Eld1MIX == 9 ~ 4,  # No sufficient information
                                                    TRUE ~ NA_real_  # Si hay otros valores los dejamos como NA
))
data$Cognitive_groupsHUNT4 <- factor(data$Cognitive_groupsHUNT4, levels = 0:4, labels = c("CU", "MCI", "Dementia", "O-CI", "NSI"))

##### Age groups #####
data$Age_cat_NT3BLQ1 <- cut(x = data$PartAg_NT3BLQ1, breaks = c(57, 60, 65, 70, 75, 80, 85, 90, Inf), right = FALSE)
data$Age_cat_NT4BLM  <- cut(x = data$PartAg_NT4BLM, breaks = c(70, 75, 80, 85, 90, Inf), right = FALSE)
data$AgeH3_1y <- cut(data$PartAg_NT3BLQ1, breaks = c(57:93, Inf), right = FALSE)
data$AgeH4_1y <- cut(data$PartAg_NT4BLM, breaks = c(70:100, Inf), right = FALSE)
##### Education groups #####
data <- data %>% 
  mutate(Education_cat_H1 = case_when(Educ_NT1BLQ2 %in% 1:4 ~ 0,  # Basic Education
                                      Educ_NT1BLQ2 %in% 5:6 ~ 1,  # Secondary or Vocational Education
                                      Educ_NT1BLQ2 %in% 7:8 ~ 2,  # Higher Education
                                      TRUE ~ NA_real_  # Si hay otros valores los dejamos como NA
  ),
  Education_cat_H2 = case_when(Educ_NT2BLQ1 == 1 ~ 0,  # Basic Education
                               Educ_NT2BLQ1 %in% 2:3 ~ 1,  # Secondary or Vocational Education
                               Educ_NT2BLQ1 %in% 4:5 ~ 2,  # Higher Education
                               TRUE ~ NA_real_  # Si hay otros valores los dejamos como NA
  ),
  Education_cat_H4 = case_when(Educ_NT4BLQ1 == 1 ~ 0,  # Basic Education
                               Educ_NT4BLQ1 %in% 2:4 ~ 1,  # Secondary or Vocational Education
                               Educ_NT4BLQ1 %in% 5:6 ~ 2,  # Higher Education
                               TRUE ~ NA_real_  # Si hay otros valores los dejamos como NA
  )
  )
data$Education_cat <- ifelse(is.na(data$Education_cat_H4), data$Education_cat_H2, data$Education_cat_H4)
data$Education_cat <- ifelse(is.na(data$Education_cat), data$Education_cat_H1, data$Education_cat)
data$Education_cat <- ifelse(is.na(data$Education_cat), 3, data$Education_cat)
data$Education_cat <- factor(data$Education_cat, levels = 0:3, labels = c("Primary", "Secondary", "Tertiary", "Missing"))

##### Co-morbidity groups #####
# Self-reported diseases
data <- within(data = data, expr = {
  New_angina = ifelse(CarAngEv_NT4BLQ1 == 1 | CarAngEv_NT3BLQ1 == 1, 1, 0)
  New_infarc = ifelse(CarInfEv_NT4BLQ1 == 1 | CarInfEv_NT3BLQ1 == 1, 1, 0)
  New_heartf = ifelse(CarFaiEv_NT4BLQ1 == 1 | CarFaiEv_NT3BLQ1== 1, 1, 0)
  New_atrial = ifelse(CarAtrFibrEv_NT4BLQ1 == 1, 1, 0) # Only HUNT4
  New_stroke = ifelse(ApoplEv_NT4BLQ1 == 1 | ApoplEv_NT3BLQ1 == 1, 1, 0)
  New_copdem = ifelse(CopdEv_NT4BLQ1 == 1 | CopdEv_NT3BLQ1 == 1, 1, 0)
  New_diabet = ifelse(DiaEv_NT4BLQ1 == 1 | DiaEv_NT3BLQ1 == 1, 1, 0)
  New_cancer = ifelse(CaEv_NT4BLQ1 == 1| CaEv_NT3BLQ1 == 1, 1, 0)
  New_migrai = ifelse(MigEv_NT4BLQ1 == 1, 1, 0) # Only HUNT4
  New_psoria = ifelse(PsorEv_NT4BLQ1 == 1 | PsorEv_NT3BLQ1 == 1, 1, 0)
  New_kidney = ifelse(RenDisEv_NT4BLQ1 == 1 | RenDisEv_NT3BLQ1 == 1, 1, 0)
  New_rheuma = ifelse(RhArthEv_NT4BLQ1 == 1, 1, 0) # Only HUNT4
  New_spondy = ifelse(SponArthEv_NT4BLQ1 == 1 , 1, 0) # Only HUNT4
  New_gout   = ifelse(GouEv_NT4BLQ1 == 1 , 1, 0) # Only HUNT4
  New_mental = ifelse(MentPrEv_NT4BLQ1 == 1 , 1, 0) # Only HUNT4
  New_epylep = ifelse(EpilEv_NT3BLQ1 == 1 , 1, 0) # Only HUNT3
  New_eczema = ifelse(EczEv_NT3BLQ1 == 1 , 1, 0) # Only HUNT3
  })

##### Cut-off groups for HUNT3 and HUNT4 70+ #####
# HUNT3
data$H3Cutlow0.40  <- ifelse(data$PlaPTau217NT3_113229 <  0.40, 1, 0)
data$H3Cutinterm   <- ifelse(data$PlaPTau217NT3_113229 >= 0.40 & data$PlaPTau217NT3_113229 < 0.63, 1, 0)
data$H3CutHigh0.63 <- ifelse(data$PlaPTau217NT3_113229 >= 0.63,1,0)
data$H3CutOffs     <- cut(x = data$PlaPTau217NT3_113229, breaks = c(-Inf, 0.40, 0.63, Inf), right = FALSE)
# HUNT4 70+
data$H4Cutlow0.40  <- ifelse(data$PlaPTau217NT4_113229 < 0.40, 1, 0)
data$H4Cutinterm   <- ifelse(data$PlaPTau217NT4_113229 >= 0.40 & data$PlaPTau217NT4_113229<0.63, 1, 0)
data$H4CutHigh0.63 <- ifelse(data$PlaPTau217NT4_113229 >= 0.63, 1, 0)
data$HUNT4CutOffs  <- cut(x = data$PlaPTau217NT4_113229, breaks = c(-Inf, 0.40, 0.63, Inf), right = FALSE)

##### Weights to correct HUNT4 70+ and HUNT3 58-70 #####
# Probability of participation in HUNT4 70+ from DOI:10.1177/08982643221131926
data <- data %>%
  mutate(w1 = case_when(
    Education_cat == "Primary" & Age_cat_NT4BLM == "[70,75)" & Sex == "Male" ~ 2.52,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[70,75)" & Sex == "Female" ~ 2.36,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[75,80)" & Sex == "Male" ~ 2.48,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[75,80)" & Sex == "Female" ~ 2.32,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[80,85)" & Sex == "Male" ~ 2.73,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[80,85)" & Sex == "Female" ~ 2.54,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[85,90)" & Sex == "Male" ~ 2.89,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[85,90)" & Sex == "Female" ~ 2.69,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[90,Inf)" & Sex == "Male" ~ 2.64,
    Education_cat == "Primary" & Age_cat_NT4BLM == "[90,Inf)" & Sex == "Female" ~ 2.46,
    
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[70,75)" & Sex == "Male" ~ 1.84,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[70,75)" & Sex == "Female" ~ 1.75,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[75,80)" & Sex == "Male" ~ 1.82,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[75,80)" & Sex == "Female" ~ 1.73,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[80,85)" & Sex == "Male" ~ 1.96,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[80,85)" & Sex == "Female" ~ 1.86,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[85,90)" & Sex == "Male" ~ 2.05,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[85,90)" & Sex == "Female" ~ 1.94,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[90,Inf)" & Sex == "Male" ~ 1.91,
    Education_cat == "Secondary" & Age_cat_NT4BLM == "[90,Inf)" & Sex == "Female" ~ 1.81,
    
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[70,75)" & Sex == "Male" ~ 1.51,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[70,75)" & Sex == "Female" ~ 1.46,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[75,80)" & Sex == "Male" ~ 1.50,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[75,80)" & Sex == "Female" ~ 1.44,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[80,85)" & Sex == "Male" ~ 1.58,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[80,85)" & Sex == "Female" ~ 1.52,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[85,90)" & Sex == "Male" ~ 1.63,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[85,90)" & Sex == "Female" ~ 1.57,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[90,Inf)" & Sex == "Male" ~ 1.55,
    Education_cat == "Tertiary" & Age_cat_NT4BLM == "[90,Inf)" & Sex == "Female" ~ 1.49,
    
    TRUE ~ NA_real_
  ),
  p1 = 1 / w1 # We use the inverse to get the probability
  )
## we calculate the average to impute weights in the missing education
p1_avg <- data %>% 
  filter(Education_cat != "Missing") %>% 
  group_by(Age_cat_NT4BLM, Sex) %>% 
  summarise(p1_avg = mean(p1), .groups = "drop")

## replace the missing weights
data <- data %>% 
  left_join(p1_avg, by = c("Age_cat_NT4BLM", "Sex")) %>%
  mutate(p1 = if_else(Education_cat == "Missing", p1_avg, p1))
data <- subset(data, select = -c(w1, p1_avg))

# Probability of participation with plasma p-tau217 in HUNT4
data$PartHunt4Blood <- ifelse(!is.na(data$PlaPTau217NT4_113229), 1, 0)
data$DxDem <- ifelse(is.na(data$Cognitive_HUNT4_70), "Other", data$Cognitive_HUNT4_70)
mod1 <- glm(PartHunt4Blood ~ DxDem + Sex + Age_cat_NT4BLM + Education_cat + E4_num, data = data, family = binomial)
summary(mod1)
data$p2 <- predict(mod1, newdata = data, type = "response")

# Probability of selection in HUNT3
data$PartHunt3Blood <- ifelse(data$Part_NT3BLM == 1, 0, NA)
data$PartHunt3Blood <- ifelse(!is.na(data$PlaPTau217NT3_113229), 1, data$PartHunt3Blood)
mod2 <- glm(PartHunt3Blood ~ DxDem + Sex + Age_cat_NT4BLM + Education_cat + E4_num, data = data, family = binomial)
summary(mod2)
data$p3 <- predict(mod2, newdata = data, type = "response")

# Final Weights
data <- within(data, expr = {
  weight_H3 = 1 / (p1*p2*p3)
  weight_H4 = 1 / (p1*p2)
})
summary(data$weight_H3)
summary(data$weight_H4)
data$weight_H3 <- ifelse(data$weight_H3 < median(data$weight_H3) - 3*IQR(data$weight_H3), median(data$weight_H3) - 3*IQR(data$weight_H3), data$weight_H3)
data$weight_H3 <- ifelse(data$weight_H3 > median(data$weight_H3) + 3*IQR(data$weight_H3), median(data$weight_H3) + 3*IQR(data$weight_H3), data$weight_H3)
data$weight_H4 <- ifelse(data$weight_H4 < median(data$weight_H4) - 3*IQR(data$weight_H4), median(data$weight_H4) - 3*IQR(data$weight_H4), data$weight_H4)
data$weight_H4 <- ifelse(data$weight_H4 > median(data$weight_H4) + 3*IQR(data$weight_H4), median(data$weight_H4) + 3*IQR(data$weight_H4), data$weight_H4)
summary(data$weight_H3)
summary(data$weight_H4)

# Returning Missing values
data$E4_num <- factor(data$E4_num, levels = levels(data$E4_num)[-4])
data$Education_cat <- factor(data$Education_cat, levels = levels(data$Education_cat)[-4])

# Remove not necessary objects
rm(mod1, mod2, p1_avg)

##### Final steps ##### 
design_HUNT3 <- svydesign(ids = ~1, data = subset(data, subset = PartAg_NT3BLQ1 < 70), weights = ~weight_H3)
design_HUNT4 <- svydesign(ids = ~1, data = data, weights = ~weight_H4)
write.csv(x = data, file = "DDI Project p-tau217 cut-off/DATA/datos.csv", na = "", row.names = FALSE)
