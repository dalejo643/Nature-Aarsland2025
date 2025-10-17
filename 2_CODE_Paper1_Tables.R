source(file = "SCRIPTS/1_CODE_Data_Manipulation.R")

##### Supplementary Table 1 ##### 
# HUNT 3 57-69 age
table(is.na(data$PlaPTau217NT3_113229[data$PartAg_NT3BLQ1 < 70]))

mean(data$PartAg_NT3BLQ1[cond0 & data$PartAg_NT3BLQ1<70], na.rm = TRUE)
sd(data$PartAg_NT3BLQ1[cond0 & data$PartAg_NT3BLQ1<70], na.rm = TRUE)
median(data$PartAg_NT3BLQ1[cond0 & data$PartAg_NT3BLQ1<70], na.rm = TRUE)
min(data$PartAg_NT3BLQ1[cond0 & data$PartAg_NT3BLQ1<70], na.rm = TRUE)
max(data$PartAg_NT3BLQ1[cond0 & data$PartAg_NT3BLQ1<70], na.rm = TRUE)

cbind(table(data[cond0 & data$PartAg_NT3BLQ1 < 70, "Sex"]),
      prop.table(table(data[cond0 & data$PartAg_NT3BLQ1 < 70, "Sex"]))*100) # Sex, 0 = Female

cbind(table(data$E4_num[cond0 & data$PartAg_NT3BLQ1 < 70], useNA = "always"),
      prop.table(table(data$E4_num[cond0 & data$PartAg_NT3BLQ1 < 70], useNA = "always"))*100)


cbind(table(data$Education_cat[cond0 & data$PartAg_NT3BLQ1<70], useNA = "always"),
      prop.table(table(data$Education_cat[cond0 & data$PartAg_NT3BLQ1<70], useNA = "always"))*100)

# HUNT 4 70+
table(is.na(data$PlaPTau217NT4_113229))

mean(data$PartAg_NT4BLM[cond1], na.rm = TRUE)
sd(data$PartAg_NT4BLM[cond1], na.rm = TRUE)
median(data$PartAg_NT4BLM[cond1], na.rm = TRUE)
min(data$PartAg_NT4BLM[cond1], na.rm = TRUE)
max(data$PartAg_NT4BLM[cond1], na.rm = TRUE)

cbind(table(data[cond1, "Sex"]),
      prop.table(table(data[cond1, "Sex"]))*100) # Sex, 0 = Female

cbind(table(data$E4_num[cond1], useNA = "always"),
      prop.table(table(data$E4_num[cond1], useNA = "always"))*100)


mean(data$PlaPTau217NT4_113229, na.rm = TRUE)
sd(data$PlaPTau217NT4_113229, na.rm = TRUE)
median(data$PlaPTau217NT4_113229, na.rm = TRUE)
min(data$PlaPTau217NT4_113229, na.rm = TRUE)
max(data$PlaPTau217NT4_113229, na.rm = TRUE)

cbind(table(data$Education_cat[cond1], useNA = "always"),
      prop.table(table(data$Education_cat[cond1], useNA = "always"))*100)


##### Supplementary Table 2 #####
# HUNT3: n
table(data$Age_cat_NT3BLQ1[cond0])[1:3]
# HUNT3: pTau217 mean ± se
svyby(formula = ~PlaPTau217NT3_113229, by = ~Age_cat_NT3BLQ1, design = design_HUNT3, FUN = svymean, na.rm = TRUE, keep.var = TRUE)
# HUNT3: Cut-off groups (pg/mL) - Lower
table(data$Age_cat_NT3BLQ1[data$H3Cutlow0.40 == 1]) [1:3]
svyby(formula = ~H3Cutlow0.40, by = ~Age_cat_NT3BLQ1, design = design_HUNT3, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
# HUNT3: Cut-off groups (pg/mL) - Intermediate
table(data$Age_cat_NT3BLQ1[data$H3Cutinterm == 1])[1:3]
svyby(formula = ~H3Cutinterm, by = ~Age_cat_NT3BLQ1, design = design_HUNT3, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
# HUNT3: Cut-off groups (pg/mL) - Upper
table(data$Age_cat_NT3BLQ1[data$H3CutHigh0.63 == 1])[1:3]
svyby(formula = ~H3CutHigh0.63, by = ~Age_cat_NT3BLQ1, design = design_HUNT3, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")

# HUNT4: n
table(data$Age_cat_NT4BLM[cond1])
# HUNT4: pTau217 mean ± se
svyby(formula = ~PlaPTau217NT4_113229, by = ~Age_cat_NT4BLM, design = design_HUNT4, FUN = svymean, na.rm = TRUE, keep.var = TRUE)
# HUNT4: Cut-off groups (pg/mL) - Lower
table(data$Age_cat_NT4BLM[data$H4Cutlow0.40 == 1])
svyby(formula = ~H4Cutlow0.40, by = ~Age_cat_NT4BLM, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
# HUNT4: Cut-off groups (pg/mL) - Intermediate
table(data$Age_cat_NT4BLM[data$H4Cutinterm == 1])
svyby(formula = ~H4Cutinterm, by = ~Age_cat_NT4BLM, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
# HUNT4: Cut-off groups (pg/mL) - Upper
table(data$Age_cat_NT4BLM[data$H4CutHigh0.63 == 1])
svyby(formula = ~H4CutHigh0.63, by = ~Age_cat_NT4BLM, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")

##### Supplementary Table 3 #####
# HUNT4: Cut-off groups (pg/mL) - Lower
table(data$Age_cat_NT4BLM[data$H4Cutlow0.40 == 1], data$Cognitive_HUNT4_70[data$H4Cutlow0.40 == 1])
table(data$Age_cat_NT4BLM[data$H4Cutlow0.40 == 1])
table(data$Cognitive_HUNT4_70[data$H4Cutlow0.40 == 1])

svyby(formula = ~H4Cutlow0.40, by = ~Age_cat_NT4BLM + Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
p_values <- c()
for(i in levels(data$Age_cat_NT4BLM)){
  p_values[i] <- regTermTest(svyglm(H4Cutlow0.40 ~ Cognitive_HUNT4_70, design = subset(design_HUNT4, subset = Age_cat_NT4BLM == i) , family = quasibinomial()), ~Cognitive_HUNT4_70)$p
}
svyby(formula = ~H4Cutlow0.40, by = ~Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
p_values <- c(p_values, 
             regTermTest(svyglm(H4Cutlow0.40 ~ Cognitive_HUNT4_70, design = design_HUNT4, family = quasibinomial()), ~Cognitive_HUNT4_70)$p)
clipr::write_clip(p.adjust(p_values, method = "bonferroni"))
# Total
svyby(formula = ~H4Cutlow0.40, by = ~Age_cat_NT4BLM, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
svyciprop(formula = ~H4Cutlow0.40, design = design_HUNT4, na.rm = TRUE, vartype = "ci", method = "likelihood")
table(data$H4Cutlow0.40)

# HUNT4: Cut-off groups (pg/mL) - Upper
table(data$Age_cat_NT4BLM[data$H4CutHigh0.63 == 1], data$Cognitive_HUNT4_70[data$H4CutHigh0.63 == 1])
table(data$Age_cat_NT4BLM[data$H4CutHigh0.63 == 1])
table(data$Cognitive_HUNT4_70[data$H4CutHigh0.63 == 1])
svyby(formula = ~H4CutHigh0.63, by = ~Age_cat_NT4BLM + Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
p_values <- c()
for(i in levels(data$Age_cat_NT4BLM)){
  p_values[i] <- regTermTest(svyglm(H4CutHigh0.63 ~ Cognitive_HUNT4_70, design = subset(design_HUNT4, subset = Age_cat_NT4BLM == i) , family = quasibinomial()), ~Cognitive_HUNT4_70)$p
}
svyby(formula = ~H4CutHigh0.63, by = ~Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
p_values <- c(p_values, 
              regTermTest(svyglm(H4CutHigh0.63 ~ Cognitive_HUNT4_70, design = design_HUNT4, family = quasibinomial()), ~Cognitive_HUNT4_70)$p)
clipr::write_clip(p.adjust(p_values, method = "bonferroni"))
# Total
svyby(formula = ~H4CutHigh0.63, by = ~Age_cat_NT4BLM, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
svyciprop(formula = ~H4CutHigh0.63, design = design_HUNT4, na.rm = TRUE, vartype = "ci", method = "likelihood")
table(data$H4CutHigh0.63)

##### Supplementary Table 4 #####
# Female
data %>%
  filter(cond1, Sex == "Female") %>%
  group_by(Age_cat_NT4BLM, Cognitive_HUNT4_70) %>%
  summarise(
    x = n(),
    n = sum(H4CutHigh0.63, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  add_tally(x, name = "N") %>%
  print(n = Inf)
## Female: CU vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
   a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "CU"), 
                  design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k & Sex == "Female"))
   Table[Table$Age == k, 2] <- round(a[1]*100,2)
   Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
   Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Female: MCI vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "MCI"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k & Sex == "Female"))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Female: Dementia vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "Dementia"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k & Sex == "Female"))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Female: Total vs Cognition
data %>%
  filter(cond1, Sex == "Female") %>%
  group_by(Cognitive_HUNT4_70) %>%
  summarise(
    x = n(),
    n = sum(H4CutHigh0.63, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  add_tally(x, name = "N") %>%
  print(n = Inf)
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "CU"), design = subset(design_HUNT4, subset = Sex == "Female"))
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "MCI"), design = subset(design_HUNT4, subset = Sex == "Female"))
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "Dementia"), design = subset(design_HUNT4, subset = Sex == "Female"))

# Male
data %>%
  filter(cond1, Sex == "Male") %>%
  group_by(Age_cat_NT4BLM, Cognitive_HUNT4_70) %>%
  summarise(
    x = n(),
    n = sum(H4CutHigh0.63, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  add_tally(x, name = "N") %>%
  print(n = Inf)
## Male: CU vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "CU"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k & Sex == "Male"))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Male: MCI vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "MCI"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k & Sex == "Male"))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Male: Dementia vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "Dementia"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k & Sex == "Male"))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Male: Total vs Cognition
data %>%
  filter(cond1, Sex == "Male") %>%
  group_by(Cognitive_HUNT4_70) %>%
  summarise(
    x = n(),
    n = sum(H4CutHigh0.63, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  add_tally(x, name = "N") %>%
  print(n = Inf)
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "CU"), design = subset(design_HUNT4, subset = Sex == "Male"))
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "MCI"), design = subset(design_HUNT4, subset = Sex == "Male"))
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "Dementia"), design = subset(design_HUNT4, subset = Sex == "Male"))

# Total
data %>%
  filter(cond1) %>%
  group_by(Age_cat_NT4BLM, Cognitive_HUNT4_70) %>%
  summarise(
    x = n(),
    n = sum(H4CutHigh0.63, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  add_tally(x, name = "N") %>%
  print(n = Inf)
## Total: CU vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "CU"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Total: MCI vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "MCI"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Total: Dementia vs Age
Table <- data.frame(Age = levels(data$Age_cat_NT4BLM), Pre = NA, LI = NA, LS = NA)
for(k in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  a <- svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "Dementia"), 
                 design = subset(design_HUNT4, subset = Age_cat_NT4BLM == k))
  Table[Table$Age == k, 2] <- round(a[1]*100,2)
  Table[Table$Age == k, 3] <- round(confint(a)[1]*100,2)
  Table[Table$Age == k, 4] <- round(confint(a)[2]*100,2)
}
print(Table)
## Total: Total vs Cognition
data %>%
  filter(cond1) %>%
  group_by(Cognitive_HUNT4_70) %>%
  summarise(
    x = n(),
    n = sum(H4CutHigh0.63, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  add_tally(x, name = "N") %>%
  print(n = Inf)
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "CU"), design = design_HUNT4)
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "MCI"), design = design_HUNT4)
svyciprop(~I(H4CutHigh0.63 == 1 & Cognitive_HUNT4_70 == "Dementia"), design = design_HUNT4)

rm(k, a,Table)

##### Supplementary Table 5#####
# Plasma pTau217 < 0.40 pg/mL
table(data$E4_num[data$H4Cutlow0.40 == 1], data$Cognitive_HUNT4_70[data$H4Cutlow0.40 == 1])
table(data$E4_num[data$H4Cutlow0.40 == 1])
svyby(formula = ~H4Cutlow0.40, by = ~E4_num + Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
svyby(formula = ~H4Cutlow0.40, by = ~E4_num, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
p_values <- c()
for(i in levels(data$E4_num)){
  p_value <- regTermTest(svyglm(H4Cutlow0.40 ~ Cognitive_HUNT4_70, design = subset(design_HUNT4, subset = E4_num == i) , family = quasibinomial()), ~Cognitive_HUNT4_70)$p
  p_values <- c(p_values, p_value)
}
# Plasma 0.40 pm/mL <= pTau217 < 0.63 pg/mL
table(data$E4_num[data$H4Cutinterm == 1], data$Cognitive_HUNT4_70[data$H4Cutinterm == 1])
table(data$E4_num[data$H4Cutinterm == 1])
svyby(formula = ~H4Cutinterm, by = ~E4_num + Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
svyby(formula = ~H4Cutinterm, by = ~E4_num, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
for(i in levels(data$E4_num)){
  p_value <- regTermTest(svyglm(H4Cutinterm ~ Cognitive_HUNT4_70, design = subset(design_HUNT4, subset = E4_num == i) , family = quasibinomial()), ~Cognitive_HUNT4_70)$p
  p_values <- c(p_values, p_value)
}
# Plasma pTau217 >= 0.63 pg/mL
table(data$E4_num[data$H4CutHigh0.63 == 1], data$Cognitive_HUNT4_70[data$H4CutHigh0.63 == 1])
table(data$E4_num[data$H4CutHigh0.63 == 1])
svyby(formula = ~H4CutHigh0.63, by = ~E4_num + Cognitive_HUNT4_70, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
svyby(formula = ~H4CutHigh0.63, by = ~E4_num, design = design_HUNT4, FUN = svyciprop, na.rm = TRUE, vartype = "ci", method = "likelihood")
for(i in levels(data$E4_num)){
  p_value <- regTermTest(svyglm(H4CutHigh0.63 ~ Cognitive_HUNT4_70, design = subset(design_HUNT4, subset = E4_num == i) , family = quasibinomial()), ~Cognitive_HUNT4_70)$p
  p_values <- c(p_values, p_value)
}
# Bonferroni p--values
clipr::write_clip(p.adjust(p_values, method = "bonferroni"))
rm(p_value, i, p_values)

##### Supplementary Table 6 #####
# Angina pectorix
table(!is.na(data$New_angina[cond1]))
table(data$New_angina[cond1]); svymean(~New_angina, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_angina, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_angina[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_angina + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m01 <- svyglm(H4CutHigh0.63 ~ New_angina + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m01))[2]
exp(confint(m01))[2,]

# Myocardial infarction
table(!is.na(data$New_infarc[cond1]))
table(data$New_infarc[cond1]); svymean(~New_infarc, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_infarc, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_infarc[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_infarc + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m02 <- svyglm(H4CutHigh0.63 ~ New_infarc + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m02))[2]
exp(confint(m02))[2,]

# Heart failure
table(!is.na(data$New_heartf[cond1]))
table(data$New_heartf[cond1]); svymean(~New_heartf, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_heartf, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_heartf[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_heartf + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m03 <- svyglm(H4CutHigh0.63 ~ New_heartf + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m03))[2]
exp(confint(m03))[2,]

# Atrial fibrillation¤
table(!is.na(data$New_atrial[cond1]))
table(data$New_atrial[cond1]); svymean(~New_atrial, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_atrial, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_atrial[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_atrial + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m04 <- svyglm(H4CutHigh0.63 ~ New_atrial + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m04))[2]
exp(confint(m04))[2,]

# Stroke/brain haemorrhage
table(!is.na(data$New_stroke[cond1]))
table(data$New_stroke[cond1]); svymean(~New_stroke, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_stroke, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_stroke[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_stroke + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m05 <- svyglm(H4CutHigh0.63 ~ New_stroke + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m05))[2]
exp(confint(m05))[2,]

# COPD or emphysema
table(!is.na(data$New_copdem[cond1]))
table(data$New_copdem[cond1]); svymean(~New_copdem, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_copdem, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_copdem[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_copdem + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m06 <- svyglm(H4CutHigh0.63 ~ New_copdem + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m06))[2]
exp(confint(m06))[2,]

# Diabetes
table(!is.na(data$New_diabet[cond1]))
table(data$New_diabet[cond1]); svymean(~New_diabet, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_diabet, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_diabet[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_diabet + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m07 <- svyglm(H4CutHigh0.63 ~ New_diabet + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m07))[2]
exp(confint(m07))[2,]

# Cancer
table(!is.na(data$New_cancer[cond1]))
table(data$New_cancer[cond1]); svymean(~New_cancer, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_cancer, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_cancer[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_cancer + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m08 <- svyglm(H4CutHigh0.63 ~ New_cancer + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m08))[2]
exp(confint(m08))[2,]

# Migraine¤
table(!is.na(data$New_migrai[cond1]))
table(data$New_migrai[cond1]); svymean(~New_migrai, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_migrai, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_migrai[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_migrai + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m09 <- svyglm(H4CutHigh0.63 ~ New_migrai + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m09))[2]
exp(confint(m09))[2,]

# Psoriasis
table(!is.na(data$New_psoria[cond1]))
table(data$New_psoria[cond1]); svymean(~New_psoria, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_psoria, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_psoria[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_psoria + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m10 <- svyglm(H4CutHigh0.63 ~ New_psoria + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m10))[2]
exp(confint(m10))[2,]

# Kidney disease (other than urinary tract infection)
table(!is.na(data$New_kidney[cond1]))
table(data$New_kidney[cond1]); svymean(~New_kidney, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_kidney, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_kidney[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_kidney + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m11 <- svyglm(H4CutHigh0.63 ~ New_kidney + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m11))[2]
exp(confint(m11))[2,]

# Rheumatoid arthritis¤
table(!is.na(data$New_rheuma[cond1]))
table(data$New_rheuma[cond1]); svymean(~New_rheuma, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_rheuma, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_rheuma[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_rheuma + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m12 <- svyglm(H4CutHigh0.63 ~ New_rheuma + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m12))[2]
exp(confint(m12))[2,]

# Spondyloarthritis¤
table(!is.na(data$New_spondy[cond1]))
table(data$New_spondy[cond1]); svymean(~New_spondy, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_spondy, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_spondy[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_spondy + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m13 <- svyglm(H4CutHigh0.63 ~ New_spondy + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m13))[2]
exp(confint(m13))[2,]

# Gout¤
table(!is.na(data$New_gout[cond1]))
table(data$New_gout[cond1]); svymean(~New_gout, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_gout, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_gout[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_gout + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m14 <- svyglm(H4CutHigh0.63 ~ New_gout + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m14))[2]
exp(confint(m14))[2,]

# Seeking help for mental health problem¤
table(!is.na(data$New_mental[cond1]))
table(data$New_mental[cond1]); svymean(~New_mental, design = design_HUNT4, na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_mental, design = design_HUNT4, FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_mental[cond1], data$H4CutHigh0.63[cond1])
prop.table(svytable(~New_mental + H4CutHigh0.63, design = design_HUNT4), margin = 1)*100
m15 <- svyglm(H4CutHigh0.63 ~ New_mental + AgeH4_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT4BLM, 
              design = design_HUNT4, family = quasibinomial()) 
exp(coef(m15))[2]
exp(confint(m15))[2,]

# Epylepsy*
table(!is.na(data$New_epylep[cond0 & data$PartAg_NT3BLQ1 < 70]))
table(data$New_epylep[cond0 & data$PartAg_NT3BLQ1 < 70]); svymean(~New_epylep, design = subset(design_HUNT3, subset = PartAg_NT3BLQ1 < 70), na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_epylep, design = subset(design_HUNT3, PartAg_NT3BLQ1 < 70), FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_epylep[cond0 & data$PartAg_NT3BLQ1 < 70], data$H4CutHigh0.63[cond0 & data$PartAg_NT3BLQ1 < 70])
prop.table(svytable(~New_epylep + H4CutHigh0.63, design = subset(design_HUNT3, PartAg_NT3BLQ1 < 70)), margin = 1)*100
m16 <- svyglm(H3CutHigh0.63 ~ New_epylep + AgeH3_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT3BLM, 
              design = subset(design_HUNT3, PartAg_NT3BLQ1 < 70), family = quasibinomial())
exp(coef(m16))[2]
exp(confint(m16))[2,]

# Eczema on hands*
table(!is.na(data$New_eczema[cond0 & data$PartAg_NT3BLQ1 < 70]))
table(data$New_eczema[cond0 & data$PartAg_NT3BLQ1 < 70]); svymean(~New_eczema, design = subset(design_HUNT3, subset = PartAg_NT3BLQ1 < 70), na.rm = TRUE)
svyby(formula = ~PlaPTau217NT4_113229, by = ~New_eczema, design = subset(design_HUNT3, subset = PartAg_NT3BLQ1 < 70), FUN = svymean, na.rm = TRUE, vartype = "se", method = "likelihood")
table(data$New_eczema[cond0 & data$PartAg_NT3BLQ1 < 70], data$H4CutHigh0.63[cond0 & data$PartAg_NT3BLQ1 < 70])
prop.table(svytable(~New_eczema + H4CutHigh0.63, design = subset(design_HUNT3, subset = PartAg_NT3BLQ1 < 70)), margin = 1)*100
m17 <- svyglm(H3CutHigh0.63 ~ New_eczema + AgeH3_1y + Sex + E4_num + Education_cat + DxDem + SeCrea_NT3BLM, 
              design = subset(design_HUNT3, subset = PartAg_NT3BLQ1 < 70), family = quasibinomial())
exp(coef(m17))[2]
exp(confint(m17))[2,]

# P-values calculation and Bonferri correction
p_values<- c(summary(m01)$coefficients[2,4], summary(m02)$coefficients[2,4], summary(m03)$coefficients[2,4], summary(m04)$coefficients[2,4],
             summary(m05)$coefficients[2,4], summary(m06)$coefficients[2,4], summary(m07)$coefficients[2,4], summary(m08)$coefficients[2,4],
             summary(m09)$coefficients[2,4], summary(m10)$coefficients[2,4], summary(m11)$coefficients[2,4], summary(m12)$coefficients[2,4],
             summary(m13)$coefficients[2,4], summary(m14)$coefficients[2,4], summary(m15)$coefficients[2,4], summary(m16)$coefficients[2,4],
             summary(m17)$coefficients[2,4])

clipr::write_clip(p_values)
clipr::write_clip(p.adjust(p_values, method = "bonferroni"))

rm(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15,m16,m17,p_values)
