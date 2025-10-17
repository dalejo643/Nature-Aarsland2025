##### Figure 1 #####
data1 <- data[data$PartAg_NT3BLQ1 < 70& !is.na(data$PlaPTau217NT3_113229), c("Age_cat_NT3BLQ1", "PlaPTau217NT3_113229")]
data2 <- data[!is.na(data$PartAg_NT4BLM) & !is.na(data$PlaPTau217NT4_113229),c("Age_cat_NT4BLM", "PlaPTau217NT4_113229")]
names(data1) <- c("Age", "Ptau217")
names(data2) <- c("Age", "Ptau217")
data3 <- rbind(data1,data2)

pdf(file = "RESULTS AND GRAPHS/GRAPHS/Figure 1.pdf", width = 8, height = 5, colormodel = "rgb")
#png(filename = "RESULTS AND GRAPHS/GRAPHS/Figure 1.png", width = 8, height = 5, units = "in", res = 300)
ggplot(data = data3, mapping = aes(x = Age, y = Ptau217, colour = Age)) +
  geom_quasirandom(method = "pseudorandom", alpha = 0.2, cex = 0.1) +
  geom_boxplot(fill = NA, outliers = FALSE, linewidth = 1) +
  scale_x_discrete(labels = c("58-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")) +
  scale_color_brewer(palette = "Dark2") + 
  coord_cartesian(ylim = c(0, 3)) +
  geom_hline(yintercept = c(0.40, 0.63), linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 0, y = c(0.40, 0.63), label = c("Cut-off: 0.40", "Cut-off: 0.63"), color = "black", size = 3, hjust = -0.1, vjust = -0.5) +
  theme_light() +
  labs(x = "Age groups, years", y = "Plasma pTau217, pg/mL") +
  theme(legend.position = "none") +
  annotate("text", x = 1:8, y = 2.80, label = paste0(c("3.91", "2.47", "7.69", "18.0", "28.3", "44.1", "57.9", "65.2"), "%"), color = "black", size = 4)  +
  annotate("text", x = 1:8, y = 2.65, label = c("[1.82 - 7.01]", "[1.80 - 3.28]", "[6.28 - 9.29]", "[16.8 - 19.3]", 
                                                "[26.6 - 30.2]", "[41.6 - 46.9]", "[54.7 - 61.7]", "[60.3 - 68.9]"), color = "black", size = 3) 
dev.off()

rm(data1,data2,data3)
##### Figure 2 #####
pdf(file = "RESULTS AND GRAPHS/GRAPHS/Figure 2.pdf", width = 8, height = 5, colormodel = "rgb")
#png(filename = "RESULTS AND GRAPHS/GRAPHS/Figure 2.png", width = 8, height = 5, units = "in", res = 300)
ggplot(data = data[cond1 & !is.na(data$Cognitive_HUNT4_70),], 
       mapping = aes(x = Age_cat_NT4BLM, y = PlaPTau217NT4_113229, colour = Cognitive_HUNT4_70)) +
  geom_boxplot(fill = NA, outlier.shape = NA, linewidth = 1) +
  scale_x_discrete(labels = c("70–74", "75–79", "80–84", "85–89", "90+")) +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(ylim = c(0, 4.2)) +
  geom_hline(yintercept = c(0.40, 0.63), linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 0.3, y = c(0.40, 0.63), 
           label = c("Cut-off: 0.40", "Cut-off: 0.63"), 
           color = "black", size = 3, hjust = -0.1, vjust = -0.5) +
  geom_text(data = data.frame(
    x = rep(1:5, 3),  # 5 grupos de edad
    y = c(rep(4.1,5),rep(3.6,5),rep(3.1,5)),          # altura donde quieres poner la tabla (ajusta si es necesario)
    label = c(
      "14.6%\n[13.2 - 16.2]", "22.9%\n[20.7 - 25.3]", "34.8%\n[31.2 - 38.5]", "45.5%\n[39.4 - 52.1]", "62.5%\n[50.6 - 73.5]",
      "20.3%\n[18.1 - 22.5]", "29.8%\n[26.8 - 33.0]", "45.1%\n[40.5 - 49.8]", "60.7%\n[47.3 - 50.0]", "60.7%\n[52.4 - 68.6]",
      "33.1%\n[26.8 - 39.8]", "49.6%\n[42.8 - 56.4]", "63.5%\n[57.1 - 59.6]", "72.0%\n[66.0 - 77.5]", "66.7%\n[60.5 - 72.6]"
    ),
    group = rep(c("CU", "MCI", "Dementia"), each = 5)
  ), aes(x = x, y = y, label = label, colour = group), 
  inherit.aes = FALSE, size = 3.1) +
  theme_light() +
  labs(x = "Age groups, years", y = "Plasma pTau217, pg/mL") +
  theme(legend.position = "bottom", legend.title = element_blank())
dev.off()


##### Figure 3 #####
# Data from Supplementary Table 4
data1 <- data.frame(
  Age = c(rep("70 - 74", 6), rep("75 - 79", 6), rep("80 - 84", 6), rep("85 - 89", 6), rep("90 +",6)),
  Total = c(1947, 1947, 1947, 1881, 1881, 1881,
            1286, 1286, 1286, 1160, 1160, 1160,
            760,  760,  760,  656,  656,  656,
            480,  480,  480,  292,  292,  292,
            325,  325,  325,  162,  162,  162),
  Prevalence = c(7.85, 6.97, 1.88, 7.56, 7.21, 2.03,
                 12.1, 8.63, 4.63, 10.4, 12.5, 4.92,
                 12.1, 12.3, 12.3, 17.2, 14.8, 12.6,
                 8.87, 12.2, 26.1, 14.0, 16.3, 26.0,
                 4.68, 11.1, 38.0, 7.92, 26.9, 35.0),
  Sex = c("Female", "Female", "Female", "Male", "Male", "Male",
          "Female", "Female", "Female", "Male", "Male", "Male",
          "Female", "Female", "Female", "Male", "Male", "Male",
          "Female", "Female", "Female", "Male", "Male", "Male",
          "Female", "Female", "Female", "Male", "Male", "Male"),
  Diagnosis = factor(c("CU", "MCI", "Dementia", "CU", "MCI", "Dementia",
                       "CU", "MCI", "Dementia", "CU", "MCI", "Dementia",
                       "CU", "MCI", "Dementia", "CU", "MCI", "Dementia",
                       "CU", "MCI", "Dementia", "CU", "MCI", "Dementia",
                       "CU", "MCI", "Dementia", "CU", "MCI", "Dementia"), levels = c("CU", "MCI", "Dementia")),
  ADNC = c(164, 134, 32, 155, 140, 39,
           169, 116, 52, 133, 148, 56,
           107, 103, 78, 133, 103, 71,
           59, 75, 105, 57, 59, 64,
           25, 51, 110, 20, 38, 52)
)

# Plot generation
pdf(file = "RESULTS AND GRAPHS/GRAPHS/Figure 3.pdf", width = 12, height = 5, colormodel = "rgb")
#png(filename = "RESULTS AND GRAPHS/GRAPHS/Figure 3.png", width = 12, height = 5, units = "in", res = 300)
# First graph (Prevalence)
p1 <- ggplot(data1, aes(x = Sex, y = Prevalence, fill = Diagnosis)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  facet_grid(~Age) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) +
  labs(y = "Prevalence of ADNC (%)") +  
  theme_light() +
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position = "none",  
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_blank())  
# Second graph (cases of ADNC)
p2 <- ggplot(data1, aes(x = Sex, y = ADNC, fill = Diagnosis)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(aes(label = ADNC), position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
  facet_grid(~Age) +
  labs(y = "Number of ADNC cases") + 
  theme_light() +
  scale_fill_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.x = element_blank())  
# Merge graphs and share legend
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
dev.off()
rm(p1, p2, data1)

##### Supplementary Figure 2 #####
# These values are used for the Total in the graph and complement Supplementary Table 4
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Female" & Age_cat_NT4BLM == "[70,75)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Female" & Age_cat_NT4BLM == "[75,80)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Female" & Age_cat_NT4BLM == "[80,85)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Female" & Age_cat_NT4BLM == "[85,90)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Female" & Age_cat_NT4BLM == "[90,Inf)"))

svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Male" & Age_cat_NT4BLM == "[70,75)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Male" & Age_cat_NT4BLM == "[75,80)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Male" & Age_cat_NT4BLM == "[80,85)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Male" & Age_cat_NT4BLM == "[85,90)"))
svyciprop(~I(H4CutHigh0.63 == 1), design = subset(design_HUNT4, subset = Sex == "Male" & Age_cat_NT4BLM == "[90,Inf)"))
# data
data1 <- data.frame(
Sex = c("Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female", "Male", "Male", "Male", "Male", "Male"),
Age = c("70-74", "70-74", "70-74", "75-79", "75-79", "75-79", "80-84", "80-84", "80-84", "85-89", "85-89", "85-89", "90+", "90+", "90+", "70-74", "70-74", "70-74", "75-79", "75-79", "75-79", "80-84", "80-84", "80-84", "85-89", "85-89", "85-89", "90+", "90+", "90+", "70-74", "75-79", "80-84", "85-89", "90+", "70-74", "75-79", "80-84", "85-89", "90+"),
Dx  = c("Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Preclin. AD", "Prodro. AD", "AD Dementia", "Total", "Total", "Total", "Total", "Total", "Total", "Total", "Total", "Total", "Total"),
Pre = c(7.85, 6.97, 1.88, 12.1, 8.63, 4.63, 12.1, 12.3, 12.3, 8.87, 12.2, 26.1, 4.68, 11.1, 38, 7.56, 7.46, 2.23, 10.4, 12.5, 5.49, 17.2, 14.8, 12.6, 14, 16.3, 26, 7.92, 16.9, 35, 17.8, 27.0, 40.4, 53.9, 62.0, 18.3, 30.0, 48.9, 66.0, 70.8),
LI  = c(6.75, 5.9, 1.33, 10.4, 7.22, 3.54, 10, 10.2, 9.95, 6.88, 9.74, 22.1, 3.15, 8.44, 32.7, 6.73, 6.33, 1.62, 8.77, 10.7, 4.23, 14.6, 12.3, 10.1, 10.9, 12.7, 20.9, 5.08, 12.33, 27.9, 16.1, 24.6, 36.9, 49.3, 56.5, 16.5, 27.3, 45.0, 60.2, 63.0),
LS  = c(9.11, 8.22, 2.66, 13.9, 10.3, 6.04, 14.5, 14.8, 15.1, 11.4, 15.1, 30.6, 6.9, 14.3, 43.6, 9.16, 8.77, 3.07, 12.2, 14.5, 7.09, 20.1, 17.7, 15.6, 18, 20.6, 31.7, 12.1, 22.6, 43, 19.6, 29.6, 44.1, 58.4, 67.2, 20.1, 32.7, 52.8, 71.3, 77.5)
)
data1$Dx <- factor(data1$Dx, levels = c("Preclin. AD", "Prodro. AD", "AD Dementia", "Total"))

pdf(file = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 2.pdf", width = 9, height = 7)
#png(filename = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 2.png", width = 9, height = 7, units = "in", res = 500)
ggplot(data = data1, mapping = aes(x = Age, y = Pre, group = Sex, colour = Sex)) + 
  geom_point(position = position_dodge(width = 0.2)) + 
  geom_line(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = LI, ymax = LS), width = 0.2, position = position_dodge(width = 0.2)) +
  facet_wrap(~Dx, ncol = 2, nrow = 2) +
  theme_light() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Age", y = "ADNC prevalence (%)") +  
  theme(legend.position = "bottom", legend.title = element_blank()) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) 
dev.off()

rm(data1)

##### Supplementary Figure 3 #####
png(filename = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 3.png", units = "in", width = 7, height = 5, res = 600)

mod1 <- lm(log(PlaPTau217NT4_113229) ~ eGFR_H4, weights = weight_H4, data = data)
seg1 <- segmented(mod1, seg.Z = ~ eGFR_H4)
summary(seg1)

pdf(file = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 3.pdf", width = 7, height = 5)
#png(filename = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 3.png", units = "in", width = 7, height = 5, res = 600)
par(mar = c(4,4,1,1))
plot(data$PlaPTau217NT4_113229 ~ data$eGFR_H4, pch = 19, cex = 0.5,
     xlab = expression("eGFR (ml/min/1.73 m"^2*")"),
     ylab = "Plasma pTau217 (pg/mL)")
x <- cbind(seg1$model$eGFR_H4, exp(seg1$fitted.values))
x <- x[order(x[,1]), ]
lines(x, col = "blue", lwd = 2)
abline(v = seg1$psi[2], lty = 2, col = "red")
text(seg1$psi[2], exp(max(seg1$model[,1])), 
     labels = paste("Cut-off =", round(seg1$psi[2], 2)), pos = 4, col = "red", cex = 0.8)
legend("topright", legend = c("Piecewise Regression", "Inflection Point"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.8)
dev.off()

##### Supplementary Figure 4 #####
Table <- data.frame(expand.grid(Age=levels(data$Age_cat_NT4BLM), Edu=levels(data$Education_cat), Sex=levels(data$Sex)), Pre = NA, LI = NA, LS = NA)
for(i in levels(design_HUNT4$variables$Age_cat_NT4BLM)){
  for(j in levels(design_HUNT4$variables$Sex)){
    a <- svyciprop(~I(H4CutHigh0.63 == 1 & Education_cat == "Primary"), 
                   design = subset(design_HUNT4, subset = Age_cat_NT4BLM == i & Sex == j))
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Primary", 4] <- round(a[1]*100,2)
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Primary", 5] <- round(confint(a)[1]*100,2)
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Primary", 6] <- round(confint(a)[2]*100,2)
    
    b <- svyciprop(~I(H4CutHigh0.63 == 1 & Education_cat == "Secondary"), 
                   design = subset(design_HUNT4, subset = Age_cat_NT4BLM == i & Sex == j))
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Secondary", 4] <- round(b[1]*100,2)
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Secondary", 5] <- round(confint(b)[1]*100,2)
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Secondary", 6] <- round(confint(b)[2]*100,2)
    
    c <- svyciprop(~I(H4CutHigh0.63 == 1 & Education_cat == "Tertiary"), 
                   design = subset(design_HUNT4, subset = Age_cat_NT4BLM == i & Sex == j))
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Tertiary", 4] <- round(c[1]*100,2)
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Tertiary", 5] <- round(confint(c)[1]*100,2)
    Table[Table$Age == i & Table$Sex == j & Table$Edu == "Tertiary", 6] <- round(confint(c)[2]*100,2)
  }
}

pdf(file = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 4.pdf", width = 8, height = 4)
#png(filename = "RESULTS AND GRAPHS/GRAPHS/Supplementary Figure 4.png", width = 8, height = 4, units = "in", res = 500)
ggplot(data = Table, mapping = aes(x = Age, y = Pre, group = Edu, colour = Edu)) + 
  geom_point(position = position_dodge(width = 0.2)) + 
  geom_line(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = LI, ymax = LS), width = 0.2, position = position_dodge(width = 0.2)) +
  facet_wrap(~Sex, ncol = 2, nrow = 2) +
  theme_light() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Age", y = "ADNC prevalence (%)") +  
  theme(legend.position = "bottom", legend.title = element_blank()) +  
  scale_x_discrete(labels = c("[70,75)" = "70-74", 
                              "[75,80)" = "75-79", 
                              "[80,85)" = "80-84", 
                              "[85,90)" = "85-89",
                              "[90,Inf)" = "90 +")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) 
dev.off()

rm(a,b,c,i,j,Table)

