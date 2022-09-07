####Heterosis and transgression identification and plots####
#Updated 2022-09-05
#Analyses includes ANOVAs, general linear mixed models, linear models and linear mixed effect models
#Plots by ggplot2 with gridExtra
#R version 4.1.0 (2021-05-18)
#Packages used - car, dplyr, emmeans, ggplot2, gridExtra and MASS

####install prerequisite packages####
install.packages(c("dplyr", "ggplot2", "gridExtra"))

####Heterosis####

#####Import data#####
setwd("C:/Target/Directory")
#Change to directory containing fitness tables - "Table S1.txt" and "Table S2.txt"

phenotype <- read.delim("Table S1.txt")
#relative fitness
#Absolute fitness values in table S2
#phenotype <- read.delim("Table S2.txt")
str(phenotype)
#Population is which population is being tested 
#There are 4 parental populations - EvoN (adapted in NaCl 0.75M), EvoE (adapted in Ethanol 8%), EvoL1 (adapted in LiAc 0.01M) and EvoL2 (adapted in LiAc 0.02M)
#There are also 3 hybrid crosses (H1-H3), both F1 and F2. H1 is a cross between EvoN x EvoL1, H2 is a cross between EvoN and EvoL2, and H3 is a cross between EvoN and EvoE
#Generation is the number of generations adapted for the parental populations, or parental divergence for the hybrids
#Cross is a combination of population and replicate for analyses and plotting
#Replicate is an independent replicate population (and indicates which replicate of the parental populations make up the hybrid crosses)
#Phenotypes.GenerationTime is the absolute generation time in hours
#Phenotypes.ExperimentGrowthGT is the absolute final GT (number of cells) after 48h growth
#Environment is which environment phenotype is bring tested in
phenotype$Group <- as.factor(phenotype$Group)
phenotype$Population <- as.factor(phenotype$Population)
phenotype$Environment <- as.factor(phenotype$Environment)
phenotype$Replicate <- as.factor(phenotype$Replicate)

str(phenotype)

Env_All_h1_h2 <- c("NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M","LiAc 0.02M",  "Glucose 2%","1-Butanol 1.5%", "1-Butanol 3%",
                   "AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", "As(V) 0.1 mM", "BaCl 0.3M", 
                   "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", "DMSO 5%",
                   "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", "H2O2 3mM",
                   "HU 10mg/mL", "Lactose 2%", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%",
                   "MnCl2 1mM", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", "Rapamycin 0.8ug/mL", "RbCl 400 mM",
                   "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", "Xylose 8%", "ZnCl2 7mM")

Env_All_h3 <- c("NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M", "1-Butanol 1.5%", "1-Butanol 3%", "Glucose 2%",
                "AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", "As(V) 0.1 mM", "BaCl 0.3M", 
                "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", "DMSO 5%",
                "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", "H2O2 3mM",
                "HU 10mg/mL", "Lactose 2%", "LiAc 0.02M", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%",
                "MnCl2 1mM", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", "Rapamycin 0.8ug/mL", "RbCl 400 mM",
                "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", "Xylose 8%", "ZnCl2 7mM")


#####LiAc 0.01 x NaCl#####
h1 <- subset(phenotype, Population == "EvoN"| Population == "EvoL1" | Population == "EvoLE" | Population == "H1F1")
#Change to EvoL2 & H2F1 or EvoE & H3F1 here and below
h1$Population <- as.character(h1$Population)
h1$Population[h1$Population=="EvoN"]<-"P1"
h1$Population[h1$Population=="EvoLE"]<-"P2"
h1$Population[h1$Population=="EvoL1"]<-"P2"
h1$Population[h1$Population=="H1F1"]<-"F1"

NaCl <- subset(h1, Population == "P1")
NaCl <- subset(NaCl, Replicate != "R3")
N300_700 <- subset(NaCl, Generation == "300" | Generation == "700")
N300_700 <- subset(N300_700, Replicate != "R4")
N300_700 <- droplevels(N300_700)
str(N300_700)
NaCl <- subset(NaCl, Generation == "100" | Generation == "500") #Add | Generation == "1000" if cross 3
NaCl <- droplevels(NaCl)
NaCl <- rbind(NaCl, N300_700)

LiAc0.01 <- subset(h1, Population == "P2")
L300_700 <- subset(LiAc0.01, Generation == "300" | Generation == "700")
L300_700 <- subset(L300_700, Replicate != "R4")
L300_700 <- droplevels(L300_700)
str(L300_700)
#For F1 hybrids only 3 replicates are present at gens 300 & 700. 4th replicate is dropped for parents
LiAc0.01 <- subset(LiAc0.01, Generation == "100" | Generation == "500") #Add | Generation == "1000" if cross 3
LiAc0.01 <- droplevels(LiAc0.01)
LiAc0.01 <- rbind(LiAc0.01, L300_700)

F1 <- subset(h1, Population == "F1")
F1 <- droplevels(F1)
str(F1)
h1 <- rbind(F1, LiAc0.01, NaCl)
LiAc0.01$Population <- "P"
NaCl$Population <- "P"
h1 <- rbind(h1, LiAc0.01, NaCl)
#Need midparent mean (for all parents) and separate parental means. P is used for midparents, P1 & P2 for NaCl and other parent respectively
h1 <- droplevels(h1)
h1$Population <- as.factor(h1$Population)
h1$Population <- ordered(h1$Population, levels = c("P", "P1", "P2", "F1"))
h1$Environment <- ordered(h1$Environment, levels = c(Env_All_h1_h2))
#Change to Env_All_h3 if cross 3
h1 <- subset(h1, Generation != "0")
#h1_gen_1000 <- subset(h1, Generation =="1000")
#Only needed for cross 3
h1 <- subset(h1, Generation != "1000")
#h1_gen_1000 <- subset(h1_gen_1000, Replicate == "R2" | Replicate == "R3")
#Only needed for cross 3
#h1_gen_1000 <- droplevels(h1_gen_1000)
#str(h1_gen_1000)
#h1 <- rbind(h1, h1_gen_1000)
h1 <- droplevels(h1)
str(h1)
levels(h1$Replicate)
levels(h1$Environment)
levels(h1$Population)

library(dplyr)
h1_yield_by_environment <- h1 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
h1_yield_by_environment$ci <- 1.96*(h1_yield_by_environment$sd/sqrt(h1_yield_by_environment$n))

P1 <- subset(h1_yield_by_environment, Population == "P1")
P2 <- subset(h1_yield_by_environment, Population == "P2")
P <- subset(h1_yield_by_environment, Population == "P")
F1 <- subset(h1_yield_by_environment, Population == "F1")

P1$midparent <- P$mean_yield
P1$additive <- P1$mean_yield-P1$midparent
P2$midparent <- P$mean_yield
P2$additive <- P2$mean_yield-P2$midparent
P <- rbind(P1, P2)
P <- subset(P, additive >= 0)
P$Population <- "P"
additive <- P %>% group_by(Population, Generation, Environment, Replicate)

F1$midparent <- P$mean_yield
F1$dominant <- F1$mean_yield-F1$midparent
F1$additive <- additive$additive
F1$heterosis <- F1$dominant/F1$additive
F1$overdominance <- ifelse(F1$heterosis>1.2, "1", "0")
F1$dominance_category <- ifelse(F1$heterosis< -1.2, "underdominance", ifelse(F1$heterosis>1.2, "overdominance", ifelse(F1$heterosis>=0.8, "complete dominance of more fit parent", ifelse(F1$heterosis<=-0.8, "complete dominance of less fit parent", ifelse(F1$heterosis<0.2, "partial dominance of less fit parent", ifelse(F1$heterosis>0.2,"partial dominance of more fit parent","co-dominance"))))))
#Classes are as follows
# H<-1.2 - underdominant, -1.2<=H<=-0.8 - complete dominance of less fit parent, -0.8<H<-0.2 - partial dominance of less fit parent, 
# -0.2<=H<=0.2 - co-dominance, 0.2<H<0.8 - partial dominance of more fit parent, 0.8<=H<=1.2 - complete dominance of more fit parent, H>1.2 - overdominance
# classed as heterosis if overdominant
write.table(F1, file = "Stats/Heterosis_F1s_Cross1Yield.txt", sep = "\t", row.names = F)

Heterosis <- read.delim("Stats/Heterosis_F1s_Cross1Yield.txt")
Heterosis <- subset(Heterosis, overdominance == "1")

Heterosis_Cross1 <- Heterosis %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
Heterosis_Cross1$Prop <- Heterosis_Cross1$n/50
Heterosis_Cross1$Population <- "NaCl x LiAc0.01"
write.table(Heterosis_Cross1, file = "Stats/Heterosis_Cross1Yield_Prop.txt", sep = "\t", row.names = F)

#####LiAc 0.02M x NaCl#####

h2 <- subset(phenotype, Population == "EvoN"| Population == "EvoL2" | Population == "EvoLE" | Population == "H2F1")
#Change to EvoL2 & H2F1 or EvoE & H3F1 here and below
h2$Population <- as.character(h2$Population)
h2$Population[h2$Population=="EvoN"]<-"P1"
h2$Population[h2$Population=="EvoLE"]<-"P2"
h2$Population[h2$Population=="EvoL2"]<-"P2"
h2$Population[h2$Population=="H2F1"]<-"F1"

parental <- subset(h2, Population != "F1")
P300_700 <- subset(parental, Generation == "300" | Generation == "700")
P300_700 <- subset(P300_700, Replicate != "R4")
P300_700 <- droplevels(P300_700)
str(P300_700)
#For F1 hybrids only 3 replicates are present at gens 300 & 700. 4th replicate is dropped for parents
parental <- subset(parental, Generation == "100" | Generation == "500") #Add | Generation == "1000" if cross 3
parental <- droplevels(parental)
parental <- rbind(parental, P300_700)
F1 <- subset(h2, Population == "F1")
F1 <- droplevels(F1)
str(F1)
h2 <- rbind(F1, parental)
parental$Population <- "P"
h2 <- rbind(h2, parental)
#Need midparent mean (for all parents) and separate parental means. P is used for midparents, P1 & P2 for NaCl and other parent respectively
h2 <- droplevels(h2)
h2$Population <- as.factor(h2$Population)
h2$Population <- ordered(h2$Population, levels = c("P", "P1", "P2", "F1"))
h2$Environment <- ordered(h2$Environment, levels = c(Env_All_h1_h2))
#Change to Env_All_h3 if cross 3
h2 <- subset(h2, Generation != "0")
#h2_gen_1000 <- subset(h2, Generation =="1000")
#Only needed for cross 3
h2 <- subset(h2, Generation != "1000")
#h2_gen_1000 <- subset(h2_gen_1000, Replicate == "R2" | Replicate == "R3")
#Only needed for cross 3
#h2_gen_1000 <- droplevels(h2_gen_1000)
#str(h2_gen_1000)
#h2 <- rbind(h2, h2_gen_1000)
h2 <- droplevels(h2)
str(h2)
levels(h2$Replicate)
levels(h2$Environment)
levels(h2$Population)

library(dplyr)
h2_yield_by_environment <- h2 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
h2_yield_by_environment$ci <- 1.96*(h2_yield_by_environment$sd/sqrt(h2_yield_by_environment$n))

P1 <- subset(h2_yield_by_environment, Population == "P1")
P2 <- subset(h2_yield_by_environment, Population == "P2")
P <- subset(h2_yield_by_environment, Population == "P")
F1 <- subset(h2_yield_by_environment, Population == "F1")

P1$midparent <- P$mean_yield
P1$additive <- P1$mean_yield-P1$midparent
P2$midparent <- P$mean_yield
P2$additive <- P2$mean_yield-P2$midparent
P <- rbind(P1, P2)
P <- subset(P, additive >= 0)
P$Population <- "P"
additive <- P %>% group_by(Population, Generation, Environment, Replicate)

F1$midparent <- P$mean_yield
F1$dominant <- F1$mean_yield-F1$midparent
F1$additive <- additive$additive
F1$heterosis <- F1$dominant/F1$additive
F1$overdominance <- ifelse(F1$heterosis>1.2, "1", "0")
F1$dominance_category <- ifelse(F1$heterosis< -1.2, "underdominance", ifelse(F1$heterosis>1.2, "overdominance", ifelse(F1$heterosis>=0.8, "complete dominance of more fit parent", ifelse(F1$heterosis<=-0.8, "complete dominance of less fit parent", ifelse(F1$heterosis<0.2, "partial dominance of less fit parent", ifelse(F1$heterosis>0.2,"partial dominance of more fit parent","co-dominance"))))))
#Classes are as follows
# H<-1.2 - underdominant, -1.2<=H<=-0.8 - complete dominance of less fit parent, -0.8<H<-0.2 - partial dominance of less fit parent, 
# -0.2<=H<=0.2 - co-dominance, 0.2<H<0.8 - partial dominance of more fit parent, 0.8<=H<=1.2 - complete dominance of more fit parent, H>1.2 - overdominance
# classed as heterosis if overdominant
write.table(F1, file = "Stats/Heterosis_F1s_Cross2Yield.txt", sep = "\t", row.names = F)

Heterosis <- read.delim("Stats/Heterosis_F1s_Cross2Yield.txt")
Heterosis <- subset(Heterosis, overdominance == "1")

Heterosis_Cross2 <- Heterosis %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
Heterosis_Cross2$Prop <- Heterosis_Cross2$n/50
Heterosis_Cross2$Population <- "NaCl x LiAc0.02"

write.table(Heterosis_Cross2, file = "Stats/Heterosis_Cross2Yield_Prop.txt", sep = "\t", row.names = F)

#####EtOH x NaCl#####

h3 <- subset(phenotype, Population == "EvoN"| Population == "EvoE" | Population == "EvoLE" | Population == "H3F1")
#Change to EvoE & H3F1 or EvoE & H3F1 here and below
h3$Population <- as.character(h3$Population)
h3$Population[h3$Population=="EvoN"]<-"P1"
h3$Population[h3$Population=="EvoLE"]<-"P2"
h3$Population[h3$Population=="EvoE"]<-"P2"
h3$Population[h3$Population=="H3F1"]<-"F1"

parental <- subset(h3, Population != "F1")
P300_700 <- subset(parental, Generation == "300" | Generation == "700")
P300_700 <- subset(P300_700, Replicate != "R4")
P300_700 <- droplevels(P300_700)
str(P300_700)
#For F1 hybrids only 3 replicates are present at gens 300 & 700. 4th replicate is dropped for parents
parental <- subset(parental, Generation == "100" | Generation == "500" | Generation == "1000") #Add | Generation == "1000" if cross 3
parental <- droplevels(parental)
parental <- rbind(parental, P300_700)
F1 <- subset(h3, Population == "F1")
F1 <- droplevels(F1)
str(F1)
h3 <- rbind(F1, parental)
parental$Population <- "P"
h3 <- rbind(h3, parental)
#Need midparent mean (for all parents) and separate parental means. P is used for midparents, P1 & P2 for NaCl and other parent respectively
h3 <- droplevels(h3)
h3$Population <- as.factor(h3$Population)
h3$Population <- ordered(h3$Population, levels = c("P", "P1", "P2", "F1"))
h3$Environment <- ordered(h3$Environment, levels = c(Env_All_h3))
#Change to Env_All_h3 if cross 3
h3 <- subset(h3, Generation != "0")
h3_gen_1000 <- subset(h3, Generation =="1000")
#Only needed for cross 3
h3 <- subset(h3, Generation != "1000")
h3_gen_1000 <- subset(h3_gen_1000, Replicate == "R2" | Replicate == "R3")
#Only needed for cross 3
h3_gen_1000 <- droplevels(h3_gen_1000)
str(h3_gen_1000)
h3 <- rbind(h3, h3_gen_1000)
h3 <- droplevels(h3)
str(h3)
levels(h3$Replicate)
levels(h3$Environment)
levels(h3$Population)

library(dplyr)
h3_yield_by_environment <- h3 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
h3_yield_by_environment$ci <- 1.96*(h3_yield_by_environment$sd/sqrt(h3_yield_by_environment$n))

P1 <- subset(h3_yield_by_environment, Population == "P1")
P2 <- subset(h3_yield_by_environment, Population == "P2")
P <- subset(h3_yield_by_environment, Population == "P")
F1 <- subset(h3_yield_by_environment, Population == "F1")

P1$midparent <- P$mean_yield
P1$additive <- P1$mean_yield-P1$midparent
P2$midparent <- P$mean_yield
P2$additive <- P2$mean_yield-P2$midparent
P <- rbind(P1, P2)
P <- subset(P, additive >= 0)
P$Population <- "P"
additive <- P %>% group_by(Population, Generation, Environment, Replicate)

F1$midparent <- P$mean_yield
F1$dominant <- F1$mean_yield-F1$midparent
F1$additive <- additive$additive
F1$heterosis <- F1$dominant/F1$additive
F1$overdominance <- ifelse(F1$heterosis>1.2, "1", "0")
F1$dominance_category <- ifelse(F1$heterosis< -1.2, "underdominance", ifelse(F1$heterosis>1.2, "overdominance", ifelse(F1$heterosis>=0.8, "complete dominance of more fit parent", ifelse(F1$heterosis<=-0.8, "complete dominance of less fit parent", ifelse(F1$heterosis<0.2, "partial dominance of less fit parent", ifelse(F1$heterosis>0.2,"partial dominance of more fit parent","co-dominance"))))))
#Classes are as follows
# H<-1.2 - underdominant, -1.2<=H<=-0.8 - complete dominance of less fit parent, -0.8<H<-0.2 - partial dominance of less fit parent, 
# -0.2<=H<=0.2 - co-dominance, 0.2<H<0.8 - partial dominance of more fit parent, 0.8<=H<=1.2 - complete dominance of more fit parent, H>1.2 - overdominance
# classed as heterosis if overdominant
write.table(F1, file = "Stats/Heterosis_F1s_Cross3Yield.txt", sep = "\t", row.names = F)

Heterosis <- read.delim("Stats/Heterosis_F1s_Cross3Yield.txt")
Heterosis <- subset(Heterosis, overdominance == "1")

Heterosis_Cross3 <- Heterosis %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
Heterosis_Cross3$Prop <- Heterosis_Cross3$n/50
Heterosis_Cross3$Population <- "NaCl x EtOH"

write.table(Heterosis_Cross3, file = "Stats/Heterosis_Cross3Yield_Prop.txt", sep = "\t", row.names = F)

Heterosis_all <- rbind(Heterosis_Cross1, Heterosis_Cross2, Heterosis_Cross3)
write.table(Heterosis_all, file = "Stats/Heterosis_Yield_relative.txt", sep = "\t", row.names = F)

#####Compare Environments Identified#####
library(dplyr)

Heterosis1 <- read.delim("Stats/Heterosis_F1s_Cross1Yield.txt")
Heterosis1 <- subset(Heterosis1, overdominance == "1")
Heterosis1 <- Heterosis1 %>% group_by(Population, Generation, Environment) %>% summarise(n = n())
Heterosis1unique <- subset(Heterosis1, n == "1")
Heterosis1unique <- Heterosis1unique %>% group_by(Population, Generation) %>% summarise(n = n())
Heterosis1common <- subset(Heterosis1, n != "1")
Heterosis1common <- Heterosis1common %>% group_by(Population, Generation) %>% summarise(n = n())
Heterosis1unique$Proportion <- Heterosis1unique$n/Heterosis1common$n
Heterosis1unique$Total <- Heterosis1unique$n+Heterosis1common$n
Heterosis1unique$Proportion <- Heterosis1unique$n/Heterosis1unique$Total

Heterosis2 <- read.delim("Stats/Heterosis_F1s_Cross2Yield.txt")
Heterosis2 <- subset(Heterosis2, overdominance == "1")
Heterosis2 <- Heterosis2 %>% group_by(Population, Generation, Environment) %>% summarise(n = n())
Heterosis2unique <- subset(Heterosis2, n == "1")
Heterosis2unique <- Heterosis2unique %>% group_by(Population, Generation) %>% summarise(n = n())
Heterosis2common <- subset(Heterosis2, n != "1")
Heterosis2common <- Heterosis2common %>% group_by(Population, Generation) %>% summarise(n = n())
Heterosis2unique$Total <- Heterosis2unique$n+Heterosis2common$n
Heterosis2unique$Proportion <- Heterosis2unique$n/Heterosis2unique$Total

Heterosis3 <- read.delim("Stats/Heterosis_F1s_Cross3Yield.txt")
Heterosis3 <- subset(Heterosis3, overdominance == "1")
Heterosis3 <- Heterosis3 %>% group_by(Population, Generation, Environment) %>% summarise(n = n())
Heterosis3unique <- subset(Heterosis3, n == "1")
Heterosis3unique <- Heterosis3unique %>% group_by(Population, Generation) %>% summarise(n = n())
Heterosis3common <- subset(Heterosis3, n != "1")
Heterosis3common <- Heterosis3common %>% group_by(Population, Generation) %>% summarise(n = n())
Heterosis3unique$Proportion <- Heterosis3unique$n/Heterosis3common$n
Heterosis3unique$Total <- Heterosis3unique$n+Heterosis3common$n
Heterosis3unique$Proportion <- Heterosis3unique$n/Heterosis3unique$Total

#####Get percentages for each generation#####

Heterosis_all <- read.delim("Stats/Heterosis_Yield_relative.txt")
Heterosis_gen <- Heterosis_all %>% group_by(Generation) %>% summarise(mean = mean(Prop))


Heterosis1 <- read.delim("Stats/Heterosis_F1s_Cross1Yield.txt")
Heterosis1$Population <- "NaCl x LiAc0.01"
Heterosis1 <- subset(Heterosis1, dominance_category == "underdominance")
Heterosis_Cross1 <- Heterosis1 %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
Heterosis_Cross1$Prop <- Heterosis_Cross1$n/50


Heterosis2 <- read.delim("Stats/Heterosis_F1s_Cross2Yield.txt")
Heterosis2$Population <- "NaCl x LiAc0.02"
Heterosis2 <- subset(Heterosis2, dominance_category == "underdominance")
Heterosis_Cross2 <- Heterosis2 %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
Heterosis_Cross2$Prop <- Heterosis_Cross2$n/50

Heterosis3 <- read.delim("Stats/Heterosis_F1s_Cross3Yield.txt")
Heterosis3$Population <- "NaCl x EtOH"
Heterosis3 <- subset(Heterosis3, dominance_category == "underdominance")
Heterosis_Cross3 <- Heterosis3 %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
Heterosis_Cross3$Prop <- Heterosis_Cross3$n/50

Heterosis_all <- rbind(Heterosis_Cross1, Heterosis_Cross2, Heterosis_Cross3)
write.table(Heterosis_all, file = "Stats/Heterosis_Negative_Yield_relative.txt", sep = "\t", row.names = F)

Heterosis_gen <- Heterosis_all %>% group_by(Generation) %>% summarise(mean = mean(Prop))
rm(list = ls())

####Transgressive F2 Hybrids####
#Calculating proportion of environments hybrids are transgressive in
#Calculate mean + SD of "midparent value" - per environment and generation (and for each replicate?)
#Calculate mean of F2 hybrids
#dplyr for above
#If >2x SD of parental mean = positive transgression (1)
#If <2x SD of parental mean = negative transgression (-1)
#Else - no transgression (0)
#Calculate percentage of transgressives at each generation/replicate combo
#Percentage based on cross (look at cross between NaCl x EtOH and LiAc0.02M)
#Percentage based on generation within cross
#Variation between replicates

#####Import data#####
setwd("C:/Target/Directory")
# Set as directory containing Table S1 or S2
phenotype <- read.delim("Table S1.txt")
#relative fitness
#phenotype <- read.delim("Table S2.txt")
#absolute fitness
str(phenotype)
#Population is which population is being tested 
#There are 4 parental populations - EvoN (adapted in NaCl 0.75M), EvoE (adapted in Ethanol 8%), EvoL1 (adapted in LiAc 0.01M) and EvoL2 (adapted in LiAc 0.02M)
#There are also 3 hybrid crosses (H1-H3), both F1 and F2. H1 is a cross between EvoN x EvoL1, H2 is a cross between EvoN and EvoL2, and H3 is a cross between EvoN and EvoE
#Generation is the number of generations adapted for the parental populations, or parental divergence for the hybrids
#Group is a combination of population and replicate for analyses and plotting
#Replicate is an independent replicate population (and indicates which replicate of the parental populations make up the hybrid crosses)
#Phenotypes.GenerationTime is the absolute generation time in hours
#Phenotypes.ExperimentGrowthYield is the absolute final yield (number of cells) after 48h growth
#Environment is which environment phenotype is bring tested in
phenotype$Group <- as.factor(phenotype$Group)
phenotype$Population <- as.factor(phenotype$Population)
phenotype$Environment <- as.factor(phenotype$Environment)
phenotype$Replicate <- as.factor(phenotype$Replicate)

str(phenotype)

Env_All_h1_h2 <- c("NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M","LiAc 0.02M",  "Glucose 2%","1-Butanol 1.5%", "1-Butanol 3%",
                   "AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", "As(V) 0.1 mM", "BaCl 0.3M", 
                   "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", "DMSO 5%",
                   "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", "H2O2 3mM",
                   "HU 10mg/mL", "Lactose 2%", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%",
                   "MnCl2 1mM", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", "Rapamycin 0.8ug/mL", "RbCl 400 mM",
                   "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", "Xylose 8%", "ZnCl2 7mM")

Env_All_h3 <- c("NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M", "1-Butanol 1.5%", "1-Butanol 3%", "Glucose 2%",
                "AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", "As(V) 0.1 mM", "BaCl 0.3M", 
                "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", "DMSO 5%",
                "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", "H2O2 3mM",
                "HU 10mg/mL", "Lactose 2%", "LiAc 0.02M", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%",
                "MnCl2 1mM", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", "Rapamycin 0.8ug/mL", "RbCl 400 mM",
                "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", "Xylose 8%", "ZnCl2 7mM")

#####LiAc0.01 x NaCl####
h1 <- subset(phenotype, Population == "EvoN"| Population == "EvoL1" | Population == "EvoLE" | Population == "H1F2")
#Change to EvoL1, H1F1 & H1F2 or EvoL1, H1F1 & H1F2 here and below
h1$Population <- as.character(h1$Population)
h1_NaCl <- subset(h1, Population =="EvoN")
h1_NaCl <- subset(h1_NaCl, Replicate !="R3")
h1_other <- subset(h1, Population != "EvoN")
h1 <- rbind(h1_NaCl, h1_other)

h1$Population[h1$Population=="EvoN"]<-"P"
h1$Population[h1$Population=="EvoLE"]<-"P"
h1$Population[h1$Population=="EvoL1"]<-"P"
h1$Population[h1$Population=="H1F2"]<-"F2"
h1 <- droplevels(h1)
h1$Population <- as.factor(h1$Population)
h1$Population <- ordered(h1$Population, levels = c("P", "F2"))
h1$Environment <- ordered(h1$Environment, levels = c(Env_All_h1_h2))
#Change to Env_All_h1 if cross 3
h1 <- subset(h1, Generation != "0")
#h1_gen_1000 <- subset(h1, Generation =="1000")
#Only needed for cross 3
h1 <- subset(h1, Generation != "1000")
#h1_gen_1000 <- subset(h1_gen_1000, Replicate == "R2" | Replicate == "R3")
#Only needed for cross 3
#h1_gen_1000 <- droplevels(h1_gen_1000)
#str(h1_gen_1000)
#h1 <- rbind(h1, h1_gen_1000)

h1 <- droplevels(h1)
str(h1)
levels(h1$Replicate)
levels(h1$Environment)
levels(h1$Population)

#Identifying environments with transgressive hybrids
library(dplyr)
h1_yield_by_environment <- h1 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
h1_yield_by_environment$ci <- 1.96*(h1_yield_by_environment$sd/sqrt(h1_yield_by_environment$n))
h1_yield_by_environment$doubleSD <- 2*h1_yield_by_environment$sd

P <- subset(h1_yield_by_environment, Population == "P")
P$pos_t <- P$mean_yield + P$doubleSD
P$neg_t <- P$mean_yield - P$doubleSD
#Hybrids +ve trangression if >2xsd higher than midparent mean
#Hybrids -ve transgression if >2xsd lower than midparent mean
#Hybrids intermediate if none of the above are true
F2 <- subset(h1_yield_by_environment, Population == "F2")
F2$pos_par <- P$pos_t
F2$neg_par <- P$neg_t
F2$Transgression <- ifelse(F2$mean_yield > F2$pos_par, "1", ifelse(F2$mean_yield < F2$neg_par, "-1", "0"))
#+ve transgression = 1, -ve = -1, intermediate = 0
write.table(F2, file = "Stats/Transgressive_F2s_Cross1Yield.txt", sep = "\t", row.names = F)

F2 <- read.delim("Stats/Transgressive_F2s_Cross1Yield.txt")

F2pos <- subset(F2, Transgression == "1")
F2pos$Replicate <- as.factor(F2pos$Replicate)
F2pos$Population <- as.factor(F2pos$Population)
F2pos_Cross1 <- F2pos %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2pos_Cross1$Prop <- F2pos_Cross1$n/50
F2pos_Cross1$Population <- "NaCl x LiAc0.01"

write.table(F2pos_Cross1, file = "Stats/Pos_Transgression_Cross1Yield_Prop.txt", sep = "\t", row.names = F)

F2neg <- subset(F2, Transgression == "-1")
F2neg$Replicate <- as.factor(F2neg$Replicate)
F2neg$Population <- as.factor(F2neg$Population)
F2neg_Cross1 <- F2neg %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2neg_Cross1$Prop <- F2neg_Cross1$n/50
F2neg_Cross1$Population <- "NaCl x LiAc0.01"

write.table(F2neg_Cross1, file = "Stats/Neg_Transgression_Cross1Yield_Prop.txt", sep = "\t", row.names = F)

F2int <- subset(F2, Transgression == "0")
F2int$Replicate <- as.factor(F2int$Replicate)
F2int$Population <- as.factor(F2int$Population)
F2int_Cross1 <- F2int %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2int_Cross1$Prop <- F2int_Cross1$n/50
F2int_Cross1$Population <- "NaCl x LiAc0.01"

write.table(F2int_Cross1, file = "Stats/Int_Transgression_Cross1Yield_Prop.txt", sep = "\t", row.names = F)

#####LiAc0.02 x NaCl####

h2 <- subset(phenotype, Population == "EvoN"| Population == "EvoL2" | Population == "EvoLE" | Population == "H2F2")
#Change to EvoE, H2F1 & H2F2 or EvoE, H2F1 & H2F2 here and below
h2$Population <- as.character(h2$Population)
h2$Population[h2$Population=="EvoN"]<-"P"
h2$Population[h2$Population=="EvoLE"]<-"P"
h2$Population[h2$Population=="EvoL2"]<-"P"
h2$Population[h2$Population=="H2F2"]<-"F2"
h2 <- droplevels(h2)
h2$Population <- as.factor(h2$Population)
h2$Population <- ordered(h2$Population, levels = c("P", "F2"))
h2$Environment <- ordered(h2$Environment, levels = c(Env_All_h1_h2))
#Change to Env_All_h2 if cross 3
h2 <- subset(h2, Generation != "0")
#h2_gen_1000 <- subset(h2, Generation =="1000")
#Only needed for cross 3
h2 <- subset(h2, Generation != "1000")
#h2_gen_1000 <- subset(h2_gen_1000, Replicate == "R2" | Replicate == "R3")
#Only needed for cross 3
#h2_gen_1000 <- droplevels(h2_gen_1000)
#str(h2_gen_1000)
#h2 <- rbind(h2, h2_gen_1000)

h2 <- droplevels(h2)
str(h2)
levels(h2$Replicate)
levels(h2$Environment)
levels(h2$Population)

#Identifying environments with transgressive hybrids
library(dplyr)
h2_yield_by_environment <- h2 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
h2_yield_by_environment$ci <- 1.96*(h2_yield_by_environment$sd/sqrt(h2_yield_by_environment$n))
h2_yield_by_environment$doubleSD <- 2*h2_yield_by_environment$sd

P <- subset(h2_yield_by_environment, Population == "P")
P$pos_t <- P$mean_yield + P$doubleSD
P$neg_t <- P$mean_yield - P$doubleSD
#Hybrids +ve trangression if >2xsd higher than midparent mean
#Hybrids -ve transgression if >2xsd lower than midparent mean
#Hybrids intermediate if none of the above are true
F2 <- subset(h2_yield_by_environment, Population == "F2")
F2$pos_par <- P$pos_t
F2$neg_par <- P$neg_t
F2$Transgression <- ifelse(F2$mean_yield > F2$pos_par, "1", ifelse(F2$mean_yield < F2$neg_par, "-1", "0"))
#+ve transgression = 1, -ve = -1, intermediate = 0
write.table(F2, file = "Stats/Transgressive_F2s_Cross2Yield.txt", sep = "\t", row.names = F)

F2 <- read.delim("Stats/Transgressive_F2s_Cross2Yield.txt")

F2pos <- subset(F2, Transgression == "1")
F2pos$Replicate <- as.factor(F2pos$Replicate)
F2pos$Population <- as.factor(F2pos$Population)
F2pos_Cross2 <- F2pos %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2pos_Cross2$Prop <- F2pos_Cross2$n/50
F2pos_Cross2$Population <- "NaCl x LiAc0.02"

write.table(F2pos_Cross2, file = "Stats/Pos_Transgression_Cross2Yield_Prop.txt", sep = "\t", row.names = F)

F2neg <- subset(F2, Transgression == "-1")
F2neg$Replicate <- as.factor(F2neg$Replicate)
F2neg$Population <- as.factor(F2neg$Population)
F2neg_Cross2 <- F2neg %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2neg_Cross2$Prop <- F2neg_Cross2$n/50
F2neg_Cross2$Population <- "NaCl x LiAc0.02"

write.table(F2neg_Cross2, file = "Stats/Neg_Transgression_Cross2Yield_Prop.txt", sep = "\t", row.names = F)

F2int <- subset(F2, Transgression == "0")
F2int$Replicate <- as.factor(F2int$Replicate)
F2int$Population <- as.factor(F2int$Population)
F2int_Cross2 <- F2int %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2int_Cross2$Prop <- F2int_Cross2$n/50
F2int_Cross2$Population <- "NaCl x LiAc0.02"

write.table(F2int_Cross2, file = "Stats/Int_Transgression_Cross2Yield_Prop.txt", sep = "\t", row.names = F)

#####EtOH x NaCl####
h3 <- subset(phenotype, Population == "EvoN"| Population == "EvoE" | Population == "EvoLE" | Population == "H3F2")
#Change to EvoE, H3F1 & H3F2 or EvoE, H3F1 & H3F2 here and below
h3$Population <- as.character(h3$Population)
h3$Population[h3$Population=="EvoN"]<-"P"
h3$Population[h3$Population=="EvoLE"]<-"P"
h3$Population[h3$Population=="EvoE"]<-"P"
h3$Population[h3$Population=="H3F2"]<-"F2"
h3 <- droplevels(h3)
h3$Population <- as.factor(h3$Population)
h3$Population <- ordered(h3$Population, levels = c("P", "F2"))
h3$Environment <- ordered(h3$Environment, levels = c(Env_All_h3))
#Change to Env_All_h3 if cross 3
h3 <- subset(h3, Generation != "0")
h3_gen_1000 <- subset(h3, Generation =="1000")
#Only needed for cross 3
h3 <- subset(h3, Generation != "1000")
h3_gen_1000 <- subset(h3_gen_1000, Replicate == "R2" | Replicate == "R3")
#Only needed for cross 3
h3_gen_1000 <- droplevels(h3_gen_1000)
str(h3_gen_1000)
h3 <- rbind(h3, h3_gen_1000)

h3 <- droplevels(h3)
str(h3)
levels(h3$Replicate)
levels(h3$Environment)
levels(h3$Population)

#Identifying environments with transgressive hybrids
library(dplyr)
h3_yield_by_environment <- h3 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
h3_yield_by_environment$ci <- 1.96*(h3_yield_by_environment$sd/sqrt(h3_yield_by_environment$n))
h3_yield_by_environment$doubleSD <- 2*h3_yield_by_environment$sd

P <- subset(h3_yield_by_environment, Population == "P")
P$pos_t <- P$mean_yield + P$doubleSD
P$neg_t <- P$mean_yield - P$doubleSD
#Hybrids +ve trangression if >2xsd higher than midparent mean
#Hybrids -ve transgression if >2xsd lower than midparent mean
#Hybrids intermediate if none of the above are true
F2 <- subset(h3_yield_by_environment, Population == "F2")
F2$pos_par <- P$pos_t
F2$neg_par <- P$neg_t
F2$Transgression <- ifelse(F2$mean_yield > F2$pos_par, "1", ifelse(F2$mean_yield < F2$neg_par, "-1", "0"))
#+ve transgression = 1, -ve = -1, intermediate = 0
write.table(F2, file = "Stats/Transgressive_F2s_Cross3Yield.txt", sep = "\t", row.names = F)

F2 <- read.delim("Stats/Transgressive_F2s_Cross3Yield.txt")

F2pos <- subset(F2, Transgression == "1")
F2pos$Replicate <- as.factor(F2pos$Replicate)
F2pos$Population <- as.factor(F2pos$Population)
F2pos_Cross3 <- F2pos %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2pos_Cross3$Prop <- F2pos_Cross3$n/50
F2pos_Cross3$Population <- "NaCl x EtOH"

write.table(F2pos_Cross3, file = "Stats/Pos_Transgression_Cross3Yield_Prop.txt", sep = "\t", row.names = F)

F2neg <- subset(F2, Transgression == "-1")
F2neg$Replicate <- as.factor(F2neg$Replicate)
F2neg$Population <- as.factor(F2neg$Population)
F2neg_Cross3 <- F2neg %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2neg_Cross3$Prop <- F2neg_Cross3$n/50
F2neg_Cross3$Population <- "NaCl x EtOH"

write.table(F2neg_Cross3, file = "Stats/Neg_Transgression_Cross3Yield_Prop.txt", sep = "\t", row.names = F)

F2int <- subset(F2, Transgression == "0")
F2int$Replicate <- as.factor(F2int$Replicate)
F2int$Population <- as.factor(F2int$Population)
F2int_Cross3 <- F2int %>% group_by(Population, Generation, Replicate, .drop = FALSE) %>% summarise(n = n())
F2int_Cross3$Prop <- F2int_Cross3$n/50
F2int_Cross3$Population <- "NaCl x EtOH"

write.table(F2int_Cross3, file = "Stats/Int_Transgression_Cross3Yield_Prop.txt", sep = "\t", row.names = F)

Pos_Trans_All <- rbind(F2pos_Cross1, F2pos_Cross2, F2pos_Cross3)
write.table(Pos_Trans_All, file = "Stats/Transgressive_Yield_Relative.txt", sep = "\t", row.names = F)
mean(Pos_Trans_All$Prop)
#Gives mean transgression across all F2 populations tested
#6.5%

Neg_Trans_All <- rbind(F2neg_Cross1, F2neg_Cross2, F2neg_Cross3)
write.table(Neg_Trans_All, file = "Stats/Transgressive_Neg_Yield_Relative.txt", sep = "\t", row.names = F)

Int_Trans_All <- rbind(F2int_Cross1, F2int_Cross2, F2int_Cross3)
write.table(Int_Trans_All, file = "Stats/Transgressive_Int_Yield_Relative.txt", sep = "\t", row.names = F)

F2_C1 <- read.delim("Stats/Transgressive_F2s_Cross1Yield.txt")
F2_C1$Transgression_Status <- ifelse(F2_C1$Transgression == "1", "Positive", ifelse(F2_C1$Transgression == "0", "Intermediate", "Negative"))
F2_C1$Population <- "NaCl x LiAc0.01"

F2_C2 <- read.delim("Stats/Transgressive_F2s_Cross2Yield.txt")
F2_C2$Transgression_Status <- ifelse(F2_C2$Transgression == "1", "Positive", ifelse(F2_C2$Transgression == "0", "Intermediate", "Negative"))
F2_C2$Population <- "NaCl x LiAc0.02"

F2_C3 <- read.delim("Stats/Transgressive_F2s_Cross3Yield.txt")
F2_C3$Transgression_Status <- ifelse(F2_C3$Transgression == "1", "Positive", ifelse(F2_C3$Transgression == "0", "Intermediate", "Negative"))
F2_C3$Population <- "NaCl x EtOH"

F2_Trans <- rbind(F2_C1, F2_C2, F2_C3)
write.table(F2_Trans, file = "Stats/Transgressive_All_Yield_Relative.txt", sep = "\t", row.names = F)

#####Compare Transgressive Environments Found#####
library(dplyr)

Trans1 <- read.delim("Stats/Transgressive_F2s_Cross1Yield.txt")
Trans1 <- subset(Trans1, Transgression == "1")
Trans1 <- Trans1 %>% group_by(Population, Generation, Environment) %>% summarise(n = n())
Trans1unique <- subset(Trans1, n == "1")
Trans1unique <- Trans1unique %>% group_by(Population, Generation) %>% summarise(n = n())
Trans1common <- subset(Trans1, n != "1")
Trans1common <- Trans1common %>% group_by(Population, Generation) %>% summarise(n = n())
Trans1unique$Proportion <- Trans1unique$n/Trans1common$n
Trans1unique$Total <- Trans1unique$n+Trans1common$n
Trans1unique$Proportion <- Trans1unique$n/Trans1unique$Total

Trans2 <- read.delim("Stats/Transgressive_F2s_Cross2Yield.txt")
Trans2 <- subset(Trans2, Transgression == "1")
Trans2 <- Trans2 %>% group_by(Population, Generation, Environment) %>% summarise(n = n())
Trans2unique <- subset(Trans2, n == "1")
Trans2unique <- Trans2unique %>% group_by(Population, Generation) %>% summarise(n = n())
Trans2common <- subset(Trans2, n != "1")
Trans2common <- Trans2common %>% group_by(Population, Generation) %>% summarise(n = n())
Trans2unique$Total <- Trans2unique$n+Trans2common$n
Trans2unique$Proportion <- Trans2unique$n/Trans2unique$Total

Trans3 <- read.delim("Stats/Transgressive_F2s_Cross3Yield.txt")
Trans3 <- subset(Trans3, Transgression == "1")
Trans3 <- Trans3 %>% group_by(Population, Generation, Environment) %>% summarise(n = n())
Trans3unique <- subset(Trans3, n == "1")
Trans3unique <- Trans3unique %>% group_by(Population, Generation) %>% summarise(n = n())
Trans3common <- subset(Trans3, n != "1")
Trans3common <- Trans3common %>% group_by(Population, Generation) %>% summarise(n = n())
Trans3unique$Proportion <- Trans3unique$n/Trans3common$n
Trans3unique$Total <- Trans3unique$n+Trans3common$n
Trans3unique$Proportion <- Trans3unique$n/Trans3unique$Total

#####Get percentages for each generation#####

Transgression_all <- read.delim("Stats/Transgressive_Yield_Relative.txt")
Transgression_gen <- Transgression_all %>% group_by(Generation) %>% summarise(mean = mean(Prop))

Transgression_all <- read.delim("Stats/Transgressive_Neg_Yield_Relative.txt")
Transgression_gen <- Transgression_all %>% group_by(Generation) %>% summarise(mean = mean(Prop))

rm(list = ls())

####Heterosis and Transgression Plots####
#####Lineplots for Heterosis and Transgression#####

Yield <- read.delim("Stats/Heterosis_Yield_relative.txt")
str(Yield)

Hybrid_Trans_Yield <- read.delim("Stats/Transgressive_Yield_Relative.txt")
str(Hybrid_Trans_Yield)


library(ggplot2)
library(gridExtra)

p1 <- ggplot(data = Yield, aes(x = Generation, y = Prop, colour = Population, group = Replicate))  + 
  geom_line(size=0.75) + labs(x = "Parental divergence (n generations)", y = "Prop. of environments where \n hybrid fitness > parent fitness", title = "A) F1 hybrid fitness across novel environments") + 
  geom_point(data = Yield, aes(x = Generation, y = Prop, fill = Population), shape = 21, colour = "black", size = 3) +
  #geom_linerange(aes(ymin = mean_relative_yield-ci, ymax = mean_relative_yield+ci, colour = Replicate), size = 0.75) +
  #geom_ribbon(alpha=0.1, colour = NA, aes(ymin = mean_relative_yield-ci, ymax = mean_relative_yield+ci, fill = Replicate)) +
  #geom_hline(yintercept=1, linetype="dashed", size = 0.5) +
  scale_x_continuous(breaks=c(100, 300, 500, 700, 1000)) +
  scale_y_continuous(breaks=seq(0.1, 0.8, 0.1), limit = c(0.1, 0.85)) +
  facet_wrap(.~Population, scales = "free", ncol = 3) +
  scale_colour_manual(values=c("#AC1016", "#800080", "#228B22")) + 
  scale_fill_manual(values=c("#AC1016", "#800080", "#228B22")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=24,face="bold"),  
                     axis.text=element_text(size=20), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=20,face="bold"), 
                     strip.text.x = element_text(size = 22, face ="bold"), strip.background = element_rect(colour="black", fill="white", size=1, linetype="blank"),
                     legend.position = "blank")
                     #legend.position = c(1, 1), legend.justification = c(1, 1),legend.title=element_text(size=16), legend.text=element_text(size=16), legend.background = element_rect(fill = NA))

p2 <- ggplot(data = Hybrid_Trans_Yield, aes(x = Generation, y = Prop, colour = Population, group = Replicate))  + 
  geom_line(size=0.75) + labs(x = "Parental divergence (n generations)", y = "Prop. of environments where \n hybrid fitness > parent fitness", title = "B) F2 hybrid fitness across novel environments") + 
  geom_point(data = Hybrid_Trans_Yield, aes(x = Generation, y = Prop, fill = Population), shape = 21, colour = "black", size = 3) +
  #geom_linerange(aes(ymin = mean_relative_yield-ci, ymax = mean_relative_yield+ci, colour = Replicate), size = 0.75) +
  #geom_ribbon(alpha=0.1, colour = NA, aes(ymin = mean_relative_yield-ci, ymax = mean_relative_yield+ci, fill = Replicate)) +
  #geom_hline(yintercept=1, linetype="dashed", size = 0.5) +
  scale_x_continuous(breaks=c(100, 300, 500, 700, 1000)) +
  scale_y_continuous(breaks=seq(0, 0.3, 0.05), limit = c(0, 0.3)) +
  facet_wrap(.~Population, scales = "free", ncol = 3) +
  scale_colour_manual(values=c("#AC1016", "#800080", "#228B22")) + 
  scale_fill_manual(values=c("#AC1016", "#800080", "#228B22")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=24,face="bold"),  
                     axis.text=element_text(size=20), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=20,face="bold"), 
                     strip.text.x = element_text(size = 22, face ="bold"), strip.background = element_rect(colour="black", fill="white", size=1, linetype="blank"),
                     legend.position = "blank")

png("Plots/Figure 2.png", res = 1000, units = "in", height = 10, width = 15)
grid.arrange(p1, p2, nrow = 2)
dev.off()

#####Barplots of transgression#####
library(dplyr)
library(ggplot2)
Trans <- read.delim("Stats/Transgressive_All_Yield_Relative.txt")

Transgression <- Trans %>% group_by(Population, Generation, Replicate, Transgression_Status, .drop = FALSE) %>% summarise(n = n())
Transgression$Proportion <- Transgression$n/50
Transgression$Percentage <- Transgression$Proportion*100
write.table(Transgression, file = "Stats/TransgressionCategories_F2s_Yield.txt", sep = "\t", row.names = F)

Transgression <- read.delim("Stats/TransgressionCategories_F2s_Yield.txt")
Transgression$Generation <- as.factor(Transgression$Generation)
Transgression$Transgression_Status <- ordered(Transgression$Transgression_Status, 
                                        levels = c("Positive", "Intermediate",
                                                   "Negative"))

Transgression_Yield <- Transgression %>% group_by(Population, Generation, Transgression_Status) %>% summarise(mean_Prop = mean(Proportion, na.rm = TRUE), 
                                                                                                    mean_Per = mean(Percentage, na.rm = TRUE), 
                                                                                                    n = n_distinct(Percentage, na.rm = TRUE))

write.table(Transgression_Yield, file = "Stats/TransgressionCategories_Gens_F2s_Yield_Mean.txt", sep = "\t", row.names = F)

Transgression_Yield <- read.delim("Stats/TransgressionCategories_Gens_F2s_Yield_Mean.txt")
Transgression_Yield <- subset(Transgression_Yield, Transgression_Status != "NA")
Transgression_Yield$Generation <- as.factor(Transgression_Yield$Generation)
Transgression_Yield$Transgression_Status <- ordered(Transgression_Yield$Transgression_Status, 
                                              levels = c("Positive", "Intermediate",
                                                         "Negative"))


p4 <- ggplot(data = Transgression_Yield, aes(x = Generation, y = mean_Per))+ 
  geom_bar(stat = "identity", aes(fill=Transgression_Status), width = 0.5) + 
  facet_wrap(.~Population, scales = "free", nrow = 3) +
  labs(x = "Parental Divergence (n generations)", y= "Percentage across replicates", title = "Transgression and parental divergence") +
  guides(fill = guide_legend("Transgression category")) +
  scale_fill_manual(values=c("#00FF00", "#FFD700", "#FF0000")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), title = element_text(size=16, face="bold"),axis.text=element_text(size=14), axis.ticks.length = unit(0.5, "cm"), 
                     strip.text.x = element_text(size = 16, face ="bold"), strip.background = element_rect(colour="black", fill="white", size=1, linetype="blank"),
                     axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=16), legend.text=element_text(size=16))

png("Plots/Figure S1.png", res = 1000, units = "in", height = 15, width = 10)
p4
dev.off()

#####Dominance Plots#####
library(dplyr)
Heterosis1 <- read.delim("Stats/Heterosis_F1s_Cross1Yield.txt")
Heterosis1$Population <- "NaCl x LiAc0.01"
Heterosis2 <- read.delim("Stats/Heterosis_F1s_Cross2Yield.txt")
Heterosis2$Population <- "NaCl x LiAc0.02"
Heterosis3 <- read.delim("Stats/Heterosis_F1s_Cross3Yield.txt")
Heterosis3$Population <- "NaCl x EtOH"

Heterosis <- rbind(Heterosis1, Heterosis2, Heterosis3)
Dominance <- Heterosis %>% group_by(Population, Generation, Replicate, dominance_category, .drop = FALSE) %>% summarise(n = n())
Dominance$Proportion <- Dominance$n/50
Dominance$Percentage <- Dominance$Proportion*100
write.table(Dominance, file = "Stats/DominanceCategories_F1s_Yield.txt", sep = "\t", row.names = F)


Dominance <- read.delim("Stats/DominanceCategories_F1s_Yield.txt")
Dominance$Generation <- as.factor(Dominance$Generation)
Dominance$dominance_category[Dominance$dominance_category=="complete dominance of more fit parent"]<- "complete dominance of fitter parent"
Dominance$dominance_category[Dominance$dominance_category=="partial dominance of more fit parent"]<- "partial dominance of fitter parent"
Dominance$dominance_category <- ordered(Dominance$dominance_category, 
                                        levels = c("overdominance", "complete dominance of fitter parent",
                                                   "partial dominance of fitter parent", "co-dominance",
                                                   "partial dominance of less fit parent", 
                                                   "complete dominance of less fit parent", "underdominance"))


library(dplyr)
library(ggplot2)
library(gridExtra)

Dominance_Yield <- Dominance %>% group_by(Population, Generation, dominance_category) %>% summarise(mean_Prop = mean(Proportion, na.rm = TRUE), 
                                                                                                    mean_Per = mean(Percentage, na.rm = TRUE), 
                                                                                                    n = n_distinct(Percentage, na.rm = TRUE))

write.table(Dominance_Yield, file = "Stats/DominanceCategories_Gens_F1s_Yield_Mean.txt", sep = "\t", row.names = F)
Dominance_Yield <- subset(Dominance_Yield, dominance_category != "NA")

p1 <- ggplot(data = Dominance_Yield, aes(x = Generation, y = mean_Per))+ 
  geom_bar(stat = "identity", aes(fill=dominance_category), width = 0.5) + 
  facet_wrap(.~Population, scales = "free", nrow = 3) +
  labs(x = "Parental divergence (n generations)", y= "Percentage across replicates", title = "Dominance categories and parental divergence") +
  guides(fill = guide_legend("Dominance category")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), title = element_text(size=20, face="bold"),axis.text=element_text(size=18), axis.ticks.length = unit(0.5, "cm"), 
                     strip.text.x = element_text(size = 20, face ="bold"), strip.background = element_rect(colour="black", fill="white", size=1, linetype="blank"),
                     axis.title=element_text(size=18,face="bold"), legend.title=element_text(size=20), legend.text=element_text(size=20))

png("Plots/Figure 3.png", res = 1000, units = "in", height = 15, width = 12)
p1
dev.off()