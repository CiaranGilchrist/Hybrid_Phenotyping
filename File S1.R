####Parental population data extraction and plotting####
#Updated 2022-09-05
#Plots by ggplot2 with gridExtra
#R version 4.1.0 (2021-05-18)
#Packages used - dplyr, ggplot2 and gridExtra


install.packages(c("dplyr", "ggplot2", "gridExtra"))
####Get data####
setwd("C:/Target/Directory")
#Set as directory containing Table S1 or S2
phenotype <- read.delim("Table S1.txt")
#absolute also available in Table S2.txt
#phenotype <- read.delim("Table S2.txt")
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

#Parental Population Environment Lists
Env_home_LiAc <- c("LiAc 0.02M")

Env_Novel_LiAc <- c("1-Butanol 1.5%", "1-Butanol 3%", "AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", 
                    "As(V) 0.1 mM", "BaCl 0.3M", "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", 
                    "DMSO 5%", "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", 
                    "H2O2 3mM", "HU 10mg/mL", "Lactose 2%", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%", "MnCl2 1mM", 
                    "NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", "Rapamycin 0.8ug/mL",
                    "RbCl 400 mM", "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", "Xylose 8%", "ZnCl2 7mM")

#H3 Environment Lists
Env_home_EtOH <- c("1-Butanol 1.5%", "1-Butanol 3%")

Env_Novel_EtOH <- c("AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", "As(V) 0.1 mM", "BaCl 0.3M", 
                    "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", "DMSO 5%",
                    "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", "H2O2 3mM",
                    "HU 10mg/mL", "Lactose 2%", "LiAc 0.02M", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%",
                    "MnCl2 1mM", "NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", 
                    "Rapamycin 0.8ug/mL", "RbCl 400 mM", "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", 
                    "Xylose 8%", "ZnCl2 7mM")

Env_home_NaCl <- c("NaCl 0.01M", "NaCl 0.325M", "NaCl 0.75 M")

Env_Novel_NaCl <- c("1-Butanol 1.5%", "1-Butanol 3%", "AlCl3", "Arabinose 2%", "As(III) 0.1 mM", "As(III) 0.5 mM", "As(III) 1 mM", "As(V) 0.1 mM", "BaCl 0.3M", 
                    "BaCl2 3mM", "Caffeine 1.5mg/mL", "CHX 0.1 ug/mL", "clotrimazole 50 uM", "CsCl2", "CuSO4 1mM", "DMSO 5%",
                    "fenpropimorph 50ug/mL", "Fructose 2%", "Galactose 2%", "Glucose 2%", "Glucose 8%", "Glycerol 2%", "H2O2 2mM", "H2O2 3mM",
                    "HU 10mg/mL", "Lactose 2%", "LiAc 0.02M", "LiCl 0.02M", "LiCl 0.05M", "Maltose 2%", "Mannitol 2%", "Melibiose 2%",
                    "MnCl2 1mM", "NiSO4 3mM", "Paraquat 400 ug/mL", "Raffinose 2%", "Raffinose 8%", 
                    "Rapamycin 0.8ug/mL", "RbCl 400 mM", "Sb(III) 0.1 mM", "Sucrose 2%", "Trehalose 2%", "Triton X-100 1%", "Xylitol 2%", "Xylose 2%", 
                    "Xylose 8%", "ZnCl2 7mM")

####Home environment data####
#Subset by population history

#####NaCl all populations#####
#Includes all populations in NaCl
NaCl <- subset(phenotype, Environment == "NaCl 0.325M")
NaCl$Population <- as.character(NaCl$Population)
NaCl$Population[NaCl$Population=="EvoN"]<-"NaCl"
NaCl$Population[NaCl$Population=="EvoL1"]<-"LiAc0.01"
NaCl$Population[NaCl$Population=="EvoL2"]<-"LiAc0.02"
NaCl$Population[NaCl$Population=="EvoE"]<-"EtOH"
NaCl$Population[NaCl$Population=="H1F1"]<-"NxL1_F1"
NaCl$Population[NaCl$Population=="H1F2"]<-"NxL1_F2"
NaCl$Population[NaCl$Population=="H2F1"]<-"NxL2_F1"
NaCl$Population[NaCl$Population=="H2F2"]<-"NxL2_F2"
NaCl$Population[NaCl$Population=="H3F1"]<-"NxE_F1"
NaCl$Population[NaCl$Population=="H3F2"]<-"NxE_F2"



NaCl_f <- subset(NaCl, Generation == "0" & Population == "NaCl")
LE_f <- subset(NaCl, Population == "EvoLE")
NaCl <- subset(NaCl, Generation != "0")

NaCl_f$Group <- as.character(NaCl_f$Group)
NaCl_f <- droplevels(NaCl_f)
str(NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_"]<- "EvoN_R1"
NaCl <- rbind(NaCl, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R1"]<- "EvoN_R2"
NaCl <- rbind(NaCl, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R2"]<- "EvoN_R3"
NaCl <- rbind(NaCl, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R3"]<- "EvoN_R4"
NaCl <- rbind(NaCl, NaCl_f)

LE_f$Population <- "LiAc0.01"
LE_f$Group[LE_f$Group=="EvoLE_"]<- "EvoL1_R1"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoL1_R1"]<- "EvoL1_R2"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoL1_R2"]<- "EvoL1_R4"
NaCl <- rbind(NaCl, LE_f)

LE_f$Population <- "LiAc0.02"
LE_f$Group[LE_f$Group=="EvoL1_R4"]<- "EvoL2_R1"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoL2_R1"]<- "EvoL2_R2"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoL2_R2"]<- "EvoL2_R3"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoL2_R3"]<- "EvoL2_R4"
NaCl <- rbind(NaCl, LE_f)

LE_f$Population <- "EtOH"
LE_f$Group[LE_f$Group=="EvoL2_R4"]<- "EvoE_R1"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoE_R1"]<- "EvoE_R2"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoE_R2"]<- "EvoE_R3"
NaCl <- rbind(NaCl, LE_f)
LE_f$Group[LE_f$Group=="EvoE_R3"]<- "EvoE_R4"
NaCl <- rbind(NaCl, LE_f)

NaCl <- droplevels(NaCl)

NaCl$Population <- as.factor(NaCl$Population)
NaCl$Group <- as.factor(NaCl$Group)
NaCl$Population <- ordered(NaCl$Population, levels = c("NaCl", "LiAc0.01", "LiAc0.02", "EtOH", "NxL1_F1", "NxL1_F2", "NxL2_F1", "NxL2_F2", "NxE_F1", "NxE_F2"))

str(NaCl)
levels(NaCl$Group)
levels(NaCl$Population)
write.table(NaCl, file = "Stats/NaCl_allpops_Fitness.txt", sep = "\t", row.names = F)


#####NaCl Home Populations#####
#Includes NaCl parents and hybrids generated from these populations
NaCl <- subset(phenotype, Population != "EvoE"& Population != "EvoL1" & Population != "EvoL2" & Population != "EvoLE")
NaCl$Population <- as.character(NaCl$Population)
NaCl$Population[NaCl$Population=="EvoN"]<-"NaCl"
NaCl$Population[NaCl$Population=="H1F1"]<-"NxL1_F1"
NaCl$Population[NaCl$Population=="H1F2"]<-"NxL1_F2"
NaCl$Population[NaCl$Population=="H2F1"]<-"NxL2_F1"
NaCl$Population[NaCl$Population=="H2F2"]<-"NxL2_F2"
NaCl$Population[NaCl$Population=="H3F1"]<-"NxE_F1"
NaCl$Population[NaCl$Population=="H3F2"]<-"NxE_F2"

NaCl_f <- subset(NaCl, Generation == "0")
NaCl <- subset(NaCl, Generation != "0")

NaCl_f$Group <- as.character(NaCl_f$Group)
NaCl_f <- droplevels(NaCl_f)
str(NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_"]<- "EvoN_R1"
NaCl <- rbind(NaCl, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R1"]<- "EvoN_R2"
NaCl <- rbind(NaCl, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R2"]<- "EvoN_R3"
NaCl <- rbind(NaCl, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R3"]<- "EvoN_R4"
NaCl <- rbind(NaCl, NaCl_f)

NaCl <- droplevels(NaCl)

NaCl$Population <- as.factor(NaCl$Population)
NaCl$Group <- as.factor(NaCl$Group)
NaCl$Population <- ordered(NaCl$Population, levels = c("NaCl", "NxL1_F1", "NxL1_F2", "NxL2_F1", "NxL2_F2", "NxE_F1", "NxE_F2"))

NaCl <- subset(NaCl, Environment == "NaCl 0.325M")
NaCl <- droplevels(NaCl)
str(NaCl)
levels(NaCl$Group)
levels(NaCl$Population)
write.table(NaCl, file = "Stats/NaCl_Fitness.txt", sep = "\t", row.names = F)


#####LiAc0.01 home populations#####
#Includes LiAc0.01 adapted populations and hybrids generated from these
LiAc0.01 <- subset(phenotype, Population == "EvoN"| Population == "EvoL1"| Population == "EvoLE" | Population == "H1F1"| Population == "H1F2")
LiAc0.01$Population <- as.character(LiAc0.01$Population)
LiAc0.01$Population[LiAc0.01$Population=="EvoN"]<-"NaCl"
LiAc0.01$Population[LiAc0.01$Population=="EvoL1"]<-"LiAc0.01"
LiAc0.01$Population[LiAc0.01$Population=="EvoLE"]<-"LiAc0.01"
LiAc0.01$Population[LiAc0.01$Population=="H1F1"]<-"NxL1_F1"
LiAc0.01$Population[LiAc0.01$Population=="H1F2"]<-"NxL1_F2"


LiAc0.01_f <- subset(LiAc0.01, Generation == "0")
LiAc0.01 <- subset(LiAc0.01, Generation != "0")
LiAc0.01 <- subset(LiAc0.01, Generation != "1000")

LiAc0.01_f$Group <- as.character(LiAc0.01_f$Group)
LiAc0.01_f <- droplevels(LiAc0.01_f)
str(LiAc0.01_f)
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoLE_"]<- "EvoL1_R1"
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoN_"]<- "EvoN_R1"
LiAc0.01 <- rbind(LiAc0.01, LiAc0.01_f)
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoL1_R1"]<- "EvoL1_R2"
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoN_R1"]<- "EvoN_R2"
LiAc0.01 <- rbind(LiAc0.01, LiAc0.01_f)
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoL1_R2"]<- "EvoL1_R4"
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoN_R2"]<- "EvoN_R3"
LiAc0.01 <- rbind(LiAc0.01, LiAc0.01_f)
LiAc0.01_f <- subset(LiAc0.01_f, Group == "EvoN_R3")
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoN_R3"]<- "EvoN_R4"
LiAc0.01 <- rbind(LiAc0.01, LiAc0.01_f)

LiAc0.01 <- droplevels(LiAc0.01)

LiAc0.01$Population <- as.factor(LiAc0.01$Population)
LiAc0.01$Group <- as.factor(LiAc0.01$Group)
LiAc0.01$Population <- ordered(LiAc0.01$Population, levels = c("NaCl", "LiAc0.01", "NxL1_F1", "NxL1_F2"))

LiAc0.01 <- subset(LiAc0.01, Environment == "LiAc 0.02M")
LiAc0.01 <- droplevels(LiAc0.01)
str(LiAc0.01)
levels(LiAc0.01$Group)
levels(LiAc0.01$Population)
write.table(LiAc0.01, file = "Stats/LiAc0.01_Fitness.txt", sep = "\t", row.names = F)

#####LiAc0.02 home populations#####
#Includes LiAc0.02M adapted populations and hybrids generated from these
LiAc0.02 <- subset(phenotype, Population == "EvoN"| Population == "EvoL2"| Population == "EvoLE" | Population == "H2F1"| Population == "H2F2")
LiAc0.02$Population <- as.character(LiAc0.02$Population)
LiAc0.02$Population[LiAc0.02$Population=="EvoN"]<-"NaCl"
LiAc0.02$Population[LiAc0.02$Population=="EvoL2"]<-"LiAc0.02"
LiAc0.02$Population[LiAc0.02$Population=="EvoLE"]<-"LiAc0.02"
LiAc0.02$Population[LiAc0.02$Population=="H2F1"]<-"NxL2_F1"
LiAc0.02$Population[LiAc0.02$Population=="H2F2"]<-"NxL2_F2"


LiAc0.02_f <- subset(LiAc0.02, Generation == "0")
LiAc0.02 <- subset(LiAc0.02, Generation != "0")
LiAc0.02 <- subset(LiAc0.02, Generation != "1000")

LiAc0.02_f$Group <- as.character(LiAc0.02_f$Group)
LiAc0.02_f <- droplevels(LiAc0.02_f)
str(LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoLE_"]<- "EvoL2_R1"
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoN_"]<- "EvoN_R1"
LiAc0.02 <- rbind(LiAc0.02, LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R1"]<- "EvoL2_R2"
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoN_R1"]<- "EvoN_R2"
LiAc0.02 <- rbind(LiAc0.02, LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R2"]<- "EvoL2_R3"
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoN_R2"]<- "EvoN_R3"
LiAc0.02 <- rbind(LiAc0.02, LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R3"]<- "EvoL2_R4"
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoN_R3"]<- "EvoN_R4"
LiAc0.02 <- rbind(LiAc0.02, LiAc0.02_f)
LiAc0.02_f <- subset(LiAc0.02_f, Group == "EvoL2_R4")
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R4"]<- "EvoL2_R5"
LiAc0.02 <- rbind(LiAc0.02, LiAc0.02_f)

LiAc0.02 <- droplevels(LiAc0.02)

LiAc0.02$Population <- as.factor(LiAc0.02$Population)
LiAc0.02$Group <- as.factor(LiAc0.02$Group)
LiAc0.02$Population <- ordered(LiAc0.02$Population, levels = c("NaCl", "LiAc0.02", "NxL2_F1", "NxL2_F2"))

LiAc0.02 <- subset(LiAc0.02, Environment == "LiAc 0.02M")
LiAc0.02 <- droplevels(LiAc0.02)
str(LiAc0.02)
levels(LiAc0.02$Group)
levels(LiAc0.02$Population)
write.table(LiAc0.02, file = "Stats/LiAc0.02_Fitness.txt", sep = "\t", row.names = F)

#####EtOH home populations#####
#Includes EtOH adapted populations and hybrids generated from these
EtOH <- subset(phenotype, Population == "EvoN" | Population == "EvoE"| Population == "EvoLE" | Population == "H3F1"| Population == "H3F2")
EtOH$Population <- as.character(EtOH$Population)
EtOH$Population[EtOH$Population=="EvoN"]<-"NaCl"
EtOH$Population[EtOH$Population=="EvoE"]<-"EtOH"
EtOH$Population[EtOH$Population=="EvoLE"]<-"EtOH"
EtOH$Population[EtOH$Population=="H3F1"]<-"NxE_F1"
EtOH$Population[EtOH$Population=="H3F2"]<-"NxE_F2"


EtOH_f <- subset(EtOH, Generation == "0")
EtOH <- subset(EtOH, Generation != "0")

EtOH_f$Group <- as.character(EtOH_f$Group)
EtOH_f <- droplevels(EtOH_f)
str(EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoLE_"]<- "EvoE_R1"
EtOH_f$Group[EtOH_f$Group=="EvoN_"]<- "EvoN_R1"
EtOH <- rbind(EtOH, EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoE_R1"]<- "EvoE_R2"
EtOH_f$Group[EtOH_f$Group=="EvoN_R1"]<- "EvoN_R2"
EtOH <- rbind(EtOH, EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoE_R2"]<- "EvoE_R3"
EtOH_f$Group[EtOH_f$Group=="EvoN_R2"]<- "EvoN_R3"
EtOH <- rbind(EtOH, EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoE_R3"]<- "EvoE_R4"
EtOH_f$Group[EtOH_f$Group=="EvoN_R3"]<- "EvoN_R4"
EtOH <- rbind(EtOH, EtOH_f)

EtOH <- droplevels(EtOH)

EtOH$Population <- as.factor(EtOH$Population)
EtOH$Group <- as.factor(EtOH$Group)
EtOH$Population <- ordered(EtOH$Population, levels = c("NaCl", "EtOH", "NxE_F1", "NxE_F2"))

EtOH <- subset(EtOH, Environment == "1-Butanol 1.5%")
EtOH <- droplevels(EtOH)
str(EtOH)
levels(EtOH$Group)
levels(EtOH$Population)
write.table(EtOH, file = "Stats/EtOH_Fitness.txt", sep = "\t", row.names = F)

####Novel Environment Data####
#####NaCl-adapted novel environments#####
NaCl <- subset(phenotype, Population == "EvoN")
NaCl$Population <- as.character(NaCl$Population)
NaCl$Population[NaCl$Population=="EvoN"]<-"NaCl"
NaCl <- droplevels(NaCl)
str(NaCl)
levels(NaCl$Group)

NaCl_adap <- subset(NaCl, Generation != "0")
NaCl_f <- subset(NaCl, Generation == "0")

NaCl_f$Group <- as.character(NaCl_f$Group)
NaCl_f <- droplevels(NaCl_f)
str(NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_"]<- "EvoN_R1"
NaCl_adap <- rbind(NaCl_adap, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R1"]<- "EvoN_R2"
NaCl_adap <- rbind(NaCl_adap, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R2"]<- "EvoN_R3"
NaCl_adap <- rbind(NaCl_adap, NaCl_f)
NaCl_f$Group[NaCl_f$Group=="EvoN_R3"]<- "EvoN_R4"
NaCl_adap <- rbind(NaCl_adap, NaCl_f)

NaCl <- NaCl_adap
NaCl <- droplevels(NaCl)
NaCl$Population <- as.factor(NaCl$Population)
NaCl$Group <- as.factor(NaCl$Group)
str(NaCl)

NaCl_novel <- subset(NaCl, Environment != Env_home_NaCl)
NaCl_novel$Environment <- ordered(NaCl_novel$Environment, levels = c(Env_Novel_NaCl))
NaCl_novel <- droplevels(NaCl_novel)
str(NaCl_novel)
levels(NaCl_novel$Group)
levels(NaCl_novel$Environment)
levels(NaCl_novel$Population)
write.table(NaCl_novel, file = "Stats/Parents_NaCl_Novel.txt", sep = "\t", row.names = F)

#####EtOH-adapted novel environments#####
EtOH <- subset(phenotype, Population == "EvoE"| Population == "EvoLE")
EtOH$Population <- as.character(EtOH$Population)
EtOH$Population[EtOH$Population=="EvoE"]<-"EtOH"
EtOH$Population[EtOH$Population=="EvoLE"]<-"EtOH"
EtOH <- droplevels(EtOH)
str(EtOH)
levels(EtOH$Group)

EtOH_adap <- subset(EtOH, Generation != "0")
EtOH_f <- subset(EtOH, Generation == "0")

EtOH_f$Group <- as.character(EtOH_f$Group)
EtOH_f <- droplevels(EtOH_f)
str(EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoLE_"]<- "EvoE_R1"
EtOH_adap <- rbind(EtOH_adap, EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoE_R1"]<- "EvoE_R2"
EtOH_adap <- rbind(EtOH_adap, EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoE_R2"]<- "EvoE_R3"
EtOH_adap <- rbind(EtOH_adap, EtOH_f)
EtOH_f$Group[EtOH_f$Group=="EvoE_R3"]<- "EvoE_R4"
EtOH_adap <- rbind(EtOH_adap, EtOH_f)

EtOH <- EtOH_adap
EtOH <- droplevels(EtOH)
EtOH$Population <- as.factor(EtOH$Population)
EtOH$Group <- as.factor(EtOH$Group)
str(EtOH)

EtOH_novel <- subset(EtOH, Environment != Env_home_EtOH)
EtOH_novel$Environment <- ordered(EtOH_novel$Environment, levels = c(Env_Novel_EtOH))
EtOH_novel <- droplevels(EtOH_novel)
str(EtOH_novel)
levels(EtOH_novel$Group)
levels(EtOH_novel$Environment)
levels(EtOH_novel$Population)
write.table(EtOH_novel, file = "Stats/Parents_EtOH_Novel.txt", sep = "\t", row.names = F)

#####LiAc0.01-adapted populations novel environments#####
LiAc0.01 <- subset(phenotype, Population == "EvoL1"| Population == "EvoLE")
LiAc0.01$Population <- as.character(LiAc0.01$Population)
LiAc0.01$Population[LiAc0.01$Population=="EvoL1"]<-"LiAc0.01"
LiAc0.01$Population[LiAc0.01$Population=="EvoLE"]<-"LiAc0.01"
LiAc0.01 <- droplevels(LiAc0.01)
str(LiAc0.01)
levels(LiAc0.01$Group)

LiAc0.01_adap <- subset(LiAc0.01, Generation != "0")
LiAc0.01_f <- subset(LiAc0.01, Generation == "0")

LiAc0.01_f$Group <- as.character(LiAc0.01_f$Group)
LiAc0.01_f <- droplevels(LiAc0.01_f)
str(LiAc0.01_f)
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoLE_"]<- "EvoL1_R1"
LiAc0.01_adap <- rbind(LiAc0.01_adap, LiAc0.01_f)
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoL1_R1"]<- "EvoL1_R2"
LiAc0.01_adap <- rbind(LiAc0.01_adap, LiAc0.01_f)
LiAc0.01_f$Group[LiAc0.01_f$Group=="EvoL1_R2"]<- "EvoL1_R4"
LiAc0.01_adap <- rbind(LiAc0.01_adap, LiAc0.01_f)

LiAc0.01 <- LiAc0.01_adap
LiAc0.01 <- droplevels(LiAc0.01)
LiAc0.01$Population <- as.factor(LiAc0.01$Population)
LiAc0.01$Group <- as.factor(LiAc0.01$Group)
str(LiAc0.01)

LiAc0.01_novel <- subset(LiAc0.01, Environment != Env_home_LiAc)
LiAc0.01_novel$Environment <- ordered(LiAc0.01_novel$Environment, levels = c(Env_Novel_LiAc))
LiAc0.01_novel <- droplevels(LiAc0.01_novel)
str(LiAc0.01_novel)
levels(LiAc0.01_novel$Group)
levels(LiAc0.01_novel$Environment)
levels(LiAc0.01_novel$Population)
write.table(LiAc0.01_novel, file = "Stats/Parents_LiAc0.01_Novel.txt", sep = "\t", row.names = F)

#####LiAc0.02 adapted populations novel environments#####
LiAc0.02 <- subset(phenotype, Population == "EvoL2"| Population == "EvoLE")
LiAc0.02$Population <- as.character(LiAc0.02$Population)
LiAc0.02$Population[LiAc0.02$Population=="EvoL2"]<-"LiAc0.02"
LiAc0.02$Population[LiAc0.02$Population=="EvoLE"]<-"LiAc0.02"
LiAc0.02 <- droplevels(LiAc0.02)
str(LiAc0.02)
levels(LiAc0.02$Group)

LiAc0.02_adap <- subset(LiAc0.02, Generation != "0")
LiAc0.02_f <- subset(LiAc0.02, Generation == "0")

LiAc0.02_f$Group <- as.character(LiAc0.02_f$Group)
LiAc0.02_f <- droplevels(LiAc0.02_f)
str(LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoLE_"]<- "EvoL2_R1"
LiAc0.02_adap <- rbind(LiAc0.02_adap, LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R1"]<- "EvoL2_R2"
LiAc0.02_adap <- rbind(LiAc0.02_adap, LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R2"]<- "EvoL2_R3"
LiAc0.02_adap <- rbind(LiAc0.02_adap, LiAc0.02_f)
LiAc0.02_f$Group[LiAc0.02_f$Group=="EvoL2_R3"]<- "EvoL2_R4"
LiAc0.02_adap <- rbind(LiAc0.02_adap, LiAc0.02_f)

LiAc0.02 <- LiAc0.02_adap
LiAc0.02 <- droplevels(LiAc0.02)
LiAc0.02$Population <- as.factor(LiAc0.02$Population)
LiAc0.02$Group <- as.factor(LiAc0.02$Group)
str(LiAc0.02)

LiAc0.02_novel <- subset(LiAc0.02, Environment != Env_home_LiAc)
LiAc0.02_novel$Environment <- ordered(LiAc0.02_novel$Environment, levels = c(Env_Novel_LiAc))
LiAc0.02_novel <- droplevels(LiAc0.02_novel)
str(LiAc0.02_novel)
levels(LiAc0.02_novel$Group)
levels(LiAc0.02_novel$Environment)
levels(LiAc0.02_novel$Population)
write.table(LiAc0.02_novel, file = "Stats/Parents_LiAc0.02_Novel.txt", sep = "\t", row.names = F)

####Population in arsenite####

#####All populations#####
#Includes all populations in Arsenite 1mM
Arse <- subset(phenotype, Environment == "As(III) 1 mM")
Arse$Population <- as.character(Arse$Population)
Arse$Population[Arse$Population=="EvoN"]<-"NaCl"
Arse$Population[Arse$Population=="EvoL1"]<-"LiAc0.01"
Arse$Population[Arse$Population=="EvoL2"]<-"LiAc0.02"
Arse$Population[Arse$Population=="EvoE"]<-"EtOH"
Arse$Population[Arse$Population=="H1F1"]<-"NxL1_F1"
Arse$Population[Arse$Population=="H1F2"]<-"NxL1_F2"
Arse$Population[Arse$Population=="H2F1"]<-"NxL2_F1"
Arse$Population[Arse$Population=="H2F2"]<-"NxL2_F2"
Arse$Population[Arse$Population=="H3F1"]<-"NxE_F1"
Arse$Population[Arse$Population=="H3F2"]<-"NxE_F2"

Arse_f <- subset(Arse, Generation == "0" & Population == "NaCl")
LE_f <- subset(Arse, Population == "EvoLE")
Arse <- subset(Arse, Generation != "0")

Arse_f$Group <- as.character(Arse_f$Group)
Arse_f <- droplevels(Arse_f)
str(Arse_f)
Arse_f$Group[Arse_f$Group=="EvoN_"]<- "EvoN_R1"
Arse <- rbind(Arse, Arse_f)
Arse_f$Group[Arse_f$Group=="EvoN_R1"]<- "EvoN_R2"
Arse <- rbind(Arse, Arse_f)
Arse_f$Group[Arse_f$Group=="EvoN_R2"]<- "EvoN_R3"
Arse <- rbind(Arse, Arse_f)
Arse_f$Group[Arse_f$Group=="EvoN_R3"]<- "EvoN_R4"
Arse <- rbind(Arse, Arse_f)

LE_f$Population <- "LiAc0.01"
LE_f$Group[LE_f$Group=="EvoLE_"]<- "EvoL1_R1"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoL1_R1"]<- "EvoL1_R2"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoL1_R2"]<- "EvoL1_R4"
Arse <- rbind(Arse, LE_f)

LE_f$Population <- "LiAc0.02"
LE_f$Group[LE_f$Group=="EvoL1_R4"]<- "EvoL2_R1"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoL2_R1"]<- "EvoL2_R2"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoL2_R2"]<- "EvoL2_R3"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoL2_R3"]<- "EvoL2_R4"
Arse <- rbind(Arse, LE_f)

LE_f$Population <- "EtOH"
LE_f$Group[LE_f$Group=="EvoL2_R4"]<- "EvoE_R1"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoE_R1"]<- "EvoE_R2"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoE_R2"]<- "EvoE_R3"
Arse <- rbind(Arse, LE_f)
LE_f$Group[LE_f$Group=="EvoE_R3"]<- "EvoE_R4"
Arse <- rbind(Arse, LE_f)

Arse <- droplevels(Arse)

Arse$Population <- as.factor(Arse$Population)
Arse$Group <- as.factor(Arse$Group)
Arse$Population <- ordered(Arse$Population, levels = c("NaCl", "LiAc0.01", "LiAc0.02", "EtOH", "NxL1_F1", "NxL1_F2", "NxL2_F1", "NxL2_F2", "NxE_F1", "NxE_F2"))

str(Arse)
levels(Arse$Group)
levels(Arse$Population)
write.table(Arse, file = "Stats/Arse_allpops_Fitness.txt", sep = "\t", row.names = F)

####Parental Divergence####
####EtOH and NaCl divergence####
ExN <- subset(phenotype, Population == "EvoN"| Population == "EvoE" | Population == "EvoLE")
#Change to EvoL2, EvoL1 or EvoE here and below
ExN$Population <- as.character(ExN$Population)
ExN$Population[ExN$Population=="EvoN"]<-"P1"
ExN$Population[ExN$Population=="EvoLE"]<-"P2"
ExN$Population[ExN$Population=="EvoE"]<-"P2"
ExN$Population <- as.factor(ExN$Population)


#Below only needed for cross 3
ExN_gen_1000 <- subset(ExN, Generation = "1000")
str(ExN_gen_1000)
ExN_gen_1000 <- subset(ExN_gen_1000, Replicate == "R2" | Replicate == "R3")
ExN_gen_1000 <- droplevels(ExN_gen_1000)
str(ExN_gen_1000)

ExN <- subset(ExN, Generation != "1000")
#only needed for cross 3
ExN <- rbind(ExN, ExN_gen_1000)
#only needed for cross 3
ExN <- droplevels(ExN)
str(ExN)
levels(ExN$Replicate)
levels(ExN$Environment)
levels(ExN$Population)

#Calculate mean and sd
library(dplyr)
ExN_yield_by_environment <- ExN %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
ExN_yield_by_environment$ci <- 1.96*(ExN_yield_by_environment$sd/sqrt(ExN_yield_by_environment$n))
ExN_yield_by_environment$doubleSD <- 2*ExN_yield_by_environment$sd

#Calculate environments where P2 +/-2sd from P1
P1 <- subset(ExN_yield_by_environment, Population == "P1")
P1$pos <- P1$mean_yield + P1$doubleSD
P1$neg <- P1$mean_yield - P1$doubleSD
#Increased fitness if >2sd from P1
#Decreased fitness if <2sd from P2
#Intermediate fitness if between
P2 <- subset(ExN_yield_by_environment, Population == "P2")
P2$pos_P1 <- P1$pos
P2$neg_P1 <- P1$neg
P2$FitDifferences <- ifelse(P2$mean_yield > P2$pos_P1, "1", ifelse(P2$mean_yield < P2$neg_P1, "-1", "0"))
#+ve FitDifferences = 1, -ve = -1, intermediate = 0
write.table(P2, file = "Stats/ExN_Parents_phenotype.txt", sep = "\t", row.names = F)

P2 <- read.delim("Stats/ExN_Parents_phenotype.txt")
P2dif <- subset(P2, FitDifferences != "0")
P2dif <- subset(P2dif, FitDifferences != "NA")
library(dplyr)
ExNDiff <- P2dif %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
ExNDiff$Prop <- ExNDiff$n/50
ExNDiff$Population <- "EtOH"
write.table(ExNDiff, file = "Stats/ExN_Phenotypic_Distance.txt", sep = "\t", row.names = F)

cor.test(ExNDiff$Prop, ExNDiff$Generation)
#t = 2.52, df = 17, p < 0.05, R = 0.52

#####LiAc0.01 and NaCl divergence#####
ExL1 <- subset(phenotype, Population == "EvoN"| Population == "EvoL1" | Population == "EvoLE")
#Change to EvoL2, EvoL1 or EvoL1 here and below
ExL1$Population <- as.character(ExL1$Population)
ExL1$Population[ExL1$Population=="EvoN"]<-"P1"
ExL1$Population[ExL1$Population=="EvoLE"]<-"P2"
ExL1$Population[ExL1$Population=="EvoL1"]<-"P2"
ExL1$Population <- as.factor(ExL1$Population)

NaCl <- subset(ExL1, Population == "P1")
NaCl <- subset(NaCl, Generation != "1000")
NaCl <- subset(NaCl, Replicate != "R3")
LiAc0.01<- subset(ExL1, Population == "P2")

ExL1 <- rbind(NaCl, LiAc0.01)
ExL1 <- droplevels(ExL1)

ExL1$Replicate <- as.factor(ExL1$Replicate)
ExL1$Population <- as.factor(ExL1$Population)
ExL1$Environment <- as.factor(ExL1$Environment)
str(ExL1)
levels(ExL1$Replicate)
levels(ExL1$Environment)
levels(ExL1$Population)

#Calculate mean and sd
library(dplyr)
ExL1_yield_by_environment <- ExL1 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
ExL1_yield_by_environment$ci <- 1.96*(ExL1_yield_by_environment$sd/sqrt(ExL1_yield_by_environment$n))
ExL1_yield_by_environment$doubleSD <- 2*ExL1_yield_by_environment$sd

#Calculate environments where P2 +/-2sd from P1
P1 <- subset(ExL1_yield_by_environment, Population == "P1")
P1$pos <- P1$mean_yield + P1$doubleSD
P1$neg <- P1$mean_yield - P1$doubleSD
#Increased fitness if >2sd from P1
#Decreased fitness if <2sd from P2
#Intermediate fitness if between
P2 <- subset(ExL1_yield_by_environment, Population == "P2")
P2$pos_P1 <- P1$pos
P2$neg_P1 <- P1$neg
P2$FitDifferences <- ifelse(P2$mean_yield > P2$pos_P1, "1", ifelse(P2$mean_yield < P2$neg_P1, "-1", "0"))
#+ve FitDifferences = 1, -ve = -1, intermediate = 0
write.table(P2, file = "Stats/ExL1_Parents_phenotype.txt", sep = "\t", row.names = F)

P2 <- read.delim("Stats/ExL1_Parents_phenotype.txt")
P2dif <- subset(P2, FitDifferences != "0")
P2dif <- subset(P2dif, FitDifferences != "NA")
library(dplyr)
ExL1Diff <- P2dif %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
ExL1Diff$Prop <- ExL1Diff$n/50
ExL1Diff$Population <- "LiAc0.01"
write.table(ExL1Diff, file = "Stats/ExL1_Phenotypic_Distance.txt", sep = "\t", row.names = F)
cor.test(ExL1Diff$Prop, ExL1Diff$Generation)
#t =1.43, df = 11, p > 0.05, R = 0.395

#####LiAc0.02 and NaCl parental divergence#####
ExL2 <- subset(phenotype, Population == "EvoN"| Population == "EvoL2" | Population == "EvoLE")
#Change to EvoL2, EvoL2 or EvoL2 here and below
ExL2$Population <- as.character(ExL2$Population)
ExL2$Population[ExL2$Population=="EvoN"]<-"P1"
ExL2$Population[ExL2$Population=="EvoLE"]<-"P2"
ExL2$Population[ExL2$Population=="EvoL2"]<-"P2"
ExL2$Population <- as.factor(ExL2$Population)


ExL2 <- subset(ExL2, Generation != "1000")
ExL2 <- subset(ExL2, Replicate != "R5")
ExL2 <- droplevels(ExL2)
str(ExL2)
levels(ExL2$Replicate)
levels(ExL2$Environment)
levels(ExL2$Population)

#Calculate mean and sd
library(dplyr)
ExL2_yield_by_environment <- ExL2 %>% group_by(Population, Generation, Environment, Replicate) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
ExL2_yield_by_environment$ci <- 1.96*(ExL2_yield_by_environment$sd/sqrt(ExL2_yield_by_environment$n))
ExL2_yield_by_environment$doubleSD <- 2*ExL2_yield_by_environment$sd

#Calculate environments where P2 +/-2sd from P1
P1 <- subset(ExL2_yield_by_environment, Population == "P1")
P1$pos <- P1$mean_yield + P1$doubleSD
P1$neg <- P1$mean_yield - P1$doubleSD
#Increased fitness if >2sd from P1
#Decreased fitness if <2sd from P2
#Intermediate fitness if between
P2 <- subset(ExL2_yield_by_environment, Population == "P2")
P2$pos_P1 <- P1$pos
P2$neg_P1 <- P1$neg
P2$FitDifferences <- ifelse(P2$mean_yield > P2$pos_P1, "1", ifelse(P2$mean_yield < P2$neg_P1, "-1", "0"))
#+ve FitDifferences = 1, -ve = -1, intermediate = 0
write.table(P2, file = "Stats/ExL2_Parents_phenotype.txt", sep = "\t", row.names = F)

P2 <- read.delim("Stats/ExL2_Parents_phenotype.txt")
P2dif <- subset(P2, FitDifferences != "0")
P2dif <- subset(P2dif, FitDifferences != "NA")
library(dplyr)
ExL2Diff <- P2dif %>% group_by(Population, Generation, Replicate) %>% summarise(n = n())
ExL2Diff$Prop <- ExL2Diff$n/50
ExL2Diff$Population <- "LiAc0.02"
write.table(ExL2Diff, file = "Stats/ExL2_Phenotypic_Distance.txt", sep = "\t", row.names = F)

cor.test(ExL2Diff$Prop, ExL2Diff$Generation)
#t = 2.19, df = 15, p < 0.05, R = 0.49

PhenoDist <- rbind(ExNDiff, ExL1Diff, ExL2Diff)
write.table(PhenoDist, file = "Stats/Parents_phenotype_distance.txt", sep = "\t", row.names = F)


####Plots####

#####Home environment plots#####

library(dplyr)
library(ggplot2)
library(gridExtra)
#Generates mean and CI from above data. One mean/line per population. Add Group to group_by for mean per replicate

NaCl <- read.delim("Stats/NaCl_allpops_Fitness.txt")
LiAc0.01 <- read.delim("Stats/LiAc0.01_Fitness.txt")
LiAc0.02 <- read.delim("Stats/LiAc0.02_Fitness.txt")
EtOH <- read.delim("Stats/EtOH_Fitness.txt")

NaCl_Y_by_environment <- NaCl %>% group_by(Population, Generation, Environment) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
NaCl_Y_by_environment$ci <- 1.96*(NaCl_Y_by_environment$sd/sqrt(NaCl_Y_by_environment$n))
NaCl_Y_by_environment$Population <- ordered(NaCl_Y_by_environment$Population, levels = c("NaCl", "LiAc0.01", "LiAc0.02", "EtOH", "NxL1_F1", "NxL1_F2", "NxL2_F1", "NxL2_F2", "NxE_F1", "NxE_F2"))

LiAc0.01_Y_by_environment <- LiAc0.01 %>% group_by(Population, Generation, Environment) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
LiAc0.01_Y_by_environment$ci <- 1.96*(LiAc0.01_Y_by_environment$sd/sqrt(LiAc0.01_Y_by_environment$n))

LiAc0.02_Y_by_environment <- LiAc0.02 %>% group_by(Population, Generation, Environment) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
LiAc0.02_Y_by_environment$ci <- 1.96*(LiAc0.02_Y_by_environment$sd/sqrt(LiAc0.02_Y_by_environment$n))

EtOH_Y_by_environment <- EtOH %>% group_by(Population, Generation, Environment) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
EtOH_Y_by_environment$ci <- 1.96*(EtOH_Y_by_environment$sd/sqrt(EtOH_Y_by_environment$n))


p1 <- ggplot(NaCl_Y_by_environment, aes(x = Generation, y = mean_yield, colour = Population, fill = Population)) + 
  #Set plot parameters
  #based on list of home environments generated when importing data
  geom_line(data = subset(NaCl_Y_by_environment, Population != "NaCl"), size=0.75) + geom_line(data = subset(NaCl_Y_by_environment, Population == "NaCl"), size=0.75, linetype="dashed") +
  labs(x = "Parental divergence (n generations)", y = "Fitness", title = "A) NaCl 0.325M") +
  #plots lines
  geom_point(shape = 21, colour = "black", size = 3) +
  #Add mean to figure as points
  geom_ribbon(alpha=0.2, colour = NA, aes(ymin = mean_yield-ci, ymax = mean_yield+ci)) +
  #add 95% CI shading
  scale_colour_manual(values=c("#D3D3D3", "#808080", "#2F4F4F", "#000000","#FF0000", "#8B0000", "#D8BFD8", "#800080", "#ADFF2F", "#228B22")) + 
  scale_fill_manual(values=c("#D3D3D3", "#808080", "#2F4F4F", "#000000","#FF0000", "#8B0000", "#D8BFD8", "#800080", "#ADFF2F", "#228B22")) +
  #Add colours
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 1000)) +
  #make x axis only show points measured
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=22,face="bold"),  
                     axis.text=element_text(size=20), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=20,face="bold"), 
                     legend.title=element_text(size=18), legend.text=element_text(size=18), legend.background = element_rect(fill = NA))
#modify aesthetics (remove grids, plain background, text size etc.)

p2 <- ggplot(LiAc0.01_Y_by_environment, aes(x = Generation, y = mean_yield, colour = Population, fill = Population)) + 
  #Set plot parameters
  #based on list of home environments generated when importing data
  geom_line(data = subset(LiAc0.01_Y_by_environment, Population != "NaCl"), size=0.75) + geom_line(data = subset(LiAc0.01_Y_by_environment, Population == "NaCl"), size=0.75, linetype="dashed") +
  labs(x = "Parental divergence (n generations)", y = "Fitness", title = "B) LiAc 0.02M") +
  #plots lines
  geom_point(shape = 21, colour = "black", size = 3) +
  #Add mean to figure as points
  geom_ribbon(alpha=0.2, colour = NA, aes(ymin = mean_yield-ci, ymax = mean_yield+ci)) +
  #add 95% CI shading
  scale_colour_manual(values=c("#A9A9A9","#000000", "#D8BFD8", "#800080")) + 
  scale_fill_manual(values=c("#A9A9A9","#000000", "#D8BFD8", "#800080")) +
  #Add colours
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700)) +
  #make x axis only show points measured
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=22,face="bold"),  
                     axis.text=element_text(size=20), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=20,face="bold"), 
                     legend.title=element_text(size=18), legend.text=element_text(size=18), legend.background = element_rect(fill = NA))
#modify aesthetics (remove grids, plain background, text size etc.)

p3 <- ggplot(LiAc0.02_Y_by_environment, aes(x = Generation, y = mean_yield, colour = Population, fill = Population)) + 
  #Set plot parameters
  #based on list of home environments generated when importing data
  geom_line(data = subset(LiAc0.02_Y_by_environment, Population != "NaCl"), size=0.75) + geom_line(data = subset(LiAc0.02_Y_by_environment, Population == "NaCl"), size=0.75, linetype="dashed") +
  labs(x = "Parental divergence (n generations)", y = "Fitness", title = "C) LiAc 0.02M") +
  #plots lines
  geom_point(shape = 21, colour = "black", size = 3) +
  #Add mean to figure as points
  geom_ribbon(alpha=0.2, colour = NA, aes(ymin = mean_yield-ci, ymax = mean_yield+ci)) +
  #add 95% CI shading
  scale_colour_manual(values=c("#A9A9A9","#000000", "#ADFF2F", "#228B22")) + 
  scale_fill_manual(values=c("#A9A9A9","#000000", "#ADFF2F", "#228B22")) +
  #Add colours
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700)) +
  #make x axis only show points measured
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=22,face="bold"),  
                     axis.text=element_text(size=20), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=20,face="bold"), 
                     legend.title=element_text(size=18), legend.text=element_text(size=18), legend.background = element_rect(fill = NA))
#modify aesthetics (remove grids, plain background, text size etc.)

p4 <- ggplot(EtOH_Y_by_environment, aes(x = Generation, y = mean_yield, colour = Population, fill = Population)) + 
  #Set plot parameters
  #based on list of home environments generated when importing data
  geom_line(data = subset(EtOH_Y_by_environment, Population != "NaCl"), size=0.75) + geom_line(data = subset(EtOH_Y_by_environment, Population == "NaCl"), size=0.75, linetype="dashed") +
  labs(x = "Parental divergence (n generations)", y = "Fitness", title = "D) 1-Butanol 1.5%") +
  #plots lines
  geom_point(shape = 21, colour = "black", size = 3) +
  #Add mean to figure as points
  geom_ribbon(alpha=0.2, colour = NA, aes(ymin = mean_yield-ci, ymax = mean_yield+ci)) +
  #add 95% CI shading
  scale_colour_manual(values=c("#A9A9A9","#000000", "#FF0000", "#8B0000")) + 
  scale_fill_manual(values=c("#A9A9A9","#000000", "#FF0000", "#8B0000")) +
  #Add colours
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 1000)) +
  #make x axis only show points measured
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=22,face="bold"),  
                     axis.text=element_text(size=20), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=20,face="bold"), 
                     legend.title=element_text(size=18), legend.text=element_text(size=18), legend.background = element_rect(fill = NA))
#modify aesthetics (remove grids, plain background, text size etc.)

png("Plots/Figure 5.png", res = 1000, units = "in", height = 12, width = 12)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

#####Novel environment plots#####

library(dplyr)
library(ggplot2)

NaCl_novel <- read.delim("Stats/Parents_NaCl_Novel.txt")
LiAc0.01_novel <- read.delim("Stats/Parents_LiAc0.01_Novel.txt")
LiAc0.02_novel <- read.delim("Stats/Parents_LiAc0.02_Novel.txt")
EtOH_novel <- read.delim("Stats/Parents_EtOH_Novel.txt")


NaCl_Y_by_environment <- NaCl_novel %>% group_by(Population, Generation) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
NaCl_Y_by_environment$ci <- 1.96*(NaCl_Y_by_environment$sd/sqrt(NaCl_Y_by_environment$n))


LiAc0.01_Y_by_environment <- LiAc0.01_novel %>% group_by(Population, Generation) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
LiAc0.01_Y_by_environment$ci <- 1.96*(LiAc0.01_Y_by_environment$sd/sqrt(LiAc0.01_Y_by_environment$n))

LiAc0.02_Y_by_environment <- LiAc0.02_novel %>% group_by(Population, Generation) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
LiAc0.02_Y_by_environment$ci <- 1.96*(LiAc0.02_Y_by_environment$sd/sqrt(LiAc0.02_Y_by_environment$n))

EtOH_Y_by_environment <- EtOH_novel %>% group_by(Population, Generation) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
EtOH_Y_by_environment$ci <- 1.96*(EtOH_Y_by_environment$sd/sqrt(EtOH_Y_by_environment$n))

Novel <- rbind(NaCl_Y_by_environment, LiAc0.01_Y_by_environment, LiAc0.02_Y_by_environment, EtOH_Y_by_environment)

str(Novel)
Novel$Population <- ordered(Novel$Population, levels = c("EtOH", "LiAc0.01", "LiAc0.02", "NaCl"))

p1 <- ggplot(Novel, aes(x = Generation, y = mean_yield, colour = Population, fill = Population)) + 
  #Set plot parameters
  #based on list of home environments generated when importing data
  geom_line(size=0.75) +
  labs(x = "Parental divergence (n generations)", y = "Mean parental fitness across \n all novel environments") +
  #plots lines
  geom_point(shape = 21, colour = "black", size = 3) +
  #Add mean to figure as points
  geom_ribbon(alpha=0.2, colour = NA, aes(ymin = mean_yield-ci, ymax = mean_yield+ci)) +
  #add 95% CI shading
  scale_colour_manual(values=c("#B22222", "#800080", "#228B22", "#4169E1")) + 
  scale_fill_manual(values=c("#B22222","#800080", "#228B22", "#4169E1")) +
  #Add colours
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 1000)) +
  #make x axis only show points measured
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
                     axis.ticks.length = unit(0.25, "cm"), legend.key.size = unit(0.75, 'cm'), legend.title = element_text(size=12),legend.text = element_text(size=10))
#modify aesthetics (remove grids, plain background, text size etc.)

png("Plots/Figure 4.png", res = 1000, units = "in", height = 5, width = 7)
p1
dev.off()

#####Arsenite/Arsenate environment plots#####

library(dplyr)
library(ggplot2)
library(gridExtra)
#Generates mean and CI from above data. One mean/line per population. Add Group to group_by for mean per replicate

Arse <- read.delim("Stats/Arse_allpops_Fitness.txt")
Arse_1M <- subset(Arse, Environment == "As(III) 1 mM")
Arse_NE <- subset(Arse_1M, Population == "NaCl" | Population == "EtOH" | Population == "NxE_F1"|Population == "NxE_F2")
Arse_NE <- subset(Arse_NE, Generation == "700" | Generation == "1000")

Arse_NE_by_environment <- Arse_NE %>% group_by(Population, Generation, Environment, Group) %>% summarise(mean_yield = mean(Phenotypes.ExperimentGrowthYield, na.rm = TRUE),  sd = sd(Phenotypes.ExperimentGrowthYield, na.rm = TRUE), n = n_distinct(Phenotypes.ExperimentGrowthYield, na.rm = TRUE))
Arse_NE_by_environment$ci <- 1.96*(Arse_NE_by_environment$sd/sqrt(Arse_NE_by_environment$n))
Arse_NE_by_environment$Population <- ordered(Arse_NE_by_environment$Population, levels = c("EtOH", "NaCl", "NxE_F1", "NxE_F2"))


p1 <- ggplot(data = Arse_NE_by_environment, aes(x = Generation, y = mean_yield, colour = Population, group = Group))  + 
  geom_line(size=0.75) + labs(x = "Parental divergence (n generations)", y = "Mean fitness in Arsenite 1mM", title = "A) NaCl x EtOH cross") + 
  geom_point(data = Arse_NE_by_environment, aes(x = Generation, y = mean_yield, fill = Population), shape = 21, colour = "black", size = 3) +
  #geom_linerange(aes(ymin = mean_relative_yield-ci, ymax = mean_relative_yield+ci, colour = Replicate), size = 0.75) +
  #geom_ribbon(alpha=0.1, colour = NA, aes(ymin = mean_yield-ci, ymax = mean_yield+ci, fill = Population)) +
  #geom_hline(yintercept=1, linetype="dashed", size = 0.5) +
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 1000)) +
  #scale_y_continuous(breaks=seq(-5, 2, 1), limit = c(-5.2, 2)) +
  scale_colour_manual(values=c("#A9A9A9", "#000000", "#FF0000", "#8B0000")) + 
  scale_fill_manual(values=c("#A9A9A9", "#000000", "#FF0000", "#8B0000")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), title = element_text(size=16,face="bold"),  
                     axis.text=element_text(size=16), axis.ticks.length = unit(0.25, "cm"), axis.title=element_text(size=16,face="bold"), 
                     legend.title=element_text(size=14), legend.text=element_text(size=14), legend.background = element_rect(fill = NA))



png("Plots/Figure S3.png", res = 1000, units = "in", height = 5, width = 5)
p1
dev.off()

#####Parental Divergence Plots#####
Parental_Divergence <- read.delim("Stats/Parents_phenotype_distance.txt")
Parental_Divergence$RelativeProp <- Parental_Divergence$Prop/0.72
Divergence_Yield <- Parental_Divergence %>% group_by(Population, Generation) %>% summarise(mean_prop = mean(RelativeProp, na.rm = TRUE), sd = sd(RelativeProp, na.rm = TRUE),
                                                                                           n = n_distinct(RelativeProp, na.rm = TRUE))
Divergence_Yield$ci <- 1.96*(Divergence_Yield$sd/sqrt(Divergence_Yield$n))


library(ggplot2)
p1 <- ggplot(Divergence_Yield, aes(x = Generation, y = mean_prop, colour = Population, fill = Population)) + 
  #Set plot parameters
  geom_line(size=0.75) + labs(x = "Parental divergence (n generations)", y = "Relative Phenotypic Distance") +
  #plots lines and adds axes labels
  geom_point(shape = 21, colour = "black", size = 3) +
  #Add mean to figure as points
  geom_ribbon(alpha=0.1, colour = NA, aes(ymin = mean_prop-ci, ymax = mean_prop+ci)) +
  #add 95% CI shading
  geom_hline(yintercept=1, linetype="dashed", size = 0.5) +
  #Add dotted line at 1 (where phenotypic distance is same as founder)
  scale_colour_manual(values=c("#FF0000", "#9400D3", "#32CD32")) + scale_fill_manual(values=c("#FF0000", "#9400D3", "#32CD32")) +
  #Add colours
  scale_x_continuous(breaks=c(0, 100, 300, 500, 700, 1000)) +
  #make x axis only show points measured
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"), axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
                     axis.ticks.length = unit(0.25, "cm"), legend.key.size = unit(0.75, 'cm'), legend.title = element_text(size=12),legend.text = element_text(size=10))
#modify aesthetics (remove grids, plain background, text size etc.)

png("Plots/Figure S2.png", res = 1000, units = "in", height = 5, width = 7.55)
p1
dev.off()