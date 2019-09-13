#01_data_exploration

#Loading Libraries
library(openxlsx)

#Setting working directory
setwd("Y:/Oral Health Services Research Core/Spiro/DuneganKara")

#Reading in data
##Reading in Water absbsorption data
absorptiondat <- read.xlsx("Kara_Final_Study_Data.xlsx",
                           startRow = 3,
                           colNames = TRUE,
                           skipEmptyRows = TRUE,
                           skipEmptyCols = TRUE,
                           cols = c(1,2,6,7,8),
                           rows = c(3:23))
dim(absorptiondat)
#20  5

##Reading in HV data
hvdat <- read.xlsx("Kara_Final_Study_Data.xlsx",
                   startRow = 3,
                   colNames = TRUE,
                   skipEmptyRows = TRUE,
                   skipEmptyCols = TRUE,
                   cols = c(10:14))
dim(hvdat)
#100  5


#Data exploration for absorption data

##changing column names
names(absorptiondat) <- c("Method", "Replicate", "PreMass", "PostMass", "PercentChange")
str(absorptiondat)

##changing variables to factors
absorptiondat$Method <- factor(absorptiondat$Method)
absorptiondat$Replicate <- factor(absorptiondat$Replicate)

##putting in long format
absorptiondat2 <- data.frame(ID = rep(1:10,4),
                             Method = c(rep("TVF",20),rep("3D Printed",20)),
                             Timing = c(rep("Pre",10),
                                        rep("Post",10),
                                        rep("Pre",10),
                                        rep("Post",10)),
                             Absorption = c(absorptiondat$PreMass[1:10],
                                    absorptiondat$PostMass[1:10],
                                    absorptiondat$PreMass[11:20],
                                    absorptiondat$PostMass[11:20]))
absorptiondat2$Timing <- factor(absorptiondat2$Timing, levels= c("Pre", "Post"))

#Data exploration for HV data
names(hvdat) <- c("Method", "Replicate", "Diamond_units", "Diamond_mm", "HV")
str(hvdat)

##changing variables to factors
hvdat$Method <- factor(hvdat$Method)
hvdat$ID <- factor(c(sort(rep(1:10,5)), sort(rep(1:10,5))))
hvdat$Replicate <- factor(rep(1:5,20))

##reordering data
hvdat <- hvdat[,c(6,2,1,3:5)]

# Saving Datasets
saveRDS(absorptiondat2, "Y:/Oral Health Services Research Core/Spiro/DuneganKara/absorptiondat.rds")
saveRDS(hvdat, "Y:/Oral Health Services Research Core/Spiro/DuneganKara/hvdat.rds")

