library(dplyr)
library(ggplot2)
library(stringr)
library(gdata)
library(readr)
library(plyr)

bls <- read_csv("blsdata.csv")
soc <- read_csv("soc_index.csv")
auto <- read_csv("automationdata.csv")

bls <- bls %>% group_by(occupation = stringi::stri_trans_totitle(occupation))
soc <- soc %>% group_by(occupation = stringi::stri_trans_totitle(occupation))

bls <-merge(bls,soc, by.x="occupation", by.y = "occupation")

auto <- merge(auto, bls, by.x="soc_codes", by.y="soc_code")

autorace <- auto[c("occupation.x", "prob_computerization", "tot_employed","women","black","asian","hisp_latino")]
colnames(autorace)[1] <- "occupation"
write.csv(autorace,file = "automation_race_merged")
