library(dplyr)
library(ggplot2)

tx3 <- read_csv("Texas_District_03.csv")
names(tx3) <- c("Topic", "Subject", "Title", "District.03.Value", "District.03.MOE")
tx3_datasets = list()

# Total population: 819626
tx3_datasets[[1]] <- tx3_sex_age <- tx3 %>%
  filter(Topic == "People") %>% filter(Subject == "Sex and Age")
tx3_datasets[[2]] <- tx3_race <- tx3 %>%
  filter(Topic == "People") %>% filter(Subject == "Race")
tx3_datasets[[3]] <- tx3_place_of_birth <- tx3 %>%
  filter(Topic == "People") %>% filter(Subject == "Place of Birth")
tx3_datasets[[4]] <- tx3_ancestry <- tx3 %>%
  filter(Topic == "People") %>% filter(Subject == "Ancestry")
tx3_datasets[[5]] <- tx3_vets <- tx3 %>%
  filter(Topic == "People") %>% filter(Subject == "Veteran Status")
tx3_datasets[[6]] <- tx3_employment <- tx3 %>%
  filter(Topic == "Workers") %>% filter(Subject == "Employment Status")
tx3_datasets[[7]] <- tx3_occupation <- tx3 %>%
  filter(Topic == "Workers") %>% filter(Subject == "Occupation")
tx3_datasets[[8]] <- tx3_industry <- tx3 %>%
  filter(Topic == "Workers") %>% filter(Subject == "Industry")
tx3_datasets[[9]] <- tx3_income <- tx3 %>%
  filter(Topic == "Socioeconomic") %>% filter(Subject == "Income and Benefits (In 2015 inflation-adjusted dollars)")
tx3_datasets[[10]] <- tx3_insurancecov <- tx3 %>%
  filter(Topic == "Socioeconomic") %>% filter(Subject == "Health Insurance Coverage")
tx3_datasets[[11]] <- tx3_education <- tx3 %>%
  filter(Topic == "Education") %>% filter(Subject == "Educational Attainment")
tx3_datasets[[12]] <- tx3_num_employees <- tx3 %>%
  filter(Topic == "Business") %>% filter(Subject == "Paid employees for pay period including March 12")

#tx3_race <- tx3_race %>% mutate(proportion = District.03.Value/)
ggplot(tx3_race, aes(x = Title, y = District.03.Value, fill = Title)) + geom_col()


for(i in 1:12){
  file_name <- paste("tx3_", i, ".csv", sep = "")
  file(file_name, "wb")
  write.table(tx3_datasets[[i]], file = file_name, sep = ",", row.names = FALSE,
              qmethod = "double")
  }

