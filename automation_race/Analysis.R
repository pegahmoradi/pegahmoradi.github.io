library(dplyr)
library(ggplot2)
library(stringr)
library(gdata)
library(readr)
library(plyr)
library(tidyr)
library(mosaic)
library(vcdExtra)

data<-read.csv("automation_race_merged_clean.csv")
data <- data %>% na.omit()

# Add white and men
data <- data %>% mutate(white = 100 - black - asian)
data <- data %>% mutate(men = 100 - women)

# Race/ethnicity data for geom_smooth
data_RE <- data %>%  gather(race_group, pct_race, 
                            -occupation, -tot_employed, 
                            -women, -prob_computerization, -men) 
# Gender data for geom_smooth
data_gend <- data %>%  gather(gender, pct_gend, 
                              -occupation, -tot_employed, 
                              -white, -black, -asian, -hisp_latino, -prob_computerization) 

# All race/ethnicity groups
ggplot(data_RE, aes(y = prob_computerization, x = pct_race, color = race_group)) +
  geom_jitter(alpha = .3) + geom_smooth(se= F, size = 1.3) + xlim(0, 100) +theme_bw() + 
  labs(title = "All workers") +scale_color_discrete(name="Race/Ethnicity",
                                                    labels=c("Asian", "Black", "Hispanic/Latino","White"))


# Minority groups
ggplot(filter(data_RE, race_group == "black" | race_group == "asian" | race_group == "hisp_latino"), aes(y = prob_computerization, x = pct_race, color = race_group)) +
  geom_jitter(alpha = .3) + geom_smooth(se= F, size = 1.3) + xlim(0, 30) +theme_bw()+ 
  labs(title = "Minority workers") +scale_color_discrete(name="Race/Ethnicity",
                                                         labels=c("Asian", "Black", "Hispanic/Latino"))

# White
ggplot(filter(data_RE, race_group == "white"), aes(y = prob_computerization, x = pct_race, color = race_group)) +
  geom_jitter(alpha = .3) + geom_smooth(size = 1.3) + xlim(50, 100) +theme_bw()+ 
  labs(title = "White workers")

# Women vs. Men

# ******** WOMEN*********
ggplot(data_RE, aes(y = prob_computerization,
                    x = women)) +
  geom_jitter(alpha = .2) + 
  geom_smooth(method = lm) + 
  xlim(0, 100) +
  theme_bw()+ 
  labs(title = "Female workers", x = "Percent women in occupation", y= "Probability of automation")

# ****** MEN********
ggplot(data_RE, aes(y = prob_computerization,
                    x = men)) +
  geom_jitter(alpha = .2) + 
  geom_smooth(method = lm) + 
  xlim(0, 100) +
  theme_bw()+ 
  labs(title = "Male workers", x = "Percent men in occupation", y= "Probability of automation")



##################################################################################

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
data <- data %>% mutate(white_num = round(tot_employed * white/100))
data <- data %>% mutate(women_num = round(tot_employed * women/100))
data <- data %>% mutate(black_num = round(tot_employed * black/100))
data <- data %>% mutate(asian_num = round(tot_employed * asian/100))
data <- data %>% mutate(hisp_latino_num = round(tot_employed * hisp_latino/100))
data <- data %>% mutate(men_num = round(tot_employed * men/100))

data_num <- data[c("occupation",
                   "prob_computerization",
                   "tot_employed",
                   "men_num",
                   "women_num",
                   "asian_num",
                   "black_num",
                   "hisp_latino_num",
                   "white_num")]
colnames(data_num)[4] <- "men"
colnames(data_num)[5] <- "women"
colnames(data_num)[6] <- "asian"
colnames(data_num)[7] <- "black"
colnames(data_num)[8] <- "hisp_latino"
colnames(data_num)[9] <- "white"

# Race/Ethnicity counts
race_counts <- data_num %>% gather(race_group, race_count,
                                        -occupation, -tot_employed, 
                                        -women, -prob_computerization, -men)
race_counts <- race_counts %>% mutate(pr_comp= derivedFactor(
  "0.90" = (prob_computerization < 1 & prob_computerization >= 0.9),
  "0.80" = (prob_computerization < 0.9 & prob_computerization >= 0.8),
  "0.70" = (prob_computerization < 0.8 & prob_computerization >= 0.7),
  "0.60" = (prob_computerization < 0.7 & prob_computerization >= 0.6),
  "0.50" = (prob_computerization < 0.6 & prob_computerization >= 0.5),
  "0.40" = (prob_computerization < 0.5 & prob_computerization >= 0.4),
  "0.30" = (prob_computerization < 0.4 & prob_computerization >= 0.3),
  "0.20" = (prob_computerization < 0.3 & prob_computerization >= 0.2),
  "0.10" = (prob_computerization < 0.2 & prob_computerization >= 0.1),
  "0.00" = (prob_computerization < 0.1 & prob_computerization >= 0.0)
))

race_freq <- as.data.frame(xtabs(race_count ~ pr_comp+race_group, race_counts))


# RACE/ETHNICITY: COUNTS
ggplot(race_freq, aes(y = Freq, x = as.numeric.factor(pr_comp), color = race_group)) + geom_jitter(width = 0.01) + geom_smooth(se = F)+ theme_bw() + labs(x = "Probability of computerization", y= "Number of workers (Thousands)") +scale_color_discrete(name="Race/Ethnicity",
                                                                                                                                                                                                                                                                                                                                             labels=c("Asian", "Black", "Hispanic/Latino","White"))

# Gender counts
gend_counts <- data_num %>% gather(gender, gend_count,
                                        -occupation, -tot_employed, 
                                        -white, -black, -asian, -hisp_latino,
                                        -prob_computerization)

gend_counts <- gend_counts %>% mutate(pr_comp= derivedFactor(
  "0.90" = (prob_computerization < 1 & prob_computerization >= 0.9),
  "0.80" = (prob_computerization < 0.9 & prob_computerization >= 0.8),
  "0.70" = (prob_computerization < 0.8 & prob_computerization >= 0.7),
  "0.60" = (prob_computerization < 0.7 & prob_computerization >= 0.6),
  "0.50" = (prob_computerization < 0.6 & prob_computerization >= 0.5),
  "0.40" = (prob_computerization < 0.5 & prob_computerization >= 0.4),
  "0.30" = (prob_computerization < 0.4 & prob_computerization >= 0.3),
  "0.20" = (prob_computerization < 0.3 & prob_computerization >= 0.2),
  "0.10" = (prob_computerization < 0.2 & prob_computerization >= 0.1),
  "0.00" = (prob_computerization < 0.1 & prob_computerization >= 0.0)
))

gend_freq <- as.data.frame(xtabs(gend_count ~ pr_comp+gender, gend_counts))


# GENDER: COUNTS
ggplot(gend_freq, aes(y = Freq, x = as.numeric.factor(pr_comp), color = gender)) + geom_jitter(width = 0.01) + geom_smooth(se = F)+ theme_bw() + labs( x = "Probability of computerization", y= "Number of workers (Thousands)") + scale_color_discrete(name = "Gender", labels=c("Men","Women"))







######


data <- data %>% mutate(men_effect = (tot_employed * men/100 * prob_computerization))
data <- data %>% mutate(women_effect = (tot_employed * women/100 * prob_computerization))
data <- data %>% mutate(asian_effect = (tot_employed * asian/100 * prob_computerization))
data <- data %>% mutate(black_effect = (tot_employed * black/100 * prob_computerization))
data <- data %>% mutate(hisp_latino_effect = (tot_employed * hisp_latino/100 * prob_computerization))
data <- data %>% mutate(white_effect = (tot_employed * white/100 * prob_computerization))

effects <- data[c("men_effect", "women_effect", "asian_effect", "black_effect", "hisp_latino_effect", "white_effect")]

sum(effects$men_effect)
sum(effects$women_effect)
sum(effects$asian_effect)
sum(effects$black_effect)
sum(effects$hisp_latino_effect)
sum(effects$white_effect)
race <- c("asian","black","hispanic/latino","white")
sum <- c(
  sum(effects$asian_effect),
  sum(effects$black_effect),
  sum(effects$hisp_latino_effect),
  sum(effects$white_effect)
)

gend <- c("men", "women")
sum_gend <- c(
  sum(effects$men_effect),
  sum(effects$women_effect)
)

effect_race <- data.frame(race,sum)
effect_gend <- data.frame(gend, sum_gend)

# EFFECT - NOT ACCOUNTING FOR POPULATION
ggplot(effect_race, aes(x = race, y = sum, fill = race)) +geom_col() + labs(x = "Race", y = "Aggregate automation effect value") + guides(fill=FALSE) + theme_bw()
ggplot(effect_gend, aes(x = gend, y = sum_gend, fill = gend)) +geom_col() + labs(x = "Gender", y = "Aggregate automation effect value") + guides(fill=FALSE) + theme_bw()

# EFFECT, ACCOUNTING FOR POPULATION
controlled <- c(
  sum(effects$asian_effect)*0.061,
  sum(effects$black_effect)*0.119,
  sum(effects$hisp_latino_effect)*0.167,
  sum(effects$white_effect)*0.653
)
controlled_gend <- c(
  sum(effects$women_effect)*0.468,
  sum(effects$men_effect)*0.532
)

effect_controlled <- data.frame(race, controlled)
effect_controlled_gend <- data.frame(gend, controlled_gend)

ggplot(effect_race, aes(x = race, y = controlled, fill = race)) +geom_col() + theme_bw() + labs(x = "Race", y = "Aggregate automation effect value \n controlled for proportion of workforce") + guides(fill=FALSE) 
ggplot(effect_gend, aes(x = gend, y = controlled_gend, fill = gend)) +geom_col() + theme_bw() + labs(x = "Gender", y = "Aggregate automation effect value \n controlled for proportion of workforce") + guides(fill=FALSE) 


# Each case: Regression
race_case <- expand.dft(race_counts, freq = "race_count")

m_race_gend <- lm(prob_computerization ~ poly(race_group, 2), data = race_case)
summary(m_race_gend)

