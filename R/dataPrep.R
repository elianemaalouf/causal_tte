# Reference :
## [1] Li J, Yu K, Bu F, Li P, Hao L. Exploring the impact of coffee consumption and caffeine intake on cognitive performance
## in older adults: a comprehensive analysis using NHANES data and gene correlation analysis.
## Nutr J. 2025 Jul 1;24(1):102. doi: 10.1186/s12937-025-01173-x. PMID: 40597402; PMCID: PMC12220005.
#
# setup -------------------------------------------------------------------------
library(haven)
library(dplyr)
library(ggplot2)
library(rlang)

source("./R/coffee_drinks_codes.R") # read coffee drinks FNDDS codes

# locate data files for datasets 2011/2012 and 2013/2014 ------------------------
data_dir <- paste0(getwd(), "/data")

demographics_1112_file <- paste0(data_dir, "/2011_2012/DEMO_G.xpt")
dietaryD1_1112_file <- paste0(data_dir, "/2011_2012/DR1IFF_G.xpt")
totalDietarD1_1112_file <- paste0(data_dir, "/2011_2012/DR1TOT_G.xpt")
alcohol_1112_file <- paste0(data_dir, "/2011_2012/ALQ_G.xpt")
phys_1112_file <- paste0(data_dir, "/2011_2012/PAQ_G.xpt")
smoking_1112_file <- paste0(data_dir, "/2011_2012/SMQ_G.xpt")
cognitiveFn_1112_file <- paste0(data_dir, "/2011_2012/CFQ_G.xpt")

demographics_1314_file <- paste0(data_dir, "/2013_2014/DEMO_H.xpt")
dietaryD1_1314_file <- paste0(data_dir, "/2013_2014/DR1IFF_H.xpt")
totalDietarD1_1314_file <- paste0(data_dir, "/2013_2014/DR1TOT_H.xpt")
alcohol_1314_file <- paste0(data_dir, "/2013_2014/ALQ_H.xpt")
phys_1314_file <- paste0(data_dir, "/2013_2014/PAQ_H.xpt")
smoking_1314_file <- paste0(data_dir, "/2013_2014/SMQ_H.xpt")
cognitiveFn_1314_file <- paste0(data_dir, "/2013_2014/CFQ_H.xpt")

# prepare Inclusion criteria ----------------------------------------------------
#
## demogaphics
#
### variables map
demographic_vars <- c(
  SEQN = "SEQN",
  age = "RIDAGEYR",
  gender = "RIAGENDR",
  race = "RIDRETH3",
  edu = "DMDEDUC2"
)

### inclusion condition
inclusion_mask <- expr(age >= 60)

### read data & rename variables of interest & apply inclusion condition

demographics_1112 <- read_xpt(demographics_1112_file) %>%
  rename(!!!demographic_vars) %>%
  filter(!!inclusion_mask)

range(demographics_1112$age)
included_SEQN_1112 <- demographics_1112$SEQN

demographics_1314 <- read_xpt(demographics_1314_file) %>%
  rename(!!!demographic_vars) %>%
  filter(!!inclusion_mask)

range(demographics_1314$age)
included_SEQN_1314 <- demographics_1314$SEQN

### check whether individuals overlap between datasets

intersect(demographics_1112$SEQN, demographics_1314$SEQN)
length(unique(demographics_1112$SEQN))
length(unique(demographics_1314$SEQN))

### prepare demographics covariates
#
### Education (fix levels as per [1])
#
edu_logic <- expr(case_when(
  edu %in% c(1, 2) ~ "Less than High School",
  edu %in% c(3, 4) ~ "Above High School",
  edu == 5 ~ "College+",
  TRUE ~ NA
))

demographics_1112 <- demographics_1112 %>%
  mutate(edu_cat = !!edu_logic)

head(demographics_1112[, c("edu", "edu_cat")])

demographics_1314 <- demographics_1314 %>%
  mutate(edu_cat = !!edu_logic)

head(demographics_1314[, c("edu", "edu_cat")])

### race (fix levels as per [1])

race_logic <- expr(case_when(
  race %in% c(1, 2) ~ "Mexican American/other Hispanic",
  race == 3 ~ "Non-Hispanic White",
  race == 4 ~ "Non-Hispanic Black",
  race == 5 ~ "other races",
  TRUE ~ NA
))

demographics_1112 <- demographics_1112 %>%
  mutate(race_cat = !!race_logic)

head(demographics_1112[, c("race", "race_cat")])

demographics_1314 <- demographics_1314 %>%
  mutate(race_cat = !!race_logic)

head(demographics_1314[, c("race", "race_cat")])

### recode gender

gender_logic <- expr(case_when(
  is.na(gender) ~ NA_character_,
  gender == 1 ~ "Male",
  gender == 2 ~ "Female"
))

demographics_1112 <- demographics_1112 %>%
  mutate(gender = !!gender_logic)

demographics_1314 <- demographics_1314 %>%
  mutate(gender = !!gender_logic)


### drop unused columns

to_keep_vars <- c("SEQN", "gender", "age", "edu_cat", "race_cat")

data_1112 <- demographics_1112[, to_keep_vars, drop = FALSE]
data_1314 <- demographics_1314[, to_keep_vars, drop = FALSE]

data <- rbind(data_1112, data_1314)

rm(list = c("data_1112", "data_1314", "demographics_1112", "demographics_1314"))

# prepare Exposure --------------------------------------------------------------
#
# Day1 dietary data & filter included participants only & filter coffee drinks only
#
dietary_vars <- c(
  SEQN = "SEQN",
  foodCode = "DR1IFDCD", #  USDA food code
  grams = "DR1IGRMS"
)

dietaryD1_1112 <- read_xpt(dietaryD1_1112_file) %>%
  rename(!!!dietary_vars) %>%
  filter(SEQN %in% included_SEQN_1112) %>%
  group_by(SEQN, foodCode) %>% # sum same foods grams for each individual
  summarize(total_grams = sum(grams, na.rm = TRUE), .groups = "drop")

dietaryD1_1314 <- read_xpt(dietaryD1_1314_file) %>%
  rename(!!!dietary_vars) %>%
  filter(SEQN %in% included_SEQN_1314) %>%
  group_by(SEQN, foodCode) %>% # sum same foods grams for each individual
  summarize(total_grams = sum(grams, na.rm = TRUE), .groups = "drop")

### Find total grams of coffee based products
dietaryD1_1112 <- dietaryD1_1112 %>%
  group_by(SEQN) %>%
  summarize(
    total_coffee_grams = sum(
      total_grams[foodCode %in% coffee_drinks_codes], # coffee drinks code retrieved from Table 12, supp. material in [1]
      na.rm = TRUE
    )
  )

dietaryD1_1314 <- dietaryD1_1314 %>%
  group_by(SEQN) %>%
  summarize(
    total_coffee_grams = sum(
      total_grams[foodCode %in% coffee_drinks_codes],
      na.rm = TRUE
    )
  )

total_coffee_data <- rbind(dietaryD1_1112, dietaryD1_1314)

### join demographics data to total coffee data
data <- data %>%
  inner_join(total_coffee_data, by = "SEQN")

data <- data %>%
  mutate(coffee_above_threshold = as.integer(total_coffee_grams >= 480))

rm(list = c("dietaryD1_1112", "dietaryD1_1314", "total_coffee_data"))

###

ggplot(data, aes(x = total_coffee_grams)) +
  geom_histogram()

ggplot(data, aes(x = factor(coffee_above_threshold), y = age)) +
  geom_boxplot()

ggplot(data, aes(x = factor(race_cat), y = total_coffee_grams)) +
  geom_boxplot()

ggplot(data, aes(x = factor(edu_cat), y = total_coffee_grams)) +
  geom_boxplot()

tab <- table(data$edu_cat, data$coffee_above_threshold)
chisq.test(tab)

tab <- table(data$gender, data$coffee_above_threshold)
chisq.test(tab)

tab <- table(data$race_cat, data$coffee_above_threshold)
chisq.test(tab)

rm(tab)

# prepare Outcome ---------------------------------------------------------------
#
## CERAD scores
#
cognFn_vars <- c(
  SEQN = "SEQN",
  trial1Score = "CFDCST1",
  trial2Score = "CFDCST2",
  trial3Score = "CFDCST3",
  trial4Score = "CFDCSR" # delayed recall test
)

cognitiveFn_1112 <- read_xpt(cognitiveFn_1112_file) %>%
  rename(!!!cognFn_vars) %>%
  select(names(cognFn_vars)) %>%
  filter(complete.cases(.)) %>% # drop any individuals with missing one or more of the trial scores
  mutate(score = trial1Score + trial2Score + trial3Score + trial4Score) %>% # outcome
  select(c("SEQN", "score"))

cognitiveFn_1314 <- read_xpt(cognitiveFn_1314_file) %>%
  rename(!!!cognFn_vars) %>%
  select(names(cognFn_vars)) %>%
  filter(complete.cases(.)) %>% # drop any individuals with missing one or more of the trial scores
  mutate(score = trial1Score + trial2Score + trial3Score + trial4Score) %>% # outcome
  select(c("SEQN", "score"))

score_data <- rbind(cognitiveFn_1112, cognitiveFn_1314)

data <- data %>%
  inner_join(score_data, by = "SEQN")

rm(list = c("cognitiveFn_1112", "cognitiveFn_1314", "score_data"))

###

score_cutoff <- quantile(data$score, p = 0.25) # 25% percentile as per [1]

data <- data %>%
  mutate(score_above_cutoff = as.integer(score > score_cutoff))

###

ggplot(data, aes(x = score)) +
  geom_histogram()

# prepare Covariates ------------------------------------------------------------
#
## total sugar & Total fat
#
totalDiet_vars <- c(
  SEQN = "SEQN",
  total_sugar = "DR1TSUGR",
  total_fat = "DR1TTFAT"
)

totalDietarD1_1112 <- read_xpt(totalDietarD1_1112_file) %>%
  rename(!!!totalDiet_vars) %>%
  select(names(totalDiet_vars)) %>%
  filter(complete.cases(.))

totalDietarD1_1314 <- read_xpt(totalDietarD1_1314_file) %>%
  rename(!!!totalDiet_vars) %>%
  select(names(totalDiet_vars)) %>%
  filter(complete.cases(.))

totalDiet_data <- rbind(totalDietarD1_1112, totalDietarD1_1314)

data <- data %>%
  inner_join(totalDiet_data, by = "SEQN")

rm(list = c("totalDietarD1_1112", "totalDietarD1_1314", "totalDiet_data"))
#
## alcohol
#
alcohol_vars <- c(
  SEQN = "SEQN",
  atLeast12perY = "ALQ101" # same as used in [1]
)

alcohol_1112 <- read_xpt(alcohol_1112_file) %>%
  rename(!!!alcohol_vars) %>%
  select(names(alcohol_vars)) %>%
  mutate(
    alcohol_consumption = case_when(
      is.na(atLeast12perY) ~ NA_integer_,
      atLeast12perY == 1 ~ 1L,
      atLeast12perY == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(complete.cases(.)) %>%
  select(c("SEQN", "alcohol_consumption"))

alcohol_1314 <- read_xpt(alcohol_1314_file) %>%
  rename(!!!alcohol_vars) %>%
  select(names(alcohol_vars)) %>%
  mutate(
    alcohol_consumption = case_when(
      is.na(atLeast12perY) ~ NA_integer_,
      atLeast12perY == 1 ~ 1L,
      atLeast12perY == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(complete.cases(.)) %>%
  select(c("SEQN", "alcohol_consumption"))

alcohol_data <- rbind(alcohol_1112, alcohol_1314)

data <- data %>%
  inner_join(alcohol_data, by = "SEQN")

rm(list = c("alcohol_1112", "alcohol_1314", "alcohol_data"))
#
## smoking
#
smoking_vars <- c(
  SEQN = "SEQN",
  smokedAtLeast100CigInLife = "SMQ020" # same as used in [1]
)

smoking_1112 <- read_xpt(smoking_1112_file) %>%
  rename(!!!smoking_vars) %>%
  select(names(smoking_vars)) %>%
  mutate(
    smoking_history = case_when(
      is.na(smokedAtLeast100CigInLife) ~ NA_integer_,
      smokedAtLeast100CigInLife == 1 ~ 1L,
      smokedAtLeast100CigInLife == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(complete.cases(.)) %>%
  select("SEQN", "smoking_history")

smoking_1314 <- read_xpt(smoking_1314_file) %>%
  rename(!!!smoking_vars) %>%
  select(names(smoking_vars)) %>%
  mutate(
    smoking_history = case_when(
      is.na(smokedAtLeast100CigInLife) ~ NA_integer_,
      smokedAtLeast100CigInLife == 1 ~ 1L,
      smokedAtLeast100CigInLife == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(complete.cases(.)) %>%
  select("SEQN", "smoking_history")

smoking_data <- rbind(smoking_1112, smoking_1314)

data <- data %>%
  inner_join(smoking_data, by = "SEQN")

rm(list = c("smoking_1112", "smoking_1314", "smoking_data"))

#
## physical activity
#
### considered only variables for which there is a suggested MET equivalent in the dataset
phys_vars <- c(
  SEQN = "SEQN",
  vigWork = "PAQ605", # Vigorous work activity
  vigWorkDaysPerWeek = "PAQ610",
  vigWorkMinPerDay = "PAD615",
  modWork = "PAQ620", # Moderate work activity
  modWorkDaysPerWeek = "PAQ625",
  modWorkMinPerDay = "PAD630",
  activeTransportation = "PAQ635", # Walk or bicycle
  activeTransportationDaysPerWeek = "PAQ640",
  activeTransportationMinPerDay = "PAD645",
  vigRecreational = "PAQ650", #Vigorous recreational activities
  vigRecreationalDaysPerWeek = "PAQ655",
  vigRecreationalMinPerDay = "PAD660",
  modRecreational = "PAQ665", # Moderate recreational activities
  modRecreationalDaysPerWeek = "PAQ670",
  modRecreationalMinPerDay = "PAD675"
)

MET_per_activity = list(
  # per minute
  vigWork = 8.0,
  modWork = 4.0,
  activeTransportation = 4.0,
  vigRecreational = 8.0,
  modRecreational = 4.0
)

phys_1112 <- read_xpt(phys_1112_file) %>%
  rename(!!!phys_vars) %>%
  select(names(phys_vars)) %>%
  mutate(
    # Compute METs only if the categorical 'Yes/No' variable == 1
    met_vigWork = if_else(
      vigWork == 1,
      vigWorkDaysPerWeek * vigWorkMinPerDay * MET_per_activity$vigWork,
      0,
      missing = NA
    ),

    met_modWork = if_else(
      modWork == 1,
      modWorkDaysPerWeek * modWorkMinPerDay * MET_per_activity$modWork,
      0,
      missing = NA
    ),

    met_activeTrans = if_else(
      activeTransportation == 1,
      activeTransportationDaysPerWeek *
        activeTransportationMinPerDay *
        MET_per_activity$activeTransportation,
      0,
      missing = NA
    ),

    met_vigRec = if_else(
      vigRecreational == 1,
      vigRecreationalDaysPerWeek *
        vigRecreationalMinPerDay *
        MET_per_activity$vigRecreational,
      0,
      missing = NA
    ),

    met_modRec = if_else(
      modRecreational == 1,
      modRecreationalDaysPerWeek *
        modRecreationalMinPerDay *
        MET_per_activity$modRecreational,
      0,
      missing = NA
    ),

    total_active_met = met_vigWork +
      met_modWork +
      met_activeTrans +
      met_vigRec +
      met_modRec
  ) %>%
  select(c("SEQN", "total_active_met")) %>%
  filter(complete.cases(.))

phys_1314 <- read_xpt(phys_1314_file) %>%
  rename(!!!phys_vars) %>%
  select(names(phys_vars)) %>%
  mutate(
    # Compute METs only if the categorical 'Yes/No' variable == 1
    met_vigWork = if_else(
      vigWork == 1,
      vigWorkDaysPerWeek * vigWorkMinPerDay * MET_per_activity$vigWork,
      0,
      missing = NA
    ),

    met_modWork = if_else(
      modWork == 1,
      modWorkDaysPerWeek * modWorkMinPerDay * MET_per_activity$modWork,
      0,
      missing = NA
    ),

    met_activeTrans = if_else(
      activeTransportation == 1,
      activeTransportationDaysPerWeek *
        activeTransportationMinPerDay *
        MET_per_activity$activeTransportation,
      0,
      missing = NA
    ),

    met_vigRec = if_else(
      vigRecreational == 1,
      vigRecreationalDaysPerWeek *
        vigRecreationalMinPerDay *
        MET_per_activity$vigRecreational,
      0,
      missing = NA
    ),

    met_modRec = if_else(
      modRecreational == 1,
      modRecreationalDaysPerWeek *
        modRecreationalMinPerDay *
        MET_per_activity$modRecreational,
      0,
      missing = NA
    ),

    total_active_met = met_vigWork +
      met_modWork +
      met_activeTrans +
      met_vigRec +
      met_modRec
  ) %>%
  select(c("SEQN", "total_active_met")) %>%
  filter(complete.cases(.))

phys_data <- rbind(phys_1112, phys_1314)

data <- data %>%
  inner_join(phys_data, by = "SEQN")

data <- data %>%
  mutate(
    phys_within_guidelines = as.integer(total_active_met >= 500),
    phys_cat = case_when(
      total_active_met < 500 ~ "not active",
      total_active_met >= 500 & total_active_met < 1000 ~ "active",
      total_active_met >= 1000 ~ "highly active"
    )
  )

rm(list = c("phys_1112", "phys_1314", "phys_data"))

#
## keep complete cases only
#
data <- data %>%
  filter(complete.cases(.))

#
## store data to file
#
output_file <- paste0(data_dir, "/dataset.rda")
save(data, file = output_file)
