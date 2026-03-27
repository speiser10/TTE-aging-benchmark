# Time to event benchmark study with aging data
# Jaime Lynn Speiser
# 3/18/2026
# summaries of the datasets 


#read in the datasets
# Load the packages
library(haven)
library(dplyr)


# Define the base path
base_path <- "C:/Users/jspeiser/OneDrive - Advocate Health/Biostat Projects/OAIC Multicenter Pilot/Program/Datasets"

# Define full file paths for baseline datasets
baseline_files <- list(
  chs  = file.path(base_path, "baseline chs.sas7bdat"),
  habc = file.path(base_path, "baseline habc.sas7bdat"),
  mros = file.path(base_path, "baseline mros.sas7bdat"),
  sof  = file.path(base_path, "baseline sof.sas7bdat")
)

# Define full file paths for extra datasets
extra_files <- list(
  chs  = file.path(base_path, "year1 chs.sas7bdat"),
  sof  = file.path(base_path, "year10 sof.sas7bdat")
)

# Define full file paths for outcome datasets
outcome_files <- list(
  chs  = file.path(base_path, "outcome chs.sas7bdat"),
  habc = file.path(base_path, "outcome habc.sas7bdat"),
  mros = file.path(base_path, "outcome mros.sas7bdat"),
  sof  = file.path(base_path, "outcome sof.sas7bdat")
)

# Read in the datasets
baseline_data <- lapply(baseline_files, read_sas)
outcome_data <- lapply(outcome_files, read_sas)
extra_data <- lapply(extra_files, read_sas)

# Select specific variables from each outcome dataset
names(outcome_data$chs)
names(outcome_data$habc)
names(outcome_data$mros)
names(outcome_data$sof)

# Define which variables to keep for each dataset
vars_to_keep <- list(
  chs  = c("idno", "RACE", "EDUC", "MALE", "MOBDISFU", "MOBDIS"),
  habc = c("HABCID", "RACE", "EDUC", "MALE", "MOBDISFU", "MOBDIS"),
  mros = c("MROSID", "RACE", "EDUC", "MOBDISFU", "MOBDIS"),
  sof  = c("ID", "RACE", "EDUC", "MOBDISFU", "MOBDIS")
)

#notes: had to delete male from mros and sof because it was constant

# Apply variable selection using Map
outcome_data_selected <- Map(function(df, vars) {
  df_selected <- df %>% select(any_of(vars))
  names(df_selected)[1] <- "ID"
  df_selected
}, outcome_data, vars_to_keep)

summary(outcome_data_selected$chs)
summary(outcome_data_selected$habc)
summary(outcome_data_selected$mros)
summary(outcome_data_selected$sof)

# Select specific variables from each baseline dataset
names(baseline_data$chs)
names(baseline_data$habc)
names(baseline_data$mros)
names(baseline_data$sof)

# Define which variables to keep for each dataset
vars_to_keep1 <- list(
  chs  = c("idno", "WLKYN", "GAITLT8", "AGE", "SRSTAT", "SMK", "DRNK", "MHBP", "MHCHF", "MHDIAB", "GSGRPAVG", "CHAIR10","CREATIN","IL6","BMI"),
  habc = c("habcid", "GAITLT8", "AGE", "SRSTAT", "SMK", "DRNK", "MHBP", "MHCHF", "MHDIAB", "MHOSTEO", "GSGRPAVG", "CHAIR10","CREATIN","IL6","BMI"),
  mros = c("MROSID", "WLKYN", "GAITLT8", "AGE", "SRSTAT", "SMK", "DRNK", "MHBP", "MHCHF", "MHDIAB", "MHOSTEO", "GSGRPAVG", "CHAIR10","CREATIN","BMI"),
  sof  = c("ID", "WLKYN", "GAITLT8", "AGE", "SRSTAT", "SMK", "DRNK", "MHDIAB", "MHOSTEO", "GSGRPAVG", "CHAIR10","BMI")
)

# Apply variable selection using Map
baseline_data_selected <- Map(function(df, vars) {
  df_selected <- df %>% select(any_of(vars))
  names(df_selected)[1] <- "ID"
  df_selected
}, baseline_data, vars_to_keep1)

#add in WLKYN=0 for all habc participants since none had difficulty working at baseline per inclusion criteria
baseline_data_selected$habc$WLKYN <- rep(0,length(baseline_data_selected$habc$AGE))

summary(baseline_data_selected$chs)
summary(baseline_data_selected$habc)
summary(baseline_data_selected$mros)
summary(baseline_data_selected$sof)

# Select specific variables from each extra dataset
names(extra_data$chs)
names(extra_data$sof)

# Define which variables to keep for each dataset
vars_to_keep1 <- list(
  chs  = c("idno", "MHOSTEO"),
  sof  = c("ID", "MHBP","MHCHF")
)

#merge in Y10 for sof: "MHBP","MHCHF","IL6"
#merge in Y1 for chs: "MHOSTEO"

# Apply variable selection using Map
extra_data_selected <- Map(function(df, vars) {
  df_selected <- df %>% select(any_of(vars))
  names(df_selected)[1] <- "ID"
  df_selected
}, extra_data, vars_to_keep1)

summary(extra_data_selected$chs)
summary(extra_data_selected$sof)

#merge in the extra data for chs and sof
baseline_data_selected$chs <- baseline_data_selected$chs %>%
  left_join(extra_data_selected$chs, by = "ID")
baseline_data_selected$sof <- baseline_data_selected$sof %>%
  left_join(extra_data_selected$sof, by = "ID")

#merge data from baseline and outcome for each study
merged_data <- Map(function(base, outcome) {
  inner_join(base, outcome, by = "ID")
}, baseline_data_selected, outcome_data_selected)

summary(merged_data$chs)
summary(merged_data$habc)
summary(merged_data$mros)
summary(merged_data$sof)

dim(merged_data$chs)
dim(merged_data$habc)
dim(merged_data$mros)
dim(merged_data$sof)



summary(bm_data$chs)
summary(bm_data$habc)
summary(bm_data$mros)
summary(bm_data$sof)

#exclude people who had mobility limitaiton at baseline, based on difficulty walking or gaitspeed less than 0.8 m/s
filter_gait_walk <- function(df) {
  df[df$GAITLT8 == 0 & df$WLKYN == 0, ]
}
filtered_data <- list()
filtered_data$chs <- filter_gait_walk(merged_data$chs)
filtered_data$habc <- filter_gait_walk(merged_data$habc)
filtered_data$mros <- filter_gait_walk(merged_data$mros)
filtered_data$sof <- filter_gait_walk(merged_data$sof)


dim(filtered_data$chs)
dim(filtered_data$habc)
dim(filtered_data$mros)
dim(filtered_data$sof)

#remove the id variable and filtering variables for use in the benchmark study
bm_data <- lapply(filtered_data, function(df) {
  df[, -c(1,2)]
})

#manually delete other filter variable since it is in different locations in the datasets
bm_data$chs <- bm_data$chs[,-1]
bm_data$habc <- bm_data$habc[,-14]
bm_data$mros <- bm_data$mros[,-1]
bm_data$sof <- bm_data$sof[,-1]


#standardize continuous variables
standardize_numeric <- function(df, vars) {
  # vars should be a character vector of variable names to standardize
  
  # Check that all requested variables exist
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop("These variables are not in the dataset: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Keep only existing + numeric variables
  non_numeric <- vars[!sapply(df[vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop("These variables are not numeric: ",
         paste(non_numeric, collapse = ", "))
  }
  
  # Standardize each variable
  for (v in vars) {
    df[[v]] <- as.numeric(scale(df[[v]], center = TRUE, scale = TRUE))
  }
  
  return(df)
}

#summary of variables to put into table for paper
summary(bm_data$chs)
summary(bm_data$habc)
summary(bm_data$mros)
summary(bm_data$sof)


#standardized variables
bm_data$chs <- standardize_numeric(bm_data$chs, vars = c("AGE","GSGRPAVG","CREATIN","IL6","BMI"))
bm_data$habc <- standardize_numeric(bm_data$habc, vars = c("AGE","GSGRPAVG","CREATIN","IL6","BMI"))
bm_data$mros <- standardize_numeric(bm_data$mros, vars = c("AGE","GSGRPAVG","CREATIN","BMI"))
bm_data$sof <- standardize_numeric(bm_data$sof, vars = c("AGE","GSGRPAVG","BMI"))


summary(bm_data$chs)
summary(bm_data$habc)
summary(bm_data$mros)
summary(bm_data$sof)



###PACKAGE LOADING############################################################################################

# Load required packages
library(mlr3)
library(mlr3proba)
library(mlr3learners)
library(mlr3benchmark)
library(mlr3extralearners)
library(mlr3viz)
library(mlr3pipelines)
library(survival)
library(ggplot2)
library(gbm)
library(xgboost)
library(randomForestSRC)
library(CoxBoost)
library(survAUC)
library(survivalmodels)
library(data.table)
library(mlr3verse)
library(mlr3misc)
library(purrr)
library(paradox)
library(distr6)
library(future)
library(progressr)
library(parallelly)
library(lgr)



# Install dependencies for specific learners
install_learners(c("surv.ranger", "surv.coxboost", "surv.penalized", "surv.parametric", "surv.svm",
	 "surv.aorsf", "surv.blockforest","surv.bart", "surv.cforest", "surv.ctree", "surv.flexible", 
	"surv.gamboost", "surv.glmboost", "surv.glmnet", "surv.mboost"))


###DATASETS############################################################################################

summary(bm_data$chs)
summary(bm_data$habc)
summary(bm_data$mros)
summary(bm_data$sof)

dim(bm_data$chs)
dim(bm_data$habc)
dim(bm_data$mros)
dim(bm_data$sof)

#delete NAs for complete case analysis

bm_data$chs<- na.omit(bm_data$chs)
bm_data$habc<- na.omit(bm_data$habc)
bm_data$mros<- na.omit(bm_data$mros)
bm_data$sof<- na.omit(bm_data$sof)

names(bm_data$chs)
names(bm_data$habc)
names(bm_data$mros)
names(bm_data$sof)

summary(bm_data$chs)
summary(bm_data$habc)
summary(bm_data$mros)
summary(bm_data$sof)

summary(bm_data$chs$MALE)
summary(bm_data$habc$MALE)

#make categorical variables into factors
bm_data$chs$RACE<-as.factor(bm_data$chs$RACE)
bm_data$habc$RACE<-as.factor(bm_data$habc$RACE)
bm_data$mros$RACE<-as.factor(bm_data$mros$RACE)
bm_data$sof$RACE<-as.factor(bm_data$sof$RACE)

bm_data$chs$EDUC<-as.factor(bm_data$chs$EDUC)
bm_data$habc$EDUC<-as.factor(bm_data$habc$EDUC)
bm_data$mros$EDUC<-as.factor(bm_data$mros$EDUC)
bm_data$sof$EDUC<-as.factor(bm_data$sof$EDUC)

bm_data$chs$SRSTAT<-as.factor(bm_data$chs$SRSTAT)
bm_data$habc$SRSTAT<-as.factor(bm_data$habc$SRSTAT)
bm_data$mros$SRSTAT<-as.factor(bm_data$mros$SRSTAT)
bm_data$sof$SRSTAT<-as.factor(bm_data$sof$SRSTAT)

bm_data$chs$SMK<-as.factor(bm_data$chs$SMK)
bm_data$habc$SMK<-as.factor(bm_data$habc$SMK)
bm_data$mros$SMK<-as.factor(bm_data$mros$SMK)
bm_data$sof$SMK<-as.factor(bm_data$sof$SMK)

bm_data$chs$DRNK<-as.factor(bm_data$chs$DRNK)
bm_data$habc$DRNK<-as.factor(bm_data$habc$DRNK)
bm_data$mros$DRNK<-as.factor(bm_data$mros$DRNK)
bm_data$sof$DRNK<-as.factor(bm_data$sof$DRNK)

bm_data$chs$MHBP<-as.factor(bm_data$chs$MHBP)
bm_data$habc$MHBP<-as.factor(bm_data$habc$MHBP)
bm_data$mros$MHBP<-as.factor(bm_data$mros$MHBP)
bm_data$sof$MHBP<-as.factor(bm_data$sof$MHBP)

bm_data$chs$MHCHF<-as.factor(bm_data$chs$MHCHF)
bm_data$habc$MHCHF<-as.factor(bm_data$habc$MHCHF)
bm_data$mros$MHCHF<-as.factor(bm_data$mros$MHCHF)
bm_data$sof$MHCHF<-as.factor(bm_data$sof$MHCHF)

bm_data$chs$MHDIAB<-as.factor(bm_data$chs$MHDIAB)
bm_data$habc$MHDIAB<-as.factor(bm_data$habc$MHDIAB)
bm_data$mros$MHDIAB<-as.factor(bm_data$mros$MHDIAB)
bm_data$sof$MHDIAB<-as.factor(bm_data$sof$MHDIAB)

bm_data$chs$MHOSTEO<-as.factor(bm_data$chs$MHOSTEO)
bm_data$habc$MHOSTEO<-as.factor(bm_data$habc$MHOSTEO)
bm_data$mros$MHOSTEO<-as.factor(bm_data$mros$MHOSTEO)
bm_data$sof$MHOSTEO<-as.factor(bm_data$sof$MHOSTEO)

bm_data$chs$MALE<-as.factor(bm_data$chs$MALE)
bm_data$habc$MALE<-as.factor(bm_data$habc$MALE)

bm_data$chs$MOBDIS<-as.factor(bm_data$chs$MOBDIS)
bm_data$habc$MOBDIS<-as.factor(bm_data$habc$MOBDIS)
bm_data$mros$MOBDIS<-as.factor(bm_data$mros$MOBDIS)
bm_data$sof$MOBDIS<-as.factor(bm_data$sof$MOBDIS)



#MAKE SURE mobility disability time is greater than 0
bm_data$chs1 <- bm_data$chs %>% filter(MOBDISFU > 0)
bm_data$habc1 <- bm_data$habc %>% filter(MOBDISFU > 0)
bm_data$mros1 <- bm_data$mros %>% filter(MOBDISFU > 0)
bm_data$sof1 <- bm_data$sof %>% filter(MOBDISFU > 0)

dim(bm_data$chs1)
dim(bm_data$habc1)
dim(bm_data$mros1)
dim(bm_data$sof1)

#make summary table of all data
chs  <- bm_data$chs  %>% mutate(cohort = "CHS")
habc <- bm_data$habc %>% mutate(cohort = "HABC")
mros <- bm_data$mros %>% mutate(cohort = "MROS")
sof  <- bm_data$sof  %>% mutate(cohort = "SOF")

df_all <- bind_rows(chs, habc, mros, sof)

continuous_vars <- c("AGE", "GSGRPAVG", "CHAIR10", "CREATIN", "IL6", "BMI", "MOBDISFU")

cont_summary <- df_all %>%
  select(cohort, any_of(continuous_vars)) %>%
  pivot_longer(
    cols = -cohort,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable, cohort) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stat = ifelse(
      is.nan(mean),
      NA_character_,
      sprintf("%.1f (%.1f)", mean, sd)
    )
  ) %>%
  select(variable, cohort, stat)

categorical_vars <- setdiff(names(df_all), c("cohort", continuous_vars))

cat_summary <- df_all %>%
  select(cohort, any_of(categorical_vars)) %>%
  pivot_longer(
    cols = -cohort,
    names_to = "variable",
    values_to = "category"
  ) %>%
  filter(!is.na(category)) %>%                 # important
  group_by(variable, category, cohort) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable, cohort) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(
    stat = sprintf("%d (%.1f%%)", n, percent),
    variable = paste0(variable, ": ", category)
  ) %>%
  select(variable, cohort, stat)

cont_summary_overall <- df_all %>%
  select(any_of(c("cohort", continuous_vars))) %>%
  pivot_longer(
    cols = -cohort,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cohort = "Overall",
    stat = ifelse(
      is.nan(mean),
      NA_character_,
      sprintf("%.1f (%.1f)", mean, sd)
    )
  ) %>%
  select(variable, cohort, stat)

cat_summary_overall <- df_all %>%
  select(any_of(c("cohort", categorical_vars))) %>%
  pivot_longer(
    cols = -cohort,
    names_to = "variable",
    values_to = "category"
  ) %>%
  filter(!is.na(category)) %>%
  group_by(variable, category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(
    cohort = "Overall",
    stat = sprintf("%d (%.1f%%)", n, percent),
    variable = paste0(variable, ": ", category)
  ) %>%
  select(variable, cohort, stat)


summary_long_all <- bind_rows(
  cont_summary,
  cont_summary_overall,
  cat_summary,
  cat_summary_overall
)

summary_table <- summary_long_all %>%
  pivot_wider(
    names_from  = cohort,
    values_from = stat,
    values_fill = NA
  )

library(writexl)

write_xlsx(
  summary_table,
  "baseline_characteristics_all_variables.xlsx"
)








