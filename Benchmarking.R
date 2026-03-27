# Time to event benchmark study with aging data
# Jaime Lynn Speiser
# 2/20/2026


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

dim(bm_data$chs)
dim(bm_data$habc)
dim(bm_data$mros)
dim(bm_data$sof)

summary(bm_data$chs$MALE)
summary(bm_data$habc$MALE)

#make srstat, race and education categorical 
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


#MAKE SURE mobility disability time is greater than 0
bm_data$chs1 <- bm_data$chs %>% filter(MOBDISFU > 0)
bm_data$habc1 <- bm_data$habc %>% filter(MOBDISFU > 0)
bm_data$mros1 <- bm_data$mros %>% filter(MOBDISFU > 0)
bm_data$sof1 <- bm_data$sof %>% filter(MOBDISFU > 0)


#convert categorical variables into numeric binary variables
# Remove the outcome columns first
features = bm_data$chs1[, !(names(bm_data$chs1) %in% c("MOBDISFU", "MOBDIS"))]
# One-hot encode
encoded = model.matrix(~ . - 1, data = features)
# Combine with outcome columns
bm_data$chs2 = cbind(encoded, bm_data$chs1[, c("MOBDISFU", "MOBDIS")])

# Remove the outcome columns first
features = bm_data$habc1[, !(names(bm_data$habc1) %in% c("MOBDISFU", "MOBDIS"))]
# One-hot encode
encoded = model.matrix(~ . - 1, data = features)
# Combine with outcome columns
bm_data$habc2 = cbind(encoded, bm_data$habc1[, c("MOBDISFU", "MOBDIS")])

# Remove the outcome columns first
features = bm_data$mros1[, !(names(bm_data$mros1) %in% c("MOBDISFU", "MOBDIS"))]
# One-hot encode
encoded = model.matrix(~ . - 1, data = features)
# Combine with outcome columns
bm_data$mros2 = cbind(encoded, bm_data$mros1[, c("MOBDISFU", "MOBDIS")])

# Remove the outcome columns first
features = bm_data$sof1[, !(names(bm_data$sof1) %in% c("MOBDISFU", "MOBDIS"))]
# One-hot encode
encoded = model.matrix(~ . - 1, data = features)
# Combine with outcome columns
bm_data$sof2 = cbind(encoded, bm_data$sof1[, c("MOBDISFU", "MOBDIS")])


#datasets with numeric type binary data for indicator variables
summary(bm_data$chs2)
summary(bm_data$habc2)
summary(bm_data$mros2)
summary(bm_data$sof2)

#delete variables for groups with <10% in a category for categorical variables
bm_data$chs2 <- bm_data$chs2[,-c(6,10,17,18,19)]
bm_data$habc2 <- bm_data$habc2[,-c(6,7,10,12)]
bm_data$mros2 <- bm_data$mros2[,-c(4,5,6,7,10,11,12,17,18)]
bm_data$sof2 <- bm_data$sof2[,-c(4,5,6,7,9,15,16)]


bm_data$chs3 = bm_data$chs2
bm_data$chs3$SRSTAT1= as.factor(bm_data$chs3$SRSTAT1)
bm_data$chs3$SRSTAT2= as.factor(bm_data$chs3$SRSTAT2)
bm_data$chs3$SRSTAT3= as.factor(bm_data$chs3$SRSTAT3)
bm_data$chs3$SRSTAT4= as.factor(bm_data$chs3$SRSTAT4)
bm_data$chs3$SMK= as.factor(bm_data$chs3$SMK)
bm_data$chs3$DRNK= as.factor(bm_data$chs3$DRNK)
bm_data$chs3$MHBP= as.factor(bm_data$chs3$MHBP)
bm_data$chs3$MHDIAB= as.factor(bm_data$chs3$MHDIAB)
bm_data$chs3$EDUC2= as.factor(bm_data$chs3$EDUC2)
bm_data$chs3$EDUC3= as.factor(bm_data$chs3$EDUC3)
bm_data$chs3$MALE= as.factor(bm_data$chs3$MALE)

bm_data$habc3 = bm_data$habc2
bm_data$habc3$SRSTAT1= as.factor(bm_data$habc3$SRSTAT1)
bm_data$habc3$SRSTAT2= as.factor(bm_data$habc3$SRSTAT2)
bm_data$habc3$SRSTAT3= as.factor(bm_data$habc3$SRSTAT3)
bm_data$habc3$SRSTAT4= as.factor(bm_data$habc3$SRSTAT4)
bm_data$habc3$DRNK = as.factor(bm_data$habc3$DRNK)
bm_data$habc3$MHBP = as.factor(bm_data$habc3$MHBP)
bm_data$habc3$MHDIAB = as.factor(bm_data$habc3$MHDIAB)
bm_data$habc3$RACE2= as.factor(bm_data$habc3$RACE2)
bm_data$habc3$EDUC2= as.factor(bm_data$habc3$EDUC2)
bm_data$habc3$EDUC3= as.factor(bm_data$habc3$EDUC3)
bm_data$habc3$MALE = as.factor(bm_data$habc3$MALE)

bm_data$mros3 = bm_data$mros2
bm_data$mros3$SRSTAT1= as.factor(bm_data$mros3$SRSTAT1)
bm_data$mros3$SRSTAT3= as.factor(bm_data$mros3$SRSTAT3)
bm_data$mros3$DRNK = as.factor(bm_data$mros3$DRNK)
bm_data$mros3$MHBP = as.factor(bm_data$mros3$MHBP)
bm_data$mros3$EDUC2= as.factor(bm_data$mros3$EDUC2)
bm_data$mros3$EDUC3= as.factor(bm_data$mros3$EDUC3)

bm_data$sof3 = bm_data$sof2
bm_data$sof3$SRSTAT1= as.factor(bm_data$sof3$SRSTAT1)
bm_data$sof3$SRSTAT3= as.factor(bm_data$sof3$SRSTAT3)
bm_data$sof3$DRNK = as.factor(bm_data$sof3$DRNK)
bm_data$sof3$MHOSTEO = as.factor(bm_data$sof3$MHOSTEO)
bm_data$sof3$MHBP= as.factor(bm_data$sof3$MHBP)
bm_data$sof3$EDUC2= as.factor(bm_data$sof3$EDUC2)
bm_data$sof3$EDUC3= as.factor(bm_data$sof3$EDUC3)

#datasets with all binary variables as factors, for mboost method
summary(bm_data$chs3)
summary(bm_data$habc3)
summary(bm_data$mros3)
summary(bm_data$sof3)

#plot followup time by mobility outcome
boxplot(bm_data$chs$MOBDISFU~bm_data$chs$MOBDIS)
boxplot(bm_data$habc$MOBDISFU~bm_data$habc$MOBDIS)
boxplot(bm_data$mros$MOBDISFU~bm_data$mros$MOBDIS)
boxplot(bm_data$sof$MOBDISFU~bm_data$sof$MOBDIS)

#summarize datasets
summary(bm_data$chs2)
summary(bm_data$habc2)
summary(bm_data$mros2)
summary(bm_data$sof2)



###SET UP BENCHMARK############################################################################################




# Create survival tasks
task2 = TaskSurv$new(id = "CHS", backend = bm_data$chs2, time = "MOBDISFU", event = "MOBDIS")
task3 = TaskSurv$new(id = "HABC", backend = bm_data$habc2, time = "MOBDISFU", event = "MOBDIS")
task4 = TaskSurv$new(id = "MROS", backend = bm_data$mros2, time = "MOBDISFU", event = "MOBDIS")
task5 = TaskSurv$new(id = "SOF", backend = bm_data$sof2, time = "MOBDISFU", event = "MOBDIS")

task2a = TaskSurv$new(id = "CHS", backend = bm_data$chs3, time = "MOBDISFU", event = "MOBDIS")
task3a = TaskSurv$new(id = "HABC", backend = bm_data$habc3, time = "MOBDISFU", event = "MOBDIS")
task4a = TaskSurv$new(id = "MROS", backend = bm_data$mros3, time = "MOBDISFU", event = "MOBDIS")
task5a = TaskSurv$new(id = "SOF", backend = bm_data$sof3, time = "MOBDISFU", event = "MOBDIS")

tasks <- list(task2, task3, task4, task5)
tasksa <- list(task2a, task3a, task4a, task5a)


# Define survival learners
learners = list(
  lrn("surv.kaplan", id = "KM"),
  lrn("surv.xgboost.cox", id = "XGBoost"),  
  lrn("surv.coxboost", id = "CoxBoost"),
  lrn("surv.aorsf", id = "Aorsf"),
  lrn("surv.cforest", id = "Cforest"),
  lrn("surv.ctree", id = "Ctree"),
  lrn("surv.glmboost", id = "GlmBoost", center=FALSE),
  lrn("surv.glmnet", id = "LASSO", alpha=1, lambda=0.02, s=0.02),
  lrn("surv.glmnet", id = "Ridge", alpha=0, lambda=0.1, s=0.1),
  lrn("surv.glmnet", id = "ElasticNet", alpha=0.5, lambda=0.02, s=0.02)
)

## 2) Base learners (plain learners, no fallback here)
lrn_mboost <- lrn("surv.mboost",
                  id = "MBoost",
                  family = "coxph",
                  baselearner = "bols",
                  mstop = 300,
                  nu = 0.05)

lrn_gamboost <- lrn("surv.gamboost",
                    id = "GAMBoost",
                    family = "coxph",
                    mstop = 300,
                    nu = 0.05,
			  baselearner="bbs", dfbase=3)

## 4) Use these in your factor-based list
learnersa <- list(lrn_mboost, lrn_gamboost)

#define RF learners separately
learners_rf = list(
   lrn( "surv.rfsrc", id = "RSF",nodesize = 100,na.action = "na.impute",importance = "none", predict_type="distr", ntime=100),          
   lrn("surv.ranger", id = "RangerRF",min.node.size=100, importance="none",save.memory=TRUE,num.threads=1, predict_type="distr")
)

# Define performance measures
ts <- seq(100, 5000, length.out = 50)
measures = list(
  msr("surv.cindex", id = "cindex"),
  msr("surv.brier", id = "brier", times = ts),
  msr("surv.intlogloss", id ="intlogloss", times = ts),
  msr("surv.calib_index", id = "calib_index"),
  msr("time_train", id = "time_train")
)

#define benchmark function
run_benchmark<-function(seed,file,tasks,tasksa,learners,learnersa,learners_rf){

  set.seed(seed)

  # Define resampling strategy
  resampling = rsmp("repeated_cv", folds = 4, repeats = 10)

  # Create benchmark grid for methods that need all numeric (even if 0/1)
  grid <- benchmark_grid(
    tasks       = tasks,              
    learners    = learners,
    resamplings = resampling
  )

  # Create benchmark grid for methods that need factors specified
  grida <- benchmark_grid(
    tasks       = tasksa,
    learners    = learnersa,
    resamplings = resampling
  )

  # Create benchmark grid for RF methods
  gridb <- benchmark_grid(
    tasks       = tasks,              
    learners    = learners_rf,
    resamplings = resampling 
  )

  bmr = benchmark(grid, encapsulate = "callr")
  bmra = benchmark(grida, encapsulate = "callr")
  bmrb = benchmark(gridb)
  allbmr = c(bmr,bmra,bmrb)
  agg1 = as_benchmark_aggr(allbmr, measures=measures)

  saveRDS(agg1, file)

  grid<-grida<-gridb<-bmr<-bmra<-bmrb<-allbmr<-agg1<-NA

}

###RUN BENCHMARK############################################################################################


#set one core for each model
set_one_core <- function(lrn) {
  ps <- lrn$param_set$ids()
  # Common thread parameter names across packages:
  if ("nthread"      %in% ps) lrn$param_set$values$nthread       <- 1L
  if ("nthreads"     %in% ps) lrn$param_set$values$nthreads      <- 1L
  if ("n_threads"    %in% ps) lrn$param_set$values$n_threads     <- 1L
  if ("n_thread"     %in% ps) lrn$param_set$values$n_thread      <- 1L
  if ("threads"      %in% ps) lrn$param_set$values$threads       <- 1L
  if ("num.threads"  %in% ps) lrn$param_set$values$`num.threads` <- 1L
  if ("num_threads"  %in% ps) lrn$param_set$values$num_threads   <- 1L
  lrn
}

learners <- lapply(learners, set_one_core)
learnersa <- lapply(learnersa, set_one_core)

#set up cluster on local computer

# Logging: escalate to see issues coming from workers
lgr::get_logger("mlr3")$set_threshold("info")

# Limit thread use inside learners / BLAS
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {

RhpcBLASctl::blas_set_num_threads(1)
  RhpcBLASctl::omp_set_num_threads(1)
}
Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1")

# Conservative worker count
workers <- max(1L, parallelly::availableCores(logical = FALSE) - 1L)
plan(multisession, workers = workers)
on.exit(plan(sequential), add = TRUE)

# Future stability options
options(
  future.wait.timeout   = 30*60,                 # increase if models are slow
  future.rng.onMisuse   = "ignore",
  future.globals.maxSize = 8 * 1024^3            # tune for your machine
)
  


# Run benchmark and analysis for grids, run separately by increments of 10

run_benchmark(20, "agg1_results n10 k4 seed20.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(30, "agg1_results n10 k4 seed30.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(40, "agg1_results n10 k4 seed40.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(50, "agg1_results n10 k4 seed50.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(60, "agg1_results n10 k4 seed60.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(70, "agg1_results n10 k4 seed70.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(80, "agg1_results n10 k4 seed80.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(90, "agg1_results n10 k4 seed90.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(100, "agg1_results n10 k4 seed100.rds",tasks,tasksa,learners,learnersa,learners_rf)
run_benchmark(110, "agg1_results n10 k4 seed110.rds",tasks,tasksa,learners,learnersa,learners_rf)



