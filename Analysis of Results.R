# Time to event benchmark study with aging data: analysis of results
# Jaime Lynn Speiser
# 3/17/2026


# Load the packages
library(haven)
library(dplyr)
library(tidyr)
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
library(purrr)
library(dplyr)
library(ggplot2)
library(tune)
library(writexl)


# read in saved results (batched in 10)
agg1_loaded20 <- readRDS("agg1_results n10 k4 seed20.rds")
agg1_loaded30 <- readRDS("agg1_results n10 k4 seed30.rds")
agg1_loaded40 <- readRDS("agg1_results n10 k4 seed40.rds")
agg1_loaded50 <- readRDS("agg1_results n10 k4 seed50.rds")
agg1_loaded60 <- readRDS("agg1_results n10 k4 seed60.rds")
agg1_loaded70 <- readRDS("agg1_results n10 k4 seed70.rds")
agg1_loaded80 <- readRDS("agg1_results n10 k4 seed80.rds")
agg1_loaded90 <- readRDS("agg1_results n10 k4 seed90.rds")
agg1_loaded100 <- readRDS("agg1_results n10 k4 seed100.rds")
agg1_loaded110 <- readRDS("agg1_results n10 k4 seed110.rds")

names(agg1_loaded20)

#make them data frames
results20<-as.data.frame(agg1_loaded20$data)
results30<-as.data.frame(agg1_loaded30$data)
results40<-as.data.frame(agg1_loaded40$data)
results50<-as.data.frame(agg1_loaded50$data)
results60<-as.data.frame(agg1_loaded60$data)
results70<-as.data.frame(agg1_loaded70$data)
results80<-as.data.frame(agg1_loaded80$data)
results90<-as.data.frame(agg1_loaded90$data)
results100<-as.data.frame(agg1_loaded100$data)
results110<-as.data.frame(agg1_loaded110$data)

#aggregate the data frames
agg_data<-rbind(results20,results30,results40,results50,results60,results70,results80,results90,results100,results110)
summary(agg_data)


#analysis for all datasets pooled together
# Convert to long format for ggplot
agg_long <- agg_data %>%
  pivot_longer(
    cols = c(cindex, brier, intlogloss, calib_index, time_train),
    names_to = "measure",
    values_to = "score"
  )


measure_labels <- c(
  cindex = "C-Index",
  brier = "Brier Score",
  calib_index = "Calibration Index",
  intlogloss = "Integrated Log Loss",
  time_train = "Training Time"
)


fig1 <- ggplot(agg_long, aes(x = learner_id, y = score)) +
  geom_boxplot() +
  facet_wrap(~ measure, scales = "free_y",labeller = as_labeller(measure_labels)) +
  labs(
    x = "Learner",
    y = "Score",
    title = "Model Performance Across All Tasks"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "model_performance_boxplots.png",
  plot     = fig1,
  width    = 10,
  height   = 7,
  dpi      = 300
)


#summarize in a table with 95% confidence intervals
summary_table <- agg_long %>%
  group_by(learner_id, measure) %>%
  summarise(
    n        = n(),
    mean     = mean(score, na.rm = TRUE),
    sd       = sd(score, na.rm = TRUE),
    se       = sd / sqrt(n),
    ci_low  = mean - 1.96 * se,
    ci_high = mean + 1.96 * se,
    .groups = "drop"
  )

summary_table_fmt <- summary_table %>%
  mutate(
    estimate_ci = sprintf("%.3f (%.3f, %.3f)", mean, ci_low, ci_high)
  ) %>%
  select(learner_id, measure, estimate_ci)


summary_table_wide <- summary_table_fmt %>%
  pivot_wider(
    names_from  = measure,
    values_from = estimate_ci
  )
write_xlsx(summary_table_wide,  path = "learner_performance_summary.xlsx")



###analysis for each dataset separately

#plots
plots_by_task <- agg_long %>%
  split(.$task_id) %>%
  imap(~ {
    p <- ggplot(.x, aes(x = learner_id, y = score)) +
      geom_boxplot() +
      facet_wrap(~ measure, scales = "free_y",labeller = as_labeller(measure_labels)) +
      labs(
        x = "Learner",
        y = "Score",
        title = paste("Model Performance:", .y)
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(
      filename = paste0("model_performance_", .y, ".png"),
      plot     = p,
      width    = 10,
      height   = 7,
      dpi      = 300
    )
    p
  })


#summary table
summary_table_task <- agg_long %>%
  group_by(task_id, learner_id, measure) %>%
  summarise(
    n        = n(),
    mean     = mean(score, na.rm = TRUE),
    sd       = sd(score, na.rm = TRUE),
    se       = sd / sqrt(n),
    ci_low   = mean - 1.96 * se,
    ci_high  = mean + 1.96 * se,
    .groups  = "drop"
  )

summary_table_task_fmt <- summary_table_task %>%
  mutate(
    estimate_ci = sprintf("%.3f (%.3f, %.3f)", mean, ci_low, ci_high)
  ) %>%
  select(task_id, learner_id, measure, estimate_ci)

summary_table_task_wide <- summary_table_task_fmt %>%
  pivot_wider(
    names_from  = measure,
    values_from = estimate_ci
  )

sheets <- summary_table_task_wide %>%
  split(.$task_id)

write_xlsx(
  sheets,
  path = "learner_performance_summary_by_dataset.xlsx"
)




