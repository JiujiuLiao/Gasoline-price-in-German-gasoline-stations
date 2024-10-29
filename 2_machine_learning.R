# in case regr.ksvm is not found execute following comments
# options(repos = c(
#  mlrorg = "https://mlr-org.r-universe.dev",
#  CRAN = "https://cloud.r-project.org/"
# ))

# install.packages("mlr3extralearners")
# install.packages("mboost")
# install.packages("nett")
# install.packages("DiceKriging")
# install.packages("rgenoud")
# install.packages("iml")
# install.packages("Matrix")

# mlr_learners$get("regr.glm")
# mlr_learners$get("regr.nnet")
# mlr_learners$get("regr.glmboost")

#set work directory
setwd("D:/TU/DataProjectGasStations/testfolder")
set.seed(2024)

library(dplyr)
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3verse)
library(mlr3tuning)
library(mlr3extralearners)
library(mlr3hyperband)
library(mlr3viz)
library(ranger)
library(rpart)
library(kknn)
library(kernlab)
library(paradox)
library(mlr3misc)
library(purrr)  
library(iml)
library(glmnet)
library(tidyr)

df <- read.csv("aggregated_data_Saxony_13-15_big_brands.csv",row.names = 1)
sapply(lapply(df, unique), length)

# make up a new subset for ML task
df.subset.e5 = df[,-which(colnames(df) %in% c("diesel_price","e10_price"))]
str(df.subset.e5)
rm(df)
colnames(df.subset.e5)

#set character variables into factor so that regression tree can work
df.subset.e5[, c("brand", "settlement","day","hour", "krs_name_s")] <- lapply(
  df.subset.e5[, c("brand", "settlement","day","hour", "krs_name_s")], as.factor)

#Partition data 
train_data <- df.subset.e5 %>% filter(day %in% c("Tue","Wed"))
train_data <- train_data[,-which(colnames(train_data) %in% c("day"))]
test_data <- df.subset.e5 %>% filter(day %in% c("Thu"))
test_data <- test_data[,-which(colnames(test_data) %in% c("day"))]

#count unique values for each variable
sapply(lapply(train_data, unique), length)


###### Machine learning - first benchmark with default learners ################

# loading the task
tsk_e5.price = as_task_regr(train_data, target = "e5_price", id= "id")
print(tsk_e5.price)

# Define learners and resampling strategy
learners <- list(
  lrn("regr.rpart"),
  lrn("regr.kknn", k = 10L),
  lrn("regr.ranger"),
  lrn("regr.glm"),
  lrn("regr.nnet"),
  lrn("regr.ksvm"),
  lrn("regr.featureless")
)

resampling <- rsmp("cv", folds = 5)
resampling$instantiate(tsk_e5.price)

# Benchmark learners
design <- benchmark_grid(
  tasks = tsk_e5.price,
  learners = learners,
  resamplings = resampling
)
bmr <- benchmark(design,store_models = TRUE) 

# Aggregate the benchmark results by RMSE instead of MSE
bmr_rmse <- bmr$aggregate(msr("regr.rmse"))
print(bmr_rmse)

#Visualize the Comparison of Benchmark (Boxplot)
plot_benchmark <- autoplot(bmr, measure = msr("regr.rmse")) +
  theme_minimal() +
  ylab("RMSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_blank())

print(plot_benchmark)

# Save the plot
# ggsave(filename = "benchmark_results_test.df.png", plot = plot_benchmark, width = 18, units = "cm")


###### Hyperparameter Optimization for decision tree, random forest and kknn ###

# search space by default setting for the decision tree
lts_rpart = lts("regr.rpart.default")

# search space kknn
kknn_ps <- ps(k = p_int(lower = 1, upper = 300))

# search space random forest
ranger_ps <- ps(mtry.ratio = p_dbl(lower = 0, upper = 1),
                sample.fraction = p_dbl(lower = 0.1, upper = 1))

# Define tuning instances
tuner <- mlr3tuning::tnr("mbo")

# setting up the autotuners 
at_rpart <- AutoTuner$new(
  learner = lrn("regr.rpart"),
  resampling = rsmp("cv", folds = 3),
  measure = msr("regr.rmse"),
  search_space = lts_rpart,
  terminator = trm("run_time", secs = 20),
  tuner = tuner
)

at_kknn <- AutoTuner$new(
  learner = lrn("regr.kknn"),
  resampling = rsmp("cv", folds = 3),
  measure = msr("regr.rmse"),
  search_space = kknn_ps,
  terminator = trm("run_time", secs = 60),
  tuner = tuner
)

at_ranger <- AutoTuner$new(
  learner = lrn("regr.ranger", num.trees = 1000),
  resampling = rsmp("cv", folds = 3),
  measure = msr("regr.rmse"),
  search_space = ranger_ps,
  terminator = trm("run_time", secs = 60),
  tuner = tuner
)

# Setting up second benchmark
learners_total <- list(lrn("regr.rpart"),
                       at_rpart, 
                       lrn("regr.kknn", k = 10L),
                       at_kknn, 
                       lrn("regr.ranger"),
                       at_ranger,
                       lrn("regr.featureless")
)

# Define outer resampling
outer_resampling <- rsmp("cv", folds = 5)

# Benchmark grid
design_all.learners <- benchmark_grid(
  tasks = tsk_e5.price,
  learners = learners_total,
  resamplings = outer_resampling
)

# Run the second benchmark
bmr_all.lrs <- benchmark(design_all.learners, store_models = TRUE)

bmr_rmse_all.lrs <- bmr_all.lrs$aggregate(msr("regr.rmse"))

# Visual comparison of the benchmark results
plot_benchmark_all.lrs <- autoplot(bmr_all.lrs, measure = msr("regr.rmse")) +
  theme_minimal() +
  ylab("RMSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_blank())

print(plot_benchmark_all.lrs)

# Save the plot
# ggsave(filename = "benchmark_results_all.lrs.png", plot = plot_benchmark_all.lrs, width = 18, units = "cm")

# Checking if our learners overfit and how good they are at predicting prices 
# on Thursday, which they haven't seen before

tuning_results = as.data.table(bmr_all.lrs)
tuning_results$learner
# Scores from training
score_list <- as.data.table(bmr_all.lrs$score(measures = msr("regr.rmse"))) 
score_list <- score_list[,c(6,9,11)]


# Adding the scores for the predicitions on the test data
for (i in 1:35){
  score_list$test_regr.rmse[i] <- tuning_results$learner[[i]]$predict_newdata(newdata = test_data, task = tsk_e5.price)$score(measures = msr("regr.rmse"))
}

score_list
score_list <- pivot_longer(data = score_list, cols = c("regr.rmse", "test_regr.rmse"))
score_list$name <- as.factor(score_list$name)
score_list$learner_id <- as.factor(score_list$learner_id)

# comparing them visually
colnames(score_list)
unique(score_list$learner_id)

levels(score_list$name) <- c("training score", "testing score")
levels(score_list$learner_id) <- c("featureless", "kknn", "tuned kknn", "ranger", "tuned ranger", "rpart", "tuned rpart")

test_training <- ggplot(data = score_list, aes(x= name, y=value)) + 
  geom_boxplot(aes(colour = name)) + 
  facet_grid(. ~ learner_id) +
  labs(title="",x="", y = "RMSE") +
  theme_minimal() + 
  theme(legend.position="bottom", axis.text.x = element_blank(), 
        legend.title = element_blank())

# ggsave(filename = "test_train_score.png", plot = test_training, device = "png", width = 18, units = "cm" )


###### Investigating the best tuned learner(regr.ranger - learner no. 28) ######

# Define the model
model_at_ranger <- tuning_results$learner[[28]]

# comparing tuned to the default random forest
learners_ranger.at_ranger <- list(lrn("regr.ranger"), model_at_ranger)

predictions <- map_df(learners_ranger.at_ranger, function(learner) {
  learner$train(tsk_e5.price)
  prediction <- learner$predict_newdata(test_data)
  data.frame(
    actual = test_data$e5_price,
    predicted = prediction$response,
    learner = learner$id
  )
})

# Plot predictions vs actual values
plot_prediction.actual <- ggplot(predictions, aes(x = actual, y = predicted, color = learner)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predictions vs Actual Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()
print(plot_prediction.actual)

# ggsave(filename = "Prediction vs Actual values.png", plot = plot_prediction.actual, width = 8, height = 6)

# Plot residuals
predictions_residual <- predictions %>%    #add residual column to prediction data frame
  mutate(residual = actual - predicted)

plot_residual.actual <- ggplot(predictions_residual, aes(x = actual, y = residual, color = learner)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Actual Values",
       x = "Actual Values",
       y = "Residuals") +
  theme_minimal()
print(plot_residual.actual)
# ggsave(filename = "Residual vs Actual values.png", plot = plot_residual.actual, width = 8, height = 6)

# Plot density of residuals
plot_Den.residual <- ggplot(predictions_residual, aes(x = residual, fill = learner)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Residuals",
       x = "Residuals",
       y = "Density") +
  theme_minimal()
print(plot_Den.residual)
# ggsave(filename = "Density of residual.png", plot = plot_Den.residual, width = 8, height = 6)


###### Interpretable Machine Learning on regr.ranger - learner no. 28 ##########

# Create a Predictor object
predictor <- Predictor$new(
  model_at_ranger,
  data = tsk_e5.price$data(),
  y = tsk_e5.price$target_names
)

# Feature importance
importance <- FeatureImp$new(predictor, loss = "rmse")
plot_Imp <- importance$plot() + theme_minimal()
# ggsave(filename = "feature_importance.png", plot = plot_Imp, width = 18, units = "cm")

# highway distance pdp plot
pdp_d_road <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_road",
  method = "pdp"
); 
plot_d_road <- pdp_d_road$plot()
plot(plot_d_road)

plot_d_road + xlab("Distance to the closest highway") + ylab("Super E5 price") + theme_minimal()
# ggsave(filename = "pdp_d_road.png", plot = plot_d_road, width = 8, height = 6)

# brand ale plot
ale_brand <- FeatureEffect$new(
  predictor = predictor,
  feature = "brand",
  method = "ale"
); ale_brand$plot()
plot_brand <- ale_brand$plot()
plot(plot_brand)
# ggsave(filename = "brand.png", plot = plot_brand, width = 8, height = 6)

# distance to next gas station pdp plot
pdp_d_1 <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_1",
  method = "pdp"
); 
plot_d1 <- pdp_d_1$plot() + theme_minimal()
plot(plot_d1)
# ggsave(filename = "pdp_d1.png", plot = plot_d1, width = 8, height = 6)

# frequency of price changes ale plot
ale_freq <- FeatureEffect$new(
predictor = predictor,
 feature = "freq",
 method = "ale"
);
plot_ale.freq <- ale_freq$plot()
plot(plot_ale.freq)

# distance to next city ale plot
ale_d_city <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_city",
  method = "ale"
); 
plot_ale_d_city <- ale_d_city$plot() + theme_minimal()
plot(plot_ale_d_city)
# ggsave(filename = "plot_ale_d_city.png", plot = plot_ale_d_city, width = 8, height = 6)

# distance to closest gas station ale plot
ale_d_1 <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_1",
  method = "ale"
); plot_ale_d_1 <- ale_d_1$plot() + theme_minimal()
# ggsave(filename = "ale_d_1.png", plot = plot_ale_d_1, width = 18, units = "cm")

# hour of the day ale plot
ale_hour <- FeatureEffect$new(
  predictor = predictor,
  feature = "hour",
  method = "ale"
); ale_hour$plot() + theme_minimal()

hour_plot <- cbind(as.numeric(substr(ale_hour$results$hour,1,2)), ale_hour$results$.value)
hour_plot <- hour_plot[c(9,5,6,7,8,2,3,4,1,11,12,13,14,15,16,17,18,20,19,21,22,23,24,10),]
hour_plot <- as.data.frame(hour_plot)
colnames(hour_plot) <- c("hour", "prediction")

ale_hour <- ggplot(data = hour_plot, aes(x = hour, y = prediction)) +
  geom_col() +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23)
# ggsave(filename = "ale_hour.png", plot = ale_hour, width = 18, units = "cm")


# distance to the highway ale plot
ale_road <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_road",
  method = "ale"
); ale_road$plot() + theme_minimal()

# rural / urban ale plot
ale_region <- FeatureEffect$new(
  predictor = predictor,
  feature = "settlement",
  method = "ale"
); ale_region$plot()

# population density ale plot
ale_popul_dens <- FeatureEffect$new(
  predictor = predictor,
  feature = "popul_dens",
  method = "ale"
); ale_popul_dens$plot() + theme_minimal()

# Kreis name ale plot
ale_krs_name_s <- FeatureEffect$new(
  predictor = predictor,
  feature = "krs_name_s",
  method = "ale"
); ale_krs_plot <- ale_krs_name_s$plot() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(filename = "ale_krs.png", plot = ale_krs_plot, width = 18, units = "cm")

#plot all ALE at once
effs <- FeatureEffects$new(predictor, grid.size = 10)
plot(effs)

# frequency of price changes ice plots
ice_freq <- FeatureEffect$new(
  predictor = predictor,
  feature = "freq",
  method = "pdp+ice"
); 
plot_ice.freq <- ice_freq$plot() + theme_minimal()
plot(plot_ice.freq)
# ggsave(filename = "plot_ice.freq.png", plot = plot_ice.freq, width = 18, units = "cm")

# distance to closest gas station ice plot
ice_d_1 <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_1",
  method = "pdp+ice"
); ice_d_1$plot() + theme_minimal()

# hour of the day ice plot
ice_hour <- FeatureEffect$new(
  predictor = predictor,
  feature = "hour",
  method = "pdp+ice"
); ice_hour$plot()

# distance to the closest highway ice plot
ice_d_road <- FeatureEffect$new(
  predictor = predictor,
  feature = "d_road",
  method = "pdp+ice"
); ice_d_road$plot()

# brand ice plot
ice_brand <- FeatureEffect$new(
  predictor = predictor,
  feature = "brand",
  method = "pdp+ice"
); ice_brand$plot()

# rural / urban ice plot
ice_settlement <- FeatureEffect$new(
  predictor = predictor,
  feature = "settlement",
  method = "pdp+ice"
); ice_settlement$plot()

# shapley plot
shapley <- Shapley$new(
  predictor = predictor,
  x.interest = as.data.frame(tsk_e5.price$data(1))
); shapley$plot()
