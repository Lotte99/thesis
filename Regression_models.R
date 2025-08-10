# Disclaimer: ChatGPTo4-mini-high helped me debug and improve the script. 
# It also helped structuring it and making it visibly more pleasing.

# This R script performs the following steps:
# 1. Selecting variables and demeaning of data for hypothesis 1 (henceforth H1)
# 2. Assumptions testing
# 3. Model training and fitting (panel, LASSO, horseshoe, random forest) for H1
# 4. Comparing model performance
# 5. Fitting best performing model on all three response variables for H1
# 6. Selecting variables and demeaning of data for hypothesis 2 (henceforth H2)
# 7. Testing significance for step 1-3 of mediation analysis for H2
# 8. Bootstrapping to measure indirect effect for H2

# Note:
# - This script is based on the previous scripts, particularly Preprocessing.R

##### 0. Setup: Load Required Libraries ########################################

library(bayesplot)
library(brms)        
library(broom)      
library(car)
library(caret)
library(dplyr)
library(doParallel)
library(future)
library(glmnet)
library(gridExtra)
library(lmtest)
library(lubridate)
library(MASS)
library(mediation)
library(parallel)
library(plm)
library(purrr)
library(psych)
library(ranger)
library(readr)
library(readxl)
library(sandwich)
library(tidyr)


data <- read_csv("data_standardized.csv")

##### H1. Variable Preparation #################################################

# Excluding second lag CS talk variables and identifiers
topic_lag1_vars  <- paste0("Topic", c(1:19)) # Base created in other script 
h1_control_vars  <- c("TQ_lag1", "TQ_lag1_sq", "slack", 
                      "ci", "sr", "roa", "size", "TRESGCCS")
h1_predictors    <- c(topic_lag1_vars, h1_control_vars)
protected_topics <- paste0("Topic", c(2,3,4,5,7,8,13,15,16,17,18,19))  
y_agg            <- "aggregate_CSwalk"
h1_response    <- c("aggregate_CSwalk", "symbolic_CSwalk", "substantive_CSwalk")

# 5-fold splits by ticker for firm-fixed effects
K        <- 5
tickers  <- unique(data$ticker)
folds    <- createFolds(tickers, K)
ticker2fold <- setNames(rep(NA, length(tickers)), tickers)
for (k in seq_along(folds)) {
  ticker2fold[tickers[folds[[k]]]] <- k
}

# Holdout split (20% test set, 80% train set)
set.seed(1942)
test_idx_all <- createDataPartition(data[[y_agg]], p = 0.2, list = FALSE)
data_test  <- data[test_idx_all, ]
data_train <- data[-test_idx_all, ]

# Compute within‐ticker means on training set
train_means <- data_train %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarize(across(all_of(c(y_agg, h1_predictors)), mean), .groups="drop")

# Demean train
data_train_dm <- data_train %>%
  dplyr::left_join(train_means, by="ticker", suffix=c("","_mean")) %>%
  dplyr::mutate(across(all_of(c(y_agg, h1_predictors)),
                       ~ . - get(paste0(cur_column(), "_mean")))) %>%
  dplyr::select(-ends_with("_mean"))

# Demean test
data_test_dm <- data_test %>%
  dplyr::left_join(train_means, by="ticker", suffix=c("", "_mean")) %>%
  dplyr::mutate(across(ends_with("_mean"), ~ tidyr::replace_na(., 0))) %>%
  dplyr::mutate(across(all_of(h1_predictors), 
                ~ . - get(paste0(cur_column(), "_mean"))),
         !!y_agg := .data[[y_agg]] - get(paste0(y_agg, "_mean")))

# Keeping identifiers in place
data_train_dm <- data_train_dm %>%
  dplyr::mutate(
    ticker = data_train$ticker,
    year   = data_train$year
  )

data_test_dm <- data_test_dm %>%
  dplyr::mutate(
    ticker = data_test$ticker,
    year   = data_test$year
  )

# Setting aside y test (not demeaned)
y_test_raw <- data_test[[y_agg]]

##### H1. Linear regression assumptions testing ################################

# Aggregate CS walk
model_h1_agg <- lm(
  formula = reformulate(h1_predictors, response = "aggregate_CSwalk"),
  data    = data_train
)
# Symbolic CS walk
model_h1_sym <- lm(
  formula = reformulate(h1_predictors, response = "symbolic_CSwalk"), 
  data    = data_train
)
# Substantive CS walk
model_h1_sub <- lm(
  formula = reformulate(h1_predictors, response = "substantive_CSwalk"),
  data    = data_train
)

# Results 
for (m in list(model_h1_agg, model_h1_sym, model_h1_sub)) {
  print(summary(m))
  
  vif <- vif(m)
  print(vif)
}

#--------------------
# Assumptions testing
#--------------------

par(mfrow = c(2, 2))  

for (m in list(model_h1_agg, model_h1_sym, model_h1_sub)) { 
  print(summary(m))
  
  # Multicollinearity
  print(vif(m))
  
  # Diagnostic plots: Residuals, QQ, Scale-Location, Leverage
  plot(m, which = 1:4, main = deparse(formula(m)))  
  
  # Normality test of residuals
  print(shapiro.test(residuals(m)))  # My sample size likley too large for this 
  
  # Breusch-Pagan test for heteroskedasticity
  print(bptest(m))
  
  # Durbin-Watson test for autocorrelation
  print(dwtest(m))
} 


##### H1. Panel Linear Regression ##############################################

panel_preds <- numeric(nrow(data_test))

# 5-fold CV on training data only
set.seed(1942)
folds_obs <- createFolds(data_train[[y_agg]], k = K)
n_train <- nrow(data_train_dm)

for (k in seq_along(folds_obs)) {
  train_idx <- setdiff(seq_len(n_train), folds_obs[[k]])
  val_idx   <- folds_obs[[k]]
  
  train_dat <- pdata.frame(data_train[train_idx, ], index = c("ticker","year"))
  val_dat   <- data_train[val_idx, ]
  
  panel_mod <- plm(
    reformulate(h1_predictors, response = y_agg),
    data  = train_dat,
    model = "within"
  )
}

# Fit final model on full training data
panel_mod_final <- plm(
  reformulate(h1_predictors, response = y_agg),
  data  = pdata.frame(data_train, index = c("ticker","year")),
  model = "within"
)

# Predict on test set
Xte      <- as.matrix(data_test[, h1_predictors])
beta_hat <- coef(panel_mod_final)
Xte      <- Xte[, names(beta_hat), drop = FALSE]

fe       <- fixef(panel_mod_final)
firm_id  <- as.character(data_test$ticker)
firm_eff <- fe[firm_id]
firm_eff[is.na(firm_eff)] <- 0

panel_preds <- as.numeric(Xte %*% beta_hat + firm_eff)

panel_perf <- tibble(
  Method = "FE",
  RMSE   = sqrt(mean((panel_preds - y_test_raw)^2)),
  MAE    = mean(abs(panel_preds - y_test_raw)),
  MSE    = mean((panel_preds - y_test_raw)^2)
)

saveRDS(panel_mod_final, "panel_model_full.rds")
saveRDS(panel_perf,       "panel_perf.rds")

##### H1. LASSO Regularization ################################################# 

lambda_grid <- 10^seq(3, -4, length.out = 100) 

X_train <- model.matrix(~ . -1, data=as.data.frame(data_train_dm[h1_predictors]))
y_train <- data_train_dm[[y_agg]]

X_test  <- model.matrix(~ . -1, data=as.data.frame(data_test_dm[h1_predictors]))
y_test  <- data_test_dm[[y_agg]]

# Penalty vector for protected_topics”
penalty_vec <- ifelse(
  colnames(X_train) %in% protected_topics, 0,
  ifelse(grepl("^Topic", colnames(X_train)), 2, 1)
)

# Fitting LASSO
set.seed(1942)
cvm_lasso <- glmnet::cv.glmnet(
  x              = X_train,
  y              = y_train,
  alpha          = 1,
  lambda         = lambda_grid,
  nfolds         = 5,
  standardize    = TRUE,
  penalty.factor = penalty_vec
)

# Predicting on test set 
X_test <- X_test[, colnames(X_train), drop = FALSE]
lasso_preds_dm <- as.numeric(
  predict(cvm_lasso, newx = X_test, s = "lambda.min")
)

# Re-adding each firm’s training-set mean outcome to get back to the raw scale
# If not, you're not comparing "apples-with-apples" with panel FE.
lasso_preds_raw <- lasso_preds_dm +
  data_test_dm[[ paste0(y_agg, "_mean") ]]

lasso_perf <- tibble::tibble(
  Method = "LASSO_FE",
  RMSE   = sqrt(mean((lasso_preds_raw - y_test_raw)^2)),
  MAE    = mean(abs(lasso_preds_raw - y_test_raw)),
  MSE    = mean((lasso_preds_raw - y_test_raw)^2)
)


# Top‐10 nonzero coefficients
coef_lasso  <- coef(cvm_lasso, s = "lambda.min")
top10_lasso <- names(
  sort(abs(coef_lasso[-1,1]), decreasing = TRUE))[1:10]

cvm_lasso$lambda.min     # [1] 0.00097701
cvm_lasso$lambda.1se     # [1] 0.02535364

saveRDS(cvm_lasso,  "lasso_cvm_fe.rds")
saveRDS(lasso_perf, "lasso_perf_fe.rds")

##### H1. Bayesian Variable Selection ##########################################

# Prior-predictive check 
pp_mod <- brm(
  aggregate_CSwalk ~ 1 + (1|ticker) + (1|year),
  data         = data_train_dm,
  family       = gaussian(),
  sample_prior = "only",
  chains       = 2, iter = 1000,
  seed         = 1942
)
pp_check(pp_mod)

# Calibrate priors
y_mu <- mean(data_train_dm[[y_agg]])
y_sd <- sd(data_train_dm[[y_agg]])
hs_priors <- c(
  set_prior(sprintf("normal(%f, %f)", y_mu, y_sd), class="Intercept"),
  set_prior("horseshoe(df=1, scale_global=1)", class="b"),
  set_prior(sprintf("normal(0, %f)", y_sd), class="sigma", lb=0),
  set_prior(sprintf("cauchy(0, %f)", y_sd), class="sd")
)

set.seed(1942)
rhs <- paste(
  paste(h1_predictors, collapse = " + "),
  "(1 | ticker) + (1 | year)",
  sep = " + "
)
fml <- as.formula(paste0(y_agg, " ~ ", rhs)) # Add response
pp_mod_calib <- brm(
  formula      = fml,
  data         = data_train_dm,
  family       = gaussian(),
  prior        = hs_priors,
  sample_prior = "only",
  chains       = 2, iter = 1000,
  seed         = 1942
)

# Comparing before and after calibrating priors
y_rep_before <- posterior_predict(pp_mod)    
y_rep_after  <- posterior_predict(pp_mod_calib)
y_obs <- data_train$aggregate_CSwalk

# x-axis limits
xlim <- range(y_obs) + c(-1,1) * diff(range(y_obs)) * 0.1

# Plotting
set.seed(1942)
p1 <- ppc_dens_overlay(
  y    = y_obs,
  yrep = y_rep_before[sample(nrow(y_rep_before), min(30,nrow(y_rep_before))), ]
) +
  ggtitle("Before") +
  coord_cartesian(xlim = xlim)
set.seed(1942)
p2 <- ppc_dens_overlay(
  y    = y_obs,
  yrep = y_rep_after[sample(nrow(y_rep_after), min(30,nrow(y_rep_after))), ]
) +
  ggtitle("After")  +
  coord_cartesian(xlim = xlim)

# View & save
calibr_res_plot <- gridExtra::grid.arrange(p1, p2, ncol = 2)
gridExtra::grid.arrange(p1, p2, ncol = 2)
ggsave(
  filename = "calibr_res.png",  
  plot     = calibr_res_plot,
  width    = 10, height = 5, dpi = 300
)

# Fit full model on training set
hs_mod <- brm(
  formula = fml,
  data    = data_train_dm,
  family  = gaussian(),
  prior   = hs_priors,
  chains  = 4, iter = 2000, cores = 4,
  control = list(adapt_delta   = 0.99, # Stronger than default
                 max_treedepth = 15),  # to handle divergences
  seed    = 1942
) 

# Posterior predictions on demeaned test
post_pred    <- posterior_epred(hs_mod, newdata = data_test_dm,
                                allow_new_levels = TRUE)
hs_preds_dm  <- colMeans(post_pred)

# Bringing back to raw scale as I did with LASSO
hs_preds_raw <- hs_preds_dm +
  data_test_dm[[ paste0(y_agg, "_mean") ]]
y_test_raw  <- data_test[[ y_agg ]]

hs_perf <- tibble(
  Method = "HS_FE",
  RMSE   = sqrt(mean((hs_preds_raw - y_test_raw)^2)),
  MAE    = mean(abs(hs_preds_raw - y_test_raw)),
  MSE    = mean((hs_preds_raw - y_test_raw)^2)
)

ci        <- posterior_interval(hs_mod, prob = 0.95, 
                                variable = paste0("b_", h1_predictors))
hs_coefs  <- fixef(hs_mod)[h1_predictors, "Estimate"]
credible  <- rownames(ci)[(ci[,1] > 0) | (ci[,2] < 0)]
top10_hs  <- credible[ order(abs(hs_coefs[credible]), decreasing = TRUE) ][1:10]

posterior_summary(hs_mod, variable = "tau")

saveRDS(hs_mod,  "hs_mod_fe.rds")
saveRDS(hs_perf, "hs_perf_fe.rds")


##### H1. Random Forest Model ##################################################

# Parallel for caret
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

rf_ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
# Grid search
rf_grid <- expand.grid(
  mtry          = c(floor(sqrt(length(h1_predictors))),
                    floor(length(h1_predictors)/3),
                    floor(length(h1_predictors)/2)),
  splitrule     = "variance",
  min.node.size = c(1, 5, 10)
)

# Train RF
set.seed(1942)
rf_fit <- train(
  reformulate(h1_predictors, response = y_agg),
  data      = data_train_dm[, c(h1_predictors, y_agg)],
  method    = "ranger",
  trControl = rf_ctrl,
  tuneGrid  = rf_grid,
  importance= "impurity",
  metric    = "RMSE",
  num.trees = 500,
  num.threads=1
)

# predict on demeaned test
rf_preds_dm <- predict(rf_fit, newdata = data_test_dm[, h1_predictors])

# bring back to raw scale
rf_preds_raw <- rf_preds_dm +
  data_test_dm[[ paste0(y_agg, "_mean") ]]
y_test_raw  <- data_test[[ y_agg ]]

rf_perf <- tibble(
  Method = "RF_FE",
  RMSE   = sqrt(mean((rf_preds_raw - y_test_raw)^2)),
  MAE    = mean(abs(rf_preds_raw - y_test_raw)),
  MSE    = mean((hs_preds_raw - y_test_raw)^2)
)

stopCluster(cl)

# final varImp on demeaned training
final_ctrl <- trainControl(method = "none")
set.seed(1942)
rf_final <- train(
  reformulate(h1_predictors, response = y_agg),
  data      = data_train_dm[, c(h1_predictors, y_agg)],
  method    = "ranger",
  trControl = final_ctrl,
  tuneGrid  = rf_fit$bestTune,
  importance= "impurity",
  num.trees = 500,
  num.threads=1
)

imp_df   <- varImp(rf_final, scale = FALSE)$importance
rf_imp   <- imp_df$Overall
names(rf_imp) <- rownames(imp_df)
top10_rf <- names(sort(rf_imp, decreasing = TRUE))[1:10]

rf_fit$bestTune  # mtry: 13, min.node.size: 1

saveRDS(rf_final, "rf_final_fe.rds")
saveRDS(rf_perf,  "rf_perf_fe.rds")


##### H1. Final Model Comparison with Aggregate CS Walk as Response ############

performance <- bind_rows(panel_perf, lasso_perf, hs_perf, rf_perf)
# Top 10 topics
sel <- list(LASSO = top10_lasso,
            HS    = top10_hs,
            RF    = top10_rf,)
# Roundabout way needed because top topics of hs <10
mat <- t(sapply(sel, function(v) { length(v) <- 10; v }))
colnames(mat) <- paste0("Top", 1:10)
variable_selection <- as_tibble(mat, rownames = "Model")

print(performance)
print(variable_selection)


##### H1. Best Performing Model Fitted on All Three CS Walk Variables ##########

# Recomputing train means and demeaned train and test again, now for all CSwalks
train_means <- data_train %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarise(across(all_of(c(h1_response, h1_predictors)), mean), 
                   .groups="drop")

# Demean 
data_train_dm <- data_train %>%
  dplyr::left_join(train_means, by="ticker", suffix=c("","_mean")) %>%
  dplyr::mutate(across(all_of(c(h1_response, h1_predictors)),
                ~ . - get(paste0(cur_column(), "_mean")))) %>%
  dplyr::select(-ends_with("_mean")) %>%
  dplyr::mutate(ticker = data_train$ticker, year = data_train$year)

data_test_dm <- data_test %>%
  dplyr::left_join(train_means, by="ticker", suffix=c("","_mean")) %>%
  dplyr::mutate(across(ends_with("_mean"), ~ replace_na(., 0))) %>%
  dplyr::mutate(across(all_of(c(h1_response, h1_predictors)),
                ~ . - get(paste0(cur_column(), "_mean")))) %>%
  dplyr::select(-ends_with("_mean")) %>%
  dplyr::mutate(ticker = data_test$ticker, year = data_test$year)

# Loop over each response variable to calibrate priors
calibrated_priors <- list()
prior_check_plots <- list()
for (resp in h1_response) {
  message("Calibrating priors for: ", resp)
  
  # Outcome values
  y_obs <- data_train[[resp]]
  y_dm  <- data_train_dm[[resp]]
  
  # Prior-predictive check BEFORE calibration (flat priors)
  pp_mod_uncalib <- brm(
    formula      = as.formula(paste0(resp, " ~ 1 + (1|ticker) + (1|year)")),
    data         = data_train_dm,
    family       = gaussian(),
    sample_prior = "only",
    chains       = 2, iter = 1000,
    seed         = 1942,
    silent       = TRUE
  )
  y_rep_before <- posterior_predict(pp_mod_uncalib)
  
  # Calibrate priors
  y_mu <- mean(y_dm)
  y_sd <- sd(y_dm)
  hs_priors <- c(
    set_prior(sprintf("normal(%f, %f)", y_mu, y_sd), class = "Intercept"),
    set_prior("horseshoe(df=1, scale_global=1)", class = "b"),
    set_prior(sprintf("normal(0, %f)", y_sd), class = "sigma", lb = 0),
    set_prior(sprintf("cauchy(0, %f)", y_sd), class = "sd")
  )
  calibrated_priors[[resp]] <- hs_priors
  
  # Prior-predictive check AFTER calibration
  rhs <- paste(paste(h1_predictors, collapse = " + "),
               "(1 | ticker) + (1 | year)", sep = " + ")
  fml <- as.formula(paste0(resp, " ~ ", rhs))
  
  pp_mod_calib <- brm(
    formula      = fml,
    data         = data_train_dm,
    family       = gaussian(),
    prior        = hs_priors,
    sample_prior = "only",
    chains       = 2, iter = 1000,
    seed         = 1942,
    silent       = TRUE
  )
  y_rep_after <- posterior_predict(pp_mod_calib)
  
  # Plotting
  set.seed(1942)
  sample_n <- min(30, nrow(y_rep_before))
  xlim <- range(y_obs, na.rm = TRUE) + c(-1, 1) * diff(range(y_obs, 
                                                             na.rm = TRUE))*0.1
  
  p1 <- ppc_dens_overlay(
    y = y_obs,
    yrep = y_rep_before[sample(nrow(y_rep_before), sample_n), ]
  ) +
    ggtitle(paste(resp, "- Before Calibration")) +
    coord_cartesian(xlim = xlim)
  
  p2 <- ppc_dens_overlay(
    y = y_obs,
    yrep = y_rep_after[sample(nrow(y_rep_after), sample_n), ]
  ) +
    ggtitle(paste(resp, "- After Calibration")) +
    coord_cartesian(xlim = xlim)
  
  # Save plot
  combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  prior_check_plots[[resp]] <- combined_plot
  ggsave(
    filename = paste0("prior_check_", resp, ".png"),
    plot = combined_plot,
    width = 10, height = 5, dpi = 300
  )
}

# Looping over each response
hs_results <- map(h1_response, function(resp) {
  # rebuild formula
  f <- as.formula(paste0(resp, " ~ ",
                         paste(c(h1_predictors, "(1 | ticker)", "(1 | year)"),
                               collapse = " + ")))
  # Fitting
  m <- brm(f,
           data    = data_train_dm,
           family  = gaussian(),
           prior   = calibrated_priors[[resp]],
           chains  = 4, iter = 2000, warmup = 1000, cores = 4,
           control = list(adapt_delta = 0.99, max_treedepth = 15),
           seed    = 1942,
           save_pars = save_pars(all = TRUE)) # For posterior draws of lambda
  
  # Train preds on demeaned train
  post_train <- posterior_epred(m,
                                newdata = data_train_dm,
                                allow_new_levels = TRUE)
  preds_dm_train <- colMeans(post_train)
  # Adding back per‐ticker means 
  train_mu_vec   <- train_means[[resp]][match(data_train$ticker, 
                                              train_means$ticker)]
  preds_train_raw <- preds_dm_train + train_mu_vec
  obs_train_raw   <- data_train[[resp]]
  
  # Test preds on demeaned test
  post_test <- posterior_epred(m,
                               newdata = data_test_dm,
                               allow_new_levels = TRUE)
  preds_dm_test <- colMeans(post_test)
  # for test only tickers unseen, add global mean
  global_mu      <- mean(data_train[[resp]])
  test_mu_vec    <- train_means[[resp]][match(data_test$ticker, 
                                              train_means$ticker)]
  test_mu_vec[is.na(test_mu_vec)] <- global_mu
  preds_test_raw <- preds_dm_test + test_mu_vec
  obs_test_raw   <- data_test[[resp]]
  
  # Performance metrics of train and test
  perf_train <- tibble(
    split = "train",
    RMSE  = sqrt(mean((preds_train_raw - obs_train_raw)^2)),
    MAE   = mean(abs(preds_train_raw - obs_train_raw)),
    MSE   = mean((preds_train_raw - obs_train_raw)^2)
  )
  perf_test <- tibble(
    split = "test",
    RMSE  = sqrt(mean((preds_test_raw - obs_test_raw)^2)),
    MAE   = mean(abs(preds_test_raw - obs_test_raw)),
    MSE   = mean((preds_test_raw - obs_test_raw)^2)
  )
  perf <- bind_rows(perf_train, perf_test)
  
  # Top 10 by 95% CI not covering zero, then by magnitude
  ci    <- posterior_interval(m,
                              variable = paste0("b_", h1_predictors),
                              prob     = 0.95)
  est   <- fixef(m)[h1_predictors,"Estimate"]
  cred  <- rownames(ci)[ci[,1] > 0 | ci[,2] < 0]
  top10 <- head(cred[order(abs(est[cred]), decreasing=TRUE)], 10)
  
  list(response = resp,
       perf     = perf,
       top10    = top10,
       model    = m)
})

# Performance overview
hs_perf_df <- map_dfr(hs_results, "perf", .id = "response")
hs_top10   <- tibble(
  response = map_chr(hs_results, "response"),
  top10    = map(hs_results, "top10")
)

print(hs_perf_df)
print(hs_top10)


#----------------------
# Diagnostics & Results
#----------------------

# (Un)comment print() to (avoid) review the long coefficient and lambda tables
hs_diagnostics <- map_dfr(hs_results, function(result) {
  m     <- result$model
  resp  <- result$response
  draws <- as_draws_df(m)  %>% as_tibble()
  
  # Posterior summaries of fixed effects (b_)
  coef_draws <- draws[, grepl("^b_", names(draws))]
  coef_summary <- coef_draws %>%
    summarise(across(everything(), list(
      mean = ~mean(.),
      sd = ~sd(.),
      lower = ~quantile(., 0.025),
      upper = ~quantile(., 0.975)
    ), .names = "{.col}_{.fn}")) %>%
    pivot_longer(cols = everything(),
                 names_to = c("term", ".value"),
                 names_pattern = "b_(.*)_(.*)") %>%
    rename(predictor = term) %>%
    mutate(response = resp)
  print(coef_summary, n=30)
  
  # Local shrinkage parameters (hs_local = lambda)
  lambda_draws <- draws[, grepl("^hs_local\\[", names(draws)), drop = FALSE]
  if (ncol(lambda_draws) == 0) return(coef_summary) 
  
  lambda_summary <- lambda_draws %>%
    summarise(across(everything(), list(
      lambda_mean = ~mean(.),
      lambda_sd   = ~sd(.),
      lambda_lo   = ~quantile(., 0.025),
      lambda_hi   = ~quantile(., 0.975)
    ), .names = "{.col}_{.fn}")) %>%
    pivot_longer(cols = everything(),
                 names_to = c("index", ".value"),
                 names_pattern = "hs_local\\[(\\d+)\\]_(.*)") %>%
    mutate(index = as.integer(index),
           predictor = gsub("^b_", "", names(coef_draws)[index]),
           response = resp)
    #print(lambda_summary, n = 30)
  
  dplyr::left_join(coef_summary, lambda_summary, by = c("response","predictor"))
  })

# Global shrinkage summary (hs_global = tau) 
hs_tau <- map_dfr(hs_results, function(result) {
  m <- result$model
  draws <- as_draws_df(m) %>% as_tibble()
  tau_vals <- draws$hs_global
  tibble(response = result$response,
         tau_mean = mean(tau_vals),
         tau_sd   = sd(tau_vals),
         tau_lo   = quantile(tau_vals, 0.025),
         tau_hi   = quantile(tau_vals, 0.975))
})
print(hs_tau)

# Plot sparsity pattern
ggplot(hs_diagnostics %>% filter(!is.na(lambda_mean)),
       aes(x = reorder(predictor, -lambda_mean), y = lambda_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lambda_lo, ymax = lambda_hi), width = 0.2) +
  facet_wrap(~response, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Local Shrinkage per Predictor (Lambda)",
       y = "Posterior Mean of Lambda", x = "Predictor")



#───────────────────────────────────────────────────────────────────────────────
##### H2. Mediator Regressions #################################################
#───────────────────────────────────────────────────────────────────────────────


##### H2. Variable Preparation #################################################

pdata <- read_csv("data_standardized.csv")
pdata <- pdata.frame(data, index = c("ticker","year"))

X            <- "Topic2_lag2"  # Repeat this for 2, 13, and 16         
M            <- "symbolic_CSwalk"     
Y            <- "substantive_CSwalk"  
h2_controls  <- c("TQ_lag2", "TQ_lag2_sq", "slack", 
               "ci", "sr", "roa", "size")

# To make the formulas easier to read
f1 <- reformulate(c(X, h2_controls),       response = Y)
f2 <- reformulate(c(X, h2_controls),       response = M)
f3 <- reformulate(c(M, h2_controls),       response = Y)
f4 <- reformulate(c(X, M, h2_controls),    response = Y)


##### H2. Panel Regressions Step 1-3 ###########################################

# X -> Y
mod1 <- plm(f1, data = pdata, model = "within")

# X -> M
mod2 <- plm(f2, data = pdata, model = "within")

# M -> Y
mod3 <- plm(f3, data = pdata, model = "within")

# X + M -> Y
mod4 <- plm(f4, data = pdata, model = "within") # For Baron & Kenny (1986)


# Results (If any link is non-sig, full mediation is not possible or likely)
s1 <- summary(mod1)$coefficients
s2 <- summary(mod2)$coefficients
s3 <- summary(mod3)$coefficients

print(s1[X, ]) # X -> Y
print(s2[X, ]) # X -> M
print(s3[M, ]) # M -> Y

# All are signficant.

# lm() for vif() but with factor(ticker) and factor(year) to account for panel
# M -> Y because M and Y have Pearson correlation > 0.9
h2_lm_fe <- lm(
  substantive_CSwalk ~ symbolic_CSwalk + Topic16_lag2 + TQ_lag2 + TQ_lag2_sq +
    slack + ci + sr + roa + size +
    factor(ticker) + factor(year),
  data = data
)

# VIF (discriminant validity via sym and sub CS walk must be done conceptually)
vif <- vif(h2_lm_fe)
print(vif) # Nothing problematic, but it's only predictors: Not sym vs. sub CSw.

##### H2. Step 4: Full Mediationg Model with Bootstrapping #####################

# For firm fixed effects, demeaning is needed again. Now on full data with lag2.
vars_to_demean <- c(X, M, Y, h2_controls)

# Computing firm and year means on the full data
firm_means <- pdata %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarise(across(all_of(vars_to_demean), mean, .names = "{.col}_mean"),
                   .groups = "drop")


# Demeaning
data_dm <- pdata %>%
  dplyr::left_join(firm_means, by = "ticker") %>%
  dplyr::mutate(across(all_of(vars_to_demean),
                ~ . - get(paste0(cur_column(), "_mean")))) %>%
  dplyr::select(-ends_with("_mean"))

# Fiting mediator and outcome models on the demeaned data (impossible with plm)
lm_m <- lm(
  formula = reformulate(c(X, h2_controls), response = M),
  data    = data_dm
)
lm_y <- lm(
  formula = reformulate(c(X, M, h2_controls), response = Y),
  data    = data_dm
)

# Bootstrap‐mediated effect
set.seed(1942)
med <- mediation::mediate(
  model.m  = lm_m,
  model.y  = lm_y,
  treat    = X,
  mediator = M,
  boot     = TRUE,
  sims     = 5000
)

# Results & Performance
summary(med, boot.ci.type = c("perc","bca"))
# In the thesis:
# ACME           = $\alpha\beta$
# ADE            = $\tau'$
# Total effect   = $\tau$
# Prop. Mediated = $\alpha\beta$/$\tau$

summary(lm_m)$coefficients[X, ] # Estimate = 1.535820e-01
summary(lm_y)$coefficients[M, ] # Estimate = 8.285049e-01
summary(lm_m)$r.squared         # 0.1013736
summary(lm_y)$r.squared         # 0.8282686




##### End of Script ############################################################
