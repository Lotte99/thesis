# Disclaimer: ChatGPTo4-mini-high helped me debug and improve the script. 
# It also helped structuring it and making it visibly more pleasing.

# This R script performs the following steps:
# 1.  Load SEC data and create one-year lag in text data for merging 
# 2.  Load WRDS data and create controls from raw data
# 3.  Load LSEG data and create CS walk variables from indicators
# 4.  Merge all three datasets
# 5.  Add Talk Quantity to merged data
#     - TQ was created in a different script
# 6.  Add Topics to merged data and add a second lag to all topics 
#     - Topics were created in a different script
# 7.  Review descriptives and summary stats
# 8.  Check MCAR and MAR for NA values of controls
# 9.  Impute missing values using MICE+RF
# 10. Numeric variables (excl. year) are standardized

# Note:
# - Intermediate csv, xlsx, and Rds files enable restarting from any phase. 
# - The scripts of topic modeling and talk quantity both rely on this 
#   script and are also required to move forward in this script (phase 2). 

# Abbreviations of variables:
# lt = total liabilities, at = total assets, act = current assets, sale = net
# sales, dvt = dividends, mkvalt = market value, roa = return on assets, 
# TRESGCCS = ESG controversies score (in the thesis: media controversies),
# sr = shareholder return, ci = capital intensity, size = firm size,
# slack = organizational slack, TQ = talk quantity

##### 0. Setup: Load Required Libraries ########################################

library(car)
library(caret) 
library(corrplot)
library(data.table) 
library(dplyr)
library(lubridate)
library(mice)
library(MissMech)    
library(naniar)     
library(psych)
library(purrr)
library(readr)
library(readxl)
library(texreg)

##### 1. Load All datasets and Preprocess ######################################
# Goal: Merge SEC, LSEG, WRDS datasets and Rds files of Topics and Talk Quantity
#       Create CS walk variables from raw LSEG data and controls from WRDS data

#------------
# SEC dataset
#------------

extracted_business_sections_2000_2020 <- 
  read_csv("~/Documents/extracted_business_sections_2000_2020.csv")
extracted_business_sections_2020_2023 <- 
  read_csv("~/Documents/extracted_business_sections_2020-2023.csv")


#Merge SEC datasets by year
data.SEC <- bind_rows(extracted_business_sections_2000_2020, 
                      extracted_business_sections_2020_2023)

#Remove duplicates of the year 2020
data.SEC <- data.SEC %>%
  distinct(ticker, year, .keep_all = TRUE)

#Shift business_section forward by 1 year to create the lag
data.SEC.lagged <- data.SEC %>%
  mutate(year = year + 1)  

write_csv(data.SEC.lagged, file = "data.SEC.lagged.csv")

#-------------
# WRDS dataset
#-------------

WRDS_CIK_data    <- read_csv("WRDS_CIK_data.csv")
WRDS_ticker_data <- read_csv("WRDS_ticker_data.csv")
WRDS_ROA_data    <- read_csv("WRDS_ROA_data.csv")

# Filtering for only one year-ticker combination
WRDS_ROA_data <- WRDS_ROA_data %>%
  mutate(year = year(public_date)) %>%            # extract calendar year
  group_by(TICKER, year) %>%                      # group year-ticker combo
  slice_min(public_date, with_ties = FALSE) %>%   # pick nearest date to 01-01
  ungroup() 

WRDS_ticker_data <- WRDS_ticker_data %>%
  mutate(year = year(datadate)) %>%               # extract calendar year
  group_by(tic, year) %>%                         # group year-ticker combo
  slice_min(datadate, with_ties = FALSE) %>%      # pick nearest date to 01-01
  ungroup() 

WRDS_CIK_data <- WRDS_CIK_data %>%
  mutate(year = year(datadate)) %>%               # extract calendar year
  group_by(tic, year) %>%                         # group year-ticker combo
  slice_min(datadate, with_ties = FALSE) %>%      # pick nearest date to 01-01
  ungroup() 

# Renaming ticker for easier merging downstream
WRDS_ROA_data <- WRDS_ROA_data %>% 
  rename(ticker = TICKER)

WRDS_ticker_data <- WRDS_ticker_data %>% 
  rename(ticker = tic)

WRDS_CIK_data <- WRDS_CIK_data %>% 
  rename(ticker = tic)

# Selecting variables to merge in data.WRDS
subset_roa     <- WRDS_ROA_data[, c("ticker", "year", "roa")]
subset_ticker  <- WRDS_ticker_data[, c("ticker", "year", "act", "at", "lt", 
                                       "sale", "emp", "dvt", "mkvalt")]
subset_cik     <- WRDS_CIK_data[, c("ticker", "year", "act", "at", "lt", 
                                    "sale", "emp", "dvt", "mkvalt")]

ticker_cik_combo <- bind_rows( 
  subset_ticker,
  subset_cik
)

# Many non-overlapping ticker-year combos unfortunately -> NAs introduced
data.WRDS <- subset_roa %>%
  left_join(ticker_cik_combo, by = c("ticker","year")) 

data.WRDS <- rename(data, size = emp)
  
#-------------
# LSEG dataset
#-------------

##### ESG variables and already required preprocessing #########################
data.LSEG.ESGCS <- read_excel("~/Documents/LSEG_ESG_scores.xlsx")

# Remove later:
data.LSEG.ESGCS <- LSEG_ESG_scores

data.LSEG.ESGCS$year <- as.character(data.LSEG.ESGCS$year)
data.LSEG.ESGCS$year <- as.Date(data.LSEG.ESGCS$year)
data.LSEG.ESGCS$year <- format(data.LSEG.ESGCS$year, "%Y")

data.LSEG.ESGCS <- data.LSEG.ESGCS %>%
  mutate(year = as.numeric(year))

##### CS walk indicators and already required preprocessing ####################
data.LSEG.CSwalk <- read_excel("data.LSEG.CSwalk.xlsx")

# Or if I import from the file pane in RStudio
data.LSEG.CSwalk <- data_LSEG_CSwalk

data.LSEG.CSwalk$year <- as.character(data.LSEG.CSwalk$year)
data.LSEG.CSwalk$year <- as.Date(data.LSEG.CSwalk$year)
data.LSEG.CSwalk$year <- format(data.LSEG.CSwalk$year, "%Y")

data.LSEG.CSwalk <- data.LSEG.CSwalk %>%
  mutate(year = as.numeric(year))

# Or import data.LSEG.CSwalk from the file pane.

# Recode values of ENERDP073 to binary values
map_values <- c(
"No"       = 0,
"N"        = 0,
"Yes"      = 1,
"Y"        = 1,
"Both"     = 1,
"ISO14000" = 1
)

# Looking up each existing value in that map
mapped <- map_values[ as.character(data.LSEG.CSwalk$ENERDP073) ]
data.LSEG.CSwalk$ENERDP073 <- as.integer(mapped)


# Recoding the remaining CS walk indicators
all_cols    <- names(data.LSEG.CSwalk)
to_recode   <- setdiff(all_cols[3:60], "ENERDP073")

# Function that recodes while leaving the other columns untouched
recode_yn <- function(x) {
  clean <- toupper(trimws(as.character(x)))  # trimming needed
  out   <- ifelse(clean == "Y", 1L,
                  ifelse(clean == "N", 0L, NA_integer_))
  out
}

# Applying the function to data.LSEG.CSwalk
data.LSEG.CSwalk[to_recode] <- lapply(data.LSEG.CSwalk[to_recode], recode_yn)

str(data.LSEG.CSwalk) # Non-indicators + ENERDP073 remain untouched
describe(data.LSEG.CSwalk[to_recode]) # Successfully recoded


##### Calculating CS walk variables from indicators ############################

# Create aggregate, symbolic, and substantive CS walk based on the indicators


symbolic_indic <- c(
  "CGVSDP020",  "ENERDP0161", "ENPIDP085",  "ENRRDP0121", "ENRRDP0122", 
  "ENRRDP123",  "ENRRDP0124", "ENRRDP0125", "ENRRDP0191", "ENRRDP0192", 
  "SOPRDP0121", "SOPRDP0124", "SOPRDP0126", "SOPRDP0128", "SOCODP0066", 
  "SOCODP0067", "SOCODP0069", "SOCODP013",  "SOHRDP0101", "SOHRDP0102", 
  "SOHRDP0103", "SOHRDP0105", "SOHRDP012",  "SOHRDP027",  "SODODP0081", 
  "SODODP0151", "SOHSDP0121", "SOHSDP0123", "CGCPDP0013", "SOTDDP0091",
  "SOTDDP0092", "SOCODP0121"
)

substantive_indic <- c(
  "CGVSDP005",  "ENERDP073",  "ENPIDP069",  "SOHSDP0183", "SOTDDP030", 
  "ENRRDP004",  "ENRRDP008",  "ENRRDP029",  "SOHSDP0083", "SOHSDP014",
  "ENRRDP046",  "ENRRDP058",  "ENRRDP059",  "ENRRDP066",  "SOPRDP016",  
  "SOPRDP021",  "SOPRDP022",  "SOPRDP025",  "SOCODP0109", "SOCODP011",  
  "SOCODP037",  "SOCODP040",  "CGCPO09V",   "SODODP026",  "SODODP0151", 
  "SODODP027",  "SOHSDP004",  "SOHSDP0081"  
)


aggregate_indic <- c(symbolic_indic,substantive_indic)


# Creating aggregate_CSwalk
data.LSEG.CSwalk$aggregate_CSwalk <- 
  100/60 * rowSums(
    data.LSEG.CSwalk[, aggregate_indic],
    na.rm = TRUE
  )

# Creating symbolic_CSwalk
data.LSEG.CSwalk$symbolic_CSwalk <- 
  100/32 * rowSums(
    data.LSEG.CSwalk[, symbolic_indic],
    na.rm = TRUE
  )

# Creating substantive_CSwalk
data.LSEG.CSwalk$substantive_CSwalk <- 
  100/28 * rowSums(
    data.LSEG.CSwalk[, substantive_indic], 
    na.rm = TRUE
  )

describe(data.LSEG.CSwalk)

subset_ESG   <- data.LSEG.ESGCS[, c("ticker","year","name","TRESGCCS")]
subset_CS <- data.LSEG.CSwalk[, c("ticker","year",
                                  "aggregate_CSwalk",
                                  "symbolic_CSwalk",
                                  "substantive_CSwalk")]

data.LSEG <- merge(subset_ESG, subset_CS,
                   by    = c("ticker","year"),
                   all.x = TRUE) 

# Create second lag like I created data.SEG.lagged 
data.LSEG <- data.LSEG %>%
  arrange(ticker, year) %>%         
  group_by(ticker) %>%             
  mutate(symbolic_CSwalk_lag1 = lag(symbolic_CSwalk, n = 1)) %>%
  ungroup()

str(data.LSEG)

write_csv(data.LSEG, "data.LSEG.csv")

#---------------
# MERGE datasets
#---------------

data <- data.LSEG %>%
  group_by(ticker, year) %>% 
  slice(1) %>% 
  ungroup() %>%
  # inner‐join SEC
  inner_join(
    data.SEC.lagged %>%
      group_by(ticker, year) %>% 
      slice(1) %>% 
      ungroup(),
    by = c("ticker", "year")
  ) %>%
  #left‐join WRDS because NAs may be introduced
  left_join(
    data.WRDS %>%
      group_by(ticker, year) %>% 
      slice(1) %>% 
      ungroup(),
    by = c("ticker", "year")
  )

# A_data.csv to create CS Topics & Talk Quantity as it has the final nr. of obs.
write.csv(data,"A_data.csv", row.names = FALSE) 


##### Phase 2: Add CS Topics & Talk Quantity (TQ) to Merged dataset ############
# Goal: Add the quantified CS talk variables to the merged data. 
#       Topics and TQ were derived from the SEC text data in other scripts.

#--------------------------
# Add talk quantity to data
#--------------------------

talk_quantity <- read_csv("phase5_TalkQuantity_df_27.07.csv")

# Create second lag like I created data.SEG.lagged
talk_quantity <- talk_quantity %>%
  arrange(ticker, year) %>%         
  group_by(ticker) %>%             
  mutate(
    TQ_lag2 = lag(TalkQuantity, n = 1)
  ) %>%
  ungroup()

# Renaming TalkQuantity to clarify first lag
talk_quantity <- talk_quantity %>% 
  rename(TQ_lag1 = TalkQuantity)

# Adding talk_quantity to the dataset
talk_quantity <- talk_quantity %>% 
  select(ticker, year, TQ_lag1, TQ_lag2)

data <- data %>%
          dplyr::left_join(talk_quantity %>%
          group_by(ticker, year) %>% 
          slice(1) %>% 
          ungroup(),
          by = c("ticker", "year")
)

# Square TQ_lag1 and TQ_lag2 to account for U-shape
data <- data %>%
  mutate(TQ_lag1_sq = TQ_lag1^2)

data <- data %>%
  mutate(TQ_lag2_sq = TQ_lag2^2)

write_csv(data, file = "data_excl.topics.csv")
data <- read_csv("data_excl.topics.csv")
#--------------------------
# Add CS topics to the data
#--------------------------

doc_topics_wide <- readRDS("phase9_doc_topics.Rds")

# Excluding the first year of the sample does not resolve the introduced NAs as
# many companies only file the first 10-K later, leading to 806 NAs in 5642 obs.

# Adding the topics to the existing, near complete merged dataset
data <- data %>%
  dplyr::left_join(doc_topics_wide %>%
              group_by(ticker, year) %>% 
              slice(1) %>% 
              ungroup(),
            by = c("ticker", "year")
  )

# Business section and key (i.e. doc_id) is not needed anymore. 
data <- data %>% 
  dplyr::select(-business_section, -key)


# Transforming all topics from compositional to center log-ratio (CLR)
topic_cols      <- grep("topic[0-9]+$", names(data), value = TRUE)
topic_lag2_cols <- grep("topic[0-9]+_lag2$", names(data), value = TRUE)

# Topic 20 as reference topic
ref_topic       <- "topic20"
ref_topic_lag2  <- "topic20_lag2" #---------------------------------------------> ADJUST

# ALR transformation function
alr_transform <- function(mat, ref_col) {
  # Replacing possible zeros to avoid log(0)
  mat[mat == 0] <- 1e-6
  ref_vals <- mat[[ref_col]]
  
  # Removal of reference column
  mat_no_ref <- mat[, setdiff(names(mat), ref_col)]
  
  # log-ratios
  alr_mat <- log(mat_no_ref / ref_vals)
  return(alr_mat)
}

# Transforming lag1
topic_matrix <- data[, topic_cols]
topic_alr <- alr_transform(topic_matrix, ref_topic)
colnames(topic_alr) <- paste0(colnames(topic_alr))

# Transforming lag2
topic_lag2_matrix <- data[, topic_lag2_cols]
topic_lag2_alr <- alr_transform(topic_lag2_matrix, ref_topic_lag2)
colnames(topic_lag2_alr) <- paste0(colnames(topic_lag2_alr))

# Replacing them in the data
data <- bind_cols(
  data %>% dplyr::select(-all_of(c(topic_cols, topic_lag2_cols))),
  as.data.frame(topic_alr),
  as.data.frame(topic_lag2_alr)
)

# Exclusion of obs. with NA values of response and main predictors (i.e. topics)
data <- data %>%
 filter(
  if_all(c(aggregate_CSwalk, symbolic_CSwalk, substantive_CSwalk), ~ !is.na(.)),
  if_all(starts_with("Topic"), ~ !is.na(.))
)

write_csv(data, file = "all_data.csv")

##### Phase 3: Descriptives ####################################################
# Goal: Get descriptives and summary statistics and standardize all numeric vars

# Calculating consecutive years per company for panel data opportunities
consecutive_streaks <- data %>%
  mutate(year = as.integer(year)) %>%
  arrange(ticker, year) %>%
  group_by(ticker) %>%
  mutate(
    year_diff = year - lag(year),
    year_diff = if_else(is.na(year_diff), 1L, year_diff),
    is_consecutive = year_diff == 1,
    group_id = rleid(is_consecutive)  # creates group ids for streaks
  ) %>%
  group_by(ticker, group_id) %>%
  filter(n() >= 2) %>%  # keep only actual streaks
  summarise(streak_length = n(), .groups = "drop") %>%
  group_by(ticker) %>%
  summarise(max_consecutive_years = max(streak_length), .groups = "drop")

summary_table <- consecutive_streaks %>%
  count(max_consecutive_years, name = "number_of_companies") %>%
  arrange(desc(max_consecutive_years))
print(summary_table, n = Inf)
# Conclusion: too much missing to use it as strong panel data.
# Firm-year observations in streaks ≥ 15 years = 1250. 
# Share of firms with long streaks out of total nr. of firms = 26.42%

# Nr. of companies per year
yearly_company_counts <- data %>%
  group_by(year) %>%
  summarise(number_of_companies = n_distinct(ticker), .groups = "drop") %>%
  arrange(year) %>%
  print(n=25)

# Nr. of unique companies
num_companies <- data %>%
  summarise(unique_companies = n_distinct(ticker)) %>%
  print(n=21)

describe(data)

# Checking financial performance and media controversy for aggregate_CSwalk = 0 
data2 <- data %>%
  mutate(
    zero_cs = aggregate_CS_walk == 0,
    negative_roa = ROA < 0,
    high_controversy = TRESGCCS > median(TRESGCCS, na.rm = TRUE)
  )

zero_cs <- data2 %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_zero_cs = sum(zero_cs, na.rm = TRUE),
    share_negative_roa = mean(negative_roa[zero_cs], na.rm = TRUE),
    share_high_controversy = mean(high_controversy[zero_cs], na.rm = TRUE)
  )
rm(data2)


##### Phase 4: Imputing ########################################################
# Goal: Imputing NAs using MICE+PMM after confirming MAR assumption holds.

#-------------------
# Testing MAR & MCAR
#-------------------

# Selecting the variables with missing values
vars_mcar <- c(
  "TRESGCCS", "roa", "act", "at", "lt", 
  "sale", "size", "dvt", "mkvalt"
)

dm_initial <- data %>% select(all_of(vars_mcar))

# Computing pairwise correlations, zero‐fill NAs, and drop any |r| > .90
corrm <- cor(dm_initial, use = "pairwise.complete.obs")
corrm[is.na(corrm)] <- 0
to_drop <- findCorrelation(corrm, cutoff = 0.90)  
vars_clean <- vars_mcar[-to_drop]

# Final test matrix
dm <- data %>% select(all_of(vars_clean))

# Little’s MCAR test
cat("**naniar::mcar_test()**\n")
print(mcar_test(dm))

## MAR assumption testing
# Identifying variables with missing values
vars_with_na <- names(data)[sapply(data, function(x) any(is.na(x)))]

covars_no_na <- setdiff(
  names(data)[sapply(data, function(x) !any(is.na(x)) & !is.character(x))],
  c("ticker", "name", "TQ_lag2", grep("^topic.*_lag2$", names(data), 
                                      value = TRUE))
)

# List to hold the models
missing_models <- vector("list", length(vars_with_na))
names(missing_models) <- vars_with_na

# Looping over each variable that contains NAs
for(v in vars_with_na) {
  # creating a 0/1 “missingness” flag for variable v
  df <- data %>% 
    mutate(miss = as.integer(is.na(.data[[v]])))
  
  # building the formula
  f <- as.formula(paste("miss ~", paste(covars_no_na, collapse = " + ")))             
  
  # fitting the logistic regression
  missing_models[[v]] <- glm(f, family = binomial(link = "logit"), data = df)
}

# Table with results per variable (uncomment next line to see the many tables)
#lapply(missing_models, summary)

# One concise table with results
screenreg(
  missing_models,
  custom.model.names = names(missing_models),
  override.coef = lapply(missing_models, function(m) exp(coef(m))),  # show odds‐ratios
  override.se   = lapply(missing_models, function(m) sqrt(diag(vcov(m))))
)

#---------------------
# Imputation of values
#---------------------

# First, log-transform size, at, act, lt, sale, dvd, mkvalt due to skewness
data <- data %>%
  mutate(across(c(size, at, act, lt, sale, dvt, mkvalt), 
                ~ ifelse(. >= 0, log(. + 1), NA)))

# As proposed by van Buuren & Groothuis-Oudshoorn (2011)
method <- make.method(data) 

# Turning off character identifiers
method[c("ticker", "name")] <- ""

# Turning off all topic columns by name‐pattern
topic_cols <- grep("^Topic", names(data), value = TRUE)
method[topic_cols] <- ""
method[method == "pmm"] <- "rf"

# Building predictor matrix & exclude IDs for regressions
pred_matrix <- quickpred(data, exclude = c("ticker", "name", topic_cols))

# Imputing the rest
impute <- mice(
  data            = data,
  m               = 30,
  method          = method,
  predictorMatrix = pred_matrix,
  maxit           = 10 
)

# Adding imputed values to the data  
data <- complete(impute, 1)
sum(is.na(data)) # No NAs present anymore

#---------------------
# Reviewing Imputation
#---------------------

# Overlayed density plots of imputed vs observed values (takes time to load)
densityplot(impute)

# Bivariate relationship checks
xyplot(impute, act ~ at)
xyplot(impute, dvt ~ mkvalt)
xyplot(impute, size ~ roa)
xyplot(impute, sale ~ at)

## Masking ("simulation") experiment
mask_results <- map_dfr(vars_mcar, function(v) {
  set.seed(1939)
      
  # Masking 20% of the non‐missing entries of v
  data_amp <- data
  obs_idx  <- which(!is.na(data_amp[[v]]))
  to_mask  <- sample(obs_idx, length(obs_idx) * 0.2)
  data_amp[to_mask, v] <- NA
          
  # Re‐imputing
  imp_sim <- mice(data_amp, m = 30,
                        method          = impute$method,
                        predictorMatrix = impute$predictorMatrix,
                        printFlag = FALSE)
  # Computing RMSE & bias against the original
  true_vals <- data[[v]]
  imputed1  <- complete(imp_sim, 1)[[v]]
      tibble(
      variable = v,
      RMSE     = sqrt(mean((imputed1 - true_vals)^2, na.rm = TRUE)),
      bias     = mean(imputed1 - true_vals, na.rm = TRUE),
      pct_RMSE = sqrt(mean((imputed1 - true_vals)^2, na.rm = TRUE)) / 
        mean(true_vals, na.rm = TRUE) * 100,
      pct_bias = mean(imputed1 - true_vals, na.rm = TRUE) / 
        mean(true_vals, na.rm = TRUE) * 100
      )
})
print(mask_results) 

# Delta‐sensitivity analysis under modest (10%) MNAR (missing not at random) 
delta_results <- map_dfr(vars_mcar, function(v) {
  all_imp <- complete(impute, "all")
  
  # Defining a small delta-shift (10% of the observed mean)
  delta   <- -0.10 * mean(data[[v]], na.rm = TRUE)
  
  # Applying the shift only to the original NAs
  adj_means <- map_dbl(all_imp[-1], function(d) {
    d <- d
    miss_idx <- is.na(data[[v]])
    d[[v]][miss_idx] <- d[[v]][miss_idx] + delta
    mean(d[[v]], na.rm = TRUE)
  })
  tibble(
    variable      = v,
    original_mean = mean(data[[v]], na.rm = TRUE),
    shifted_mean  = mean(adj_means),
    mean_diff     = mean(adj_means) - mean(data[[v]], na.rm = TRUE),
    pct_change    = mean_diff / mean(data[[v]], na.rm = TRUE) * 100
  )
})
print(delta_results)

write_csv(data, "imputed_data.csv")                            


##### Phase 5: Creating Control Variables from Imputed WRDS data ###############
# Goal: Calculate and add controls to data and remove now-irrelevant variables

# Organizational slack
data$slack <- data$act/data$lt # current assets/total liabilities

# Capital intensity
data$ci <- data$at/data$sale   # total assets/net sales

# Shareholder return (lag introduces NA values)
data <- data %>%
  arrange(ticker, year) %>%
  group_by(ticker) %>%
  mutate(
    mkvalt_lag = dplyr::lag(mkvalt),
    sr = (mkvalt - mkvalt_lag + dvt) / mkvalt_lag
  ) %>%
  ungroup()

data <- data %>%
  dplyr::select(-act, -lt, -at, -sale, -dvt, -mkvalt, -mkvalt_lag) 

# Bit of extra imputation due to newly introduced NA values 
method2 <- make.method(data)
method2[c("ticker","year","name","TRESGCCS", "aggregate_CSwalk",
          "symbolic_CSwalk","substantive_CSwalk", "roa", "size", "TQ_lag1", 
          "TQ_lag2", "slack", "ci")] <- ""  # Isolating shareholder return

topic_cols2 <- grep("^Topic", names(data), value = TRUE)
method2[topic_cols2] <- ""  
method[method == "pmm"] <- "rf"

# Building predictor matrix & exclude IDs for regressions in chained equations
pred_matrix2 <- quickpred(data, exclude = c("ticker", "name", topic_cols2))

# Imputing the rest
impute2 <- mice(
  data            = data,
  m               = 30,
  method          = method2,
  predictorMatrix = pred_matrix2
)

# Adding imputed values to the data  
data <- complete(impute2, 1)
sum(is.na(data)) # No NAs present anymore

# Final descriptives and stats to show in the data section & rho in methdology
describe(data)
summary(data)

# Calculating rho for the controls
vars <- c("slack", "ci", "sr", "roa", "size", "TRESGCCS")
rho_results <- list()

# Lag-1 autocorrelation function
manual_rho <- function(df, var) {
  vec <- df %>% dplyr::arrange(year) %>% dplyr::pull(!!sym(var))
  
  if (length(vec) < 4 || sd(vec, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  cor(vec[-1], vec[-length(vec)], use = "complete.obs")
}

# Looping over the controls
for (v in vars) {
  cat("Processing:", v, "\n")
  
  rho_df <- data_test %>%
    dplyr::select(ticker, year, !!sym(v)) %>%
    dplyr::filter(!is.na(!!sym(v))) %>%
    dplyr::group_by(ticker) %>%
    dplyr::group_modify(~ tibble(rho = manual_rho(.x, v))) %>%
    dplyr::filter(!is.na(rho)) %>%
    dplyr::mutate(variable = v)
  
  rho_results[[v]] <- rho_df
}

# Combine all into one tibble
rho_all <- bind_rows(rho_results)

# Summarizing mean
rho_summary <- rho_all %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    mean_rho = mean(rho),
    n_firms = dplyr::n(),
    .groups = "drop"
  )

print(rho_summary)

write_csv(data, file = "non_stand_data.csv")

##### Phase 7: Standardizing of Variables ######################################
# Goal: Standardize all non-identifier variables for comparable effect sizes

# Excluding identifier variables to retain only numeric data
numeric_vars <- data %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-year) %>%
  colnames()

# Standardizing numeric variables
data_standardized <- data %>%
  mutate(across(all_of(numeric_vars), ~ as.numeric(scale(.))))

data <- data %>%
  mutate(across(all_of(numeric_vars), ~ as.numeric(scale(.))))

# Checking that standardization worked
summary(data_standardized[numeric_vars])


write_csv(data_standardized, "data_standardized.csv")


##### Phase 8: Get Pearson Correlations ########################################

# Creating function to get significance of Pearson correlation
corstars <- function(x, y = NULL, method = "pearson", digits = 2) {
  if (is.null(y)) {
    # Full matrix
    cor_mat <- cor(x, method = method)
    vars <- colnames(cor_mat)
    p_mat <- matrix(NA, ncol = ncol(x), nrow = ncol(x),
                    dimnames = list(vars, vars))
    for (i in 1:(ncol(x) - 1)) {
      for (j in (i + 1):ncol(x)) {
        test <- cor.test(x[[i]], x[[j]], method = method)
        p_mat[i, j] <- p_mat[j, i] <- test$p.value
      }
    }
  } else {
    # x vs y
    cor_mat <- cor(x, y, method = method)
    p_mat <- matrix(NA, nrow = ncol(x), ncol = ncol(y),
                    dimnames = list(colnames(x), colnames(y)))
    for (i in 1:ncol(x)) {
      for (j in 1:ncol(y)) {
        test <- cor.test(x[[i]], y[[j]], method = method)
        p_mat[i, j] <- test$p.value
      }
    }
  }
  
  # Format result with stars
  formatted <- ifelse(p_mat < 0.05, "*", "")
  out <- round(cor_mat, digits)
  out_char <- matrix(paste0(format(out, nsmall = digits), formatted),
                     nrow = nrow(out), ncol = ncol(out),
                     dimnames = dimnames(out))
  return(as.data.frame(out_char))
}


# Correlation Matrix 1
# Topic20 is excluded. Go back to phase 2, include it, and standardize again.

# Isolating topics
topic_vars <- data_cor %>%
  dplyr::select(matches("^Topic(1?[0-9]|2[0-9])?$")) 

# Select CS walk outcomes (aggregate_CSwalk excluded due to multicollinearity) 
walk_vars <- data_cor %>%
  dplyr::select(aggregate_CSwalk, symbolic_CSwalk, substantive_CSwalk)

# Compute correlations
cor_table_1 <- corstars(topic_vars, walk_vars)
cor_table_1

# Correlation Matrix 2
# Select everything except topic variables, lagged vars., and identifiers
non_topic_data <- data %>%
  dplyr::select(-matches("^Topic(1?[0-9]|2[0-9])(_lag2)?$"), 
                -TQ_lag2, -TQ_lag1_sq, -TQ_lag2_sq,
                -ticker, -name, -year)

# Compute Pearson correlation matrix
cor_table_2 <- corstars(non_topic_data)
cor_table_2

# Correlation matrix (to spot any correlations >0.9)
topic_lag1_vars     <- paste0("Topic", c(1:29))
h1_control_vars     <- c("TQ_lag1", "TQ_lag1_sq", "slack", 
                         "ci", "sr", "roa", "size")
h1_predictors       <- c(topic_lag1_vars, h1_control_vars)
cor_mat <- cor(data[ h1_predictors ], use="pairwise.complete.obs")
diag(cor_mat) <- NA 
high_corr <- which(abs(cor_mat) > 0.9 & abs(cor_mat) < 1, arr.ind=TRUE)
high_corr 



##### End of Script ############################################################

