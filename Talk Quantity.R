# Disclaimer: ChatGPTo4-mini-high helped me debug and improve the script. 
# It also helped structuring it and making it visibly more pleasing 
# before officially handing it in.

# This R script performs the following steps:
# 1. Raw Text Cleaning (consistent with LDA code)
# 2. CS Dictionary Preprocessing (consistent with LDA code)
# 3. Creation of DFM and matching of CS words from doc with dictionary
# 4. Computation of TalkQuantity (TQ)
# 5. Creation of TQ dataframe for data merging in Preprocessing.R

# Note:
# - Intermediate RDS files enable restarting from any phase. Uncomment to use.

##### 0. Setup: Load Required Libraries ########################################

library(ggplot2)
library(readxl) 
library(reshape2)
library(slam)
library(SnowballC)
library(stm)
library(stringr)
library(stringi)
library(textclean)  
library(textstem) 
library(tokenizers)     
library(tidytext)       
library(tidyverse)
library(topicmodels)    
library(quanteda)    
library(quanteda.textstats)

data("stop_words")
stopword_list <- stop_words$word

##### Phase 1: Set working directory and load data #############################

setwd("/Users/karingeerts/Documents/Scriptie Lotte")

text_data <- read.csv("A_data.csv")

##### Phase 2: Raw Text Cleaning ###############################################
# Goal: Remove HTML/XML tags, SEC header/footer text, tables, and fix encoding
# Cleaning for NLP will be done after sentence segmentation. 

clean_text <- function(text) {
  # Fix encoding before any regex and force everything into “true” UTF-8
  text_clean <- iconv(text, from = "latin1", to = "UTF-8", sub = " ")
  text_clean <- stri_enc_toutf8(text)
  # Repair/remove broken mojibakes.
  text_clean <- str_replace_all(text_clean, "Â", "")              # removed
  text_clean <- str_replace_all(text_clean, "â€|â€œ|â€�", "\"")  # double quotes
  text_clean <- str_replace_all(text_clean, "â€˜|â€™", "'")       # single quotes/apostrophes
  text_clean <- str_replace_all(text_clean, "â€“|â€”", "-")       # en-dash/em-dash -> hyphen
  text_clean <- str_replace_all(text_clean, "â€¦", "...")         # ellipsis
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬Å“|Ã¢â‚¬Å”", "\"")  # smart double-quotes
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬â„¢", "'")       # smart apostrophe
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬â€œ|Ã¢â‚¬â€\u0092", "-")  # en-dash/em-dash
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬Â|Ã¢â‚¬", "")    # removed
  text_clean <- str_replace_all(text_clean, "Ãƒ¢Ã¢â€š¬Ã…", "")     # removed
  text_clean <- str_replace_all(text_clean, "Ãƒ¢Ã¢â€š¬", "")       # removed
  # Replace HTML entities
  text_clean <- replace_html(text_clean)
  # Remove HTML/XML tags and junk blocks
  text_clean <- gsub("<[^>]+>", "", text_clean)
  text_clean <- gsub("<\\?xml[\\s\\S]*?\\?>", "", text_clean, perl = TRUE)
  text_clean <- gsub("<xbrl[\\s\\S]*?>[\\s\\S]*?</xbrl>", "", text_clean, perl = TRUE)
  # Fix known legacy Windows characters
  text_clean <- str_replace_all(text_clean, "[\u0092]", "'")
  text_clean <- str_replace_all(text_clean, "[\u0093\u0094]", "\"")
  text_clean <- str_replace_all(text_clean, "[\u0095•]", ". ")
  text_clean <- str_replace_all(text_clean, "[\u0096\u0097]", "-")
  # Remove control characters safely
  text_clean <- gsub("[[:cntrl:]]", " ", text_clean)
  # Remove bullets
  #text_clean <- str_replace_all(text_clean, "•", ". ")
  # Remove lines that look like tables (e.g., multiple numbers)
  lines <- str_split(text_clean, "\n")[[1]]
  lines <- keep(lines, ~ !str_detect(.x, "^\\s*\\d{2,}.*\\d{2,}"))
  text_clean <- str_c(lines, collapse = " ")
  # Normalize whitespace
  text_clean <- str_replace_all(text_clean, "[\\r\\n\\t]+", " ")
  text_clean <- str_squish(text_clean)
  return(text_clean)
}

# Overwriting the original, uncleaned business sections 
text_data <- text_data %>%
  mutate(business_section = map_chr(business_section, clean_text)) 

write_rds(text_data, "text_data.Rds")

raw_word_counts <- str_count(text_data$business_section, "\\S+")
median(raw_word_counts)
sum(raw_word_counts)

# To avoid confusion with topic modeling objects
TQ_text_data <- text_data

TQ_text_data$business_section <- as.character(TQ_text_data$business_section)  %>%
  tolower() %>%
  {gsub("(\"| |\\$)-+\\.-+"," DIGIT", .)} %>%      # Find digits
  gsub("(?i)\\b(?:[0-9]{1,2}:)?[0-9]{1,2}\\s*am\\b", " ", ., perl = TRUE) %>%  # Remove time AM
  gsub("(?i)\\b(?:[0-9]{1,2}:)?[0-9]{1,2}\\s*pm\\b", " ", ., perl = TRUE) %>%  # Remove time PM
  {gsub("-+:-+","TIME", .)} %>%                     # Find general time
  {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," CURRENCYVALUE ", .)} %>%   # Find major currency values incl. dollar
  {gsub("\\b[0-9]+(?:\\.[0-9]+)?\\s*%\\b", " PERCENTAGE ", .)} %>%  # Find percentages “25%” or “3.5 %”
  {gsub("\\bco\\s*2\\b", " <<CO2>> ", ., ignore.case = TRUE)} %>%   # Replacing variants of CO2 with placeholder CO2
  {gsub("[0-9]*[\\.,]*[0-9]+"," DIGIT ", .)} %>%   # Find remaining digits
  {gsub("-"," ", .)} %>%                            # Remove all -
  {gsub("&"," and ", .)} %>%                        # Replace all & by and
  {gsub("\"+"," ", .)} %>%                          # Remove all "
  {gsub("\\|+"," ", .)} %>%                         # Remove all |
  {gsub("_+"," ", .)} %>%                           # Remove all _
  {gsub(";+"," ", .)} %>%                           # Remove excess ;
  {gsub(" +"," ", .)} %>%                           # Remove excess spaces
  {gsub("\\.+","\\.", .)} %>%                       # Remove excess .
  {gsub(";+", " ", .)} %>%                          # Remove excess semicolons
  {gsub("\\\\+", "", .)} %>%                        # Remove all backslashes
  {gsub(" +", " ", .)} %>%                          # Collapse multiple spaces
  {gsub("\\.+", "\\.", .)} %>%                      # Collapse multiple periods
  {gsub("\\(\\s*[0-9]{1,3}\\s*\\)", " ", .)} %>%    # Remove footnote markers “(1)”, “(23)”
  {gsub("\\[\\s*[0-9]{1,3}\\s*\\]", " ", .)} %>%    # Remove bracketed footnotes “[1]”, “[45]”
  {gsub("(?i)\\bexhibit\\s+[0-9a-z]+\\b", " ", ., perl = TRUE)} %>%           # Remove “Exhibit 5A”
  {gsub("(?i)^\\s*item\\s+\\.", " ", ., perl = TRUE)} %>%                     # Remove “Item” headings
  {gsub("(?i)\\btable\\s*[0-9]+(?:\\.[0-9]+)?\\b", " ", ., perl = TRUE)} %>%  # Remove “Table 3” or “Table 10.2”
  {gsub("(?i)page\\s*[0-9]+\\s*of\\s*[0-9]+", " ", ., perl = TRUE)} %>%       # Remove “Page 23 of 120”
  {gsub("(?m)^\\s*[0-9]+\\s*$", " ", ., perl = TRUE)} %>%                     # Remove standalone page numbers
  {gsub("\\b(?:[ivxlcdm]{1,5})\\b", " ", ., perl = TRUE)} %>%                 # Remove Roman numerals “xviii”
  {gsub("[®™]", " ", .)} %>%                                                  # Remove trademark symbols
  {gsub("https?://[^\\s]+", " ", .)} %>%                                      # Remove URLs
  {gsub("[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}", " ", .)} %>%                # Remove email addresses
  {gsub("<<CO2>>", "CO2", ., fixed = TRUE)}                                   # Restore original CO2

# Remove placeholders with disproportional frequency that add no semantic value
remove_words <- c("digit", "currencyvalu", "percent", "percent_percent",
                  "currencyvalu_million", "currencyvalu_currencyvalu", 
                  "digit_digit")

# Create a regex pattern from the word list
pattern <- paste0("\\b(", paste(remove_words, collapse = "|"), ")\\b")  

# Remove the words
TQ_text_data$business_section <- gsub(pattern, "", TQ_text_data$business_section, ignore.case = TRUE)

# Remove extra spaces
TQ_text_data$business_section <- gsub("\\s+", " ", TQ_text_data$business_section)
TQ_text_data$business_section <- trimws(TQ_text_data$business_section)

write_rds(TQ_text_data, "TQ_text_data.Rds")
TQ_text_data <- readRDS("TQ_text_data.Rds")

# First visual inspection after heavy cleaning
TQ_text_data$business_section[[1]]

# DEBUGGING: FALSE when nothing is flagged, TRUE if something is.
sample_indices <- sample(nrow(TQ_text_data), 5)
subset_debug <- TQ_text_data[sample_indices, ]

leftover_flags <- subset_debug %>%    #Should be updated
  mutate(
    has_stray_A            = str_detect(business_section, "Â"),
    has_html_tag           = str_detect(business_section, "<[^>]+>"),
    has_entity             = str_detect(business_section, "&[a-zA-Z]+;"), #For HTML entities
    has_broken_unicode     = str_detect(business_section, "[\u0092\u0093\u0094\u0095\u0096\u0097]"),
    has_literal_bullet     = str_detect(business_section, "•"),
    has_control_chars      = str_detect(business_section, "[[:cntrl:]]"),
    has_table_pattern      = str_detect(business_section, "\\b\\d{2,}\\b\\s+\\d{2,}\\b"),
  ) %>%
  # Flag any row with at least one issue
  mutate(any_issue = has_stray_A |
           has_html_tag |
           has_entity |
           has_broken_unicode |
           has_literal_bullet |
           has_control_chars |
           has_table_pattern )

# Show only the rows where something slipped through
problem_rows <- leftover_flags %>%
  filter(any_issue) %>%
  select(
    ticker, year,
    has_stray_A, has_html_tag, has_entity,
    has_broken_unicode, has_literal_bullet,
    has_control_chars, has_table_pattern
  )

print(problem_rows)

# Return TRUE if any sample row has an issue; FALSE otherwise
any(problem_rows$any_issue)

##### Phase 3: Loading, Tokenizing, and Stemming CS Dictionary #################
# Goal: Loading and stemming dictionary

# For consistency in CS talk, V22_sup from topic modeling is applied
V22_sup <- read_csv("V22_sup.csv", col_names=FALSE) # Already contains V22_terms 
V22_sup <- V22_sup$X1

# Stem supplementary seed words for consistency with text data 
V22_sup <- sapply(
  strsplit(V22_sup, "_"),
  function(words) paste(wordStem(words, "porter"), collapse = "_")
)

##### Phase 4: Creating Identifier doc_id and DFM ##############################
# Goal: Creating doc_id and DFM and tokenize and stem DFM.

# Creation of doc_id as identifier variable
TQ_text_data <- TQ_text_data %>%
  mutate(doc_id = paste0(ticker, "_", year))

# Tokenize and stem. Vectorized for speed
TQ_tokens <- quanteda::tokens(TQ_text_data$business_section,
                          remove_punct   = TRUE,
                          remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "porter") %>%
  tokens_remove(pattern = stopword_list) %>%
  tokens_ngrams(n = 1:2)

# Build DFM and lookup CS terms. DFM much faster than looping through docs
TQ_tokens_dfm <- dfm(TQ_tokens) %>%
  dfm_remove(pattern = c("digit", "currencyvalu", "currencyvalu_million",
                         "currencyvalu_currencyvalu", "numb_numb", "percent",
                         "percent_percent", "currencyvalu_billion",
                         "billion_currencyvalu")) 

# Only get unigrams and V22 bigrams. Excluding bigrams that would blow up freq.
TQ_keep_feats <- featnames(TQ_tokens_dfm) %>%
  (\(f) !grepl("_", f) | f %in% gsub(" ", "_", V22_sup))() 
TQ_tokens_dfm <- TQ_tokens_dfm[, TQ_keep_feats]

# Isolating CS N-grams
V22_sup_dict    <- dictionary(list(V22_sup = V22_sup))
TQ_matched_dfm  <- dfm_select(TQ_tokens_dfm, pattern = V22_sup, 
                              selection = "keep")

write_rds(TQ_tokens_dfm,  "phase4_TQ_tokens_dfm.Rds")
write_rds(TQ_matched_dfm, "phase4_TQ_matched_dfm.Rds")

##### Phase 5: Compute TalkQuantity ############################################
# Goal: Count matches with dictionary per doc, divide by total and scale 0-100

#TQ_tokens_dfm <- readRDS("phase4_TQ_tokens_dfm.Rds")
#TQ_matched_dfm <- readRDS("phase4_TQ_matched_dfm.Rds")

# CS-related word count per document      (numerator)
TQ_cs_count <- rowSums(TQ_matched_dfm)

# Total word count per document           (denominator)
TQ_total_count <- rowSums(TQ_tokens_dfm)

# Create per-document TalkQuantity
TalkQuantity_df <- TQ_text_data %>%
  transmute(
    doc_id,
    ticker,
    year,
    TQ_total_count = TQ_total_count,
    TQ_cs_count    = TQ_cs_count,
    TalkQuantity   = if_else(
      TQ_total_count > 0,
      TQ_cs_count / TQ_total_count * 100,
      0
    )
  )


# First glance
head(TalkQuantity_df)
summary(TalkQuantity_df) 

write_csv(TalkQuantity_df, "phase5_TalkQuantity_df_27.07.csv")

##### End of Script ############################################################





