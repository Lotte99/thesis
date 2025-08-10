# Disclaimer: ChatGPTo4-mini-high helped me debug and improve the script. 
# It also helped structuring it and making it visibly more pleasing.

# This R script performs the following steps:
# 1. Raw Text Cleaning
# 2. Sentence Segmentation
# 3. CS Dictionary Filtering
# 4. Preprocessing for Topic Modeling
# 5. Tokenization and Document-Term Matrix Creation
# 6. LDA Topic Modeling, Perplexity-Based K Selection 
# 7. Extraction of Document-Topic Distributions

# Note:
# - Intermediate RDS files enable restarting from any phase. Uncomment to use.
# - Script is designed as iterative process. For same final results as in the
#   thesis, some parts in the code need to be skipped. 

##### 0. Setup: Load Required Libraries ########################################

library(cluster)
library(clValid)
library(dbscan)
library(dplyr)
library(fpc)
library(furrr)
library(ggplot2)
library(irlba)
library(Matrix)
library(proxy)
library(purrr)
library(readxl) 
library(reshape2)
library(slam)
library(SnowballC)
library(stm)
library(stringr)
library(stringi)
library(seededlda)
library(textclean)
library(textmineR)
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
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬Å“|Ã¢â‚¬Å”", "\"") # smart double-quotes
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬â„¢", "'")       # smart apostrophe
  text_clean <- str_replace_all(text_clean, "Ã¢â‚¬â€œ|Ã¢â‚¬â€\u0092", "-") # en-dash/em-dash
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

# Visual inspection
text_data$business_section[[1]]

#write_rds(text_data, "phase2_data_cleaned.Rds")

##### Phase 3: Sentence Segmentation ###########################################
# Goal: Split each cleaned document into sentences, while preserving document ID 

#text_data <- readRDS("phase2_data_cleaned.Rds")

# Unnest (segment) the sentences. Each row is a sentence
df_sentences <- text_data %>%
  transmute(
    doc_id     = paste0(ticker, "_", year),
    ticker = ticker,
    year = year,
    business_section = business_section
  ) %>%
  mutate(sentences = map(business_section, ~ tokenize_sentences(.x)[[1]])) %>%
  unnest(sentences)

# Count sentences per document:
sent_count <- df_sentences %>%
  group_by(doc_id) %>%
  summarise(num_sentences = n(), .groups = "drop")

# Find all docs with fewer than 10 sentences:
few_sentence_docs <- sent_count %>%
  filter(num_sentences < 10) %>%
  pull(doc_id)

inspect_docs <- text_data %>%
  mutate(doc_id = paste0(ticker, "_", year)) %>%
  filter(doc_id %in% few_sentence_docs) %>%
  dplyr::select(ticker, year, doc_id, business_section)
length(inspect_docs$doc_id)
head(inspect_docs$business_section)
head(inspect_docs$doc_id)
inspect_docs$business_section[7:15]

# Average number of sentences per document
sent_count <- df_sentences %>%
  group_by(doc_id) %>%
  summarise(num_sentences = n())
summary(sent_count$num_sentences)

#write_rds(df_sentences, "phase3_sentences.Rds")

##### Phase 4: CS Dictionary Filtering ########################################
# Goal: Retain only sentences containing CS-related terms, loading dictionary

#df_sentences <- readRDS("phase3_sentences.Rds")

# Sentences will be cleaned further. The original documents will not.
# From the text analytics course (FEM11154) with some adjustments & additions
df_sentences$sentences[40:60] # Random subset to check pre-cleaned sentences.
df_sentences$sentences <- as.character(df_sentences$sentences)  %>%
  tolower() %>%
  {gsub("(\"| |\\$)-+\\.-+"," NUMBER", .)} %>%      # Find numbers
  {gsub("([0-9]+:)*[0-9]+ *am"," TIME_AM", .)} %>%  # Find time AM
  {gsub("([0-9]+:)*[0-9]+ *pm"," TIME_PM", .)} %>%  # Find time PM
  {gsub("-+:-+","TIME", .)} %>%                     # Find general time
  # Finding major currency values incl. dollar
  {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," CURRENCYVALUE ", .)} %>%   
  # Finding percentages “25%” or “3.5 %”
  {gsub("\\b[0-9]+(?:\\.[0-9]+)?\\s*%\\b", " PERCENTAGE ", .)} %>%  
  # Replacing variants of CO2 with placeholder CO2
  {gsub("\\bco\\s*2\\b", " <<CO2>> ", ., ignore.case = TRUE)} %>%   
  {gsub("[0-9]*[\\.,]*[0-9]+"," NUMBER ", .)} %>%   # Find remaining numbers
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
  # Remove footnote markers “(1)”, “(23)”
  {gsub("\\(\\s*[0-9]{1,3}\\s*\\)", " ", .)} %>%   
  # Remove bracketed footnotes “[1]”, “[45]”
  {gsub("\\[\\s*[0-9]{1,3}\\s*\\]", " ", .)} %>%   
  # Remove “Exhibit 5A”
  {gsub("(?i)\\bexhibit\\s+[0-9a-z]+\\b", " ", ., perl = TRUE)} %>%    
  # Remove “Item 1.” headings
  {gsub("(?i)^\\s*item\\s+[0-9a-z]+\\.", " ", ., perl = TRUE)} %>%   
  # Remove “Table 3” or “Table 10.2”
  {gsub("(?i)\\btable\\s*[0-9]+(?:\\.[0-9]+)?\\b", " ", ., perl = TRUE)} %>%  
  # Remove “Page 23 of 120”
  {gsub("(?i)page\\s*[0-9]+\\s*of\\s*[0-9]+", " ", ., perl = TRUE)} %>%      
  {gsub("(?m)^\\s*[0-9]+\\s*$", " ", ., perl = TRUE)} %>% # Remove page numbers
  # Remove Roman numerals “xviii”
  {gsub("\\b(?:[ivxlcdm]{1,5})\\b", " ", ., perl = TRUE)} %>%                 
  {gsub("[®™]", " ", .)} %>%                         # Remove trademark symbols
  {gsub("https?://[^\\s]+", " ", .)} %>%             # Remove URLs
  {gsub("[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}", " ", .)} %>% # Remove email 
  {gsub("<<CO2>>", "CO2", ., fixed = TRUE)}          # Restore original CO2
  
# First visual on same random sample to check the difference after cleaning
df_sentences$sentences[40:60]

#write_rds(df_sentences, "phase4_df_sentences.Rds")

### For identical sentence classification as used in thesis, skip to line 271.

# Load dictionary with column name "word"
V22_dictionary <- read_csv("V22_dictionary.csv")

# Extract terms as a character vector, also to use for word freq. calculation
V22_terms <- V22_dictionary$word %>%
  str_trim() %>%
  discard(~ .x == "") %>%
  str_to_lower() %>%
  str_replace_all(" ", "_") # Replaces blank space in bigrams with _

# Stem dictionary for consistency with text data and create quanteda object
V22_dictionary <- dictionary(list(
  CS = sapply(
    strsplit(V22_terms, "_"),
    function(words) paste(wordStem(words, "porter"), collapse = "_")
  )
))

# Further preprocessing of the dictionary was done manually as it required  
# minimal changes. The thesis details these manual changes.

# Tokenize, stem, and form n-grams for all sentences as vectorized for speed
sentence_tokens <- tokens(df_sentences$sentences,
                          remove_punct   = TRUE,
                          remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "porter") %>%
  tokens_ngrams(n = 1:2)

# Build dfm and lookup CS terms
sentence_dfm <- dfm(sentence_tokens)
matched_dfm  <- dfm_lookup(sentence_dfm, dictionary = V22_dictionary)

# Add CS flag to df_sentences: TRUE if any CS match
df_sentences <- df_sentences %>%
  mutate(cs_flag = ntoken(matched_dfm) > 0)

# Subset and save CS-related sentences
cs_sentences <- df_sentences %>%
  filter(cs_flag) %>%
  select(doc_id, ticker, year, sentences)

# Descriptive
cs_prop <- df_sentences %>%
  group_by(doc_id) %>%
  summarise(
    total_sent = n(),
    cs_sent   = sum(cs_flag),
    proportion = cs_sent / total_sent,
    .groups    = "drop"
  )
summary(cs_prop$proportion)

#write_rds(cs_sentences, "phase4_cs_sentences.Rds")

### This is for reclassification with the supplemented lexicon
sentence_tokens <- quanteda::tokens(df_sentences$sentences,
                          remove_punct   = TRUE,
                          remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "porter") %>%
  tokens_ngrams(n = 1:2, concatenator = "_")  # Underscore to match with V22_sup

# From phase 7 and Talk Quantity.R
V22_sup <- read_csv("V22_sup.csv", col_names=FALSE) # Already contains V22_terms 
V22_sup <- V22_sup$X1

# From phase 7 and Talk Quantity.R
V22_sup <- sapply(
  strsplit(V22_sup, "_"),
  function(words) paste(wordStem(words, "porter"), collapse = "_")
)

V22_sup_dict <- dictionary(list(cs_terms = V22_sup))

sentence_dfm <- dfm(sentence_tokens)
matched_dfm  <- dfm_lookup(sentence_dfm, dictionary = V22_sup_dict)

df_sentences <- df_sentences %>%
  mutate(cs_flag = ntoken(matched_dfm) > 0)

cs_sentences <- df_sentences %>%
  filter(cs_flag) %>%
  dplyr::select(doc_id, ticker, year, sentences)

### End of reclassification. Code continues as usual again

##### Phase 5: Further pre-processing & word frequency #########################
# Goal: Preprocessing the CS sentences corpus. E.g. collapsing back to docs

# Clean and stem CS-related sentences for quanteda
cs_sentences <- cs_sentences %>%
  mutate(sentence_clean = sentences %>%
           str_to_lower() %>%
           str_remove_all("[[:punct:]]") %>%
           textstem::stem_strings() %>%
           str_squish())

# Collapse back into documents for quanteda
cs_docs <- cs_sentences %>%
  group_by(doc_id, ticker, year) %>%
  summarise(text = str_c(sentence_clean, collapse = " "), .groups = "drop")

# Create quanteda corpus
cs_corpus <- corpus(cs_docs, text_field = "text", docid_field = "doc_id")

# Tokenize to unigrams and bigrams, remove stopwords, and apply stemming
cs_tokens <- quanteda::tokens(cs_corpus,
                     remove_punct   = TRUE,
                     remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "porter") %>%
  tokens_remove(pattern = stopword_list) %>%
  tokens_ngrams(n = 1:2)

# Create DFM and remove unwanted frequent tokens
cs_dfm <- dfm(cs_tokens) %>%
  dfm_remove(pattern = c("digit", "benefit", "currencyvalu", "percent_percent",
                         "currencyvalu_million", "currencyvalu_currencyvalu", 
                         "digit_digit", "percent", "currencyvalu_billion", 
                         "billion_currencyvalu")) %>%
  # Removing tokens with low freq. counts that add noise (after inspection)
  dfm_trim(min_termfreq = 500, termfreq_type = "count") 

# Only get unigrams and V22 bigrams
keep_feats <- featnames(cs_dfm) %>%
  (\(f) !grepl("_", f) | f %in% gsub(" ", "_", V22_sup))() # Change to term for
cs_dfm <- cs_dfm[, keep_feats]                         # initial classification

# Convert to topicmodels format for LDA
dtm_LDA <- convert(cs_dfm, to = "topicmodels")

#write_rds(cs_tokens, "phase5_cs_tokens.Rds")
#write_rds(cs_dfm,    "phase5_cs_dfm.Rds")
#write_rds(dtm_LDA,    "phase5_dtm_LDA.Rds")

##### Phase 6: CS Word Frequencies #############################################
# Goal: Gain better understanding of the CS-only corpus
# Note: Top 100 words is removed in phase 7. Thesis reran this phase for table 2

# Median number of CS sentences per document
cs_sent_per_doc <- cs_sentences %>%
  count(doc_id, ticker, year, name = "n_sentences")
median(cs_sent_per_doc$n_sentences)

# Median number of words per CS sentence
cs_sentences <- cs_sentences %>%
  mutate(n_words = str_count(sentence_clean, "\\S+"))

cs_words_per_doc <- cs_sentences %>%
  group_by(doc_id, ticker, year) %>%
  summarise(n_words = sum(n_words), .groups = "drop")
median(cs_words_per_doc$n_words)

# Get the number of unique words for CS sentences
unique_df <- tibble(word = unlist(cs_tokens))
unique_df %>% summarise(n_unique = n_distinct(word),
                        total_words = n())

# Get top 1000 N-grams
word_freq <- textstat_frequency(cs_dfm)
top_1000 <- word_freq %>%
  slice_max(frequency, n = 1000) %>%
  dplyr::select(feature, frequency) 
write_csv(top_1000, "top1000_words.csv")

# Calculating how often a word occurs in a sentence
term_count <- col_sums(dtm_LDA > 0)
term_perc <- term_count/nrow(dtm_LDA)

# Get word frequencies
word_freq <- textstat_frequency(cs_dfm)

# Save total word count before filtering
total_word_count <- sum(word_freq$frequency)

# Compute relative frequencies
word_freq <- word_freq %>%
  mutate(relative = frequency / total_word_count)

# Create general frequency table
general_word_freq_table <- bind_rows(
  word_freq %>% arrange(desc(frequency)) %>% slice_head(n = 10),
  word_freq %>% arrange(frequency)         %>% slice_head(n = 10)
) %>%
  dplyr::select(word = feature, absolute = frequency, relative)

general_word_freq_table 

# Extract CS words based on dictionary
cs_word_freq <- word_freq %>%
  filter(feature %in% V22_sup)

# Create CS frequency table
cs_word_freq_table <- bind_rows(
  cs_word_freq %>% arrange(desc(frequency)) %>% slice_head(n = 10),
  cs_word_freq %>% arrange(frequency)         %>% slice_head(n = 10)
) %>%
  dplyr::select(word = feature, absolute = frequency, relative)

cs_word_freq_table

#write_csv(cs_word_freq_table, "cs_word_freq_table.csv")
#write_rds(cs_docs, "phase6_cs_collapsed_docs.Rds")

##### Phase 7: Cosine Similarity of Word Embeddings ############################
# Goal: Add cosine similar terms combined with human evaluation to CS dictionary

#------------------------
# cs_dfm vs. V22 matching
#------------------------
tfidf      <- dfm_tfidf(cs_dfm)
tfidf_seed <- dfm_select(tfidf, pattern = V22_terms, selection = "keep")
tfidf_mat  <- as.matrix(tfidf_seed)
tfidf_t    <- t(tfidf_seed) # Transposed

embeddings <- irlba(term_mat, nv = 50)$u
rownames(embeddings) <- rownames(term_mat)

# Identify embeddings of V22_terms that match exactly:
matched_terms <- intersect(V22_terms, rownames(embeddings))
emb_seed <- embeddings[matched_terms, , drop = FALSE]

# Compute cosine similarity between seed terms and all terms:
cos_sim <- simil(x = embeddings, y = emb_seed, method = "cosine")

# Convert similarity object to matrix:
cos_sim_mat <- as.matrix(cos_sim)

# Identify closest terms for each seed term (excluding itself):
find_closest_terms <- function(seed_term, sim_mat, n = 10) {
  sim_values <- sim_mat[, seed_term]
  sim_values <- sort(sim_values, decreasing = TRUE)
  sim_values <- sim_values[names(sim_values) != seed_term]
  head(sim_values, n)
}

# Example: find top-10 closest words for each seed term:
lapply(colnames(cos_sim_mat), find_closest_terms, sim_mat = cos_sim_mat, n = 10)

#------------------------
# Global Vector (GloVe)
#------------------------

# glove.6B.100d.txt was downloaded from https://nlp.stanford.edu/projects/glove/
glove_lines <- readLines("glove.6B.100d.txt")
glove_list <- strsplit(glove_lines, " ")

# Convert to named matrix
words <- sapply(glove_list, `[[`, 1)
vecs <- do.call(rbind, lapply(glove_list, function(x) as.numeric(x[-1])))
rownames(vecs) <- words
glove <- vecs  # now your glove matrix


# Create matrix of only words in both cs_dfm and GloVe
target_mat <- as.matrix(glove[cs_vocab_in_glove, ])
rownames(target_mat) <- cs_vocab_in_glove  # ensure rownames are retained

similar_words <- list()

for (term in matched) {
  if (term %in% rownames(glove)) {
    term_vec <- glove[term, , drop = FALSE]
    
    # Compute cosine similarity between this seed word and all DFM terms
    sim <- simil(target_mat, term_vec, method = "cosine", by_rows = TRUE)
    
    if (!is.null(sim)) {
      sim_vec <- as.numeric(sim)
      names(sim_vec) <- rownames(target_mat)
      
      # Top 10 most similar terms
      top_terms <- names(sort(sim_vec, decreasing = TRUE))[1:10]
      similar_words[[term]] <- top_terms
    }
  }
}

similar_words

# words are manually chosen to supplement the dictionary

# How many supplement words are in the DFM
V22_sup <- read_csv("V22_sup.csv", col_names=FALSE) # Already contains V22_terms 
V22_sup <- V22_sup$X1

# Stem supplementary seed words for consistency with text data 
V22_sup <- sapply(
  strsplit(V22_sup, "_"),
  function(words) paste(wordStem(words, "porter"), collapse = "_")
)
matched_supplement <- intersect(V22_sup, featnames(cs_dfm))
length(matched_supplement) # 178 terms match!

# Create DFM and remove unwanted frequent tokens
cs_dfm <- dfm(cs_tokens) %>%
  dfm_remove(pattern = c("digit", "benefit", "currencyvalu", 
                         "currencyvalu_million", "currencyvalu_currencyvalu", 
                         "digit_digit")) 

# Only get unigrams and V22 bigrams
keep_feats <- featnames(cs_dfm) %>%
  (\(f) !grepl("_", f) | f %in% gsub(" ", "_", V22_sup))() # V22_sup, not _terms
cs_dfm <- cs_dfm[, keep_feats]

# Remove top100 words after visual inspection of top 100 words in later phases
freqs <- textstat_frequency(cs_dfm)
top100 <- head(freqs$feature, 100)

# Keep N-grams in V22_sup
to_remove <- setdiff(top100, V22_sup)
cs_dfm <- dfm_remove(cs_dfm, pattern = to_remove)

# Convert to topicmodels format for LDA
dtm_LDA <- convert(cs_dfm, to = "topicmodels")

### Update frequency tables from phase 5 for the final tables & update dtm_LDA!

##### Phase 7: Clustering of Seed Terms for Guided LDA #########################
# Goal: Too many seed terms, so they need to be clustered to then be assigned
#       to a topic for guided aka seeded LDA
# Note: This was merely tried but yielded poor silhouette and Dunn indices.

# Recreating tfidf_seed etc. based on the new seed terms in V22_sup 
tfidf_seed <- dfm_select(tfidf, pattern = V22_sup, selection = "keep")
tfidf_mat  <- as.matrix(tfidf_seed)
tfidf_t    <- t(tfidf_seed)

# Creating distance matrix based on cosine similarity
sim      <- textstat_simil(tfidf_seed, margin = "features", method = "cosine")
distmat  <- as.dist(1 - sim)

# Ward's linkage hierarchical clustering
hc <- hclust(distmat, method = "ward.D2")
plot(hc, main = "Ward Dendrogram of Seed Terms", xlab = "", sub = "", cex = 0.6)

# For loop over k grid
k_ward <- c(10, 20, 30, 40, 50)
results <- data.frame(k = k_ward, silhouette = NA, dunn = NA)

for (i in seq_along(k_ward)) {
  k <- k_ward[i]
  cluster_assignments <- cutree(hc, k = k)
  
  sil <- silhouette(cluster_assignments, distmat)
  avg_sil <- mean(sil[, 3])
  
  # Dunn index needs the actual matrix, not dist object
  dunn_idx <- clValid::dunn(distance = NULL, Data = tfidf_mat, 
                            clusters = cluster_assignments)
  
  results$silhouette[i] <- avg_sil
  results$dunn[i] <- dunn_idx
}

print(results)

# HDBSCAN clustering
emb <- irlba(as.matrix(tfidf_t), nv = 20)$u # Embedding terms (rows)
hdb <- hdbscan(emb, minPts = 3)             # HDBSCAN on term embeddings

# Filter out noise (cluster 0) and singleton clusters:
clust   <- hdb$cluster
keep1   <- which(clust != 0)
sizes   <- table(clust[keep1])
goodLab <- as.integer(names(sizes)[sizes > 1])
keep    <- keep1[clust[keep1] %in% goodLab]

# Silhouette score calculation
dmat <- dist(emb[keep, ])
sil  <- silhouette(clust[keep], dmat)

# Average silhouette width
avg_sil <- mean(sil[, "sil_width"])
cat("Average silhouette width:", round(avg_sil, 4), "\n")

# Additional validity metrics via fpc
cs <- cluster.stats(d = dmat, clustering = clust[keep])

cat("Dunn index:", round(cs$dunn, 4), "\n")

# Check if av.within is numeric before printing
if (is.numeric(cs$av.within)) {
  cat("Avg within-cluster dist:", round(cs$av.within, 4), "\n")
} else {
  cat("Avg within-cluster dist is non-numeric or unavailable.\n")
}

# K‐means clustering
set.seed(1939)
km   <- kmeans(emb, centers = 30, nstart = 25)

# Silhouette score for K-means
sil  <- silhouette(km$cluster, distmat)
mean(sil[, "sil_width"])   
plot(sil)                  

# After visually choosing height ‘h’, cut into K clusters:
hcut    <- 40        # Set based on dendrogram
clusters<- cutree(hc, h = hcut)
table(clusters)     

# Build list of seed-blocks:
seed_blocks <- split(names(clusters), clusters)

##### Phase 8: Training and Hyperparameter Tuning ##############################
# Goal: Identify the optimal number of topics (K) by perplexity & CV coherence &
#       identify best value for other hyperparameters

# Train & Test split
set.seed(1942)
train_indices <- sample(seq_len(nrow(cs_dfm)), size = 0.8 * nrow(cs_dfm))
dtm_train <- cs_dfm[train_indices, ]
dtm_test  <- cs_dfm[-train_indices, ]

# Grid
K_vals      <- seq(20, 50, by = 10) 
alpha_vals  <- c(0.01, 0.1, 0.5, 1)
weight_vals <- c(0.1, 0.5, 1)
grid        <- expand_grid(K = K_vals, alpha = alpha_vals, weight = weight_vals)

# Grid loop for K and alpha
results <- vector("list", length = nrow(grid))

# First creating CalcProbCoherence() since seededlda doesn't support it natively
CalcProbCoherence <- function(phi, dtm, M = 10) {
  if (!inherits(dtm, "dfm")) stop("dtm must be a quanteda dfm object")
  
  # Making the dfm binary
  dtm_bin <- dtm
  dtm_bin@x[dtm_bin@x > 0] <- 1
  
  # Computing document frequency per term
  term_doc_freq <- colSums(dtm_bin)
  
  # Getting top M terms per topic (phi is topic x term matrix)
  top_terms <- apply(phi, 1, function(row) {
    names(sort(row, decreasing = TRUE))[1:M]
  })
  
  # Coherence score for each topic
  coherence_scores <- numeric(nrow(phi))
  for (k in seq_len(nrow(phi))) {
    terms_k <- top_terms[, k]
    score <- 0
    for (m in 2:M) {
      for (l in 1:(m - 1)) {
        term_m <- terms_k[m]
        term_l <- terms_k[l]
        
        # Skip if either term is missing in dtm
        if (!(term_m %in% colnames(dtm_bin)) ||
            !(term_l %in% colnames(dtm_bin))) next
        
        df_l <- term_doc_freq[term_l]
        if (is.na(df_l) || df_l == 0) next
        
        df_ml <- sum(dtm_bin[, term_m] & dtm_bin[, term_l])
        score <- score + log((df_ml + 1) / df_l)
      }
    }
    coherence_scores[k] <- score
  }
  return(coherence_scores)
}


# Start the clock and start training (takes about 2.5 hours)
start_time <- Sys.time()

for (i in seq_len(nrow(grid))) {
  K      <- grid$K[i]
  alpha  <- grid$alpha[i]
  weight <- grid$weight[i]
  
  message(">>> Running model for K = ", K, ", alpha = ", alpha)
  
  # Loading corresponding seed file (e.g., "seeds_k20.csv")
  seed_file <- paste0("seed_k", K, ".csv")
  seed_data <- read_csv(seed_file, show_col_types = FALSE)
  
  # Converting to quanteda dictionary
  seed_dict <- split(seed_data$term, seed_data$topic)
  dict_q    <- dictionary(seed_dict)
  
  # Fitting SeededLDA
  model <- textmodel_seededlda(
    x          = dtm_train,
    dictionary = dict_q,
    residual   = 2,              # For every K, 2 unseeded topics
    weight     = weight,         
    max_iter   = 300,
    alpha      = alpha,
    beta       = 1.0,            # From 0.1 to 1.0 for smoother distributions
    verbose    = TRUE
  )
  
  # Evaluate
  perp <- seededlda::perplexity(model, newdata = dtm_test)
  phi  <- model$phi # The probability of word j in topic k
  
  # Replace with your real coherence function
  cv      <- CalcProbCoherence(phi = model$phi, dtm = dtm_train, M = 10)
  cv_mean <- mean(cv)
  
  results[[i]] <- tibble(K = K, alpha = alpha, weight = weight,
                         Perplexity = perp, Mean_CV = cv_mean)
}

end_time <- Sys.time()
print(end_time - start_time)

# Results
results_df <- bind_rows(results)
results_df$Scaled_CV <- scales::rescale(results_df$Mean_CV, to = c(0, 1))
print(results_df, n = 48)

write_rds(results_df, "phase8_seededLDA_grid_results.rds")

# Find best
best <- results_df %>%
  arrange(Perplexity, desc(Mean_CV)) %>%
  slice(1)
print(best)

##### Phase x: Fitting the Final SeededLDA Model ###############################
# Goal: Get results of the final LDA model for the downstream regression models

# Loading corresponding seed file with pre assigned terms to topics
seed_file <- "seed_k20.csv"
seed_data <- read_csv(seed_file, show_col_types = FALSE)

# Converting them to dictionary
seed_dict <- split(seed_data$term, seed_data$topic)
dict_q    <- dictionary(seed_dict)

# Fitting Final SeededLDA (hyperp. are hard-coded based on grid search results)
set.seed(1942)                                  
final_lda <- textmodel_seededlda(
  x          = cs_dfm,       
  dictionary = dict_q,
  residual   = 2,            # 2 unseeded topics based on Ren et al. (2020)
  weight     = 0.1,          # Soft guidance based on grid search
  max_iter   = 2000,         # Mimimum for actual fitting of the model
  alpha      = 0.1,         # Based on grid results
  beta       = 1.0,          # Default smoothing = 0.1: gave skewed distribution
  verbose    = TRUE
)

#saveRDS(results,   "phase8_hyper_grid_results.rds")
#saveRDS(final_lda, "phase8_final_LDA_model.rds") 

##### Phase 9: Extract Topic-Word Distributions and Top Terms ##################
# Goal: Get top words for each topic to analyze interpretability

terms_per_topic <- seededlda::terms(final_lda, n = 10)
terms_per_topic 

# Get topic-term dimensions
phi <- final_lda$phi 
vocab <- colnames(phi)

# Convert phi to tidy format
terms_per_topic <- as_tibble(phi) %>%
  mutate(topic = row_number()) %>%
  pivot_longer(
    cols = -topic,
    names_to = "term",
    values_to = "beta"
  )

# Filter top N terms per topic 
top_n <- 10
terms_per_topic_top <- terms_per_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = top_n, with_ties = FALSE) %>%
  ungroup()
# print(terms_per_topic_top, n=200) to get the betas as in table 3 in the thesis

# Maximum topic probability
maxProb <- apply(final_lda$theta, 1, max)
hist(maxProb, main="Assignment to topics", xlab="Maximum topic probability")

# Topic means
col_means(final_lda$theta)

# Create label names
labelnames <- c(
  "1. Real Estate & Leasing",
  "2. Renewable Energy & Fossil Fuels",
  "3. EPA Compliance & Clean Energy",
  "4. Public Health & Care",
  "5. Recycling & Consumer Goods",
  "6. Pension & Financial Terms",
  "7. Air Pollution & Emissions", 
  "8. Supporting Communities", 
  "9. Education & Institutions",
  "10. Banking & Credit",
  "11. IT Integration",
  "12. Finance & Equity Markets",
  "13. Ethics & Governance",
  "14. Contracts & Agreements",
  "15. Corruption & Risks",
  "16. Hazardous Materials & Construction",
  "17. Environmental Law & Waste",
  "18. Environmental Protection & Solutions",
  "19. Insurance & Life Policies",
  "20. Fiscal Performance & Spend"
)


K <- nrow(final_lda$phi) 
perplot <- 20               

for (i in 1:ceiling(K / perplot)) {
  p <- terms_per_topic_top %>% 
    filter(topic > (i - 1) * perplot & topic <= i * perplot) %>%
    mutate(term = reorder_within(term, beta, topic),
           topic_name = factor(topic, levels = 1:K, labels = labelnames)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic_name, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    theme_minimal(base_size = 11)
  
  print(p)
}

write_rds(terms_per_topic, "phase9_terms_per_topic.Rds") 

##### Phase 10: Document-Topic Distributions ###################################
# Goal: Get the document-topic distributions for the regression models

doc_topics_df <- tibble(
  doc_id = docnames(cs_dfm),
  topic  = seededlda::topics(final_lda)
)

# Link identifier variables back
doc_topics <- doc_topics_df %>%
  left_join(cs_docs %>%
              group_by(ticker, year) %>%
              slice(1) %>%
              ungroup(),
            by = c("doc_id")
  ) %>%
  dplyr::select(-doc_id, -text)
head(doc_topics)

# Get document-topic probability matrix
theta <- final_lda$theta  

# Turn into tibble
doc_topics_wide <- as_tibble(theta)
doc_topics_wide$doc_id <- docnames(cs_dfm) 

# Wide format for merging with other data
doc_topics_wide <- doc_topics_wide %>%
  dplyr::left_join(
    cs_docs %>%
      group_by(ticker, year) %>%
      slice(1) %>%
      ungroup(),
    by = "doc_id"
  ) %>%
  as_tibble() %>% 
  dplyr::select(ticker, year, everything(), -text, -doc_id)
head(doc_topics_wide)

# Renaming the final two residual topics
doc_topics_wide <- rename(doc_topics_wide, Topic19 = other1, Topic20 = other2) 

# Creating a second year lag of the topic proportions
doc_topics_wide <- doc_topics_wide %>%
  arrange(ticker, year) %>%
  group_by(ticker) %>%
  mutate(across(
    starts_with("Topic"),
    ~ lag(.),
    .names = "{.col}_lag2"
  )) %>%
  ungroup()

write_rds(doc_topics_wide, "phase10_doc_topics.Rds")

##### End of Script ############################################################


