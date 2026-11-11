# POLS 4130 | WEEK 9 | TEXT ANALYSIS PROJECT
# Open Week9_R.Rproj. Run one block at a time with Cmd/Ctrl + Enter.
# Both methods run here on the same corpus. No helper or reference file is needed.
# Tables appear in Console; figures appear in Plots.
# The supplied example runs as written. Make the YOUR PROJECT edits yourself.
#
# ONE-TIME SETUP: run only if these packages are missing.
# install.packages(c("dplyr", "stringr", "tidytext", "ggplot2",
#                    "textstem", "topicmodels", "reshape2"))
#
# YOUR PROJECT: write brief answers here before choosing results for your poster.
# My question:
# Data source and dates:
# One document is:
# My selection rule:
# What sentiment will tell me:
# What topics will add:


# W9-1 | CHOOSE AND READ ONE CORPUS -------------------------------------
# Default: 4,469 Trump account posts from 2019, one post per row.
# Example question: which themes recur, and how does dictionary tone vary by month?
# The README gives the source and preparation details.
# YOUR PROJECT: use this corpus or copy another Week 8 project CSV into data/.
# Change the filename below, then rerun this script from here.
# For your own CSV, use text for the text column and issue for a comparison group.
# If you have no group, add comments$issue <- "All documents" after reading.
library(dplyr)
comments <- read.csv("data/us_executive_orders_2009_2025.csv", colClasses = "character")
nrow(comments)
comments$text[1:3]
count(comments, issue) ##

# EXPECT: 4,469 rows and 12 months. issue is month here, not a model topic.

library(stringr)
comments$text <- str_squish(comments$text)
comments <- filter(comments, !is.na(text), text != "") ##

# str_squish() removes extra spaces. Keep rows with text.
# If your question needs a subset, put your filter() here, BEFORE both methods.
# Do not delete repeated wording unless it is an accidental duplicate import.

comments$id <- as.character(seq_len(nrow(comments)))
nrow(comments) ##

# These row labels connect results to text within this run; they are not a finding.
# seq_len(nrow(comments)) numbers the selected rows. Source metadata stays in comments.
# EXPECT with the supplied file: still 4,469 posts.


# W9-2 | SENTIMENT: MATCH WORDS AND SCORE DOCUMENTS ----------------------
# Reuse Week 7 W7B-1–2. Keep original word forms and stopwords for sentiment.
# The project dictionary has 6,556 words, rather than the tiny teaching dictionary.
library(tidytext)
tokens <- unnest_tokens(comments, word, text)
dictionary <- read.csv("data/bing_liu_dictionary.csv")
head(dictionary) ##

# Remove Trump from sentiment classifications
# filter out trump
# use %>% to make code aasy to read: sequential from left to right


# value is +1 for positive words and -1 for negative words.

scored_tokens <- left_join(tokens, dictionary, by = "word")
document_scores <- scored_tokens |>
  group_by(id) |>
  summarise(net = sum(value, na.rm = TRUE),
            words = n(),
            matched = sum(!is.na(value)))
document_scores$coverage <- document_scores$matched / document_scores$words
results <- left_join(comments, document_scores, by = "id")
select(results, id, net, words, matched, coverage) |> head() ##

# EXPECT: one net score per document with tokens; coverage = matched / words.
# net adds dictionary values. NA at the word level means no dictionary match.
# net = 0 can mean no matches OR positive and negative matches cancel.

sentiment_results <- filter(results, !is.na(net))
nrow(comments) - nrow(sentiment_results) ##

# EXPECT: 0 posts with no tokens in the supplied file.
# A text with no tokens has no score; exclude it from averages and report the count.
# A text WITH tokens but no dictionary matches stays in the analysis with net = 0.


# W9-3 | CHECK TEXTS AND PLOT SENTIMENT ---------------------------------
# Reuse Week 7 W7B-3–4: read text before interpreting an average.

inspect_id <- comments$id[1]
filter(results, id == inspect_id) |> select(id, text, net, matched, coverage)
filter(scored_tokens, id == inspect_id, !is.na(value)) |> select(word, value) ##

# YOUR PROJECT: change [1] to another row and rerun these two filter() lines.
# Check several texts, including a zero score. Record a useful and a misleading case.
# Positive wording is not necessarily support; sarcasm and quotations can mislead.
# If your question needs negation or a target window, adapt Week 7 W7B-5 or W7B-6.
# Keep ordered words for those rules, and check their results against the texts.

issue_summary <- sentiment_results |>
  group_by(issue) |>
  summarise(documents = n(),
            mean_net = mean(net),
            mean_words = mean(words),
            mean_coverage = mean(coverage))
issue_summary ##

# EXPECT: one row per group. Each document has equal weight in these averages.
# mean_coverage is the average document coverage, not an accuracy score.
# Compare mean_words too: longer documents have more chances to match the dictionary.

library(ggplot2)
sentiment_plot <- ggplot(issue_summary, aes(x = mean_net, y = issue)) +
  geom_col(fill = "steelblue") +
  labs(x = "Mean Net Dictionary Score per Administration", y = NULL,
       title = "Dictionary Sentiment by Administration") +
  theme_minimal(base_size = 16)
sentiment_plot ##

# EXPECT: 12 bars for the supplied monthly groups. Read their actual values above.
# YOUR PROJECT: write one pattern and one limitation here.
# My sentiment finding:
# Supporting document / misleading document:
# Limitation:


# W9-4 | TOPICS: PREPARE THE WORD COUNTS --------------------------------
# Reuse Week 8 W8A-2–3. This branch uses lemmas and removes stopwords.
# It does not replace the original words used for sentiment.

#poster text: First I separated the words within each document into tokens. I also discarded numeric values within the text because they are not relevant in determining the topics within executive orders. I then lemmatized the words to ensure R detects one word regardless of inflections, thus preventing them from appearing multiple times in different forms. I then filtered out stop words within the SMART lexicon to discard words that do not indicate the substance of a document. I added the extra stop words "executive", "order", "state", "agency", "section", "unite", "law", "secretary", "department", and "amend." After experimenting with plots and reading the texts, I found that many documents shared the same procedural words indicating nothing about the subject matter. For example, many documents began with similar preambles stating the president's authority to enact an executive order or amend existing orders. I removed the rest of the words similarly because they did not relate to specific topics; rather, they related to the general role and function of executive orders as a means of policy enforcement. 
topic_tokens <- unnest_tokens(comments, word, text, strip_numeric = TRUE)
library(textstem)
topic_tokens <- mutate(topic_tokens, lemma = lemmatize_words(word))

topic_stopwords <- filter(stop_words, lexicon == "SMART")$word
topic_stopwords
extra_stopwords <- c("executive" , "order", "state" , "agency" , "section" , "unite" , "law" , "secretary" , "amend" , "include") ##

# YOUR PROJECT: replace "" with an unhelpful frequent word, only if you can justify it.
# Rerun W9-4–8 and inspect how the topics change. Restore "" if it does not help.

content_words <- filter(topic_tokens,
  !word %in% topic_stopwords,
  !lemma %in% c(topic_stopwords, extra_stopwords))
count(content_words, lemma, sort = TRUE) |> head(10) ##

# EXPECT: frequent retained lemmas. Removing a meaningful political term can hide a theme.

word_counts <- count(content_words, id, lemma)
dtm <- cast_dtm(word_counts, id, lemma, n)
dim(dtm)
nrow(comments) - nrow(dtm) ##

# DTM rows are documents; columns are lemmas; cells count occurrences.
# The subtraction counts documents with no words left for topic modeling.
# EXPECT with the supplied data: 4,441 DTM rows; 28 posts have no retained words.
# Report this count on the poster. Keep those documents in the source corpus.
# Sentiment and topics start from the same corpus but can retain different numbers.


# W9-5 | FIT TOPICS AND LOOK AT THEIR WORDS ------------------------------
# Reuse Week 8 W8A-4–5. k is your chosen number of topics, not an answer key.

library(topicmodels)
k <- 5
model <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = 4130)) ##

# Keep the method and seed fixed when comparing k, as in Week 8.

library(reshape2)
word_topics <- tidy(model, matrix = "beta")
top_words <- word_topics |>
  group_by(topic) |>
  slice_max(beta, n = 15, with_ties = FALSE) |>
  ungroup()
print(top_words, n = 15 * k) ##

# EXPECT: six keywords per topic. beta is word probability within a topic.

word_plot <- ggplot(top_words, aes(x = beta, y = reorder(term, beta))) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ topic, scales = "free_y") +
  labs(x = "Word probability within each topic", y = NULL,
       title = "Top words by topic") +
  theme_minimal(base_size = 16)
word_plot ##




# EXPECT: three keyword panels. Political posts can mix themes; unclear topics are possible.
# YOUR PROJECT: try k = 2 and rerun W9-5. Compare the WORD COMBINATIONS.
# Choose and justify a k, then rerun W9-5–8 with that choice.
# Topic numbers need not mean the same thing across different fits.
# Why I chose this k:


# W9-6 | READ DOCUMENTS AND NAME THE TOPICS -----------------------------
# Reuse Week 8 W8A-6 and W8A-8. Labels need words AND source texts.
document_topics <- tidy(model, matrix = "gamma")
document_topics <- rename(document_topics, id = document, weight = gamma)
document_topics <- left_join(document_topics, comments, by = "id") ##

# weight is a document's estimated topic share; its shares sum to 1.
# It is not a sentiment score or confidence that your topic label is correct.

inspect_topic <- 1
topic_examples <- document_topics |>
  filter(topic == inspect_topic) |>
  arrange(desc(weight)) |>
  head(10)
select(topic_examples, id, weight)
topic_examples$text ##

one_order <- filter(document_topics, id == inspect_id)

one_order$text

# YOUR PROJECT: change inspect_topic and rerun this block for EVERY topic.
# Read full texts; record one supporting passage and one ambiguous example.

topic_labels <- tibble(
  topic = 1:5,
  label = c("Government and Administration", "National Security and Foreign Affairs", "Public Policy and Human Rights", "Executive Powers", "Science, Technology, and Innovation")
)


labeled_words <- left_join(top_words, topic_labels, by = "topic")
select(labeled_words, topic, label, term)


# These generic labels work for any k. Replace the label column after reading texts.
# With k = 3, add topic_labels$label <- c("your label", "your label", "your label").
# Write your own meaningful names, then rerun W9-7–8.
# My labels and supporting document IDs:
# An ambiguous example:
word_by_topic_plot <- ggplot(labeled_words, aes(x = beta, y = reorder(term, beta))) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ label, scales = "free_y") +
  labs(x = "Word probability within each topic", y = NULL,
       title = "Top words by topic") +
  theme_minimal(base_size = 16)
word_by_topic_plot ##


ggsave("outputs/wordbytopic.png", word_by_topic_plot, width = 10, height = 7, dpi = 150) ##

# W9-7 | PLOT THE OVERALL TOPIC RESULT ---------------------------------
# Reuse Week 8 W8B-3. Average topic proportions across modeled documents.
topic_summary <- document_topics |>
  group_by(topic) |>
  summarise(mean_weight = mean(weight))
topic_summary <- left_join(topic_summary, topic_labels, by = "topic")
topic_summary

topic_plot <- ggplot(topic_summary, aes(x = mean_weight, y = reorder(label, mean_weight))) +
  geom_col(fill = "steelblue") +
  labs(x = "Mean estimated topic share per document", y = NULL,
       title = "Themes in the modeled corpus") +
  xlim(0, 1) +
  theme_minimal(base_size = 16)
topic_plot ##

# EXPECT: one bar per topic. The means sum to 1; each modeled document counts equally.
# These are average proportions, not counts of posts or measures of public opinion.
# YOUR PROJECT: explain a topic finding alongside your sentiment finding.
# My topic finding:
# How the two results answer my question:
# Limitation:


# W9-8 | SAVE YOUR POSTER FIGURES ---------------------------------------
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/sentiment.png", sentiment_plot, width = 8, height = 5, dpi = 150)
ggsave("outputs/topicsub.png", word_plot, width = 10, height = 7, dpi = 150) ##

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
dir.create("outputs", showWarnings = FALSE)


library(tidytext)
topic_tokens <- unnest_tokens(
  comments,
  output = word, input = text)
topic_tokens <- left_join(
  topic_tokens, dictionary, by = "word")



sentiment_by_topic <- document_topics |>
  left_join(
    select(sentiment_results, id, net, matched),
    by = "id"
  ) |>
  group_by(topic) |>
  summarise(
    mean_sentiment = weighted.mean(
      net / matched, weight, na.rm = TRUE
    ),
    documents = n_distinct(id)
  ) |>
  left_join(topic_labels, by = "topic")

sentiment_by_topic

library(ggplot2)
sentiment_topic_plot <- ggplot(
  sentiment_by_topic,
  aes(
    x = mean_sentiment,
    y = reorder(label, mean_sentiment)
  )
) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Sentiment by Topic",
    x = "Average Sentiment",
    y = "Topic"
  ) +
  theme_minimal(base_size = 14)

sentiment_topic_plot

ggsave("outputs/sentimentbytopic.png", sentiment_topic_plot, width = 10, height = 7, dpi = 150) ##


sentiment_results[114, "text"]


topic_examples

# EXPECT in Files > outputs: two PNGs. Rerunning replaces them with current results.
# Other tables and the keyword chart stay in RStudio for your checks.
# Save an additional figure with Plots > Export only if your argument needs it.
#
# NEXT: open research_poster.qmd. The two figures are already linked.
# Replace the bracketed prompts with your question, sources, methods, and findings.
# Include original-text evidence and a limitation for EACH method.
# Click Render. Inspect research_poster.html before the showcase.
# After completion, follow README.md to publish the poster on your personal website.
