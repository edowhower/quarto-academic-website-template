# POLS 4130 | WEEK 7 | SIMPLIFIED CLASSROOM STARTER v2
# Open Week7_R.Rproj. Run one numbered section at a time.
# After the opening concept slides, stay in this file for the walkthrough.
# The coding slides are a study guide for reviewing sections and results.
# Each example runs as written. IN CLASS prompts ask you to change or explain it.
# Select a small code block and press Cmd+Enter (Mac) or Ctrl+Enter (Windows).
# Read tables in Console, static figures in Plots, and interactive clouds in Viewer.
# Run sections in order: later sections use the objects created earlier.
# All comments are fictional. The original CSV files remain unchanged.
# This version uses data/comments_starter_v2.csv for Session A.
#
# Install packages once in the Console if needed:
# install.packages(c("dplyr", "stringr", "tidytext", "ggplot2",
#                    "wordcloud", "wordcloud2", "slider", "textstem"))


# W7A-1 | READ AND CLEAN THE TEXT ---------------------------------------
# A corpus is the collection of texts we analyze.
# Here, one comment is one document.
comments <- read.csv("data/comments_starter_v2.csv")
head(comments, 3)
tail(comments, 4)
nrow(comments)
# EXPECT: 22 rows, including two duplicate copies and two empty texts.

# str_squish() removes extra spaces at the ends AND inside each text.
library(stringr)
str_squish("  I hate    missing work.  ")
# EXPECT: "I hate missing work."

comments$text <- str_squish(comments$text)
comments$issue <- str_squish(comments$issue)
head(comments, 3)

# Remove missing text (NA), then blank text ("").
library(dplyr)
# filter() keeps rows satisfying a condition; is.na() identifies missing values.
comments <- filter(comments, !is.na(text))
comments <- filter(comments, text != "")
nrow(comments)
# EXPECT: 20 rows.

# These two duplicates are known extra copies in this teaching file.
# Identical wording from different real people would not justify deletion.
comments <- distinct(comments)
nrow(comments)
# EXPECT: 18 comments, with six in each issue group.
count(comments, issue)

# Give each comment a simple number AFTER cleaning.
# The number connects its words and score back to its original text.
# mutate() adds a column; row_number() supplies 1, 2, 3, and so on.
comments <- mutate(comments, id = row_number())
head(comments, 3)
# IN CLASS: why clean extra spaces before removing duplicate copies?


# W7A-2 | TURN COMMENTS INTO WORDS --------------------------------------
# A token is one occurrence of a word. A repeated word has repeated rows.
# unnest_tokens() comes from tidytext: split text, lowercase, remove punctuation.
library(tidytext)
tokens <- unnest_tokens(comments, output = word, input = text)
head(tokens)
nrow(tokens)
# EXPECT: 181 word occurrences. The comments table still contains the full text.

inspect_id <- 3
filter(comments, id == inspect_id)
filter(tokens, id == inspect_id)
# EXPECT: comment 3 becomes 10 word rows.
# IN CLASS: change inspect_id to 4. Find "not" and "bad" in the word rows.


# W7A-3 | CHOOSE STOPWORDS, COUNT, AND PLOT -------------------------------
# Stopwords are words we choose to exclude for a particular analysis.
stop_words <- c("a", "an", "and", "are", "as", "at", "be", "because", "but",
                "by", "does", "for", "from", "i", "in", "is", "it", "of", "on",
                "our", "so", "the", "their", "this", "to", "we", "with")
stop_words

# %in% means "is in this list"; ! means "not".
content_words <- filter(tokens, !word %in% stop_words)
word_counts <- count(content_words, word, sort = TRUE)
# count() counts occurrences per word; sort = TRUE puts the largest counts first.
head(word_counts, 12)
# EXPECT: terrible = 5, library = 4; 111 occurrences of 67 different words.
# Keep tokens: sentiment analysis will use ALL words, including negators.

library(ggplot2)
# slice_head() selects the first rows of our already sorted word-count table.
top_words <- slice_head(word_counts, n = 12)
# ggplot() selects the table; aes() maps columns to axes.
# geom_col() uses n as the bar length; reorder() orders the word labels by n.
word_plot <- ggplot(top_words, aes(x = n, y = reorder(word, n))) +
  geom_col() +
  labs(x = "Word occurrences", y = NULL) +
  theme_minimal(base_size = 14)
word_plot
# EXPECT: 12 bars, one per word. Longer bars mean more occurrences.


# W7A-4 | CHANGE ONE CLEANING CHOICE ------------------------------------
# Remove one more word from the frequency table, keeping the original table.
# IN CLASS: replace the empty quotes below with "not" yourself.
# Before running the block, predict which row will disappear and why.
extra_stopword <- ""
word_counts_changed <- content_words |>
  filter(word != extra_stopword) |>
  count(word, sort = TRUE)
head(word_counts_changed, 12)
filter(word_counts, word == extra_stopword)
# BEFORE YOUR EDIT: "" removes no words, so the table is unchanged.
# After editing extra_stopword, rerun this whole W7A-4 block and inspect both tables.
# AFTER ENTERING "not": its 3 occurrences disappear from the changed table.
# IN CLASS: next try "library". How could removing "not" alter meaning?


# W7A-4.1 | BASIC LEMMATIZATION: GROUP WORD FORMS -------------------------
# A lemma is a word's dictionary form, such as "bus" for "buses".
# Lemmatization maps inflected forms to that form using a word dictionary.
library(textstem)
lemmatize_words(c("extend", "extended", "extending"))
# EXPECT: "extend" "extend" "extend".
# lemmatize_words() takes a vector of words. No extra argument is needed here.

# Keep the original word and add a separate lemma column.
lemmatized_words <- mutate(content_words, lemma = lemmatize_words(word))
lemmatized_words |>
  filter(word %in% c("extend", "extending", "library")) |>
  distinct(word, lemma)
# EXPECT: extend/extending become extend; library stays library.

lemma_counts <- count(lemmatized_words, lemma, sort = TRUE)
head(lemma_counts, 12)
filter(lemma_counts, lemma == "extend")
# EXPECT: extend = 4, combining two "extend" and two "extending" occurrences.
# IN CLASS: replace the example words above with "bus", "buses", and "libraries".
# Run that lemmatize_words() line again. Inspect which forms merge.
# Then try "rent". This lookup returns "rend", the verb meaning to tear.
# A word dictionary does not read the sentence: check meaning before using a lemma.
# The word clouds and sentiment analysis still use original words and tokens.


# W7A-5 | TWO WORD-CLOUD STYLES -----------------------------------------
# A word cloud uses font size to show frequency, not positive/negative tone.
# The bar chart is better for exact comparisons.

# Style 1: a static plot in RStudio's Plots pane.
library(wordcloud)
wordcloud(words = word_counts$word,
          freq = word_counts$n,
          min.freq = 2)
# words = labels; freq = counts; min.freq = minimum count to display.
# EXPECT: 25 words meet the threshold. Layout varies with plot size.
# IN CLASS: change min.freq to 3 and rerun. Now 12 words qualify.
# The word_counts table does not change. Restore min.freq to 2 afterward.
# To save this figure, use Plots > Export in RStudio.

# Style 2: an interactive cloud in RStudio's Viewer/browser.
library(wordcloud2)
cloud_words <- filter(word_counts, n >= 2)
wordcloud2(cloud_words)
# Input: a two-column table, with word first and count second.
# EXPECT: the same 25 qualifying words; hover to read a count.
# IN CLASS: try wordcloud2(cloud_words, color = "blue"). Do counts change?


# W7B-1 | LOOK UP SENTIMENT VALUES --------------------------------------
# Returning for Session B? Run W7A-1, W7A-2, and W7A-3 first.
# Valence means positive or negative tone. Our practice dictionary uses +1/-1.
# This small dictionary is for learning, not a validated political measure.
dictionary <- tibble(
  word = c("safe", "reliable", "good", "helpful", "fair", "excellent", "love",
           "terrible", "hate", "bad", "expensive", "unfair", "unsafe", "broken"),
  value = c(1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1)
)
dictionary

# Match each word to its dictionary entry; keep unmatched words too.
# left_join() from dplyr matches the two tables on their shared word column.
scored_tokens <- left_join(tokens, dictionary, by = "word")
filter(scored_tokens, id == 1)
# EXPECT: safe and reliable receive +1. Unmatched words receive NA.
# NA means "no match," not "proven neutral."


# W7B-2 | ONE SCORE PER COMMENT ----------------------------------------
# group_by(id) keeps each comment separate; sum() adds its matched values.
# na.rm = TRUE skips unmatched values when adding.
document_scores <- scored_tokens |>
  group_by(id) |>
  summarise(net = sum(value, na.rm = TRUE),
            words = n(),
            matched = sum(!is.na(value)))
document_scores

# Coverage: the fraction of word occurrences found in the dictionary.
# $ selects a column. Divide matched by words separately for each comment.
document_scores$coverage <- document_scores$matched / document_scores$words

# Put the score beside the comment that produced it.
results <- left_join(comments, document_scores, by = "id")
select(results, id, net, words, matched, coverage)
# EXPECT for comment 1: net = 2; 2 of 11 words match; coverage is about 18.2%.
# Coverage is not accuracy. This supplied corpus has words in every kept comment.


# W7B-3 | READ THE TEXT BESIDE THE SCORE --------------------------------
audit_ids <- c(4, 11, 16)
results |>
  filter(id %in% audit_ids) |>
  select(id, text, net, matched)
# EXPECT: 4 = -1 ("not bad"); 11 = 0 (no matches); 16 = 0 (matches cancel).
# IN CLASS: explain why the two zero scores do not mean the same thing.

filter(results, id == 6)
# Positive wording can appear in a comment that OPPOSES a policy.
# IN CLASS: identify the policy position and the positive word in comment 6.


# W7B-4 | COMPARE ISSUE GROUPS ------------------------------------------
issue_summary <- results |>
  group_by(issue) |>
  summarise(mean_net = mean(net))
issue_summary
# EXPECT: Campus services about +0.167; Housing and Transport about -0.167.

library(ggplot2)
score_plot <- ggplot(issue_summary, aes(x = issue, y = mean_net)) +
  geom_col() +
  labs(x = NULL, y = "Mean dictionary score") +
  theme_minimal(base_size = 14)
score_plot
# Each comment contributes one score. Describe these fictional comments only.
# IN CLASS: take a five-minute break before starting W7B-5.


# W7B-5 | NEGATION: LOOK BACK TWO WORDS ----------------------------------
# Use the separate ten-comment file to practice the rule.
rule_comments <- read.csv("data/sentiment_rules_v2.csv")
rule_comments

# W7B-5.1 | Tokenize and attach dictionary values.
library(tidytext)
rule_tokens <- unnest_tokens(rule_comments, output = word, input = text)
rule_tokens <- left_join(rule_tokens, dictionary, by = "word")

# W7B-5.2 | Show the two preceding words. lag() means "look back."
# group_by(id) prevents looking back into a different comment.
rule_tokens <- rule_tokens |>
  group_by(id) |>
  mutate(previous_word = lag(word),
         two_words_back = lag(word, 2)) |>
  ungroup()

rule_tokens |>
  filter(id == "V02") |>
  select(word, previous_word, two_words_back, value)
# EXPECT at "good": previous_word = "very"; two_words_back = "not".

# W7B-5.3 | If either preceding word is a negator, flip the sign once.
negators <- c("not", "no", "never")
rule_tokens <- rule_tokens |>
  mutate(negated = previous_word %in% negators | two_words_back %in% negators)
# | means OR. Membership in this list is FALSE for a missing preceding word.

rule_tokens <- rule_tokens |>
  mutate(value_neg = if_else(negated, -value, value))
# if_else(): if negated, use -value; otherwise keep value. Unmatched stays NA.
rule_tokens |>
  filter(id == "V02") |>
  select(word, value, negated, value_neg)
# EXPECT: good changes from +1 to -1 in "Housing is not very good."

rule_scores <- rule_tokens |>
  group_by(id) |>
  summarise(baseline_net = sum(value, na.rm = TRUE),
            negation_net = sum(value_neg, na.rm = TRUE))
rule_results <- left_join(rule_comments, rule_scores, by = "id")
select(rule_results, id, text, baseline_net, negation_net)
# EXPECT: V01 -1 -> +1; V02 +1 -> -1; V08 0 -> +2; V09 +1 -> -1.

# W7B-5.4 | IN CLASS: find a case the rule gets wrong.
inspect_rule <- "V06"
filter(rule_results, id == inspect_rule)
# V06: "not only good but excellent" changes +2 -> 0, which is misleading.
# Try V10: "not going to be good" stays +1 because "not" is too far back.
# The rule also ignores sentence boundaries and can miss other negation forms.
# A local rule helps some phrases; checking the original text is still essential.


# W7B-5.5 | COMPARE THE OVERALL RESULT BEFORE AND AFTER NEGATION ---------
# Average the scores over the SAME ten comments. Each comment counts equally.
negation_summary <- tibble(
  method = c("Before negation", "After negation"),
  mean_score = c(mean(rule_results$baseline_net), mean(rule_results$negation_net))
)
negation_summary
# EXPECT: the mean changes from 0.70 to 0.30.
# tibble() makes the two-row table; mean() averages the comment scores.

library(ggplot2)
# geom_col() plots the two means; geom_text() prints rounded values above the bars.
# The scale lines keep method order and colors consistent. Read this chart in Plots.
negation_plot <- ggplot(negation_summary,
                        aes(x = method, y = mean_score, fill = method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_score, 2)), vjust = -0.5) +
  scale_x_discrete(limits = negation_summary$method) +
  scale_fill_manual(values = c("Before negation" = "#4477AA",
                               "After negation" = "#EE8844"), guide = "none") +
  labs(title = "Average sentiment before and after negation",
       subtitle = "Same 10 practice comments",
       x = NULL, y = "Mean net sentiment score") +
  theme_minimal(base_size = 14)
negation_plot
# Blue = before; orange = after. The bars summarize the whole practice corpus.
# IN CLASS: does a lower average prove greater accuracy? Revisit V01 and V06.


# W7B-6 | SENTIMENT NEAR A TARGET ---------------------------------------
# A target is the entity or issue we want to focus on, such as "housing".
# Keep words within four positions on either side of ANY target mention.

# W7B-6.1 | Mark which words are the target.
target_word <- "housing"
rule_tokens <- mutate(rule_tokens, is_target = word == target_word)

# W7B-6.2 | Mark nearby words IN THE SAME TABLE.
# slide_lgl() from slider returns TRUE/FALSE for each word.
# any() asks: is there at least one TRUE in this window?
# .before and .after count the words to look at on each side.
# Including the current word also keeps the target itself in its own window.
library(slider)
target_window <- 4
rule_tokens <- rule_tokens |>
  group_by(id) |>
  mutate(near_target = slide_lgl(is_target, any,
                               .before = target_window,
                               .after = target_window)) |>
  ungroup()

rule_tokens |>
  filter(id == "V03") |>
  select(word, is_target, near_target, value_neg)
# EXPECT: housing / is / good / but / transit are TRUE; terrible is FALSE.
# Each word row is kept once, even when it is near two mentions of housing.
# The target word itself is included. Windows stay inside each comment.

# W7B-6.3 | Score only the marked words, using the negation-adjusted values.
target_scores <- rule_tokens |>
  filter(near_target) |>
  group_by(id) |>
  summarise(target_net = sum(value_neg, na.rm = TRUE),
            target_matched = sum(!is.na(value_neg)))

target_results <- left_join(rule_results, target_scores, by = "id")
select(target_results, id, negation_net, target_net, target_matched)
# EXPECT: V03 whole comment = 0, housing window = +1; V04 window = -1.
# V05 has no housing mention: target_net = NA after the left join.
# V10 has housing but no matches in its window: target_net = 0, matched = 0.
# V08 has overlapping windows: each occurrence counts once; target_net = +2.

# W7B-6.4 | IN CLASS: change target_window from 4 to 6 and rerun 6.2-6.3.
# EXPECT: V03 changes +1 -> 0 because "terrible" about transit enters the window.
# Restore 4 afterward. Then try target_word = "transit" and rerun 6.1-6.3.
# We measure tone NEAR a mention, which is not necessarily tone ABOUT it.


# W7B-6.5 | COMPARE THE OVERALL WHOLE-COMMENT AND TARGET RESULTS ---------
# Compare BOTH methods on the SAME comments that contain the selected target.
# Keep zero-match windows (score 0); exclude absent targets (score NA) from both.
target_comparison <- filter(target_results, !is.na(target_net))
nrow(target_comparison)
# EXPECT with housing / window 4: 9 comments. V05 is excluded; V10 remains.

target_summary <- tibble(
  method = c("Whole comment", "Target window"),
  mean_score = c(mean(target_comparison$negation_net),
                 mean(target_comparison$target_net))
)
target_summary
# EXPECT: about 0.22 for whole comments and 0.11 for target windows.
# Both columns use the same two-word negation rule.

library(ggplot2)
target_plot <- ggplot(target_summary,
                      aes(x = method, y = mean_score, fill = method)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_score, 2)), vjust = -0.5) +
  scale_x_discrete(limits = target_summary$method) +
  scale_fill_manual(values = c("Whole comment" = "#4477AA",
                               "Target window" = "#EE8844"), guide = "none") +
  labs(title = "Average sentiment: whole comment and target window",
       subtitle = paste("Same", nrow(target_comparison), "comments containing", target_word),
       x = NULL, y = "Mean net sentiment score") +
  theme_minimal(base_size = 14)
target_plot
# Blue = whole comment; orange = target window. These are averages, not percentages.
# IN CLASS: change the window to 6, rerun 6.2-6.3 and this section, then explain.
# EXPECT: whole-comment mean stays about 0.22; target-window mean becomes about 0.22.
# Restore target_window to 4 and rerun 6.2-6.3 and 6.5 afterward.
# An aggregate can hide different changes within comments.


# W7B-7 | SAVE ONE RESULT YOU CAN USE -----------------------------------
# Save the original 18 comments with their scores. No intermediate exports.
dir.create("outputs", showWarnings = FALSE)
write.csv(results, "outputs/comment_scores_v2.csv", row.names = FALSE)
# EXPECT: one CSV containing each comment, its issue, and its score/coverage.
# Figures can be saved through Plots > Export when needed for the poster.

# Project connection: apply sentiment analysis AND topic modeling to your corpus.
# The IN CLASS prompts above are classroom activities, not additional homework.
