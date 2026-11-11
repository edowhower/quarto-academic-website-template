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
rule_comments <- read.csv("data/us_executive_orders_2009_2025.csv")
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
  filter(id == "EO_13489") |>
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
  filter(id == "EO_13489") |>
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
inspect_rule <- "EO_13489"
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
       subtitle = "Executive Orders",
       x = NULL, y = "Mean net sentiment score") +
  theme_minimal(base_size = 14)
negation_plot
# Blue = before; orange = after. The bars summarize the whole practice corpus.
# IN CLASS: does a lower average prove greater accuracy? Revisit V01 and V06.


# W7B-6 | SENTIMENT NEAR A TARGET ---------------------------------------
# A target is the entity or issue we want to focus on, such as "housing".
# Keep words within four positions on either side of ANY target mention.

# W7B-6.1 | Mark which words are the target.
target_word <- "security"
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
  filter(id == "EO_13489") |>
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


this_issue_summary <- target_summary <- target_comparison |>
  group_by("issue") |>
  summarise(
    mean_score = mean(target_net, na.rm = TRUE),
    .groups = "drop"
  ) 

this_issue_summary

score_plot
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
  labs(title = "Average sentiment of whole executive order compared to average sentiment towards security.",
       subtitle = paste("Same", nrow(target_comparison), "comments containing", target_word),
       x = NULL, y = "Mean net sentiment score") +
  theme_minimal(base_size = 14)
target_plot

#not working HAHAHAHAH kill me 
target_by_admin_plot <- ggplot(target_summary,
                               aes(x = "issue", y = mean_score, fill = "issue")) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mean_score, 2)), vjust = -0.5) +
  facet_wrap(~ "issue") +
  labs(
    title = paste("Average sentiment towards", target_word, "by administration"),
    subtitle = paste("Comments containing", target_word),
    x = NULL,
    y = "Mean net sentiment score"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
target_by_admin_plot



# Blue = whole comment; orange = target window. These are averages, not percentages.
# IN CLASS: change the window to 6, rerun 6.2-6.3 and this section, then explain.
# EXPECT: whole-comment mean stays about 0.22; target-window mean becomes about 0.22.
# Restore target_window to 4 and rerun 6.2-6.3 and 6.5 afterward.
# An aggregate can hide different changes within comments.


# W7B-7 | SAVE ONE RESULT YOU CAN USE -----------------------------------
# Save the original 18 comments with their scores. No intermediate exports.
dir.create("outputs", showWarnings = FALSE)
write.csv(results, "outputs/comment_scores_v2.csv", row.names = FALSE)
ggsave("outputs/ogandtargetscores.png", target_plot, width = 10, height = 7, dpi = 150)
ggsave("outputs/negate.png", negation_plot, width = 10, height = 7, dpi = 150)
