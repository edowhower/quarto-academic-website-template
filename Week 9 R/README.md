# Week 9: build your text analysis project

**Use both sentiment analysis and topic modeling on one corpus.** Everything now runs in one R project; this replaces the earlier instructions to create two analysis folders.

## Start here

1. Open **`Week9_R.Rproj`**, then **`Week9_Project_Starter.R`**.
2. Write your question at the top of the script. Use the supplied corpus or put your chosen CSV in `data/` and change the filename in **W9-1**.
3. Run **W9-1–8** in order. Follow the **YOUR PROJECT** comments: check original texts, compare topic counts, choose labels, and explain both findings. The script saves two figures in `outputs/`.
4. Open **`research_poster.qmd`**. Replace its bracketed prompts; the figures are already linked. Click **Render** and inspect the resulting `research_poster.html`.

The script includes the code you need from Weeks 7 and 8. There is no separate reference file or helper script. The existing poster layout is retained.

**Tuesday, October 27:** choose your question and corpus; get both methods working.  
**Thursday, October 29:** brief review, then check interpretations and finish the poster.  
**Tuesday, November 3:** Text Analysis Project Showcase. Follow Brightspace for submission details.

## What your poster needs

- Your question, source, date range, document unit, selection rule, and document count.
- A sentiment finding and figure, the dictionary used, and its coverage.
- A topic finding and figure, your chosen `k`, labels supported by keywords and original texts, and the number of modeled documents.
- Short original-text examples and a specific limitation for **each** method.

The supplied figures are starting points. Explain what they show about your question; do not submit generic topic labels or template prompts.

## Data choices

The default is the **4,469-post, full-year 2019 Trump corpus** from Week 8, not the smaller classroom sample. `issue` contains the month. The script uses the original words for sentiment and separately prepares lemma counts for topics. Report any documents excluded by either method.

You can instead copy one CSV from Week 8's `project_datasets/data/`, or use your own structured data. One row should be one document. Use a `text` column and an `issue` column for a meaningful comparison group; W9-1 explains the one-line option if you have no groups. The script creates simple row labels to join results to texts. Keep source and date columns.

**Default corpus source:** [Mark Huang's Complete Trump Tweets Archive](https://github.com/MarkHershey/CompleteTrumpTweetsArchive), source commit `0de31782d856523e0fce5022a993781caf317d9f`. The course preparation selected 7,249 records dated 2019, excluded 2,498 `RT @` retweets and 281 records without alphabetic text after URL removal, and removed one duplicate import. The resulting 4,469 rows retain source IDs, dates, links, and `raw_text`. Prepared `text` decodes HTML entities, removes URLs, and collapses whitespace. This archive describes one account, not public opinion.

**Dictionary:** Hu and Liu (2004), *Mining and summarizing customer reviews*, distributed through [tidytext](https://juliasilge.github.io/tidytext/reference/sentiments.html). The supplied course version has 6,556 words with values +1/−1. A larger dictionary still misses context and is not validated as a measure of political support. Source notices and adaptation details are in `data/SOURCE_NOTICES.txt`. If you choose another Week 8 corpus, use its source citation from the Week 8 dataset menu.

## Put the finished poster on your website

1. Render the finished poster. Copy `research_poster.html` into your personal website's `files/` folder, naming the copy `text-analysis-poster.html`.
2. In the website's `_quarto.yml`, add `resources` **inside its existing `project:` block**, preserving its other settings. If `resources` already exists, add the list item:

   ```yaml
   resources:
     - "files/*.html"
   ```

3. Add this link to your Research page:

   ```markdown
   [View my text analysis poster](files/text-analysis-poster.html)
   ```

4. Render the website. Check the link locally, commit and push as in Week 5, then check it on the published site. With the course's `output-dir: docs` setup, the copied poster should be in `docs/files/`.

The poster uses `embed-resources: true`, so its HTML includes the figures. After any later revision, render the poster again, replace the website copy, and render/publish the website again.
