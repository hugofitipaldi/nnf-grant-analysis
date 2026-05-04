# ============================================================
# NNF Grant Portfolio Analysis
# Author: Hugo Fitipaldi
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(rio)
library(scales)
library(forcats)
library(tidytext)
library(visNetwork)
library(igraph)
library(htmlwidgets)
library(lubridate)

# ── 0. LOAD & CLEAN ──────────────────────────────────────────
df_raw <- rio::import("raw_data/grants-export-1.xlsx", sheet = "transposed")

nnf <- df_raw %>%
  filter(grepl("Novo Nordisk", `Funder Name`, ignore.case = TRUE)) %>%
  rename(
    grant_id    = `Grant Id`,
    title       = `Title`,
    year        = `Grant Year`,
    amount_dkk  = `Amount Granted`,
    abstract    = `Abstract`,
    oecd_l1     = `OECD Classification Level 1`,
    oecd_l2     = `OECD Classification Level 2`,
    instrument  = `Funder Specific Instrument`,
    org_name    = `Organisation name`,
    green_cat   = `DST green research category`,
    green_pct   = `DST green research percentage`
  ) %>%
  mutate(
    year        = as.integer(year),
    amount_dkk  = as.numeric(amount_dkk),
    amount_mDKK = amount_dkk / 1e6,
    green_pct   = suppressWarnings(as.numeric(green_pct)),
    abstract    = str_remove_all(abstract, "_x000d_|\\r|_x000D_"),
    title       = str_remove_all(title,    "_x000d_|\\r|_x000D_")
  )

# ── 1. CLASSIFIER ────────────────────────────────────────────
ds_pattern <- paste(c(
  "machine learning", "deep learning", "artificial intelligence",
  "neural network", "natural language processing", "\\bnlp\\b",
  "data science", "predictive model", "random forest", "gradient boost",
  "large language model", "\\bllm\\b", "generative ai", "transformer",
  "convolutional", "reinforcement learning", "foundation model",
  "bioinformatics", "data.driven", "bayesian",
  "computer vision", "image analysis", "statistical learning",
  "prediction model"
), collapse = "|")

ai_pattern <- paste(c(
  "machine learning", "deep learning", "artificial intelligence",
  "neural network", "\\bllm\\b", "generative ai", "transformer",
  "convolutional", "reinforcement learning", "foundation model",
  "large language model", "computer vision"
), collapse = "|")

nnf <- nnf %>%
  mutate(
    text  = paste(tolower(title), tolower(abstract)),
    is_ds = str_detect(text, ds_pattern),
    is_ai = str_detect(text, ai_pattern),
    oecd_short = case_when(
      str_detect(oecd_l1, "Medical")      ~ "Health",
      str_detect(oecd_l1, "Natural")      ~ "Natural Sciences",
      str_detect(oecd_l1, "Engineering")  ~ "Engineering & Tech",
      str_detect(oecd_l1, "Agricultural") ~ "Agriculture & Vet",
      str_detect(oecd_l1, "Social")       ~ "Social Sciences",
      str_detect(oecd_l1, "Humanities")   ~ "Humanities",
      TRUE ~ "Other"
    )
  )

# ── 2. SHARED THEME ──────────────────────────────────────────
nnf_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title       = element_text(face = "bold", color = "#0F6E56", size = 13),
    plot.subtitle    = element_text(color = "#555555", size = 10),
    plot.caption     = element_text(color = "#888888", size = 8),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(color = "#444444", size = 10),
    legend.background = element_rect(fill = "white", color = NA)
  )

domain_colors <- c(
  "Health"             = "#0F6E56",
  "Natural Sciences"   = "#1D9E75",
  "Engineering & Tech" = "#5DCAA5",
  "Agriculture & Vet"  = "#9FE1CB",
  "Social Sciences"    = "#C8DDD8",
  "Humanities"         = "#E8F5F2",
  "Other"              = "#dddddd"
)


# ── 3. PLOT 4: DS GRANTS BY DOMAIN (fixed colours) ───────────
ds_domain <- nnf %>%
  filter(is_ds, oecd_short != "Other") %>%
  group_by(oecd_short) %>%
  summarise(
    n_grants   = n(),
    total_mDKK = sum(amount_mDKK, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(total_mDKK) %>%
  mutate(oecd_short = factor(oecd_short, levels = oecd_short))

# Assign colours by rank so darkest = largest bar always
ds_domain <- ds_domain %>%
  arrange(total_mDKK) %>%
  mutate(
    oecd_short = factor(oecd_short, levels = oecd_short),
    bar_color  = colorRampPalette(c("#C8DDD8", "#0F6E56"))(n())[rank(total_mDKK)]
  )

p4 <- ggplot(ds_domain,
             aes(x = total_mDKK,
                 y = oecd_short)) +
  geom_col(aes(fill = bar_color), width = 0.65, show.legend = FALSE) +
  geom_text(aes(x = total_mDKK + 12,
                label = paste0(n_grants, " grants")),
            hjust = 0, size = 3.5, color = "#444444") +
  scale_fill_identity() +
  scale_x_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.22))) +
  labs(
    title    = "Where Does NNF's Data Science Funding Go?",
    subtitle = "Total DS/AI grant funding by research domain (2017-2024)",
    x        = "Total Funding (mDKK)",
    y        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/nnf_p4_ds_domains.png", p4,
       width = 10, height = 5, dpi = 150, bg = "white")


# ── 4. PLOT 5: INSTITUTIONS (fixed legend + labels) ──────────
top_orgs <- nnf %>%
  filter(!is.na(org_name), org_name != "N/A") %>%
  group_by(org_name) %>%
  summarise(
    n_grants   = n(),
    total_mDKK = sum(amount_mDKK, na.rm = TRUE),
    n_ds       = sum(is_ds, na.rm = TRUE),
    pct_ds     = n_ds / n_grants * 100,
    .groups    = "drop"
  ) %>%
  arrange(desc(total_mDKK)) %>%
  slice_head(n = 15) %>%
  mutate(
    org_name    = str_wrap(org_name, 35),
    label_color = ifelse(pct_ds > 10, "white", "#444444"),
    label_hjust = ifelse(total_mDKK > 3000, 1.05, -0.1)
  )

# Sort ascending so largest bar is at top
top_orgs <- top_orgs %>%
  arrange(total_mDKK) %>%
  mutate(org_name = factor(org_name, levels = org_name))

p5 <- ggplot(top_orgs,
             aes(x = total_mDKK,
                 y = org_name,
                 fill = pct_ds)) +
  geom_col(width = 0.7) +
  geom_text(aes(x = total_mDKK,
                label = paste0(round(pct_ds, 0), "% DS")),
            hjust = -0.15, size = 3, color = "#444444") +
  scale_fill_gradient(low = "#C8DDD8", high = "#0F6E56",
                      name = "% Grants\nwith DS/AI",
                      guide = guide_colorbar(
                        barwidth  = 0.8,
                        barheight = 6,
                        title.position = "top"
                      )) +
  scale_x_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Top 15 NNF-funded Institutions (2017-2024)",
    subtitle = "Colour intensity = share of grants with data science/AI component",
    x        = "Total Funding (mDKK)",
    y        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.key.width   = unit(1.5, "cm"),
    legend.key.height  = unit(0.3, "cm"),
    legend.background  = element_rect(fill = "white", color = NA)
  )

ggsave("outputs/nnf_p5_institutions.png", p5,
       width = 11, height = 6, dpi = 150, bg = "white")


# ── 5. PLOT 6: GREEN (fixed - drop flat line, use count) ──────
green_trend <- nnf %>%
  filter(!is.na(green_pct), green_pct > 0) %>%
  group_by(year) %>%
  summarise(
    n_green    = n(),
    total_mDKK = sum(amount_mDKK, na.rm = TRUE),
    .groups    = "drop"
  )

p6 <- ggplot(green_trend, aes(x = year)) +
  geom_col(aes(y = total_mDKK), fill = "#1D9E75", width = 0.7, alpha = 0.85) +
  geom_text(aes(y = total_mDKK + 20,
                label = paste0(n_green, " grants")),
            size = 3.5, color = "#0F6E56", fontface = "bold") +
  scale_x_continuous(breaks = 2017:2024) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "NNF Green Research Funding Trajectory",
    subtitle = "Annual funding in grants classified under green/sustainability research categories",
    x        = NULL,
    y        = "Funding (mDKK)",
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme

ggsave("outputs/nnf_p6_green.png", p6,
       width = 10, height = 5, dpi = 150, bg = "white")


# ── 6. PLOT 7: BIGRAMS (two-word phrases) ────────────────────
custom_stops <- c(
  "novo", "nordisk", "foundation", "danish", "denmark",
  "university", "research", "project", "study", "studies",
  "including", "provide", "within", "will", "also", "can",
  "use", "aim", "new", "key", "high", "two", "important",
  "however", "due", "thus", "e.g", "i.e", "_x000d_", "x000d",
  "based", "using", "used", "data", "grant", "funding"
)

bigrams <- nnf %>%
  filter(is_ds) %>%
  select(grant_id, abstract) %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% c(stop_words$word, custom_stops),
    !word2 %in% c(stop_words$word, custom_stops),
    !str_detect(word1, "^[0-9]"),
    !str_detect(word2, "^[0-9]"),
    str_length(word1) > 2,
    str_length(word2) > 2
  ) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 20)

p7 <- ggplot(bigrams,
             aes(x = n, y = fct_reorder(bigram, n))) +
  geom_col(fill = "#1D9E75", width = 0.7) +
  geom_text(aes(x = n + 0.3, label = n),
            hjust = 0, size = 3.2, color = "#444444") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Key Two-Word Phrases in NNF Data Science Grants",
    subtitle = "Top 20 bigrams across DS/AI grant abstracts (stopwords removed)",
    x        = "Frequency",
    y        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/nnf_p7_bigrams.png", p7,
       width = 9, height = 6.5, dpi = 150, bg = "white")

# ── 7. INTERACTIVE WORD NETWORK (visNetwork) ─────────────────

# Build word pairs that co-occur in same abstract
word_pairs <- nnf %>%
  filter(is_ds) %>%
  select(grant_id, abstract) %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    !word %in% custom_stops,
    str_length(word) > 3,
    !str_detect(word, "^[0-9]"),
    !word %in% c("x000d", "_x000d_")
  ) %>%
  # Keep top 40 words to keep network readable
  group_by(word) %>%
  mutate(word_freq = n()) %>%
  ungroup() %>%
  filter(word_freq >= 15) %>%
  # Create pairs within same grant
  inner_join(., ., by = "grant_id",
             relationship = "many-to-many") %>%
  filter(word.x < word.y) %>%
  count(word.x, word.y, sort = TRUE) %>%
  filter(n >= 8) %>%
  rename(from = word.x, to = word.y, weight = n)

# Node sizes from word frequency
node_freq <- nnf %>%
  filter(is_ds) %>%
  select(grant_id, abstract) %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    !word %in% custom_stops,
    str_length(word) > 3,
    !str_detect(word, "^[0-9]"),
    !word %in% c("x000d", "_x000d_")
  ) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 15)

# Only keep nodes that appear in edges
nodes_in_edges <- unique(c(word_pairs$from, word_pairs$to))

nodes <- node_freq %>%
  filter(word %in% nodes_in_edges) %>%
  mutate(
    id    = word,
    label = word,
    value = n,
    title = paste0("<b>", word, "</b><br>Frequency: ", n),
    color = case_when(
      word %in% c("learning", "machine", "artificial", "intelligence",
                  "deep", "neural", "models", "prediction",
                  "algorithms", "training") ~ "#0F6E56",
      word %in% c("health", "disease", "diseases", "clinical",
                  "patient", "treatment", "cells", "human",
                  "medical", "cancer") ~ "#1D9E75",
      word %in% c("protein", "proteins", "genome", "genetic",
                  "molecular", "biological", "sequence") ~ "#5DCAA5",
      TRUE ~ "#9FE1CB"
    ),
    font.color = "white",
    font.size  = 14
  )

edges <- word_pairs %>%
  mutate(
    width = scales::rescale(weight, to = c(0.5, 6)),
    title = paste0(from, " + ", to, ": ", weight, " co-occurrences"),
    color = "#cccccc"
  )

net <- visNetwork(nodes, edges,
                  width  = "100%",
                  height = "650px") %>%
  visNodes(
    shape  = "dot",
    shadow = list(enabled = TRUE, size = 5)
  ) %>%
  visEdges(
    smooth = list(type = "continuous"),
    color  = list(color = "#cccccc", highlight = "#0F6E56")
  ) %>%
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1,
                            hover = TRUE),
    nodesIdSelection = list(enabled = TRUE,
                            useLabels = TRUE,
                            style = "color:#0F6E56;")
  ) %>%
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -60,
      centralGravity        = 0.01,
      springLength          = 100,
      springConstant        = 0.08
    ),
    stabilization = list(iterations = 200)
  ) %>%
  visLayout(randomSeed = 42) %>%
  visInteraction(
    navigationButtons = TRUE,
    tooltipDelay      = 100
  ) %>%
  visLegend(
    addNodes = data.frame(
      label = c("AI / ML", "Health", "Biology", "Other"),
      color = c("#0F6E56", "#1D9E75", "#5DCAA5", "#9FE1CB"),
      shape = "dot",
      size  = 20
    ),
    useGroups = FALSE,
    position  = "right",
    main      = "Topic cluster"
  )

# saveWidget(net, "outputs/nnf_word_network.html",
#            selfcontained = TRUE,
#            title = "NNF Data Science Grant — Word Co-occurrence Network")

# ============================================================
# Funder Comparison Analysis
# How does NNF's DS/AI investment compare to other Danish funders?
# ============================================================

# Clean and classify all funders
all_funders <- df_raw %>%
  rename(
    year       = `Grant Year`,
    amount_dkk = `Amount Granted`,
    abstract   = `Abstract`,
    title      = `Title`,
    oecd_l1    = `OECD Classification Level 1`,
    funder     = `Funder Name`
  ) %>%
  mutate(
    year        = as.integer(year),
    amount_dkk  = as.numeric(amount_dkk),
    amount_mDKK = amount_dkk / 1e6,
    abstract    = str_remove_all(abstract, "_x000d_|\\r|_x000D_"),
    title       = str_remove_all(title,    "_x000d_|\\r|_x000D_"),
    text        = paste(tolower(title), tolower(abstract))
  )

# Same DS/AI classifier
ds_pattern <- paste(c(
  "machine learning", "deep learning", "artificial intelligence",
  "neural network", "natural language processing", "\\bnlp\\b",
  "data science", "predictive model", "random forest", "gradient boost",
  "large language model", "\\bllm\\b", "generative ai", "transformer",
  "convolutional", "reinforcement learning", "foundation model",
  "bioinformatics", "data.driven", "bayesian",
  "computer vision", "image analysis", "statistical learning",
  "prediction model"
), collapse = "|")

ai_pattern <- paste(c(
  "machine learning", "deep learning", "artificial intelligence",
  "neural network", "\\bllm\\b", "generative ai", "transformer",
  "convolutional", "reinforcement learning", "foundation model",
  "large language model", "computer vision"
), collapse = "|")

all_funders <- all_funders %>%
  mutate(
    is_ds = str_detect(text, ds_pattern),
    is_ai = str_detect(text, ai_pattern),
    funder_short = case_when(
      grepl("Novo Nordisk", funder)    ~ "NNF",
      grepl("Independent Research", funder) ~ "Independent Research\nFund Denmark",
      grepl("Carlsberg", funder)       ~ "Carlsberg\nFoundation",
      grepl("Lundbeck", funder)        ~ "Lundbeck\nFoundation",
      grepl("Villum", funder)          ~ "Villum\nFoundation",
      grepl("VELUX", funder)           ~ "VELUX\nFoundation",
      TRUE ~ funder
    )
  )

# ── COMPARISON 1: Overall DS% by funder ──────────────────────
funder_summary <- all_funders %>%
  group_by(funder_short) %>%
  summarise(
    n_grants     = n(),
    n_ds         = sum(is_ds, na.rm = TRUE),
    n_ai         = sum(is_ai, na.rm = TRUE),
    pct_ds       = round(n_ds / n_grants * 100, 1),
    pct_ai       = round(n_ai / n_grants * 100, 1),
    total_mDKK   = round(sum(amount_mDKK, na.rm = TRUE)),
    ds_mDKK      = round(sum(amount_mDKK[is_ds], na.rm = TRUE)),
    .groups      = "drop"
  ) %>%
  arrange(desc(pct_ds))

print(funder_summary)

# ── COMPARISON 2: DS% growth by funder over time ─────────────
funder_year <- all_funders %>%
  group_by(funder_short, year) %>%
  summarise(
    n_grants = n(),
    n_ds     = sum(is_ds, na.rm = TRUE),
    pct_ds   = round(n_ds / n_grants * 100, 1),
    .groups  = "drop"
  ) %>%
  filter(n_grants >= 10)  # remove noisy small samples

print(funder_year %>% 
        tidyr::pivot_wider(
          id_cols    = year,
          names_from = funder_short,
          values_from = pct_ds
        ) %>% arrange(year),
      n = 20)

print(funder_year %>% 
        tidyr::pivot_wider(
          id_cols     = year,
          names_from  = funder_short,
          values_from = pct_ds
        ) %>% 
        arrange(year) %>%
        select(year, NNF, everything()),
      n = 20, width = 120)


# ============================================================
# NNF New Plots: Diverging Bar + Heatmap + Open Comp + Duration
# ============================================================

nnf <- df_raw %>%
  filter(grepl("Novo Nordisk", `Funder Name`, ignore.case = TRUE)) %>%
  rename(
    grant_id   = `Grant Id`,
    year       = `Grant Year`,
    amount_dkk = `Amount Granted`,
    abstract   = `Abstract`,
    title      = `Title`,
    oecd_l1    = `OECD Classification Level 1`,
    instrument = `Funder Specific Instrument`,
    open_comp  = `Open Competition Grant`,
    start_date = `Grant Start Date`,
    end_date   = `Grant End Date`
  ) %>%
  mutate(
    year        = as.integer(year),
    amount_dkk  = as.numeric(amount_dkk),
    amount_mDKK = amount_dkk / 1e6,
    abstract    = str_remove_all(abstract, "_x000d_|\\r|_x000D_"),
    title       = str_remove_all(title,    "_x000d_|\\r|_x000D_"),
    text        = paste(tolower(title), tolower(abstract)),
    start_date  = as.Date(start_date),
    end_date    = as.Date(end_date),
    oecd_short  = case_when(
      str_detect(oecd_l1, "Medical")      ~ "Health",
      str_detect(oecd_l1, "Natural")      ~ "Natural Sciences",
      str_detect(oecd_l1, "Engineering")  ~ "Engineering & Tech",
      str_detect(oecd_l1, "Agricultural") ~ "Agriculture & Vet",
      str_detect(oecd_l1, "Social")       ~ "Social Sciences",
      str_detect(oecd_l1, "Humanities")   ~ "Humanities",
      TRUE ~ "Other"
    )
  )

ds_pattern <- paste(c(
  "machine learning", "deep learning", "artificial intelligence",
  "neural network", "natural language processing", "\\bnlp\\b",
  "data science", "predictive model", "random forest", "gradient boost",
  "large language model", "\\bllm\\b", "generative ai", "transformer",
  "convolutional", "reinforcement learning", "foundation model",
  "bioinformatics", "data.driven", "bayesian",
  "computer vision", "image analysis", "statistical learning",
  "prediction model"
), collapse = "|")

nnf <- nnf %>% mutate(is_ds = str_detect(text, ds_pattern))

nnf_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title       = element_text(face = "bold", color = "#0F6E56", size = 13),
    plot.subtitle    = element_text(color = "#666666", size = 10),
    plot.caption     = element_text(color = "#999999", size = 8),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(color = "#555555", size = 10)
  )

custom_stops <- c(
  "novo","nordisk","foundation","danish","denmark","university",
  "research","project","study","studies","including","provide",
  "within","will","also","can","use","aim","new","key","high",
  "two","important","however","due","thus","based","using",
  "used","data","grant","funding","x000d","_x000d_"
)

get_bigrams <- function(data, period_label) {
  data %>%
    select(grant_id, abstract) %>%
    unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1","word2"), sep = " ") %>%
    filter(
      !word1 %in% c(stop_words$word, custom_stops),
      !word2 %in% c(stop_words$word, custom_stops),
      !str_detect(word1, "^[0-9]"),
      !str_detect(word2, "^[0-9]"),
      str_length(word1) > 2,
      str_length(word2) > 2
    ) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(bigram, sort = TRUE) %>%
    mutate(period = period_label, pct = n / sum(n) * 100)
}

early  <- nnf %>% filter(is_ds, year <= 2020) %>% get_bigrams("2017-2020")
recent <- nnf %>% filter(is_ds, year >= 2021) %>% get_bigrams("2021-2024")

# ── PLOT 1: DIVERGING BAR ─────────────────────────────────────
emerging <- recent %>%
  left_join(early %>% select(bigram, n_early=n, pct_early=pct), by="bigram") %>%
  mutate(
    n_early   = replace_na(n_early, 0),
    pct_early = replace_na(pct_early, 0),
    net_change = n - n_early
  ) %>%
  filter(n >= 5, net_change > 0,
         !bigram %in% c("machine learning","artificial intelligence")) %>%
  arrange(desc(net_change)) %>%
  slice_head(n = 10) %>%
  mutate(
    direction = "Emerging",
    category  = case_when(
      bigram %in% c("greenhouse gas","climate change","gas emissions",
                    "cropping systems","food security","meat analogues",
                    "plant diseases") ~ "Sustainability",
      bigram %in% c("deep learning","learning methods","learning models",
                    "tensor networks","real time","image analysis",
                    "neural networks") ~ "AI / ML Methods",
      TRUE ~ "Other emerging"
    )
  )

declining <- early %>%
  left_join(recent %>% select(bigram, n_recent=n), by="bigram") %>%
  mutate(
    n_recent   = replace_na(n_recent, 0),
    net_change = -(n - n_recent)
  ) %>%
  filter(n >= 4, net_change < 0,
         !bigram %in% c("machine learning","artificial intelligence")) %>%
  arrange(net_change) %>%
  slice_head(n = 6) %>%
  mutate(direction = "Declining", category = "Declining")

div_data <- bind_rows(
  emerging %>% select(bigram, net_change, category, direction),
  declining %>% select(bigram, net_change, category, direction)
) %>%
  arrange(net_change) %>%
  mutate(
    bigram   = factor(bigram, levels = bigram),
    cat_color = case_when(
      category == "Sustainability"  ~ "#0F6E56",
      category == "AI / ML Methods" ~ "#1D9E75",
      category == "Other emerging"  ~ "#5DCAA5",
      TRUE                          ~ "#C8DDD8"
    )
  )

p_diverging <- ggplot(div_data,
                      aes(x = net_change, y = bigram, fill = cat_color)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "#555555", linewidth = 0.6) +
  geom_text(
    data = filter(div_data, net_change >= 0),
    aes(x = net_change + 0.3, label = paste0("+", net_change)),
    hjust = 0, size = 3.2, color = "#444444"
  ) +
  geom_text(
    data = filter(div_data, net_change < 0),
    aes(x = net_change - 0.3, label = net_change),
    hjust = 1, size = 3.2, color = "#888888"
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    expand    = expansion(mult = c(0.18, 0.18)),
    labels    = function(x) ifelse(x >= 0, paste0("+", x), x)
  ) +
  annotate("text", x = 12, y = 2, label = "EMERGING ▶",
           size = 3, color = "#0F6E56", fontface = "bold") +
  annotate("text", x = -4.5, y = 13, label = "◀ DECLINING",
           size = 3, color = "#9FE1CB", fontface = "bold") +
  # manual legend
  annotate("point", x = -7.5, y = 5,  size = 3, color = "#0F6E56") +
  annotate("text",  x = -7,   y = 5,  size = 3, color = "#555", label = "Sustainability", hjust=0) +
  annotate("point", x = -7.5, y = 4,  size = 3, color = "#1D9E75") +
  annotate("text",  x = -7,   y = 4,  size = 3, color = "#555", label = "AI / ML", hjust=0) +
  annotate("point", x = -7.5, y = 3,  size = 3, color = "#5DCAA5") +
  annotate("text",  x = -7,   y = 3,  size = 3, color = "#555", label = "Other emerging", hjust=0) +
  annotate("point", x = -7.5, y = 2,  size = 3, color = "#C8DDD8") +
  annotate("text",  x = -7,   y = 2,  size = 3, color = "#555", label = "Declining", hjust=0) +
  labs(
    title    = "Shifting Research Language in NNF Data Science Grants",
    subtitle = "Net change in phrase frequency: 2021-2024 vs 2017-2020",
    x        = "Net change in number of grants containing phrase",
    y        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/nnf_pB1_diverging.png", p_diverging,
       width = 10, height = 7, dpi = 150, bg = "white")

# ── PLOT 2: BIGRAM HEATMAP BY YEAR ────────────────────────────

# Select key bigrams to track - hand-picked for narrative clarity
key_bigrams <- c(
  "machine learning", "deep learning", "artificial intelligence",
  "neural networks", "image analysis", "learning models",
  "greenhouse gas", "climate change", "food security",
  "meat analogues", "cropping systems",
  "eye screening", "rna degradation", "pancreatic cancer"
)

bigram_year <- nnf %>%
  filter(is_ds) %>%
  select(grant_id, year, abstract) %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1","word2"), sep = " ") %>%
  filter(
    !word1 %in% c(stop_words$word, custom_stops),
    !word2 %in% c(stop_words$word, custom_stops),
    !str_detect(word1, "^[0-9]"),
    !str_detect(word2, "^[0-9]")
  ) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  filter(bigram %in% key_bigrams) %>%
  count(bigram, year) %>%
  # fill zeros for missing year/bigram combos
  complete(bigram, year = 2017:2024, fill = list(n = 0)) %>%
  mutate(
    category = case_when(
      bigram %in% c("greenhouse gas","climate change","food security",
                    "meat analogues","cropping systems") ~ "Sustainability",
      bigram %in% c("machine learning","deep learning",
                    "artificial intelligence","neural networks",
                    "image analysis","learning models") ~ "AI / ML",
      TRUE ~ "Declining"
    ),
    # order within category
    bigram = factor(bigram, levels = rev(c(
      "machine learning","deep learning","artificial intelligence",
      "neural networks","image analysis","learning models",
      "greenhouse gas","climate change","food security",
      "meat analogues","cropping systems",
      "eye screening","rna degradation","pancreatic cancer"
    )))
  )

# category label positions for facet-style strip
cat_ypos <- bigram_year %>%
  group_by(category) %>%
  summarise(
    ymin = min(as.integer(bigram)),
    ymax = max(as.integer(bigram)),
    ymid = (min(as.integer(bigram)) + max(as.integer(bigram))) / 2,
    .groups = "drop"
  )

p_heatmap <- ggplot(bigram_year,
                    aes(x = year, y = bigram, fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            size = 3, color = "white", fontface = "bold") +
  geom_text(aes(label = ifelse(n == 0, "·", "")),
            size = 4, color = "#cccccc") +
  scale_fill_gradientn(
    colors = c("#E8F5F2","#9FE1CB","#5DCAA5","#1D9E75","#0F6E56"),
    values = rescale(c(0, 1, 3, 8, 20)),
    name   = "Grant\ncount",
    guide  = guide_colorbar(
      barwidth  = 0.6,
      barheight = 6,
      title.position = "top"
    )
  ) +
  # category strip on left
  annotate("rect", xmin = 2016.3, xmax = 2016.7,
           ymin = 8.5, ymax = 14.5, fill = "#1D9E75", alpha = 0.8) +
  annotate("text", x = 2016.5, y = 11.5, label = "AI/ML",
           angle = 90, size = 2.8, color = "white", fontface = "bold") +
  annotate("rect", xmin = 2016.3, xmax = 2016.7,
           ymin = 3.5, ymax = 8.5, fill = "#0F6E56", alpha = 0.8) +
  annotate("text", x = 2016.5, y = 6, label = "Sustain.",
           angle = 90, size = 2.8, color = "white", fontface = "bold") +
  annotate("rect", xmin = 2016.3, xmax = 2016.7,
           ymin = 0.5, ymax = 3.5, fill = "#C8DDD8", alpha = 0.8) +
  annotate("text", x = 2016.5, y = 2, label = "Decl.",
           angle = 90, size = 2.5, color = "#555", fontface = "bold") +
  # 2021 inflection line
  geom_vline(xintercept = 2020.5, color = "#0F6E56",
             linewidth = 0.8, linetype = "dashed", alpha = 0.6) +
  annotate("text", x = 2020.5, y = 14.8,
           label = "Strategy shift", size = 2.8, color = "#0F6E56",
           hjust = 0.5, fontface = "italic") +
  scale_x_continuous(
    breaks = 2017:2024,
    expand = expansion(add = c(0.8, 0.3))
  ) +
  labs(
    title    = "Research Topic Trajectories in NNF Data Science Grants (2017-2024)",
    subtitle = "Annual frequency of key phrases in grant abstracts | Dashed line = NNF sustainability strategy shift",
    x        = NULL,
    y        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(size = 10, color = "#444444"),
    legend.position = "right"
  )

ggsave("outputs/nnf_pB2_heatmap.png", p_heatmap,
       width = 11, height = 7, dpi = 150, bg = "white")

# ── PLOT 3: OPEN COMPETITION ──────────────────────────────────
oc_trend <- nnf %>%
  filter(open_comp == "Yes") %>%
  group_by(year) %>%
  summarise(
    n_total  = n(),
    n_ds     = sum(is_ds),
    pct_ds   = n_ds / n_total * 100,
    avg_mDKK = mean(amount_mDKK, na.rm=TRUE),
    .groups  = "drop"
  )

sf_oc <- max(oc_trend$avg_mDKK) / max(oc_trend$pct_ds) * 1.1

p_oc <- ggplot(oc_trend, aes(x = year)) +
  geom_col(aes(y = pct_ds), fill = "#C8DDD8", width = 0.6) +
  geom_line(aes(y = avg_mDKK / sf_oc),
            color = "#0F6E56", linewidth = 1.3) +
  geom_point(aes(y = avg_mDKK / sf_oc),
             color = "#0F6E56", size = 3.5) +
  geom_text(aes(y = pct_ds + 0.4,
                label = paste0(round(pct_ds,1), "%")),
            size = 3, color = "#1D9E75", fontface = "bold") +
  geom_text(aes(y = avg_mDKK / sf_oc,
                label = paste0("DKK ", round(avg_mDKK,1), "M")),
            size = 2.8, color = "#0F6E56", vjust = -1) +
  scale_x_continuous(breaks = 2017:2024) +
  scale_y_continuous(
    name     = "% Open Competition Grants with DS/AI",
    sec.axis = sec_axis(
      ~ . * sf_oc,
      name   = "Avg Grant Size (mDKK)",
      labels = function(x) paste0("DKK ", round(x,1), "M")
    )
  ) +
  labs(
    title    = "Open Competition: Growing DS/AI Share and Award Sizes",
    subtitle = "Bars = % of open competition grants with DS/AI | Line = average grant size",
    x        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(axis.title.y.right = element_text(color = "#0F6E56"))

ggsave("outputs/nnf_pB3_open_comp.png", p_oc,
       width = 10, height = 5, dpi = 150, bg = "white")

# ── PLOT 4: GRANT DURATION ────────────────────────────────────
duration <- nnf %>%
  filter(!is.na(start_date), !is.na(end_date), end_date > start_date) %>%
  mutate(duration_years = as.numeric(end_date - start_date) / 365.25) %>%
  filter(duration_years > 0, duration_years < 15)

# 4a: density distribution DS vs non-DS
means_df <- duration %>%
  group_by(is_ds) %>%
  summarise(m = mean(duration_years), .groups="drop") %>%
  mutate(label = paste0(ifelse(is_ds, "DS/AI mean: ", "Others mean: "),
                        round(m,1), " yrs"))

p_dur_dist <- ggplot(duration,
                     aes(x = duration_years,
                         fill = is_ds, color = is_ds)) +
  geom_density(alpha = 0.5, linewidth = 0.8) +
  geom_vline(data = means_df,
             aes(xintercept = m, color = is_ds),
             linetype = "dashed", linewidth = 1) +
  geom_text(data = means_df,
            aes(x = m + 0.15, y = 0.42,
                label = label, color = is_ds),
            hjust = 0, size = 3.2, show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE"="#C8DDD8","TRUE"="#1D9E75"),
                    labels = c("FALSE"="Non-DS/AI","TRUE"="DS/AI"),
                    name   = NULL) +
  scale_color_manual(values = c("FALSE"="#9FE1CB","TRUE"="#0F6E56"),
                     labels = c("FALSE"="Non-DS/AI","TRUE"="DS/AI"),
                     name   = NULL) +
  scale_x_continuous(breaks=1:10, limits=c(0,10)) +
  labs(
    title    = "DS/AI Grants Run Longer Than Portfolio Average",
    subtitle = "Grant duration distribution | Dashed lines = group means",
    x        = "Grant Duration (years)",
    y        = "Density",
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(legend.position = "top")

ggsave("outputs/nnf_pB4a_duration_dist.png", p_dur_dist,
       width = 9, height = 5, dpi = 150, bg = "white")

# 4b: duration by domain
dur_domain <- duration %>%
  filter(oecd_short != "Other") %>%
  group_by(oecd_short) %>%
  summarise(
    n        = n(),
    mean_yrs = mean(duration_years),
    se       = sd(duration_years) / sqrt(n),
    .groups  = "drop"
  ) %>%
  arrange(mean_yrs) %>%
  mutate(
    oecd_short = factor(oecd_short, levels = oecd_short),
    bar_color  = colorRampPalette(c("#C8DDD8","#0F6E56"))(n())[rank(mean_yrs)]
  )

p_dur_domain <- ggplot(dur_domain,
                       aes(x = mean_yrs, y = oecd_short)) +
  geom_col(aes(fill = bar_color), width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(xmin = mean_yrs - se, xmax = mean_yrs + se),
                width = 0.2, color = "#555555") +
  geom_text(aes(x = mean_yrs + se + 0.06,
                label = paste0(round(mean_yrs,1), " yrs")),
            hjust = 0, size = 3.5, color = "#444444") +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Grant Duration by Research Domain",
    subtitle = "Mean grant length | Error bars = ±1 SE",
    x        = "Mean Duration (years)",
    y        = NULL,
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/nnf_pB4b_duration_domain.png", p_dur_domain,
       width = 9, height = 5, dpi = 150, bg = "white")

# 4c: duration trend DS vs all
dur_trend <- duration %>%
  group_by(year) %>%
  summarise(
    overall = mean(duration_years),
    ds      = mean(duration_years[is_ds], na.rm=TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(overall,ds), names_to="type", values_to="mean_yrs") %>%
  filter(!is.nan(mean_yrs)) %>%
  mutate(type = recode(type,
                       overall = "All grants",
                       ds      = "DS/AI grants"
  ))

p_dur_trend <- ggplot(dur_trend,
                      aes(x=year, y=mean_yrs,
                          color=type, linetype=type)) +
  geom_line(linewidth=1.3) +
  geom_point(size=3.5) +
  geom_text(data=filter(dur_trend, year==2024),
            aes(label=paste0(round(mean_yrs,1)," yrs")),
            hjust=-0.2, size=3.3, fontface="bold") +
  scale_color_manual(
    values = c("All grants"="#9FE1CB","DS/AI grants"="#0F6E56"),
    name   = NULL
  ) +
  scale_linetype_manual(
    values = c("All grants"="dashed","DS/AI grants"="solid"),
    name   = NULL
  ) +
  scale_x_continuous(breaks=2017:2024,
                     expand=expansion(mult=c(0.05,0.15))) +
  scale_y_continuous(limits=c(2,6)) +
  labs(
    title    = "DS/AI Grants Consistently Longer Than Portfolio Average",
    subtitle = "Mean grant duration by year",
    x        = NULL,
    y        = "Mean Duration (years)",
    caption  = "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"
  ) +
  nnf_theme +
  theme(legend.position="bottom")

ggsave("outputs/nnf_pB4c_duration_trend.png", p_dur_trend,
       width = 9, height = 5, dpi = 150, bg = "white")

