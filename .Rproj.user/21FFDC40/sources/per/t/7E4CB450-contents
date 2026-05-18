# =============================================================================
# PubMed Abstract & Title Fetcher + Filter
# =============================================================================
# Fetches titles, abstracts, and metadata from a list of PMIDs via NCBI
# Entrez API, then applies text-based filters (useful for Scopus-style reviews).
#
# Dependencies: rentrez, xml2, dplyr, stringr, readr
# Install with: install.packages(c("rentrez", "xml2", "dplyr", "stringr", "readr"))
# =============================================================================
library(rentrez)
library(xml2)
library(dplyr)
library(stringr)
library(readr)

# =============================================================================
# CONFIGURATION
# =============================================================================

# --- Input: your PMIDs ---
# Option A: provide them inline
pmids <- c(
  "30237159"
)

# Option B: read from a file (one PMID per line, or CSV with a "pmid" column)
# pmids <- read_lines("my_pmids.txt")          # one per line
# pmids <- read_csv("my_pmids.csv")$pmid       # CSV with header

# --- NCBI API key (optional but recommended for higher rate limits) ---
# Register at: https://www.ncbi.nlm.nih.gov/account/
# set_entrez_key("YOUR_KEY_HERE")
# Or set env var: Sys.setenv(ENTREZ_KEY = "YOUR_KEY_HERE")

# --- Batch size (NCBI recommends <= 200 per request) ---
BATCH_SIZE <- 100

# --- Output file ---
OUTPUT_FILE <- "pubmed_results.csv"

# =============================================================================
# STEP 1: FETCH RECORDS FROM PUBMED
# =============================================================================

#' Parse a single PubmedArticle XML node into a named list
parse_pubmed_article <- function(article_node) {
  safe_text <- function(node, xpath) {
    result <- xml_find_first(node, xpath)
    if (is.na(result)) return(NA_character_)
    xml_text(result)
  }
  
  safe_texts <- function(node, xpath) {
    results <- xml_find_all(node, xpath)
    if (length(results) == 0) return(NA_character_)
    paste(xml_text(results), collapse = "; ")
  }
  
  list(
    pmid        = safe_text(article_node,  ".//PMID"),
    title       = safe_text(article_node,  ".//ArticleTitle"),
    abstract    = safe_texts(article_node, ".//AbstractText"),
    journal     = safe_text(article_node,  ".//Journal/Title"),
    year        = safe_text(article_node,  ".//PubDate/Year"),
    doi         = safe_text(article_node,  ".//ArticleId[@IdType='doi']"),
    authors     = safe_texts(article_node, ".//Author/LastName"),
    keywords    = safe_texts(article_node, ".//Keyword"),
    pub_type    = safe_texts(article_node, ".//PublicationType"),
    mesh_terms  = safe_texts(article_node, ".//MeshHeading/DescriptorName")
  )
}

#' Fetch and parse a batch of PMIDs
fetch_batch <- function(batch_ids) {
  tryCatch({
    raw_xml <- entrez_fetch(
      db      = "pubmed",
      id      = batch_ids,
      rettype = "xml",
      retmode = "xml"
    )
    doc <- read_xml(raw_xml)
    articles <- xml_find_all(doc, "//PubmedArticle")
    lapply(articles, parse_pubmed_article)
  }, error = function(e) {
    message("  Error fetching batch: ", conditionMessage(e))
    NULL
  })
}

message("Fetching ", length(pmids), " records from PubMed in batches of ", BATCH_SIZE, "...")

batches    <- split(pmids, ceiling(seq_along(pmids) / BATCH_SIZE))
all_records <- list()

for (i in seq_along(batches)) {
  message("  Batch ", i, "/", length(batches), " (", length(batches[[i]]), " records)...")
  records <- fetch_batch(batches[[i]])
  all_records <- c(all_records, records)
  if (i < length(batches)) Sys.sleep(0.4)   # polite pause between requests
}

# Flatten to data frame
df_raw <- bind_rows(lapply(all_records, as.data.frame, stringsAsFactors = FALSE))

message("Fetched ", nrow(df_raw), " records successfully.")

# =============================================================================
# STEP 2: BASIC CLEAN-UP
# =============================================================================

df <- df_raw %>%
  mutate(
    title    = str_squish(title),
    abstract = str_squish(abstract),
    year     = as.integer(year),
    has_abstract = !is.na(abstract) & nchar(abstract) > 10
  )

# =============================================================================
# STEP 3: FILTERS
# =============================================================================
# Adapt these to your review's inclusion/exclusion criteria.
# Each filter adds a logical column; the combined filter is applied at the end.

# --- 3a. Year range ---
YEAR_MIN <- 2015
YEAR_MAX <- 2024

df <- df %>%
  mutate(filter_year = !is.na(year) & year >= YEAR_MIN & year <= YEAR_MAX)

# --- 3b. Must have an abstract ---
df <- df %>%
  mutate(filter_has_abstract = has_abstract)

# --- 3c. Keyword search in title + abstract (case-insensitive, any match) ---
# Define your include terms (OR logic within the vector)
INCLUDE_TERMS <- c(
  "machine learning",
  "artificial intelligence",
  "deep learning",
  "neural network"
)

# Define exclude terms (if any match → exclude)
EXCLUDE_TERMS <- c(
  "retracted",
  "erratum",
  "corrigendum"
)

make_pattern <- function(terms) {
  paste(str_c("\\b", str_escape(terms), "\\b"), collapse = "|")
}

include_pattern <- make_pattern(INCLUDE_TERMS)
exclude_pattern <- make_pattern(EXCLUDE_TERMS)

df <- df %>%
  mutate(
    search_text       = paste(coalesce(title, ""), coalesce(abstract, "")),
    filter_include    = str_detect(str_to_lower(search_text), include_pattern),
    filter_exclude    = !str_detect(str_to_lower(search_text), exclude_pattern)
  )

# --- 3d. Publication type filter (optional) ---
# Remove reviews, meta-analyses, etc. if you only want original research
# Uncomment to activate:
# EXCLUDE_PUB_TYPES <- c("Review", "Meta-Analysis", "Systematic Review")
# exclude_type_pattern <- paste(EXCLUDE_PUB_TYPES, collapse = "|")
# df <- df %>%
#   mutate(filter_pub_type = is.na(pub_type) |
#            !str_detect(pub_type, exclude_type_pattern))

# =============================================================================
# STEP 4: APPLY COMBINED FILTER
# =============================================================================

# Select which filter columns to combine
filter_cols <- c("filter_year", "filter_has_abstract", "filter_include", "filter_exclude")

df <- df %>%
  mutate(
    passes_all_filters = rowSums(across(all_of(filter_cols))) == length(filter_cols)
  )

df_included <- df %>% filter(passes_all_filters)
df_excluded <- df %>% filter(!passes_all_filters)

message("\n=== Filter Summary ===")
message("Total records fetched:  ", nrow(df))
message("Records INCLUDED:       ", nrow(df_included))
message("Records EXCLUDED:       ", nrow(df_excluded))
message("")
message("Breakdown by filter (records failing each):")
for (col in filter_cols) {
  n_fail <- sum(!df[[col]], na.rm = TRUE)
  message("  ", col, ": ", n_fail, " failed")
}

# =============================================================================
# STEP 5: EXPORT RESULTS
# =============================================================================

# Full table with filter flags
write_csv(df %>% select(-search_text), OUTPUT_FILE)
message("\nFull results (with filter flags) saved to: ", OUTPUT_FILE)

# Included-only table
included_file <- sub("\\.csv$", "_included.csv", OUTPUT_FILE)
write_csv(df_included %>% select(pmid, year, journal, title, abstract, doi,
                                 authors, keywords, mesh_terms),
          included_file)
message("Included-only results saved to: ", included_file)

# =============================================================================
# STEP 6: QUICK SUMMARY TABLE (console)
# =============================================================================

message("\n=== Included Records Preview ===")
df_included %>%
  select(pmid, year, journal, title) %>%
  print(n = 20, width = 120)