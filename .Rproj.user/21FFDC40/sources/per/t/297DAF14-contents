# ============================================================
# NNF Data Prep: Generate pre-processed CSVs for Shiny app
# Run this locally once, commit CSVs to repo
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(rio)
library(tidytext)
library(igraph)
library(lubridate)

df_raw <- rio::import("raw_data/grants-export-1.xlsx", sheet = "transposed")

# ── CLEAN & CLASSIFY ─────────────────────────────────────────
all_funders <- df_raw %>%
  rename(
    grant_id    = `Grant Id`,
    year        = `Grant Year`,
    amount_dkk  = `Amount Granted`,
    abstract    = `Abstract`,
    title       = `Title`,
    oecd_l1     = `OECD Classification Level 1`,
    funder      = `Funder Name`,
    instrument  = `Funder Specific Instrument`,
    org_name    = `Organisation name`,
    org_country = `Organisation country`,
    open_comp   = `Open Competition Grant`,
    start_date  = `Grant Start Date`,
    end_date    = `Grant End Date`,
    green_pct   = `DST green research percentage`
  ) %>%
  mutate(
    year        = as.integer(year),
    amount_dkk  = as.numeric(amount_dkk),
    amount_mDKK = amount_dkk / 1e6,
    green_pct   = suppressWarnings(as.numeric(green_pct)),
    abstract    = str_remove_all(abstract, "_x000d_|\\r|_x000D_"),
    title       = str_remove_all(title,    "_x000d_|\\r|_x000D_"),
    start_date  = as.Date(start_date),
    end_date    = as.Date(end_date),
    text        = paste(tolower(title), tolower(abstract)),
    funder_short = case_when(
      grepl("Novo Nordisk", funder)         ~ "NNF",
      grepl("Independent Research", funder) ~ "Independent Research Fund DK",
      grepl("Carlsberg", funder)            ~ "Carlsberg Foundation",
      grepl("Lundbeck", funder)             ~ "Lundbeck Foundation",
      grepl("Villum", funder)               ~ "Villum Foundation",
      grepl("VELUX", funder)                ~ "VELUX Foundation",
      TRUE ~ funder
    ),
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

ds_pattern <- paste(c(
  "machine learning","deep learning","artificial intelligence",
  "neural network","natural language processing","\\bnlp\\b",
  "data science","predictive model","random forest","gradient boost",
  "large language model","\\bllm\\b","generative ai","transformer",
  "convolutional","reinforcement learning","foundation model",
  "bioinformatics","data.driven","bayesian",
  "computer vision","image analysis","statistical learning","prediction model"
), collapse="|")

ai_pattern <- paste(c(
  "machine learning","deep learning","artificial intelligence",
  "neural network","\\bllm\\b","generative ai","transformer",
  "convolutional","reinforcement learning","foundation model",
  "large language model","computer vision"
), collapse="|")

all_funders <- all_funders %>%
  mutate(is_ds=str_detect(text,ds_pattern), is_ai=str_detect(text,ai_pattern))

nnf <- all_funders %>% filter(funder_short=="NNF")

custom_stops <- c(
  "novo","nordisk","foundation","danish","denmark","university",
  "research","project","study","studies","including","provide",
  "within","will","also","can","use","aim","new","key","high",
  "two","important","however","due","thus","based","using",
  "used","data","grant","funding","x000d","_x000d_"
)

dir.create("app_data", showWarnings=FALSE)

# ── 1. PORTFOLIO TREND ───────────────────────────────────────
nnf %>%
  group_by(year) %>%
  summarise(
    n_grants=n(), total_mDKK=sum(amount_mDKK,na.rm=T),
    n_ds=sum(is_ds), pct_ds=mean(is_ds)*100,
    ds_funding_mDKK=sum(amount_mDKK[is_ds],na.rm=T),
    .groups="drop"
  ) %>%
  write.csv("app_data/portfolio_trend.csv", row.names=FALSE)

# ── 2. DOMAIN TREND ──────────────────────────────────────────
nnf %>%
  filter(oecd_short!="Other") %>%
  group_by(year,oecd_short) %>%
  summarise(total_mDKK=sum(amount_mDKK,na.rm=T),.groups="drop") %>%
  write.csv("app_data/domain_trend.csv", row.names=FALSE)

# ── 3. AI TREND ──────────────────────────────────────────────
nnf %>%
  group_by(year) %>%
  summarise(n=n(),pct_ds=sum(is_ds)/n()*100,
            pct_ai=sum(is_ai)/n()*100,.groups="drop") %>%
  write.csv("app_data/ai_trend.csv", row.names=FALSE)

# ── 4. DS BY DOMAIN ──────────────────────────────────────────
nnf %>%
  filter(is_ds,oecd_short!="Other") %>%
  group_by(oecd_short) %>%
  summarise(n_grants=n(),total_mDKK=sum(amount_mDKK,na.rm=T),.groups="drop") %>%
  arrange(total_mDKK) %>%
  mutate(bar_color=colorRampPalette(c("#C8DDD8","#0F6E56"))(n())[rank(total_mDKK)]) %>%
  write.csv("app_data/ds_domain.csv", row.names=FALSE)

# ── 5. TOP INSTITUTIONS ──────────────────────────────────────
nnf %>%
  filter(!is.na(org_name),org_name!="N/A") %>%
  group_by(org_name) %>%
  summarise(n_grants=n(),total_mDKK=sum(amount_mDKK,na.rm=T),
            n_ds=sum(is_ds),pct_ds=round(n_ds/n_grants*100,1),.groups="drop") %>%
  arrange(desc(total_mDKK)) %>% slice_head(n=15) %>%
  arrange(total_mDKK) %>%
  write.csv("app_data/top_orgs.csv", row.names=FALSE)

# ── 6. FUNDER COMPARISON ─────────────────────────────────────
all_funders %>%
  group_by(funder_short,year) %>%
  summarise(n=n(),n_ds=sum(is_ds),pct_ds=n_ds/n*100,.groups="drop") %>%
  filter(n>=10) %>%
  write.csv("app_data/funder_year.csv", row.names=FALSE)

all_funders %>%
  group_by(funder_short) %>%
  summarise(
    total_grants=n(), ds_grants=sum(is_ds),
    pct_ds=paste0(round(mean(is_ds)*100,1),"%"),
    ai_grants=sum(is_ai),
    pct_ai=paste0(round(mean(is_ai)*100,1),"%"),
    total_mDKK=round(sum(amount_mDKK,na.rm=T)),
    ds_mDKK=round(sum(amount_mDKK[is_ds],na.rm=T)),
    .groups="drop"
  ) %>%
  arrange(desc(ds_mDKK)) %>%
  rename(Funder=funder_short,`Total Grants`=total_grants,
         `DS/AI Grants`=ds_grants,`% DS/AI`=pct_ds,
         `AI/ML Grants`=ai_grants,`% AI/ML`=pct_ai,
         `Total (mDKK)`=total_mDKK,`DS Funding (mDKK)`=ds_mDKK) %>%
  write.csv("app_data/funder_summary.csv", row.names=FALSE)

# ── 7. COUNTRY DATA ──────────────────────────────────────────
library(countrycode)
nnf %>%
  filter(!is.na(org_country),org_country!="N/A",org_country!="ZZ") %>%
  mutate(iso2=toupper(org_country)) %>%
  group_by(iso2) %>%
  summarise(n_grants=n(),total_mDKK=round(sum(amount_mDKK,na.rm=T),1),
            n_ds=sum(is_ds),pct_ds=round(n_ds/n()*100,1),.groups="drop") %>%
  mutate(
    country_name=countrycode(iso2,"iso2c","country.name"),
    country_name=case_when(iso2=="FO"~"Faroe Islands",iso2=="GL"~"Greenland",
                           is.na(country_name)~iso2,TRUE~country_name),
    flag=countrycode(iso2,"iso2c","unicode.symbol"),
    flag=ifelse(is.na(flag),"",flag),
    label=paste0(flag," ",country_name)
  ) %>%
  arrange(desc(total_mDKK)) %>% slice_head(n=20) %>%
  arrange(total_mDKK) %>%
  write.csv("app_data/country_data.csv", row.names=FALSE)

# ── 8. DIVERGING BAR ─────────────────────────────────────────
get_bigrams <- function(data,period_label) {
  data %>% select(grant_id,abstract) %>%
    unnest_tokens(bigram,abstract,token="ngrams",n=2) %>%
    separate(bigram,c("word1","word2"),sep=" ") %>%
    filter(!word1 %in% c(stop_words$word,custom_stops),
           !word2 %in% c(stop_words$word,custom_stops),
           !str_detect(word1,"^[0-9]"),!str_detect(word2,"^[0-9]"),
           str_length(word1)>2,str_length(word2)>2) %>%
    unite(bigram,word1,word2,sep=" ") %>%
    count(bigram,sort=T) %>%
    mutate(period=period_label,pct=n/sum(n)*100)
}

early  <- nnf %>% filter(is_ds,year<=2020) %>% get_bigrams("2017-2020")
recent <- nnf %>% filter(is_ds,year>=2021) %>% get_bigrams("2021-2024")

emerging <- recent %>%
  left_join(early %>% select(bigram,n_early=n,pct_early=pct),by="bigram") %>%
  mutate(n_early=replace_na(n_early,0),pct_early=replace_na(pct_early,0),
         net_change=n-n_early) %>%
  filter(n>=5,net_change>0,
         !bigram %in% c("machine learning","artificial intelligence")) %>%
  arrange(desc(net_change)) %>% slice_head(n=10) %>%
  mutate(category=case_when(
    bigram %in% c("greenhouse gas","climate change","gas emissions",
                  "cropping systems","food security","meat analogues",
                  "plant diseases") ~ "Sustainability",
    bigram %in% c("deep learning","learning methods","learning models",
                  "tensor networks","real time","image analysis",
                  "neural networks") ~ "AI / ML Methods",
    TRUE ~ "Other emerging"))

declining <- early %>%
  left_join(recent %>% select(bigram,n_recent=n),by="bigram") %>%
  mutate(n_recent=replace_na(n_recent,0),net_change=-(n-n_recent)) %>%
  filter(n>=4,net_change<0,
         !bigram %in% c("machine learning","artificial intelligence")) %>%
  arrange(net_change) %>% slice_head(n=6) %>%
  mutate(category="Declining")

bind_rows(
  emerging %>% select(bigram,net_change,category),
  declining %>% select(bigram,net_change,category)
) %>%
  arrange(net_change) %>%
  mutate(bar_color=case_when(
    category=="Sustainability"  ~ "#0F6E56",
    category=="AI / ML Methods" ~ "#1D9E75",
    category=="Other emerging"  ~ "#5DCAA5",
    TRUE ~ "#C8DDD8")) %>%
  write.csv("app_data/div_data.csv", row.names=FALSE)

# ── 9. BIGRAM HEATMAP ────────────────────────────────────────
key_bigrams <- c(
  "machine learning","deep learning","artificial intelligence",
  "neural networks","image analysis","learning models",
  "greenhouse gas","climate change","food security",
  "meat analogues","cropping systems",
  "eye screening","rna degradation","pancreatic cancer"
)

nnf %>%
  filter(is_ds) %>%
  select(grant_id,year,abstract) %>%
  unnest_tokens(bigram,abstract,token="ngrams",n=2) %>%
  separate(bigram,c("word1","word2"),sep=" ") %>%
  filter(!word1 %in% c(stop_words$word,custom_stops),
         !word2 %in% c(stop_words$word,custom_stops),
         !str_detect(word1,"^[0-9]"),!str_detect(word2,"^[0-9]")) %>%
  unite(bigram,word1,word2,sep=" ") %>%
  filter(bigram %in% key_bigrams) %>%
  count(bigram,year) %>%
  complete(bigram,year=2017:2024,fill=list(n=0)) %>%
  mutate(category=case_when(
    bigram %in% c("greenhouse gas","climate change","food security",
                  "meat analogues","cropping systems") ~ "Sustainability",
    bigram %in% c("machine learning","deep learning","artificial intelligence",
                  "neural networks","image analysis","learning models") ~ "AI / ML",
    TRUE ~ "Declining")) %>%
  write.csv("app_data/bigram_year.csv", row.names=FALSE)

# ── 10. OPEN COMPETITION ─────────────────────────────────────
nnf %>%
  filter(open_comp=="Yes") %>%
  group_by(year) %>%
  summarise(n_total=n(),n_ds=sum(is_ds),pct_ds=n_ds/n_total*100,
            avg_mDKK=mean(amount_mDKK,na.rm=T),.groups="drop") %>%
  write.csv("app_data/oc_trend.csv", row.names=FALSE)

# ── 11. DURATION ─────────────────────────────────────────────
duration <- nnf %>%
  filter(!is.na(start_date),!is.na(end_date),end_date>start_date) %>%
  mutate(duration_years=as.numeric(end_date-start_date)/365.25) %>%
  filter(duration_years>0,duration_years<15) %>%
  select(grant_id,year,oecd_short,is_ds,duration_years)

duration %>% write.csv("app_data/duration.csv", row.names=FALSE)

duration %>%
  filter(oecd_short!="Other") %>%
  group_by(oecd_short) %>%
  summarise(n=n(),mean_yrs=mean(duration_years),
            se=sd(duration_years)/sqrt(n),.groups="drop") %>%
  arrange(mean_yrs) %>%
  mutate(bar_color=colorRampPalette(c("#C8DDD8","#0F6E56"))(n())[rank(mean_yrs)]) %>%
  write.csv("app_data/dur_domain.csv", row.names=FALSE)

# ── 12. WORD NETWORK ─────────────────────────────────────────
word_freq_df <- nnf %>%
  filter(is_ds) %>%
  select(grant_id,abstract) %>%
  unnest_tokens(word,abstract) %>%
  anti_join(stop_words,by="word") %>%
  filter(!word %in% custom_stops,str_length(word)>3,!str_detect(word,"^[0-9]"))

word_pairs <- word_freq_df %>%
  group_by(word) %>% mutate(wf=n()) %>% ungroup() %>%
  filter(wf>=15) %>%
  inner_join(.,.,by="grant_id",relationship="many-to-many") %>%
  filter(word.x<word.y) %>%
  count(word.x,word.y,sort=T) %>%
  filter(n>=8) %>% rename(from=word.x,to=word.y,weight=n)

node_freq <- word_freq_df %>% count(word,sort=T) %>% filter(n>=15)
nodes_in_edges <- unique(c(word_pairs$from,word_pairs$to))

vis_nodes <- node_freq %>%
  filter(word %in% nodes_in_edges) %>%
  mutate(
    id=word,label=word,value=n,
    title=paste0("<b>",word,"</b><br>Frequency: ",n),
    color=case_when(
      word %in% c("learning","machine","artificial","intelligence",
                  "deep","neural","models","prediction","algorithms",
                  "training","generative") ~ "#0F6E56",
      word %in% c("health","disease","diseases","clinical","patient",
                  "treatment","cells","human","medical","cancer","risk") ~ "#1D9E75",
      word %in% c("protein","proteins","genome","genetic",
                  "molecular","biological","sequence") ~ "#5DCAA5",
      TRUE ~ "#9FE1CB"),
    font.color="white",font.size=14,physics=FALSE)

# Pre-compute fixed layout
g <- igraph::graph_from_data_frame(
  d=word_pairs %>% select(from,to),
  vertices=vis_nodes %>% select(id), directed=FALSE)
layout_coords <- igraph::layout_with_fr(g,niter=500)
vis_nodes$x <- layout_coords[,1]*200
vis_nodes$y <- layout_coords[,2]*200

vis_edges <- word_pairs %>%
  mutate(width=scales::rescale(weight,to=c(0.5,5)),
         title=paste0(from," + ",to,": ",weight," co-occurrences"),
         color="#cccccc")

vis_nodes %>% write.csv("app_data/vis_nodes.csv", row.names=FALSE)
vis_edges %>% write.csv("app_data/vis_edges.csv", row.names=FALSE)

# ── 13. GRANTS TABLE ─────────────────────────────────────────
nnf %>%
  filter(is_ds) %>%
  select(year,title,oecd_short,instrument,org_name,amount_mDKK,abstract) %>%
  mutate(amount_mDKK=round(amount_mDKK,2),
         abstract=str_trunc(abstract,300)) %>%
  rename(Year=year,Title=title,Domain=oecd_short,Instrument=instrument,
         Institution=org_name,`Amount (mDKK)`=amount_mDKK,Abstract=abstract) %>%
  write.csv("app_data/grants_table.csv", row.names=FALSE)

