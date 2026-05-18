# ============================================================
# NNF Portfolio Analysis — One-Page Infographic
# Author: Hugo Fitipaldi
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(rio)
library(scales)
library(forcats)
library(tidytext)
library(grid)
library(gridExtra)
library(qrcode)
library(png)

df_raw <- rio::import("../grants-export-1.xlsx", sheet = "transposed")

# ── DATA PREP ─────────────────────────────────────────────────
all_funders <- df_raw %>%
  rename(year=`Grant Year`, amount_dkk=`Amount Granted`,
         abstract=`Abstract`, title=`Title`,
         oecd_l1=`OECD Classification Level 1`, funder=`Funder Name`) %>%
  mutate(
    year=as.integer(year), amount_dkk=as.numeric(amount_dkk),
    amount_mDKK=amount_dkk/1e6,
    abstract=str_remove_all(abstract,"_x000d_|\\r|_x000D_"),
    title=str_remove_all(title,"_x000d_|\\r|_x000D_"),
    text=paste(tolower(title),tolower(abstract)),
    funder_short=case_when(
      grepl("Novo Nordisk",funder)         ~ "NNF",
      grepl("Independent Research",funder) ~ "Independent Research Fund DK",
      grepl("Carlsberg",funder)            ~ "Carlsberg Foundation",
      grepl("Lundbeck",funder)             ~ "Lundbeck Foundation",
      grepl("Villum",funder)               ~ "Villum Foundation",
      grepl("VELUX",funder)                ~ "VELUX Foundation",
      TRUE ~ funder
    ),
    oecd_short=case_when(
      str_detect(oecd_l1,"Medical")      ~ "Health",
      str_detect(oecd_l1,"Natural")      ~ "Natural Sciences",
      str_detect(oecd_l1,"Engineering")  ~ "Engineering & Tech",
      str_detect(oecd_l1,"Agricultural") ~ "Agriculture & Vet",
      str_detect(oecd_l1,"Social")       ~ "Social Sciences",
      str_detect(oecd_l1,"Humanities")   ~ "Humanities",
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

all_funders <- all_funders %>%
  mutate(is_ds=str_detect(text,ds_pattern))

nnf <- all_funders %>% filter(funder_short=="NNF")

# ── SHARED THEME ──────────────────────────────────────────────
infographic_theme <- theme_minimal(base_size=10) +
  theme(
    plot.background  = element_rect(fill="white",color=NA),
    panel.background = element_rect(fill="white",color=NA),
    plot.title       = element_text(face="bold",color="#0F6E56",size=10),
    plot.subtitle    = element_text(color="#666666",size=8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color="#f0f0f0"),
    axis.title       = element_text(color="#555555",size=8),
    axis.text        = element_text(color="#555555",size=7.5),
    plot.margin      = margin(8,8,8,8)
  )

domain_colors <- c(
  "Health"="#0F6E56","Natural Sciences"="#1D9E75",
  "Engineering & Tech"="#5DCAA5","Agriculture & Vet"="#9FE1CB",
  "Social Sciences"="#C8DDD8","Humanities"="#E8F5F2"
)

funder_colors <- c(
  "NNF"="#0F6E56","Villum Foundation"="#1D9E75",
  "Carlsberg Foundation"="#5DCAA5",
  "Independent Research Fund DK"="#9FE1CB",
  "Lundbeck Foundation"="#C8DDD8","VELUX Foundation"="#aaaaaa"
)

# ── PLOT 1: PORTFOLIO TREND ────────────────────────────────────
portfolio_trend <- nnf %>%
  group_by(year) %>%
  summarise(
    n_grants=n(), total_mDKK=sum(amount_mDKK,na.rm=T),
    n_ds=sum(is_ds), pct_ds=mean(is_ds)*100,
    ds_funding_mDKK=sum(amount_mDKK[is_ds],na.rm=T),
    .groups="drop"
  )

sf <- max(portfolio_trend$total_mDKK)/100

p1 <- ggplot(portfolio_trend, aes(x=year)) +
  geom_col(aes(y=total_mDKK), fill="#C8DDD8", width=0.7) +
  geom_col(aes(y=ds_funding_mDKK), fill="#1D9E75", width=0.7) +
  geom_line(aes(y=pct_ds*sf, group=1), color="#0F6E56", linewidth=1.2) +
  geom_point(aes(y=pct_ds*sf), color="#0F6E56", size=2.5) +
  geom_text(aes(y=pct_ds*sf, label=paste0(round(pct_ds,1),"%")),
            vjust=-0.7, size=2.5, color="#0F6E56", fontface="bold") +
  scale_y_continuous(
    name="Funding (mDKK)", labels=comma,
    sec.axis=sec_axis(~./sf, name="% DS/AI Grants")
  ) +
  scale_x_continuous(breaks=2017:2024) +
  labs(
    title="DS/AI Investment Growing Fast",
    subtitle="Light = total funding  |  Green = DS/AI  |  Line = % DS/AI",
    x=NULL
  ) +
  infographic_theme +
  theme(axis.title.y.right=element_text(color="#0F6E56",size=7.5))

# ── PLOT 2: FUNDER COMPARISON ──────────────────────────────────
funder_year <- all_funders %>%
  group_by(funder_short,year) %>%
  summarise(n=n(),n_ds=sum(is_ds),pct_ds=n_ds/n*100,.groups="drop") %>%
  filter(n>=10)

p2 <- ggplot(funder_year, aes(x=year,y=pct_ds,color=funder_short)) +
  geom_line(aes(linewidth=funder_short=="NNF")) +
  geom_point(aes(size=funder_short=="NNF")) +
  geom_text(data=filter(funder_year,year==2024),
            aes(label=paste0(round(pct_ds,1),"%")),
            hjust=-0.15, size=2.3, fontface="bold") +
  scale_color_manual(values=funder_colors, name=NULL) +
  scale_linewidth_manual(values=c("TRUE"=1.8,"FALSE"=0.7),guide="none") +
  scale_size_manual(values=c("TRUE"=2.5,"FALSE"=1.5),guide="none") +
  scale_x_continuous(breaks=2016:2024,
                     expand=expansion(mult=c(0.05,0.2))) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(
    title="NNF Leads Growth Among Danish Funders",
    subtitle="% of annual grants with DS/AI  |  NNF in bold",
    x=NULL, y="% of Annual Grants"
  ) +
  infographic_theme +
  theme(legend.position="bottom",
        legend.text=element_text(size=6.5),
        legend.key.size=unit(0.4,"cm"),
        legend.spacing.x=unit(0.2,"cm"))

# ── PLOT 3: EMERGING TOPICS HEATMAP ───────────────────────────
custom_stops <- c(
  "novo","nordisk","foundation","danish","denmark","university",
  "research","project","study","studies","including","provide",
  "within","will","also","can","use","aim","new","key","high",
  "two","important","however","due","thus","based","using",
  "used","data","grant","funding","x000d","_x000d_"
)

key_bigrams <- c(
  "machine learning","deep learning","artificial intelligence",
  "greenhouse gas","climate change","food security",
  "meat analogues","eye screening","rna degradation"
)

bigram_year <- nnf %>%
  filter(is_ds) %>%
  select(grant_id=`Grant Id`,year,abstract) %>%
  unnest_tokens(bigram,abstract,token="ngrams",n=2) %>%
  separate(bigram,c("word1","word2"),sep=" ") %>%
  filter(!word1 %in% c(stop_words$word,custom_stops),
         !word2 %in% c(stop_words$word,custom_stops),
         !str_detect(word1,"^[0-9]"),!str_detect(word2,"^[0-9]")) %>%
  unite(bigram,word1,word2,sep=" ") %>%
  filter(bigram %in% key_bigrams) %>%
  count(bigram,year) %>%
  complete(bigram,year=2017:2024,fill=list(n=0)) %>%
  mutate(
    category=case_when(
      bigram %in% c("greenhouse gas","climate change","food security",
                    "meat analogues") ~ "Sustainability",
      bigram %in% c("machine learning","deep learning",
                    "artificial intelligence") ~ "AI / ML",
      TRUE ~ "Declining"
    ),
    bigram=factor(bigram, levels=rev(c(
      "machine learning","deep learning","artificial intelligence",
      "greenhouse gas","climate change","food security","meat analogues",
      "eye screening","rna degradation"
    )))
  )

p3 <- ggplot(bigram_year, aes(x=year,y=bigram,fill=n)) +
  geom_tile(color="white",linewidth=0.4) +
  geom_text(aes(label=ifelse(n>0,n,"")),size=2.5,color="white",fontface="bold") +
  geom_text(aes(label=ifelse(n==0,"\u00b7","")),size=3,color="#cccccc") +
  scale_fill_gradientn(
    colors=c("#E8F5F2","#9FE1CB","#5DCAA5","#1D9E75","#0F6E56"),
    values=scales::rescale(c(0,1,3,8,20)),
    name="Count",
    guide=guide_colorbar(barwidth=0.4,barheight=3,title.position="top")
  ) +
  annotate("rect",xmin=2016.3,xmax=2016.6,ymin=5.5,ymax=9.5,fill="#1D9E75",alpha=0.85) +
  annotate("text",x=2016.45,y=7.5,label="AI",angle=90,size=2.5,color="white",fontface="bold") +
  annotate("rect",xmin=2016.3,xmax=2016.6,ymin=1.5,ymax=5.5,fill="#0F6E56",alpha=0.85) +
  annotate("text",x=2016.45,y=3.5,label="Sus.",angle=90,size=2.5,color="white",fontface="bold") +
  annotate("rect",xmin=2016.3,xmax=2016.6,ymin=0.5,ymax=1.5,fill="#C8DDD8",alpha=0.85) +
  annotate("text",x=2016.45,y=1,label="D",angle=90,size=2,color="#555",fontface="bold") +
  geom_vline(xintercept=2020.5,color="#0F6E56",linewidth=0.7,linetype="dashed",alpha=0.7) +
  annotate("text",x=2020.5,y=9.7,label="Strategy shift",size=2.3,
           color="#0F6E56",hjust=0.5,fontface="italic") +
  scale_x_continuous(breaks=2017:2024,expand=expansion(add=c(0.6,0.3))) +
  labs(
    title="Sustainability Language Emerges Post-2021",
    subtitle="Frequency of key phrases in DS/AI grant abstracts",
    x=NULL,y=NULL
  ) +
  infographic_theme +
  theme(panel.grid=element_blank(),
        axis.text.y=element_text(size=7.5,color="#444444"),
        legend.position="right")

# ── QR CODE ───────────────────────────────────────────────────
qr <- qr_code("https://hugofitipaldi.shinyapps.io/nnf-grant-analysis")
qr_file <- tempfile(fileext=".png")
png(qr_file, width=200, height=200, bg="white")
plot(qr)
dev.off()
qr_img <- readPNG(qr_file)

qr_grob <- rasterGrob(qr_img, interpolate=TRUE,
                      width=unit(2.2,"cm"), height=unit(2.2,"cm"))

qr_panel <- ggplot() +
  annotation_custom(qr_grob, xmin=-Inf,xmax=Inf,ymin=0.2,ymax=Inf) +
  annotate("text", x=0.5, y=0.15,
           label="Scan for live\ninteractive dashboard",
           hjust=0.5, size=2.8, color="#0F6E56", fontface="bold") +
  annotate("text", x=0.5, y=0.03,
           label="hugofitipaldi.shinyapps.io/nnf-grant-analysis",
           hjust=0.5, size=2.2, color="#888888") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  theme_void() +
  theme(plot.background=element_rect(fill="#F7F9F8",color="#C8DDD8",linewidth=0.5),
        plot.margin=margin(8,8,8,8))

# ── HEADER ────────────────────────────────────────────────────
# Header: title left, QR right in its own column via table-like layout
qr_grob_header <- rasterGrob(qr_img, interpolate=TRUE,
                             width=unit(1.5,"cm"), height=unit(1.5,"cm"))

header <- ggplot() +
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="#0F6E56") +
  # Title and subtitle - keep within 0 to 0.82 to leave room for QR
  annotate("text", x=0.01, y=0.70,
           label="NNF Data Science & AI Grant Landscape (2017-2024)",
           hjust=0, size=4.8, color="white", fontface="bold") +
  annotate("text", x=0.01, y=0.25,
           label="An independent analysis of the Novo Nordisk Foundation's DS/AI portfolio  |  Data: Danish Research Portal  |  Analysis: Hugo Fitipaldi",
           hjust=0, size=2.5, color="#9FE1CB") +
  # QR code in top right with label below
  annotation_custom(qr_grob_header, xmin=0.84, xmax=0.97, ymin=0.05, ymax=0.95) +
  annotate("text", x=0.905, y=0.02,
           label="Scan for live app", hjust=0.5, size=2.0, color="#9FE1CB") +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_void() +
  theme(plot.margin=margin(4,4,4,8))

# ── EXPLANATION ROW ───────────────────────────────────────────
explanation <- ggplot() +
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="#F7F9F8") +
  annotate("text", x=0.5, y=0.5,
           label="Using open grant data from the Danish Research Portal, this analysis maps how NNF's DS/AI investment evolved across 3,642 grants (2017-2024),\nwhich domains and institutions receive the most funding, how NNF compares to peer Danish funders, and what shifting abstract language reveals about research priorities.",
           hjust=0.5, vjust=0.5, size=2.9, color="#555555", lineheight=1.5) +
  scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_void() +
  theme(plot.margin=margin(6,20,6,20),
        plot.background=element_rect(fill="#F7F9F8",color=NA))

# ── COMPOSE ───────────────────────────────────────────────────
final <- header /
  explanation /
  (p1 + p2 + p3 +
     plot_layout(ncol=3, widths=c(1.1, 1.1, 1.2))) +
  plot_layout(heights=c(0.13, 0.13, 1)) &
  theme(plot.background=element_rect(fill="white",color=NA))

ggsave("outputs/nnf_infographic.pdf", final,
       width=297, height=185, units="mm", dpi=300, bg="white")

cat("Infographic saved to outputs/nnf_infographic.pdf\n")