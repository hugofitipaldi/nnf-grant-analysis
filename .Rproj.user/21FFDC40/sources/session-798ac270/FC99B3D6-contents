# ============================================================
# NNF Data Science & AI Grant Landscape — Shiny App (CSV version)
# Author: Hugo Fitipaldi
# Data: pre-processed CSVs in app_data/ folder
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)
library(scales)
library(forcats)
library(visNetwork)
library(DT)

# ── 0. LOAD PRE-PROCESSED DATA ───────────────────────────────
portfolio_trend <- read.csv("app_data/portfolio_trend.csv")
domain_trend    <- read.csv("app_data/domain_trend.csv")
ai_trend        <- read.csv("app_data/ai_trend.csv")
ds_domain       <- read.csv("app_data/ds_domain.csv") %>%
  mutate(oecd_short = factor(oecd_short, levels = oecd_short))
top_orgs        <- read.csv("app_data/top_orgs.csv") %>%
  mutate(org_name = factor(org_name, levels = org_name))
funder_year     <- read.csv("app_data/funder_year.csv")
funder_summary  <- read.csv("app_data/funder_summary.csv", check.names = FALSE)
country_data    <- read.csv("app_data/country_data.csv") %>%
  mutate(label = factor(label, levels = label))
div_data        <- read.csv("app_data/div_data.csv") %>%
  arrange(net_change) %>%
  mutate(bigram = factor(bigram, levels = bigram))
bigram_year     <- read.csv("app_data/bigram_year.csv") %>%
  mutate(bigram = factor(bigram, levels = rev(c(
    "machine learning","deep learning","artificial intelligence",
    "neural networks","image analysis","learning models",
    "greenhouse gas","climate change","food security",
    "meat analogues","cropping systems",
    "eye screening","rna degradation","pancreatic cancer"
  ))))
oc_trend        <- read.csv("app_data/oc_trend.csv")
duration        <- read.csv("app_data/duration.csv")
dur_domain      <- read.csv("app_data/dur_domain.csv") %>%
  mutate(oecd_short = factor(oecd_short, levels = oecd_short))
vis_nodes       <- read.csv("app_data/vis_nodes.csv")
vis_edges       <- read.csv("app_data/vis_edges.csv")
grants_table    <- read.csv("app_data/grants_table.csv", check.names = FALSE)

# ── DERIVED DATA ─────────────────────────────────────────────
dur_means <- duration %>%
  group_by(is_ds) %>%
  summarise(m = mean(duration_years), .groups = "drop") %>%
  mutate(label = paste0(ifelse(is_ds, "DS/AI: ", "Others: "), round(m,1), " yrs"))

dur_trend <- duration %>%
  group_by(year) %>%
  summarise(overall = mean(duration_years),
            ds      = mean(duration_years[is_ds], na.rm=TRUE),
            .groups = "drop") %>%
  pivot_longer(c(overall,ds), names_to="type", values_to="mean_yrs") %>%
  filter(!is.nan(mean_yrs)) %>%
  mutate(type = recode(type, "overall"="All grants", "ds"="DS/AI grants"))

# Summary stats for value boxes
total_grants   <- sum(portfolio_trend$n_grants)
total_funding  <- sum(portfolio_trend$total_mDKK)
total_ds       <- sum(portfolio_trend$n_ds)
pct_ds_overall <- round(total_ds / total_grants * 100, 1)
total_ds_fund  <- sum(portfolio_trend$ds_funding_mDKK)

# ── SHARED THEME ─────────────────────────────────────────────
nnf_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill="white", color=NA),
    panel.background = element_rect(fill="white", color=NA),
    plot.title       = element_text(face="bold", color="#0F6E56", size=13),
    plot.subtitle    = element_text(color="#666666", size=10),
    plot.caption     = element_text(color="#999999", size=8),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(color="#555555", size=10),
    legend.background= element_rect(fill="white", color=NA)
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

cap <- "Source: Danish Research Portal (forskningsportal.dk) | Analysis: H. Fitipaldi"

# ── UI ────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title="NNF · Data Science Grant Landscape", titleWidth=340),
  dashboardSidebar(
    width=220,
    sidebarMenu(
      menuItem("Portfolio Overview",    tabName="overview",   icon=icon("chart-line")),
      menuItem("Data Science & AI",     tabName="dsai",       icon=icon("brain")),
      menuItem("NNF vs Danish Funders", tabName="comparison", icon=icon("balance-scale")),
      menuItem("Geographic Reach",      tabName="geo",        icon=icon("globe")),
      menuItem("Research Topics",       tabName="topics",     icon=icon("lightbulb")),
      menuItem("Grant Dynamics",        tabName="dynamics",   icon=icon("clock")),
      menuItem("Explore Grants",        tabName="explore",    icon=icon("search"))
    ),
    hr(),
    div(style="padding:10px 15px;font-size:11px;color:#aaa;",
        "Data: Danish Research Portal",br(),
        "Analysis: Hugo Fitipaldi",br(),"2017-2024")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper,.right-side{background-color:#f8faf9;}
      .box{border-top:3px solid #0F6E56;}
      .insight-box{background:#E8F5F2;border-left:4px solid #0F6E56;
        padding:10px 14px;margin-bottom:14px;border-radius:4px;
        font-size:13px;color:#1a1a1a;line-height:1.5;}
      .insight-box strong{color:#0F6E56;}
      .skin-green .main-header .logo,
      .skin-green .main-header .navbar{background-color:#0F6E56!important;}
      .skin-green .main-sidebar{background-color:#1a2a1a;}
      .skin-green .sidebar-menu>li.active>a,
      .skin-green .sidebar-menu>li:hover>a{background-color:#0F6E56!important;}
    "))),
    tabItems(
      
      # ── TAB 1 ───────────────────────────────────────────────
      tabItem(tabName="overview",
              fluidRow(
                valueBoxOutput("vb_grants",width=3),
                valueBoxOutput("vb_funding",width=3),
                valueBoxOutput("vb_ds",width=3),
                valueBoxOutput("vb_dsfunding",width=3)
              ),
              fluidRow(box(width=12,
                           div(class="insight-box",strong("Key finding: "),
                               "NNF's data science investment grew from 0% to 10.2% of annual grants between 2017 and 2024,
             with total DS/AI funding exceeding DKK 2 billion. Acceleration is sharpest post-2021."),
                           plotlyOutput("p_trend", height="320px"))),
              fluidRow(box(width=12,
                           div(class="insight-box",strong("Key finding: "),
                               "Health Sciences dominates but Natural Sciences is the fastest-growing domain,
             reflecting NNF's strategic push into computational research."),
                           plotOutput("p_domains", height="300px")))
      ),
      
      # ── TAB 2 ───────────────────────────────────────────────
      tabItem(tabName="dsai",
              fluidRow(
                box(width=6,
                    div(class="insight-box",strong("Key finding: "),
                        "AI/ML-specific grants reached 7% in 2024, up from zero in 2017."),
                    plotOutput("p_ai_growth", height="280px")),
                box(width=6,
                    div(class="insight-box",strong("Key finding: "),
                        "Natural Sciences receives most DS/AI funding in absolute terms, with larger individual awards than Health."),
                    plotOutput("p_ds_domains", height="280px"))
              ),
              fluidRow(box(width=12,
                           div(class="insight-box",strong("Key finding: "),
                               "DTU leads in DS/AI intensity (13%). KU Copenhagen receives most DS funding in absolute terms. Hover for details."),
                           plotlyOutput("p_institutions", height="420px")))
      ),
      
      # ── TAB 3 ───────────────────────────────────────────────
      tabItem(tabName="comparison",
              fluidRow(box(width=12,
                           div(class="insight-box",strong("Key finding: "),
                               "NNF started from 0% in 2017 and shows the steepest growth among large Danish funders,
             reaching 10.2% in 2024. In absolute terms NNF funds 4x more data science than any other Danish funder."),
                           plotOutput("p_comparison", height="380px"))),
              fluidRow(box(width=12, title="DS/AI Investment Summary by Funder",
                           DTOutput("tbl_funders")))
      ),
      
      # ── TAB 4 ───────────────────────────────────────────────
      tabItem(tabName="geo",
              fluidRow(box(width=12,
                           div(class="insight-box",strong("Key finding: "),
                               "91% of NNF funding goes to Danish institutions, but researchers in 25 countries are supported.
             The US receives large awards despite few grants. Hover for country details."),
                           plotlyOutput("p_geo", height="520px")))
      ),
      
      # ── TAB 5 ───────────────────────────────────────────────
      tabItem(tabName="topics",
              fluidRow(
                box(width=5,
                    div(class="insight-box",strong("Key finding: "),
                        "Sustainability terms appear from 2021 onwards. AI/ML methodology terms grow steadily throughout."),
                    plotOutput("p_diverging", height="420px")),
                box(width=7,
                    div(class="insight-box",strong("Key finding: "),
                        "The 2021 strategy shift is visible as a sharp step-change in sustainability language."),
                    plotOutput("p_heatmap", height="420px"))
              ),
              fluidRow(box(width=12, title="Word Co-occurrence Network in NNF DS/AI Grants",
                           div(class="insight-box",strong("How to use: "),
                               "Nodes = frequent terms sized by frequency. Edges = co-occurrence within same grant. ",
                               "Colours: ",
                               span(style="color:#0F6E56;font-weight:bold;","AI/ML"),", ",
                               span(style="color:#1D9E75;font-weight:bold;","Health"),", ",
                               span(style="color:#5DCAA5;font-weight:bold;","Biology"),
                               ". Click a node to highlight its neighbourhood."),
                           visNetworkOutput("word_network", height="480px")))
      ),
      
      # ── TAB 6 ───────────────────────────────────────────────
      tabItem(tabName="dynamics",
              fluidRow(box(width=12,
                           div(class="insight-box",strong("Key finding: "),
                               "Open competition DS/AI grants grew from 0% to 11% of all open calls.
             Average grant size doubled from DKK 2.6M to 5.5M."),
                           plotOutput("p_oc", height="280px"))),
              fluidRow(
                box(width=4,
                    div(class="insight-box",strong("Key finding: "),
                        "DS/AI grants run 0.6 years longer on average — NNF treats data science as exploratory research."),
                    plotOutput("p_dur_dist", height="260px")),
                box(width=4,
                    div(class="insight-box",strong("Key finding: "),
                        "Agriculture & Vet grants are longest (field trials). Humanities shortest."),
                    plotOutput("p_dur_domain", height="260px")),
                box(width=4,
                    div(class="insight-box",strong("Key finding: "),
                        "DS/AI grants have been consistently longer than portfolio average every year since 2018."),
                    plotOutput("p_dur_trend", height="260px"))
              )
      ),
      
      # ── TAB 7 ───────────────────────────────────────────────
      tabItem(tabName="explore",
              fluidRow(box(width=12, title="Browse NNF Data Science & AI Grants",
                           fluidRow(
                             column(5, sliderInput("yr_filter","Filter by year:",
                                                   min=2017,max=2024,value=c(2017,2024),step=1,sep="",width="100%")),
                             column(4, selectInput("dom_filter","Filter by domain:",
                                                   choices=c("All",sort(unique(grants_table$Domain))),
                                                   selected="All",width="100%")),
                             column(3, selectInput("inst_filter","Filter by instrument:",
                                                   choices=c("All",sort(unique(grants_table$Instrument))),
                                                   selected="All",width="100%"))
                           ),
                           DTOutput("tbl_grants")))
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Value boxes
  output$vb_grants    <- renderValueBox(valueBox(total_grants,"Total NNF Grants",icon=icon("file-alt"),color="green"))
  output$vb_funding   <- renderValueBox(valueBox(paste0("DKK ",round(total_funding/1000,1),"B"),"Total Funding",icon=icon("coins"),color="green"))
  output$vb_ds        <- renderValueBox(valueBox(paste0(total_ds," (",pct_ds_overall,"%)"), "DS/AI Grants",icon=icon("brain"),color="green"))
  output$vb_dsfunding <- renderValueBox(valueBox(paste0("DKK ",round(total_ds_fund/1000,1),"B"),"DS/AI Funding",icon=icon("chart-bar"),color="green"))
  
  # ── OVERVIEW ───────────────────────────────────────────────
  output$p_trend <- renderPlotly({
    sf <- max(portfolio_trend$total_mDKK)/100
    p <- ggplot(portfolio_trend, aes(x=year)) +
      geom_col(aes(y=total_mDKK,
                   text=paste0("Year: ",year,"\nTotal: DKK ",round(total_mDKK),"M\nGrants: ",n_grants)),
               fill="#C8DDD8",width=0.7) +
      geom_col(aes(y=ds_funding_mDKK,
                   text=paste0("DS/AI Funding: DKK ",round(ds_funding_mDKK),"M\nDS Grants: ",n_ds)),
               fill="#1D9E75",width=0.7) +
      geom_line(aes(y=pct_ds*sf,group=1),color="#0F6E56",linewidth=1.2) +
      geom_point(aes(y=pct_ds*sf,text=paste0("DS/AI %: ",round(pct_ds,1),"%")),
                 color="#0F6E56",size=3) +
      scale_y_continuous(name="Funding (mDKK)",labels=comma,
                         sec.axis=sec_axis(~./sf,name="% DS/AI Grants")) +
      scale_x_continuous(breaks=2017:2024) +
      labs(title="NNF Portfolio: Funding & Data Science Trajectory",
           subtitle="Bars = total (light) / DS funding (green) | Line = % DS/AI grants",
           x=NULL,caption=cap) + nnf_theme
    ggplotly(p,tooltip="text") %>%
      layout(hovermode="x unified") %>%
      config(displayModeBar=FALSE)
  })
  
  output$p_domains <- renderPlot({
    ggplot(domain_trend,
           aes(x=year,y=total_mDKK,
               fill=fct_relevel(oecd_short,names(domain_colors)))) +
      geom_area(position="stack",alpha=0.9) +
      scale_fill_manual(values=domain_colors,name="Domain") +
      scale_x_continuous(breaks=2017:2024) +
      scale_y_continuous(labels=comma) +
      labs(title="NNF Research Portfolio by Domain",
           x=NULL,y="Funding (mDKK)",caption=cap) + nnf_theme
  })
  
  # ── DS & AI ────────────────────────────────────────────────
  output$p_ai_growth <- renderPlot({
    ai_long <- ai_trend %>%
      pivot_longer(c(pct_ds,pct_ai),names_to="type",values_to="pct") %>%
      mutate(type=recode(type,"pct_ds"="Data Science (broad)","pct_ai"="AI/ML (specific)"))
    ggplot(ai_long,aes(x=year,y=pct,color=type,linetype=type)) +
      geom_line(linewidth=1.3) + geom_point(size=3) +
      geom_text(data=filter(ai_long,year==2024),
                aes(label=paste0(round(pct,1),"%")),hjust=-0.2,size=3.5,fontface="bold") +
      scale_color_manual(values=c("Data Science (broad)"="#5DCAA5","AI/ML (specific)"="#0F6E56")) +
      scale_linetype_manual(values=c("Data Science (broad)"="dashed","AI/ML (specific)"="solid")) +
      scale_x_continuous(breaks=2017:2024,expand=expansion(mult=c(0.05,0.18))) +
      labs(title="Rise of AI/ML in NNF-funded Research",
           x=NULL,y="% of Grants",color=NULL,linetype=NULL,caption=cap) +
      nnf_theme + theme(legend.position="bottom")
  })
  
  output$p_ds_domains <- renderPlot({
    ggplot(ds_domain,aes(x=total_mDKK,y=oecd_short)) +
      geom_col(aes(fill=bar_color),width=0.65,show.legend=FALSE) +
      geom_text(aes(x=total_mDKK+12,label=paste0(n_grants," grants")),
                hjust=0,size=3.5,color="#444444") +
      scale_fill_identity() +
      scale_x_continuous(labels=comma,expand=expansion(mult=c(0,0.22))) +
      labs(title="Where Does NNF's DS Funding Go?",
           x="Total Funding (mDKK)",y=NULL,caption=cap) +
      nnf_theme + theme(panel.grid.major.y=element_blank())
  })
  
  output$p_institutions <- renderPlotly({
    p <- ggplot(top_orgs,
                aes(x=total_mDKK,y=org_name,fill=pct_ds,
                    text=paste0(org_name,"\nTotal: DKK ",round(total_mDKK),"M",
                                "\nGrants: ",n_grants,"\nDS/AI: ",n_ds," (",pct_ds,"%)"))) +
      geom_col(width=0.7,show.legend=FALSE) +
      geom_text(aes(x=total_mDKK,label=paste0(pct_ds,"% DS")),
                hjust=-0.1,size=3,color="#444444") +
      scale_fill_gradient(low="#C8DDD8",high="#0F6E56") +
      scale_x_continuous(labels=comma,expand=expansion(mult=c(0,0.18))) +
      labs(title="Top 15 NNF-funded Institutions (2017-2024)",
           subtitle="Fill = % grants with DS/AI | Hover for details",
           x="Total Funding (mDKK)",y=NULL,caption=cap) +
      nnf_theme + theme(panel.grid.major.y=element_blank())
    ggplotly(p,tooltip="text") %>% config(displayModeBar=FALSE)
  })
  
  # ── COMPARISON ─────────────────────────────────────────────
  output$p_comparison <- renderPlot({
    ggplot(funder_year,aes(x=year,y=pct_ds,color=funder_short)) +
      geom_line(aes(linewidth=funder_short=="NNF")) +
      geom_point(aes(size=funder_short=="NNF")) +
      geom_text(data=filter(funder_year,year==2024),
                aes(label=paste0(round(pct_ds,1),"%")),
                hjust=-0.2,size=3,fontface="bold") +
      scale_color_manual(values=funder_colors,name="Funder") +
      scale_linewidth_manual(values=c("TRUE"=2,"FALSE"=0.8),guide="none") +
      scale_size_manual(values=c("TRUE"=3,"FALSE"=2),guide="none") +
      scale_x_continuous(breaks=2016:2024,expand=expansion(mult=c(0.05,0.15))) +
      scale_y_continuous(labels=function(x) paste0(x,"%")) +
      labs(title="Data Science & AI Investment: NNF vs Danish Funders",
           subtitle="% of annual grants with DS/AI | NNF highlighted",
           x=NULL,y="% of Annual Grants",caption=cap) +
      nnf_theme + theme(legend.position="right")
  })
  
  output$tbl_funders <- renderDT({
    datatable(funder_summary,rownames=FALSE,
              options=list(pageLength=10,dom="t")) %>%
      formatStyle("Funder",target="row",
                  backgroundColor=styleEqual("NNF","#E8F5F2"),
                  fontWeight=styleEqual("NNF","bold"))
  })
  
  # ── GEOGRAPHIC ─────────────────────────────────────────────
  output$p_geo <- renderPlotly({
    p <- ggplot(country_data,
                aes(x=total_mDKK,y=label,fill=total_mDKK,
                    text=paste0(country_name,"\nFunding: DKK ",total_mDKK,"M",
                                "\nGrants: ",n_grants,"\nDS/AI: ",n_ds," (",pct_ds,"%)"))) +
      geom_col(width=0.7,show.legend=FALSE) +
      geom_text(aes(x=total_mDKK+max(total_mDKK)*0.01,
                    label=paste0(n_grants," grants")),
                hjust=0,size=3,color="#444444") +
      scale_fill_gradient(low="#C8DDD8",high="#0F6E56") +
      scale_x_continuous(labels=comma,expand=expansion(mult=c(0,0.18))) +
      labs(title="NNF Geographic Reach: Funding by Country (2017-2024)",
           subtitle="Top 20 countries | Hover for details",
           x="Total Funding (mDKK)",y=NULL,caption=cap) +
      nnf_theme + theme(panel.grid.major.y=element_blank())
    ggplotly(p,tooltip="text") %>% config(displayModeBar=FALSE)
  })
  
  # ── TOPICS ─────────────────────────────────────────────────
  output$p_diverging <- renderPlot({
    ggplot(div_data,aes(x=net_change,y=bigram,fill=bar_color)) +
      geom_col(width=0.7,show.legend=FALSE) +
      geom_vline(xintercept=0,color="#555555",linewidth=0.6) +
      geom_text(data=filter(div_data,net_change>=0),
                aes(x=net_change+0.3,label=paste0("+",net_change)),
                hjust=0,size=3.2,color="#444444") +
      geom_text(data=filter(div_data,net_change<0),
                aes(x=net_change-0.3,label=net_change),
                hjust=1,size=3.2,color="#888888") +
      scale_fill_identity() +
      scale_x_continuous(expand=expansion(mult=c(0.18,0.18)),
                         labels=function(x) ifelse(x>=0,paste0("+",x),x)) +
      annotate("text",x=12,y=2,label="EMERGING \u25ba",
               size=3,color="#0F6E56",fontface="bold") +
      annotate("text",x=-4.5,y=13,label="\u25c4 DECLINING",
               size=3,color="#9FE1CB",fontface="bold") +
      labs(title="Shifting Research Language",
           subtitle="Net change in phrase frequency: 2021-2024 vs 2017-2020",
           x="Net change in grant abstracts",y=NULL,caption=cap) +
      nnf_theme + theme(panel.grid.major.y=element_blank())
  })
  
  output$p_heatmap <- renderPlot({
    ggplot(bigram_year,aes(x=year,y=bigram,fill=n)) +
      geom_tile(color="white",linewidth=0.4) +
      geom_text(aes(label=ifelse(n>0,n,"")),size=3,color="white",fontface="bold") +
      geom_text(aes(label=ifelse(n==0,"\u00b7","")),size=4,color="#cccccc") +
      scale_fill_gradientn(
        colors=c("#E8F5F2","#9FE1CB","#5DCAA5","#1D9E75","#0F6E56"),
        values=scales::rescale(c(0,1,3,8,20)),name="Count",
        guide=guide_colorbar(barwidth=0.6,barheight=5,title.position="top")) +
      annotate("rect",xmin=2016.3,xmax=2016.7,ymin=8.5,ymax=14.5,fill="#1D9E75",alpha=0.8) +
      annotate("text",x=2016.5,y=11.5,label="AI/ML",angle=90,size=2.8,color="white",fontface="bold") +
      annotate("rect",xmin=2016.3,xmax=2016.7,ymin=3.5,ymax=8.5,fill="#0F6E56",alpha=0.8) +
      annotate("text",x=2016.5,y=6,label="Sustain.",angle=90,size=2.8,color="white",fontface="bold") +
      annotate("rect",xmin=2016.3,xmax=2016.7,ymin=0.5,ymax=3.5,fill="#C8DDD8",alpha=0.8) +
      annotate("text",x=2016.5,y=2,label="Decl.",angle=90,size=2.5,color="#555",fontface="bold") +
      geom_vline(xintercept=2020.5,color="#0F6E56",linewidth=0.8,linetype="dashed",alpha=0.6) +
      annotate("text",x=2020.5,y=14.8,label="Strategy shift",size=2.8,
               color="#0F6E56",hjust=0.5,fontface="italic") +
      scale_x_continuous(breaks=2017:2024,expand=expansion(add=c(0.8,0.3))) +
      labs(title="Research Topic Trajectories (2017-2024)",
           subtitle="Annual frequency of key phrases | Dashed = strategy shift",
           x=NULL,y=NULL,caption=cap) +
      nnf_theme + theme(panel.grid=element_blank(),
                        axis.text.y=element_text(size=10,color="#444444"))
  })
  
  output$word_network <- renderVisNetwork({
    visNetwork(vis_nodes,vis_edges,width="100%",height="460px") %>%
      visNodes(shape="dot",shadow=list(enabled=TRUE,size=4)) %>%
      visEdges(smooth=FALSE,color=list(color="#cccccc",highlight="#0F6E56")) %>%
      visOptions(highlightNearest=list(enabled=TRUE,degree=1,hover=TRUE),
                 nodesIdSelection=list(enabled=TRUE,useLabels=TRUE)) %>%
      visPhysics(enabled=FALSE) %>%
      visInteraction(navigationButtons=TRUE,tooltipDelay=80) %>%
      visLayout(randomSeed=42)
  })
  
  # ── DYNAMICS ───────────────────────────────────────────────
  output$p_oc <- renderPlot({
    sf_oc <- max(oc_trend$avg_mDKK)/max(oc_trend$pct_ds)*1.1
    ggplot(oc_trend,aes(x=year)) +
      geom_col(aes(y=pct_ds),fill="#C8DDD8",width=0.6) +
      geom_line(aes(y=avg_mDKK/sf_oc),color="#0F6E56",linewidth=1.3) +
      geom_point(aes(y=avg_mDKK/sf_oc),color="#0F6E56",size=3.5) +
      geom_text(aes(y=pct_ds+0.4,label=paste0(round(pct_ds,1),"%")),
                size=3,color="#1D9E75",fontface="bold") +
      geom_text(aes(y=avg_mDKK/sf_oc,label=paste0("DKK ",round(avg_mDKK,1),"M")),
                size=2.8,color="#0F6E56",vjust=-1) +
      scale_x_continuous(breaks=2017:2024) +
      scale_y_continuous(name="% Open Competition with DS/AI",
                         sec.axis=sec_axis(~.*sf_oc,name="Avg Grant Size (mDKK)",
                                           labels=function(x) paste0("DKK ",round(x,1),"M"))) +
      labs(title="Open Competition: Growing DS/AI Share & Award Sizes",
           subtitle="Bars = % grants with DS/AI | Line = average grant size",
           x=NULL,caption=cap) +
      nnf_theme + theme(axis.title.y.right=element_text(color="#0F6E56"))
  })
  
  output$p_dur_dist <- renderPlot({
    ggplot(duration,aes(x=duration_years,fill=is_ds,color=is_ds)) +
      geom_density(alpha=0.5,linewidth=0.8) +
      geom_vline(data=dur_means,aes(xintercept=m,color=is_ds),
                 linetype="dashed",linewidth=1) +
      geom_text(data=dur_means,aes(x=m+0.15,y=0.42,label=label,color=is_ds),
                hjust=0,size=3,show.legend=FALSE) +
      scale_fill_manual(values=c("FALSE"="#C8DDD8","TRUE"="#1D9E75"),
                        labels=c("FALSE"="Non-DS/AI","TRUE"="DS/AI"),name=NULL) +
      scale_color_manual(values=c("FALSE"="#9FE1CB","TRUE"="#0F6E56"),
                         labels=c("FALSE"="Non-DS/AI","TRUE"="DS/AI"),name=NULL) +
      scale_x_continuous(breaks=1:10,limits=c(0,10)) +
      labs(title="DS/AI Grants Run Longer",
           x="Duration (years)",y="Density",caption=cap) +
      nnf_theme + theme(legend.position="top")
  })
  
  output$p_dur_domain <- renderPlot({
    ggplot(dur_domain,aes(x=mean_yrs,y=oecd_short)) +
      geom_col(aes(fill=bar_color),width=0.6,show.legend=FALSE) +
      geom_errorbar(aes(xmin=mean_yrs-se,xmax=mean_yrs+se),width=0.2,color="#555") +
      geom_text(aes(x=mean_yrs+se+0.06,label=paste0(round(mean_yrs,1)," yrs")),
                hjust=0,size=3.3,color="#444") +
      scale_fill_identity() +
      scale_x_continuous(expand=expansion(mult=c(0,0.2))) +
      labs(title="Duration by Domain",x="Mean Duration (years)",y=NULL,caption=cap) +
      nnf_theme + theme(panel.grid.major.y=element_blank())
  })
  
  output$p_dur_trend <- renderPlot({
    ggplot(dur_trend,aes(x=year,y=mean_yrs,color=type,linetype=type)) +
      geom_line(linewidth=1.3) + geom_point(size=3) +
      geom_text(data=filter(dur_trend,year==2024),
                aes(label=paste0(round(mean_yrs,1)," yrs")),
                hjust=-0.2,size=3,fontface="bold") +
      scale_color_manual(values=c("All grants"="#9FE1CB","DS/AI grants"="#0F6E56"),name=NULL) +
      scale_linetype_manual(values=c("All grants"="dashed","DS/AI grants"="solid"),name=NULL) +
      scale_x_continuous(breaks=2017:2024,expand=expansion(mult=c(0.05,0.18))) +
      scale_y_continuous(limits=c(2,6)) +
      labs(title="Duration Trend Over Time",x=NULL,y="Mean Years",caption=cap) +
      nnf_theme + theme(legend.position="bottom")
  })
  
  # ── EXPLORE ────────────────────────────────────────────────
  filtered_grants <- reactive({
    d <- grants_table %>%
      filter(Year >= input$yr_filter[1], Year <= input$yr_filter[2])
    if(input$dom_filter != "All") d <- d %>% filter(Domain == input$dom_filter)
    if(input$inst_filter != "All") d <- d %>% filter(Instrument == input$inst_filter)
    d
  })
  
  output$tbl_grants <- renderDT({
    datatable(filtered_grants(),rownames=FALSE,filter="top",
              options=list(pageLength=10,scrollX=TRUE,
                           columnDefs=list(list(width="260px",targets=6))))
  })
}

shinyApp(ui, server)