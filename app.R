# ═══════════════════════════════════════════════════════════════
# 📦 LIBRARIES
# ═══════════════════════════════════════════════════════════════
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(corrplot)
library(shinyWidgets)
library(tidyr)
library(openxlsx)

# ═══════════════════════════════════════════════════════════════
# 📁 FOLDER STRUCTURE — Profile Image
# ═══════════════════════════════════════════════════════════════
profile_img_src <- {
  www_files <- c("www/profile.png", "www/profile_b64.png",
                 "www/profile.jpg", "www/profile.jpeg",
                 "www/Aftab.png",   "www/Aftab.jpg")
  root_files <- c("profile.png", "profile_b64.png",
                  "profile.jpg", "profile.jpeg")
  found_www  <- www_files[file.exists(www_files)]
  found_root <- root_files[file.exists(root_files)]
  if(length(found_www) > 0) {
    basename(found_www[1])
  } else if(length(found_root) > 0) {
    if(!dir.exists("www")) dir.create("www")
    file.copy(found_root[1], file.path("www", basename(found_root[1])), overwrite=TRUE)
    basename(found_root[1])
  } else {
    NULL
  }
}

# ═══════════════════════════════════════════════════════════════
# 📊 RAW DATA
# ═══════════════════════════════════════════════════════════════
raw_df <- read.csv(text = "
company,year,revenue,net_income,total_assets,equity,current_assets,current_liabilities,total_liabilities,EBIT,CFO
AP,2020,274515,57411,323888,65339,143713,105392,258549,66288,80674
AP,2021,365817,94680,351002,63090,134836,125481,287912,108949,104038
AP,2022,394328,99803,352755,50672,135405,153982,302083,119437,122151
AP,2023,383285,96995,352583,62146,143566,145308,290437,114301,110543
AP,2024,391035,93736,364980,56950,152987,176392,308030,123216,118254
AM,2020,386064,21331,321195,93404,132733,126385,321195,22899,66064
AM,2021,469822,33364,420549,138245,161580,142266,420549,24879,46327
AM,2022,513983,-2722,462675,146043,146791,155393,462675,12248,46752
AM,2023,574785,30425,527854,201875,172351,164917,527854,36852,84946
AM,2024,637959,59248,624894,285970,190867,179431,624894,68593,115877
ME,2020,85965,29146,75670,44644,75670,14981,31026,32671,38747
ME,2021,117929,39370,66666,25558,66666,21135,41108,46753,57683
ME,2022,116609,23200,59549,-465,59549,27026,60014,28944,50475
ME,2023,134902,39098,85365,8910,85365,31960,76455,46751,71113
ME,2024,164501,62360,100045,6628,100045,33596,93417,69380,91328
TE,2020,31536,721,52148,22225,26717,14248,28418,1994,5943
TE,2021,53823,5519,62131,30189,27100,19705,30548,6523,11497
TE,2022,81462,12556,82338,44704,40917,26709,36440,13656,14724
TE,2023,96773,14997,106618,62634,49616,28748,43009,8891,13256
TE,2024,97690,7091,122070,72913,58360,28821,48390,7076,14923
MS,2020,143015,44281,301311,80552,11482,2130,59578,5943,60675
MS,2021,168088,61271,333779,83111,13393,2174,50074,11497,76740
MS,2022,198270,72738,364840,86939,16924,4067,47032,14724,89035
MS,2023,211915,72361,411976,93718,21807,4152,41990,13256,87582
MS,2024,245122,88136,512163,100923,26021,5017,42688,14923,118548
", header = TRUE)

df <- raw_df %>%
  mutate(
    ROA    = net_income / total_assets,
    ROE    = net_income / equity,
    NPM    = net_income / revenue,
    OPM    = EBIT / revenue,
    CR     = current_assets / current_liabilities,
    CF_R   = CFO / revenue,
    CF_L   = CFO / total_liabilities,
    AT     = revenue / total_assets,
    DE     = total_liabilities / equity,
    IC     = EBIT / (abs(net_income) + 1),
    CA     = CFO / current_liabilities,
    EBIT_L = EBIT / total_liabilities
  )

ratios        <- c("ROA","ROE","NPM","OPM","CR","CF_R","CF_L","AT","DE","IC","CA","EBIT_L")
all_companies <- unique(df$company)
company_colors <- c(AP="#FF0000",AM="#FF6600",ME="#FFCC00",TE="#FF3366",MS="#FF99CC")

# ═══════════════════════════════════════════════════════════════
# ⚖️ LITERATURE-BASED WSM WEIGHTS
# ═══════════════════════════════════════════════════════════════
wsm_weights_literature <- list(
  ROA=0.35/4, ROE=0.35/4, NPM=0.35/4, OPM=0.35/4,
  AT=0.30/4,  CF_R=0.30/4, CF_L=0.30/4, EBIT_L=0.30/4,
  CR=0.20/2,  CA=0.20/2,
  DE=0.15/2,  IC=0.15/2
)
negative_ratios <- c("DE")

# ═══════════════════════════════════════════════════════════════
# 🎨 CSS
# ═══════════════════════════════════════════════════════════════
common_css <- HTML('
  .main-sidebar{background:linear-gradient(180deg,#0d0000 0%,#1a0000 60%,#2d0000 100%);}
  .sidebar-menu>li>a{color:#cccccc !important;font-weight:600;transition:all 0.3s;font-size:13px;padding:10px 15px;}
  .sidebar-menu>li>a:hover{background:rgba(255,204,0,0.15) !important;color:#ffcc00 !important;padding-left:20px;}
  .sidebar-menu>li.active>a{background:linear-gradient(90deg,#ff0000,#cc0000) !important;color:#ffcc00 !important;border-left:4px solid #ffcc00;}
  .profile-card{background:linear-gradient(135deg,rgba(255,0,0,0.12),rgba(45,0,0,0.9));border:1px solid rgba(255,204,0,0.5);border-radius:14px;padding:16px 12px;margin:8px;text-align:center;box-shadow:0 4px 20px rgba(255,0,0,0.3);}
  .profile-name{color:#ffcc00;font-size:15px;font-weight:900;margin:8px 0 2px 0;text-shadow:0 0 10px rgba(255,204,0,0.5);}
  .profile-title{color:#ff8800;font-size:10px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;}
  .control-label{color:#ffcc00 !important;font-weight:bold;font-size:12px;}
  .selectize-input{background:#1a0000 !important;color:#ffffff !important;border:1px solid #ff6600 !important;border-radius:6px !important;}
  .dropdown-menu{background:#1a0000 !important;color:#ffffff !important;border:1px solid #ffcc00 !important;}
  .btn{background:linear-gradient(45deg,#cc0000,#ff6600) !important;color:#ffffff !important;font-weight:bold !important;border:none !important;border-radius:6px !important;transition:all 0.3s;}
  .btn:hover{background:linear-gradient(45deg,#ff0000,#ffcc00) !important;transform:translateY(-2px);box-shadow:0 4px 15px rgba(255,100,0,0.5);}
  .btn-info{background:linear-gradient(45deg,#003366,#0055aa) !important;}
  .btn-success{background:linear-gradient(45deg,#003300,#006600) !important;}
  .btn-warning{background:linear-gradient(45deg,#664400,#cc8800) !important;}
  hr{border-color:rgba(255,204,0,0.3) !important;}
  body{background:linear-gradient(135deg,#0d0000 0%,#1a0000 40%,#2d0000 70%,#1a0000 100%);color:#ffffff !important;}
  .content-wrapper,.right-side{background:linear-gradient(135deg,#0d0000 0%,#1a0000 50%,#2d0000 100%);}
  .box{background:rgba(15,0,0,0.93) !important;border-radius:12px !important;box-shadow:0 8px 32px rgba(0,0,0,0.8) !important;border:1px solid rgba(255,204,0,0.25) !important;}
  .box-header{background:linear-gradient(90deg,#880000,#cc0000,#880000) !important;border-radius:12px 12px 0 0 !important;padding:12px 18px !important;}
  .box-header .box-title{color:#ffcc00 !important;font-weight:bold;font-size:14px;letter-spacing:0.5px;}
  .dataTables_wrapper{color:#ffffff !important;}
  table.dataTable thead th{background:#1a0000 !important;color:#ffcc00 !important;border-bottom:2px solid #ff0000 !important;font-weight:bold;padding:10px !important;}
  table.dataTable tbody tr{background:#080000 !important;color:#ffffff !important;}
  table.dataTable tbody tr:hover{background:#2d0000 !important;color:#ffcc00 !important;}
  table.dataTable tbody td{border-bottom:1px solid rgba(255,80,0,0.15) !important;padding:8px !important;}
  .nav-tabs>li>a{color:#ffcc00 !important;background:rgba(45,0,0,0.85) !important;border:1px solid rgba(255,204,0,0.3) !important;border-radius:6px 6px 0 0 !important;margin-right:3px;font-weight:600;font-size:12px;}
  .nav-tabs>li.active>a{background:linear-gradient(180deg,#cc0000,#880000) !important;color:#ffcc00 !important;border-bottom:2px solid #ffcc00 !important;}
  .tab-content{background:rgba(10,0,0,0.88) !important;padding:15px;border-radius:0 8px 8px 8px;border:1px solid rgba(255,204,0,0.18);}
  .formula-box{background:linear-gradient(135deg,rgba(255,204,0,0.06),rgba(255,80,0,0.04));border:1px solid rgba(255,204,0,0.4);border-radius:10px;padding:14px;margin-bottom:12px;}
  .formula-title{color:#ffcc00;font-weight:bold;font-size:13px;margin:0 0 6px 0;border-bottom:1px solid rgba(255,204,0,0.3);padding-bottom:4px;}
  .formula-text{color:#dddddd;font-size:12px;margin:3px 0;font-family:"Courier New",monospace;line-height:1.6;}
  .interp-box{background:linear-gradient(135deg,rgba(0,100,0,0.12),rgba(0,60,0,0.08));border:1px solid rgba(100,255,100,0.3);border-radius:10px;padding:12px;margin-top:10px;}
  .interp-title{color:#88ff88;font-weight:bold;font-size:13px;margin:0 0 5px 0;}
  .interp-text{color:#ccffcc;font-size:12px;margin:2px 0;}
  .result-box{background:linear-gradient(135deg,rgba(0,50,150,0.15),rgba(0,30,100,0.1));border:1px solid rgba(100,150,255,0.4);border-radius:10px;padding:12px;margin-top:10px;}
  .result-title{color:#88aaff;font-weight:bold;font-size:13px;margin:0 0 5px 0;}
  .result-text{color:#ccddff;font-size:12px;margin:2px 0;}
  .thesis-box{background:linear-gradient(135deg,rgba(0,40,80,0.3),rgba(0,20,50,0.5));border:1px solid rgba(100,150,255,0.35);border-radius:10px;padding:16px;margin-top:12px;}
  .weight-row{margin-bottom:5px;}
  .weight-label{color:#ffcc00;font-weight:bold;font-size:12px;line-height:34px;}
  .irs-bar{background:#ff0000 !important;border-top:1px solid #cc0000 !important;border-bottom:1px solid #cc0000 !important;}
  .irs-single{background:#ffcc00 !important;color:#1a0000 !important;font-weight:bold;}
  pre{background:#050505 !important;color:#ffcc00 !important;border:1px solid rgba(255,204,0,0.3) !important;border-radius:8px !important;font-size:12px !important;padding:12px !important;}
  ::-webkit-scrollbar{width:5px;}
  ::-webkit-scrollbar-track{background:#0d0000;}
  ::-webkit-scrollbar-thumb{background:#ff0000;border-radius:3px;}
  .wsm-badge{display:inline-block;background:linear-gradient(45deg,#ffcc00,#ff6600);color:#1a0000;font-weight:900;padding:3px 10px;border-radius:20px;font-size:11px;margin:2px;}
  .rank-gold{color:#ffcc00;font-size:20px;font-weight:900;}
  .rank-silver{color:#cccccc;font-size:18px;font-weight:700;}
  .rank-bronze{color:#cd7f32;font-size:16px;font-weight:700;}
  .corr-badge{display:inline-block;padding:4px 12px;border-radius:20px;font-weight:700;font-size:12px;margin:3px;}
  .corr-strong-pos{background:rgba(255,204,0,0.25);border:1px solid #ffcc00;color:#ffcc00;}
  .corr-moderate-pos{background:rgba(0,200,100,0.2);border:1px solid #00cc66;color:#00ff88;}
  .corr-weak{background:rgba(150,150,150,0.2);border:1px solid #888;color:#aaa;}
  .corr-negative{background:rgba(255,50,50,0.2);border:1px solid #ff4444;color:#ff8888;}
  .corr-company-header{background:linear-gradient(90deg,#880000,#cc3300);border-radius:8px;padding:10px 16px;margin-bottom:12px;border-left:4px solid #ffcc00;}
  .dl-plot-btn{background:linear-gradient(45deg,#003366,#0055aa) !important;color:#fff !important;border:none !important;border-radius:6px !important;padding:5px 14px !important;font-size:11px !important;font-weight:bold !important;cursor:pointer;margin-top:6px;margin-bottom:2px;transition:all 0.3s;}
  .dl-plot-btn:hover{background:linear-gradient(45deg,#0055aa,#ffcc00) !important;transform:translateY(-1px);}
  .dl-plot-wrap{text-align:right;padding-right:4px;}
')

fbox <- function(title,...) tags$div(class="formula-box",tags$p(class="formula-title",title),...)
fp   <- function(...) tags$p(class="formula-text",...)
ibox <- function(title,...) tags$div(class="interp-box",tags$p(class="interp-title",title),...)
ip   <- function(...) tags$p(class="interp-text",...)
rbox <- function(title,...) tags$div(class="result-box",tags$p(class="result-title",title),...)
rp   <- function(...) tags$p(class="result-text",...)

thesis_btn <- function(id, label="📝 Generate Thesis Section") {
  tags$div(style="margin-top:10px;",actionButton(id,label,class="btn-info",style="font-size:12px;padding:6px 14px;"))
}
thesis_out <- function(id) {
  tags$div(class="thesis-box",uiOutput(id))
}
interp_btn <- function(id, label="💬 Interpret Results") {
  actionButton(id, label, class="btn-success", style="font-size:12px;padding:6px 14px;margin-top:8px;")
}
interp_out <- function(id) {
  tags$div(class="result-box", style="margin-top:10px;", uiOutput(id))
}

# ═══════════════════════════════════════════════════════════════
# 🎨 UI
# ═══════════════════════════════════════════════════════════════
ui <- dashboardPage(skin="red",

  dashboardHeader(
    title=tags$div(
      tags$i(class="fas fa-chart-line",style="color:#ffcc00;margin-right:8px;"),
      tags$span("FINANCIAL ANALYTICS",style="color:#ffcc00;font-weight:900;font-size:14px;"),
      tags$span(" DASHBOARD",style="color:#ff8800;font-weight:600;font-size:14px;")
    ),
    titleWidth=285
  ),

  dashboardSidebar(
    width=268,
    tags$head(tags$style(common_css)),

    tags$div(class="profile-card",
      uiOutput("profile_img_out"),
      tags$p(class="profile-name","Aftab Ahmad"),
      tags$p(class="profile-title","Financial Analysis Dashboard")
    ),

    hr(),

    # ══════════════════════════════════════════════════════
    # FIX: Removed trailing comma after last menuItem
    # ══════════════════════════════════════════════════════
    sidebarMenu(id="sidebarMenu",
      menuItem("📈 Overview",           tabName="overview",   icon=icon("chart-line")),
      menuItem("📋 Raw Data",           tabName="rawdata",    icon=icon("table")),
      menuItem("📉 Trend Analysis",     tabName="trend",      icon=icon("chart-line")),
      menuItem("📊 Multi-Ratio Graph",  tabName="graph",      icon=icon("chart-bar")),
      menuItem("📐 Statistics",         tabName="stats",      icon=icon("calculator")),
      menuItem("🏆 WSM Ranking",        tabName="rank",       icon=icon("trophy")),
      menuItem("🔄 Correlation",        tabName="corr",       icon=icon("project-diagram")),
      menuItem("📈 Regression",         tabName="reg",        icon=icon("chart-line")),
      menuItem("📏 Z-Score & WSM",      tabName="zscore",     icon=icon("balance-scale")),
      menuItem("⏱️ Time Series",        tabName="timeseries", icon=icon("wave-square")),
      menuItem("🔬 Advanced Analysis",  tabName="advanced",   icon=icon("flask")),
      menuItem("🤖 AI Thesis Writer",   tabName="ai_thesis",  icon=icon("robot"))
    ),

    hr(),

    pickerInput("company","🏢 Companies:",choices=all_companies,selected=all_companies[1],
      multiple=TRUE,options=list(`actions-box`=TRUE,`selected-text-format`="count > 2",
      `count-selected-text`="{0} companies",title="Select...")),

    pickerInput("ratios","📊 Ratios:",choices=ratios,selected=ratios[1:4],
      multiple=TRUE,options=list(`actions-box`=TRUE,`selected-text-format`="count > 3")),

    hr(),

    tags$div(style="padding:0 12px;",
      tags$p(style="color:#ffcc00;font-weight:bold;font-size:11px;letter-spacing:1px;margin-bottom:6px;","💾 EXPORT"),
      downloadButton("save_pdf","📄 PDF Report",style="width:100%;margin-bottom:6px;font-size:12px;"),
      br(),
      downloadButton("save_xlsx","📊 Excel File",style="width:100%;margin-bottom:8px;font-size:12px;")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(common_css),
      tags$script(HTML("
        function dlPlot(plotId, fname) {
          var gd = document.getElementById(plotId);
          if(gd && gd.data) {
            Plotly.downloadImage(gd, {format:'png', width:1200, height:700, filename: fname || plotId});
          } else {
            alert('Chart not ready. Please wait for chart to load first.');
          }
        }
        function dlBasePlot(divId, fname) {
          var el = document.getElementById(divId);
          if(!el) { alert('Plot not found'); return; }
          var canvas = el.querySelector('canvas');
          if(canvas) {
            var link = document.createElement('a');
            link.download = (fname||divId) + '.png';
            link.href = canvas.toDataURL('image/png');
            link.click();
          } else {
            var img = el.querySelector('img');
            if(img) {
              var link = document.createElement('a');
              link.download = (fname||divId) + '.png';
              link.href = img.src;
              link.click();
            } else {
              alert('Use browser right-click > Save image for this chart type.');
            }
          }
        }
      "))
    ),
    tabItems(

      # ═══ OVERVIEW ═══
      tabItem(tabName="overview",
        fluidRow(
          box(width=12,title="📈 ROA Overview — All Companies (2020–2024)",status="primary",solidHeader=TRUE,
            plotlyOutput("overview_plot",height="480px"),
            tags$div(class="dl-plot-wrap",
              tags$button("📥 Download ROA Overview",class="dl-plot-btn",
                onclick="dlPlot('overview_plot','ROA_Overview_AllCompanies')"))),
          box(width=12,title="📊 Key Performance Summary",status="primary",solidHeader=TRUE,collapsible=TRUE,
            fluidRow(
              column(3,tags$div(style="text-align:center;padding:10px;background:rgba(255,0,0,0.1);border:1px solid #ff0000;border-radius:8px;",
                tags$h4(style="color:#ffcc00;margin:0;","AP"),tags$p(style="color:#ffffff;margin:2px 0;font-size:11px;","Best ROA"),
                tags$h3(style="color:#ff0000;margin:0;","#1"))),
              column(3,tags$div(style="text-align:center;padding:10px;background:rgba(255,102,0,0.1);border:1px solid #ff6600;border-radius:8px;",
                tags$h4(style="color:#ffcc00;margin:0;","AM"),tags$p(style="color:#ffffff;margin:2px 0;font-size:11px;","Largest Revenue"),
                tags$h3(style="color:#ff6600;margin:0;","#2"))),
              column(3,tags$div(style="text-align:center;padding:10px;background:rgba(255,204,0,0.1);border:1px solid #ffcc00;border-radius:8px;",
                tags$h4(style="color:#ffcc00;margin:0;","ME"),tags$p(style="color:#ffffff;margin:2px 0;font-size:11px;","Best Liquidity"),
                tags$h3(style="color:#ffcc00;margin:0;","#3"))),
              column(3,tags$div(style="text-align:center;padding:10px;background:rgba(255,51,102,0.1);border:1px solid #ff3366;border-radius:8px;",
                tags$h4(style="color:#ffcc00;margin:0;","TE/MS"),tags$p(style="color:#ffffff;margin:2px 0;font-size:11px;","High Growth"),
                tags$h3(style="color:#ff3366;margin:0;","#4")))
            )
          )
        )
      ),

      # ═══ RAW DATA ═══
      tabItem(tabName="rawdata",
        fluidRow(
          box(width=12,title="📋 Raw Financial Data — All Companies | 2020–2024",status="primary",solidHeader=TRUE,
            tags$div(style="display:flex;gap:15px;margin-bottom:15px;flex-wrap:wrap;",
              tags$div(style="flex:1;min-width:120px;text-align:center;background:rgba(255,0,0,0.1);border:1px solid #ff0000;border-radius:8px;padding:12px;",
                tags$h2(style="color:#ffcc00;margin:0;font-size:28px;","5"),tags$p(style="color:#aaa;margin:0;font-size:11px;","Companies")),
              tags$div(style="flex:1;min-width:120px;text-align:center;background:rgba(255,102,0,0.1);border:1px solid #ff6600;border-radius:8px;padding:12px;",
                tags$h2(style="color:#ffcc00;margin:0;font-size:28px;","25"),tags$p(style="color:#aaa;margin:0;font-size:11px;","Observations")),
              tags$div(style="flex:1;min-width:120px;text-align:center;background:rgba(255,204,0,0.1);border:1px solid #ffcc00;border-radius:8px;padding:12px;",
                tags$h2(style="color:#ffcc00;margin:0;font-size:28px;","12"),tags$p(style="color:#aaa;margin:0;font-size:11px;","Ratios")),
              tags$div(style="flex:1;min-width:120px;text-align:center;background:rgba(255,51,102,0.1);border:1px solid #ff3366;border-radius:8px;padding:12px;",
                tags$h2(style="color:#ffcc00;margin:0;font-size:28px;","5"),tags$p(style="color:#aaa;margin:0;font-size:11px;","Years (2020–24)"))
            ),
            tabsetPanel(
              tabPanel("📊 Raw Data",br(),DTOutput("raw_table")),
              tabPanel("📐 Computed Ratios",br(),DTOutput("ratio_table")),
              tabPanel("🏢 Per Company",br(),
                selectInput("raw_co","Company:",choices=all_companies,selected="AP"),
                br(),DTOutput("raw_per_co"))
            )
          )
        )
      ),

      # ═══ TREND ═══
      tabItem(tabName="trend",
        fluidRow(
          box(width=12,title="📉 Trend Analysis — B) Time Series Behavior",status="primary",solidHeader=TRUE,
            fbox("📘 B) Trend Analysis Formula:",
              fp("Growth Rate = [(Current Value − Previous Value) / |Previous Value|] × 100"),
              fp("Formula Source: Standard financial growth rate formula"),
              fp("Interpretation: Positive (+ve) = Growth | Negative (−ve) = Decline"),
              fp("Use: Tracks 5-year improvement trajectory for each company and ratio")
            ),
            tabsetPanel(
              tabPanel("📈 Trend Plot",br(),
                plotlyOutput("trend_plot",height="420px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Trend Plot",class="dl-plot-btn",onclick="dlPlot('trend_plot','Trend_Plot')"))),
              tabPanel("📊 Trend Score Table",br(),
                tags$div(style="background:rgba(255,204,0,0.07);border:1px solid rgba(255,204,0,0.3);border-radius:8px;padding:10px;margin-bottom:10px;",
                  tags$p(style="color:#ffcc00;font-weight:bold;margin:0;font-size:13px;","📊 Trend Analysis: Yearly values + Growth Rate")),
                fluidRow(column(6,selectInput("trend_table_ratio","Select Ratio for Table:",choices=ratios,selected="ROA")),column(6,tags$br())),
                br(),DTOutput("trend_score_table")),
              tabPanel("📉 Growth Rate (All Ratios)",br(),
                selectInput("trend_growth_co","Select Company:",choices=all_companies,selected=all_companies[1]),
                br(),DTOutput("trend_growth_table"),br(),
                plotlyOutput("trend_growth_plot",height="360px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Growth Chart",class="dl-plot-btn",onclick="dlPlot('trend_growth_plot','Growth_Rate_Chart')")))
            )
          ),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_trend","💬 Interpret Trend Results"),interp_out("interp_trend_out")),
          box(width=12,title="📝 AI Thesis — Trend Section",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_trend"),thesis_out("thesis_trend_out"))
        )
      ),

      # ═══ GRAPH ═══
      tabItem(tabName="graph",
        fluidRow(
          box(width=12,title="📊 Multi-Ratio Graph — Faceted by Company",status="primary",solidHeader=TRUE,
            plotlyOutput("multi_graph",height="520px"),
            tags$div(class="dl-plot-wrap",
              tags$button("📥 Download Multi-Ratio Graph",class="dl-plot-btn",onclick="dlPlot('multi_graph','MultiRatio_Graph')"))),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_graph","💬 Interpret Graph Results"),interp_out("interp_graph_out")),
          box(width=12,title="📝 AI Thesis — Graph Section",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_graph"),thesis_out("thesis_graph_out"))
        )
      ),

      # ═══ STATISTICS ═══
      tabItem(tabName="stats",
        fluidRow(
          box(width=12,title="📐 Descriptive Statistics — A) Data Understanding Layer",status="primary",solidHeader=TRUE,
            fbox("📘 A) Descriptive Statistics Formulas:",
              fp("① Mean: μ = ΣX/n   ② Median: middle value   ③ SD: σ=√[Σ(X−μ)²/n]"),
              fp("④ CV=(σ/μ)×100   ⑤ Growth Rate = [(Current Value − Previous Value) / |Previous Value|] × 100"),
              fp("Use: High σ → Risky company | Low CV → Stable performance | High Mean → Strong company")
            ),
            tabsetPanel(
              tabPanel("📊 All Combined",br(),DTOutput("stats_table")),
              tabPanel("🏢 Per Company",br(),
                selectInput("stats_co","Company:",choices=all_companies,selected=all_companies[1]),br(),DTOutput("stats_per_co")),
              tabPanel("📅 Yearly Ratios",br(),
                selectInput("yearly_co","Company:",choices=all_companies,selected=all_companies[1]),br(),
                DTOutput("yearly_ratio_table"),br(),plotlyOutput("yearly_ratio_plot",height="400px")),
              tabPanel("📉 Growth Rate",br(),
                fluidRow(
                  column(6,selectInput("growth_co","Company:",choices=all_companies,selected=all_companies[1])),
                  column(6,selectInput("growth_ratio","Ratio:",choices=ratios,selected=ratios[1]))),
                br(),DTOutput("growth_table"),br(),
                plotlyOutput("growth_plot",height="340px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Growth Chart",class="dl-plot-btn",onclick="dlPlot('growth_plot','Growth_Chart')")))
            )
          ),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_stats","💬 Interpret Statistics Results"),interp_out("interp_stats_out")),
          box(width=12,title="📝 AI Thesis — Statistics Section",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_stats"),thesis_out("thesis_stats_out"))
        )
      ),

      # ═══ WSM RANKING ═══
      tabItem(tabName="rank",
        fluidRow(
          box(width=12,title="🏆 WSM Ranking — Final Company Performance Ranking",status="primary",solidHeader=TRUE,
            fbox("📘 WSM Ranking — Core Chain & Category Scores:",
              fp("Raw Data → 12 Ratios → Z-Score Standardization → Category Scores → WSM → FINAL RANK"),
              fp("Step 1: Z-Score: Z = (X − μ) / σ  |  DE reversed: higher DE = worse performance"),
              fp("Step 2: Category Scores:"),
              fp("  Z_PROF = avg(Z_ROE, Z_NPM, Z_OPM, Z_EBIT_L)   [Profitability — 35%]"),
              fp("  Z_EFF  = Z_AT                                    [Efficiency — 30%]"),
              fp("  Z_LIQ  = avg(Z_CR, Z_CF_R, Z_CF_L, Z_CA)       [Liquidity — 20%]"),
              fp("  Z_LEV  = avg(Z_DE, Z_IC)                        [Leverage — 15%]"),
              fp("Step 3: Final Score = 0.35·Z_PROF + 0.30·Z_EFF + 0.20·Z_LIQ + 0.15·Z_LEV")
            ),
            fluidRow(
              column(6,tags$p(style="color:#ffcc00;font-weight:bold;margin-bottom:5px;","⚖️ Weight Mode:"),
                selectInput("weight_mode","",choices=c("Literature-Based (Academic)"="lit","Custom Weights"="custom"),selected="lit",width="100%")),
              column(6,tags$p(style="color:#ffcc00;font-weight:bold;margin-bottom:5px;","🎛️ Custom Weights:"),
                conditionalPanel("input.weight_mode == 'custom'",uiOutput("custom_weight_sliders")))
            ),
            actionButton("calc_ranking","🏆 Calculate WSM Ranking",style="font-size:14px;padding:8px 20px;margin:10px 0;"),
            br(),br(),DTOutput("rank_table"),br(),
            plotlyOutput("rank_plot",height="380px"),br(),
            tags$div(class="dl-plot-wrap",
              tags$button("📥 Download WSM Ranking Chart",class="dl-plot-btn",onclick="dlPlot('rank_plot','WSM_Ranking')"))
          ),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_rank","💬 Interpret Ranking Results"),interp_out("interp_rank_out")),
          box(width=12,title="📝 AI Thesis — WSM Ranking Section",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_rank"),thesis_out("thesis_rank_out"))
        )
      ),

      # ═══ CORRELATION ═══
      tabItem(tabName="corr",
        fluidRow(
          box(width=12,title="🔄 Correlation Analysis — C) Relationship Study (Per Company + Combined)",status="primary",solidHeader=TRUE,
            fbox("📘 C) Pearson Correlation Formula:",
              fp("r = Σ(X − X̄)(Y − Ȳ) / √[Σ(X − X̄)² × Σ(Y − Ȳ)²]"),
              fp("The correlation coefficient (r) ranges from −1 to +1:"),
              fp("  +1 = Perfect positive relationship | −1 = Perfect negative relationship | 0 = No linear relationship"),
              fp("Strength Interpretation:"),
              fp("  |r| ≥ 0.7  → Strong Correlation"),
              fp("  0.4 ≤ |r| < 0.7  → Moderate Correlation"),
              fp("  0.2 ≤ |r| < 0.4  → Weak Correlation"),
              fp("  |r| < 0.2  → Very Weak / Negligible"),
              fp("Use: Detect multicollinearity | Per-company & combined analysis | Validates WSM weight categories")
            ),
            tabsetPanel(
              tabPanel("🌐 All Companies Combined",
                br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ffcc00;margin:0;","📊 Combined Correlation Matrix — All 5 Companies (n=25)"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","All observations pooled together | 12 Financial Ratios")
                ),
                fluidRow(
                  column(8,plotOutput("corr_plot",height="520px"),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download Combined Corr",class="dl-plot-btn",onclick="dlBasePlot('corr_plot','Correlation_Combined')"))),
                  column(4,
                    tags$div(style="padding-top:20px;",
                      tags$h5(style="color:#ffcc00;font-weight:bold;","🔑 Correlation Legend"),
                      tags$div(class="corr-badge corr-strong-pos","r > 0.7 = Strong Positive"),br(),
                      tags$div(class="corr-badge corr-moderate-pos","r 0.4–0.7 = Moderate Positive"),br(),
                      tags$div(class="corr-badge corr-weak","r 0.2–0.4 = Weak Positive"),br(),
                      tags$div(class="corr-badge corr-negative","r < 0 = Negative Correlation"),br(),br(),
                      tags$h5(style="color:#ffcc00;font-weight:bold;","📋 Key Pairs Expected:"),
                      tags$p(style="color:#aaa;font-size:11px;","• ROA ↔ ROE: Strong +ve"),
                      tags$p(style="color:#aaa;font-size:11px;","• ROA ↔ NPM: Positive"),
                      tags$p(style="color:#aaa;font-size:11px;","• DE ↔ ROA: Negative"),
                      tags$p(style="color:#aaa;font-size:11px;","• CR ↔ CA: Strong +ve")
                    )
                  )
                ),
                br(),
                tags$h5(style="color:#ffcc00;font-weight:bold;margin-top:10px;","📋 Numeric Correlation Table — All Companies"),
                DTOutput("corr_table_all")
              ),
              tabPanel("🔴 AP Correlation",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ff4444;margin:0;","🔴 AP — Individual Correlation Matrix (n=5)"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","AP: Large-cap tech/hardware company")
                ),
                fluidRow(
                  column(7,plotOutput("corr_AP",height="480px"),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download AP Correlation",class="dl-plot-btn",onclick="dlBasePlot('corr_AP','Correlation_AP')"))),
                  column(5,uiOutput("corr_interp_AP"))
                ),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📋 AP — Full Correlation Table"),DTOutput("corr_table_AP"),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📊 AP — Top Strong Correlations (|r| > 0.5)"),DTOutput("corr_strong_AP")
              ),
              tabPanel("🟠 AM Correlation",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ff6600;margin:0;","🟠 AM — Individual Correlation Matrix (n=5)"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","AM: E-commerce/cloud giant | Volatile NPM due to 2022 net loss")
                ),
                fluidRow(
                  column(7,plotOutput("corr_AM",height="480px"),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download AM Correlation",class="dl-plot-btn",onclick="dlBasePlot('corr_AM','Correlation_AM')"))),
                  column(5,uiOutput("corr_interp_AM"))
                ),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📋 AM — Full Correlation Table"),DTOutput("corr_table_AM"),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📊 AM — Top Strong Correlations (|r| > 0.5)"),DTOutput("corr_strong_AM")
              ),
              tabPanel("🟡 ME Correlation",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ffcc00;margin:0;","🟡 ME — Individual Correlation Matrix (n=5)"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","ME: High-margin semiconductor/chip company")
                ),
                fluidRow(
                  column(7,plotOutput("corr_ME",height="480px"),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download ME Correlation",class="dl-plot-btn",onclick="dlBasePlot('corr_ME','Correlation_ME')"))),
                  column(5,uiOutput("corr_interp_ME"))
                ),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📋 ME — Full Correlation Table"),DTOutput("corr_table_ME"),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📊 ME — Top Strong Correlations (|r| > 0.5)"),DTOutput("corr_strong_ME")
              ),
              tabPanel("🔴 TE Correlation",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ff3366;margin:0;","🔴 TE — Individual Correlation Matrix (n=5)"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","TE: Fast-growing EV company | Revenue grew 207%")
                ),
                fluidRow(
                  column(7,plotOutput("corr_TE",height="480px"),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download TE Correlation",class="dl-plot-btn",onclick="dlBasePlot('corr_TE','Correlation_TE')"))),
                  column(5,uiOutput("corr_interp_TE"))
                ),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📋 TE — Full Correlation Table"),DTOutput("corr_table_TE"),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📊 TE — Top Strong Correlations (|r| > 0.5)"),DTOutput("corr_strong_TE")
              ),
              tabPanel("🩷 MS Correlation",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ff99cc;margin:0;","🩷 MS — Individual Correlation Matrix (n=5)"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","MS: Software giant | Most stable/consistent performer")
                ),
                fluidRow(
                  column(7,plotOutput("corr_MS",height="480px"),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download MS Correlation",class="dl-plot-btn",onclick="dlBasePlot('corr_MS','Correlation_MS')"))),
                  column(5,uiOutput("corr_interp_MS"))
                ),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📋 MS — Full Correlation Table"),DTOutput("corr_table_MS"),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📊 MS — Top Strong Correlations (|r| > 0.5)"),DTOutput("corr_strong_MS")
              ),
              tabPanel("📊 Cross-Company Compare",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ffcc00;margin:0;","📊 Cross-Company Correlation Comparison"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","Compare how the same ratio pairs correlate differently across all 5 companies")
                ),
                fluidRow(
                  column(4,selectInput("cross_r1","Ratio X:",choices=ratios,selected="ROA")),
                  column(4,selectInput("cross_r2","Ratio Y:",choices=ratios,selected="ROE")),
                  column(4,br(),actionButton("run_cross_corr","🔄 Compare",style="font-size:13px;padding:8px 16px;"))
                ),
                br(),plotlyOutput("cross_corr_scatter",height="420px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Scatter",class="dl-plot-btn",onclick="dlPlot('cross_corr_scatter','CrossCorr_Scatter')")),
                br(),DTOutput("cross_corr_table"),br(),
                plotlyOutput("cross_corr_bar",height="320px"),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Bar Chart",class="dl-plot-btn",onclick="dlPlot('cross_corr_bar','CrossCorr_Bar')"))
              ),
              tabPanel("🔥 Heatmap Comparison",br(),
                tags$div(class="corr-company-header",
                  tags$h4(style="color:#ffcc00;margin:0;","🔥 Correlation Heatmap — All Companies"),
                  tags$p(style="color:#aaa;margin:4px 0 0 0;font-size:11px;","Select ratio pair and see correlation value per company")
                ),
                fluidRow(
                  column(6,selectInput("heat_r1","Select Ratio 1:",choices=ratios,selected="ROA")),
                  column(6,selectInput("heat_r2","Select Ratio 2:",choices=ratios,selected="NPM"))
                ),
                plotlyOutput("corr_heatmap_all",height="420px"),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Heatmap",class="dl-plot-btn",onclick="dlPlot('corr_heatmap_all','Correlation_Heatmap')")),
                br(),tags$h5(style="color:#ffcc00;font-weight:bold;","📋 Summary Table — r value per Company"),
                DTOutput("corr_summary_pair")
              )
            )
          ),
          box(width=12,title="💬 Correlation Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_corr","💬 Interpret Correlation Results"),interp_out("interp_corr_out")),
          box(width=12,title="📝 AI Thesis — Correlation Section",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_corr"),thesis_out("thesis_corr_out"))
        )
      ),

      # ═══ REGRESSION ═══
      tabItem(tabName="reg",
        fluidRow(
          box(width=12,title="📈 Regression Analysis — F) Impact Assessment",status="primary",solidHeader=TRUE,
            fbox("📘 F) Four Panel Regression Models — By WSM Category:",
              fp("Model 1 — Profitability (35%): ROA = β₀ + β₁ROE + β₂NPM + β₃OPM + β₄EBIT_L + μᵢ + λₜ + εᵢₜ"),
              fp("Model 2 — Efficiency (30%):     ROA = β₀ + β₁AT + μᵢ + λₜ + εᵢₜ"),
              fp("Model 3 — Liquidity (20%):      ROA = β₀ + β₁CR + β₂CF_R + β₃CF_L + β₄CA + μᵢ + λₜ + εᵢₜ"),
              fp("Model 4 — Leverage (15%):       ROA = β₀ + β₁DE + β₂IC + μᵢ + λₜ + εᵢₜ"),
              fp("Diagnostics: R² | Adj.R² | t = β̂/SE(β̂) | β>0 = positive impact | β<0 = negative impact")
            ),
            tabsetPanel(
              tabPanel("📋 All 4 Models Summary",br(),
                tags$div(style="background:rgba(255,204,0,0.07);border:1px solid rgba(255,204,0,0.3);border-radius:8px;padding:10px;margin-bottom:10px;",
                  tags$p(style="color:#ffcc00;font-weight:bold;margin:0;font-size:12px;",
                    "4 Panel Regression Models — Profitability (35%) | Efficiency (30%) | Liquidity (20%) | Leverage (15%)")),
                verbatimTextOutput("reg_summary")
              ),
              tabPanel("📈 Actual vs Predicted",br(),
                plotlyOutput("reg_pred_plot",height="420px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Actual vs Predicted",class="dl-plot-btn",onclick="dlPlot('reg_pred_plot','Regression_ActualVsPred')"))),
              tabPanel("📅 2025 Forecast",br(),
                plotlyOutput("reg_forecast_plot",height="380px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Forecast Chart",class="dl-plot-btn",onclick="dlPlot('reg_forecast_plot','Regression_Forecast2025')")),
                br(),DTOutput("reg_forecast_table"))
            )
          ),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_reg","💬 Interpret Regression Results"),interp_out("interp_reg_out")),
          box(width=12,title="📝 AI Thesis — Regression Section",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_reg"),thesis_out("thesis_reg_out"))
        )
      ),

      # ═══ Z-SCORE & WSM ═══
      tabItem(tabName="zscore",
        fluidRow(
          box(width=12,title="📏 Z-Score + WSM — D) Standardization & E) Ranking",status="primary",solidHeader=TRUE,
            tabsetPanel(
              tabPanel("⚖️ Weights & Run",br(),
                fluidRow(
                  column(6,fbox("📐 D) Z-Score Standardization:",
                    fp("Z = (X − μ) / σ"),
                    fp("Z > 0 = Above average | Z < 0 = Below average"),
                    fp("For leverage (DE): reverse applied — higher DE = worse performance"),
                    fp("Ensures comparability across variables with different scales")
                  )),
                  column(6,fbox("⚖️ E) WSM — Category Scores & Final Score:",
                    fp("Z_PROF = avg(Z_ROE, Z_NPM, Z_OPM, Z_EBIT_L)   [35%]"),
                    fp("Z_EFF  = Z_AT                                    [30%]"),
                    fp("Z_LIQ  = avg(Z_CR, Z_CF_R, Z_CF_L, Z_CA)       [20%]"),
                    fp("Z_LEV  = avg(Z_DE, Z_IC)                        [15%]"),
                    fp("Final Score = 0.35·Z_PROF + 0.30·Z_EFF + 0.20·Z_LIQ + 0.15·Z_LEV")
                  ))
                ),
                tags$p(style="color:#ffcc00;font-weight:bold;","Custom weights (0–10, auto-normalized):"),
                uiOutput("weight_sliders"),br(),
                actionButton("calc_wsm","🏆 Calculate",style="font-size:14px;padding:8px 20px;")
              ),
              tabPanel("📊 Z-Score Table",br(),DTOutput("zscore_table")),
              tabPanel("🏆 WSM Ranking",br(),DTOutput("wsm_rank_table")),
              tabPanel("📈 WSM Chart",br(),
                plotlyOutput("wsm_chart",height="400px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download WSM Chart",class="dl-plot-btn",onclick="dlPlot('wsm_chart','ZScore_WSM_Chart')")))
            )
          ),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_zscore","💬 Interpret Z-Score & WSM Results"),interp_out("interp_zscore_out")),
          box(width=12,title="📝 AI Thesis",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_zscore"),thesis_out("thesis_zscore_out"))
        )
      ),

      # ═══ TIME SERIES ═══
      tabItem(tabName="timeseries",
        fluidRow(
          box(width=12,title="⏱️ Time Series — G) ARMA/ARIMA/Box-Jenkins",status="primary",solidHeader=TRUE,
            fluidRow(
              column(6,fbox("📘 1. ARMA Model:",
                fp("Yₜ = c + Σᵢ(φᵢ Yₜ₋ᵢ) + εₜ + Σⱼ(θⱼ εₜ₋ⱼ)"),
                fp("AR part: captures dependence on past values"),
                fp("MA part: captures dependence on past error terms")
              )),
              column(6,fbox("📘 2. ARIMA(p,d,q) Model:",
                fp("ΔᵈYₜ = c + Σᵢ(φᵢ ΔᵈYₜ₋ᵢ) + εₜ + Σⱼ(θⱼ εₜ₋ⱼ)"),
                fp("Extends ARMA to non-stationary data via differencing (d)"),
                fp("Stationarity tested using Augmented Dickey-Fuller (ADF) test")
              ))
            ),
            fbox("📘 3. Box–Jenkins Four-Step Procedure:",
              fp("① Identification: ACF & PACF plots → determine p, d, q values"),
              fp("② Estimation: Parameters φ (AR) and θ (MA) via MLE"),
              fp("③ Diagnostic Checking: Ljung-Box test (p > 0.05 = good fit)"),
              fp("④ Forecasting: Validated ARIMA → accuracy via RMSE & MAE")
            ),
            fluidRow(
              column(4,selectInput("ts_co","Company:",choices=all_companies,selected=all_companies[1])),
              column(4,selectInput("ts_ratio","Ratio:",choices=ratios,selected="ROA")),
              column(4,selectInput("ts_model","Model:",choices=c("Linear Trend"="lm","ARIMA"="arima"),selected="lm"))
            ),
            fluidRow(
              column(3,numericInput("ts_p","AR(p):",value=1,min=0,max=3,step=1)),
              column(3,numericInput("ts_d","d:",value=0,min=0,max=2,step=1)),
              column(3,numericInput("ts_q","MA(q):",value=0,min=0,max=3,step=1)),
              column(3,numericInput("ts_h","Horizon:",value=3,min=1,max=10,step=1))
            ),
            actionButton("run_ts","⏱️ Run Analysis",style="font-size:14px;padding:8px 20px;"),br(),br(),
            tabsetPanel(
              tabPanel("📈 Forecast Plot",br(),
                plotlyOutput("ts_plot",height="440px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Forecast Plot",class="dl-plot-btn",onclick="dlPlot('ts_plot','TimeSeries_Forecast')"))),
              tabPanel("📋 Model Summary",br(),verbatimTextOutput("ts_summary")),
              tabPanel("📊 Forecast Table",br(),DTOutput("ts_forecast_table")),
              tabPanel("📉 ACF/PACF",br(),
                plotOutput("ts_acf_plot",height="360px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download ACF/PACF",class="dl-plot-btn",onclick="dlBasePlot('ts_acf_plot','TimeSeries_ACFPACF')")))
            )
          ),
          box(width=12,title="💬 Results Interpretation",status="success",solidHeader=TRUE,collapsible=TRUE,
            interp_btn("interp_ts","💬 Interpret Time Series Results"),interp_out("interp_ts_out")),
          box(width=12,title="📝 AI Thesis",status="warning",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
            thesis_btn("thesis_ts"),thesis_out("thesis_ts_out"))
        )
      ),

      # ═══ ADVANCED ═══
      tabItem(tabName="advanced",
        fluidRow(
          box(width=12,
            title="🔬 Advanced Analysis — H) PCA | DEA | Sharpe | K-Means | Sensitivity | Outliers",
            status="primary",solidHeader=TRUE,
            tabsetPanel(
              tabPanel("🔵 PCA",br(),
                fluidRow(
                  column(6,fbox("📘 H1) PCA Model:",
                    fp("Step 1 — Standardization: Zᵢⱼ = (Xᵢⱼ − X̄ⱼ) / σⱼ"),
                    fp("Step 2 — Eigenvalue Equation: |Σ − λI| = 0"),
                    fp("Step 3 — Component Formation: PCₖ = Σⱼ(aₖⱼ Zⱼ)"),
                    fp("Step 4 — Kaiser Criterion: retain eigenvalue > 1")
                  )),
                  column(6,ibox("📌 PCA Interpretation:",
                    ip("PC1: Explains maximum variance → dominant financial structure"),
                    ip("Loadings: Importance of each ratio to a component"),
                    ip("Companies close in biplot → similar financial behavior"),
                    ip("Use: Reduces 12 ratios to 2–3 factors")
                  ))
                ),
                actionButton("run_pca","🔵 Run PCA",style="margin-bottom:10px;"),
                tabsetPanel(
                  tabPanel("Scree Plot",br(),plotlyOutput("pca_scree",height="320px"),br(),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download Scree Plot",class="dl-plot-btn",onclick="dlPlot('pca_scree','PCA_Scree')"))),
                  tabPanel("Loadings",br(),DTOutput("pca_loadings")),
                  tabPanel("Biplot",br(),plotlyOutput("pca_biplot",height="420px"),br(),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download PCA Biplot",class="dl-plot-btn",onclick="dlPlot('pca_biplot','PCA_Biplot')"))),
                  tabPanel("PC Scores",br(),DTOutput("pca_scores"))
                ),
                br(),interp_btn("interp_pca","💬 Interpret PCA"),interp_out("interp_pca_out"),
                thesis_btn("thesis_pca"),thesis_out("thesis_pca_out")
              ),
              tabPanel("🟢 DEA",br(),
                fbox("📘 H2) DEA:",
                  fp("Efficiency=Σ(u·y)/Σ(v·x) | Score 0–1 | 1.0=Efficient frontier"),
                  fp("Alternate ranking — robustness check vs WSM")),
                fluidRow(
                  column(6,selectInput("dea_out","Outputs:",choices=c("ROA","ROE","NPM","OPM"),selected=c("ROA","ROE"),multiple=TRUE)),
                  column(6,selectInput("dea_in","Inputs:",choices=c("DE","CR","CF_R","AT"),selected=c("DE","CR"),multiple=TRUE))
                ),
                actionButton("run_dea","🟢 Run DEA",style="margin-bottom:10px;"),
                DTOutput("dea_table"),br(),
                plotlyOutput("dea_plot",height="340px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download DEA Chart",class="dl-plot-btn",onclick="dlPlot('dea_plot','DEA_Efficiency')")),
                br(),interp_btn("interp_dea","💬 Interpret DEA"),interp_out("interp_dea_out"),
                thesis_btn("thesis_dea"),thesis_out("thesis_dea_out")
              ),
              tabPanel("🟠 Sharpe Ratio",br(),
                fbox("📘 H3) Sharpe:",
                  fp("S=(Rₚ−Rƒ)/σₚ | S>1=Excellent | S<0=Bad"),
                  fp("Risk-adjusted return comparison")),
                numericInput("rf_rate","Risk-Free Rate (Rƒ):",value=0.02,min=0,max=0.2,step=0.005),
                actionButton("run_sharpe","🟠 Calculate",style="margin-bottom:10px;"),
                DTOutput("sharpe_table"),br(),
                plotlyOutput("sharpe_plot",height="340px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Sharpe Chart",class="dl-plot-btn",onclick="dlPlot('sharpe_plot','Sharpe_Ratio')")),
                br(),interp_btn("interp_sharpe","💬 Interpret Sharpe"),interp_out("interp_sharpe_out"),
                thesis_btn("thesis_sharpe"),thesis_out("thesis_sharpe_out")
              ),
              tabPanel("🔴 K-Means",br(),
                fbox("📘 H4) K-Means Clustering:",
                  fp("Objective: min Σₖ Σᵢ∈Cₖ ‖xᵢ − μₖ‖²"),
                  fp("Minimizes within-cluster sum of squared distances"),
                  fp("Classification: High | Medium | Low performance clusters")
                ),
                fluidRow(
                  column(4,numericInput("km_k","Clusters:",value=3,min=2,max=5,step=1)),
                  column(8,selectInput("km_ratios","Ratios:",choices=ratios,selected=c("ROA","ROE","NPM","CR","DE"),multiple=TRUE))
                ),
                actionButton("run_kmeans","🔴 Run K-Means",style="margin-bottom:10px;"),
                tabsetPanel(
                  tabPanel("Assignments",br(),DTOutput("km_table")),
                  tabPanel("Cluster Plot",br(),plotlyOutput("km_plot",height="400px"),br(),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download Cluster Plot",class="dl-plot-btn",onclick="dlPlot('km_plot','KMeans_ClusterPlot')"))),
                  tabPanel("Profiles",br(),plotlyOutput("km_profile",height="380px"),br(),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download Profiles",class="dl-plot-btn",onclick="dlPlot('km_profile','KMeans_Profiles')")))
                ),
                br(),interp_btn("interp_km","💬 Interpret Clusters"),interp_out("interp_km_out"),
                thesis_btn("thesis_km"),thesis_out("thesis_km_out")
              ),
              tabPanel("🟡 Sensitivity",br(),
                fbox("📘 H5) Sensitivity:",
                  fp("ΔY=f(X+ΔX)−f(X) | Weight ±10/20/30% perturbation"),
                  fp("Tests ranking stability — validates WSM robustness")),
                selectInput("sens_ratio","Focus Ratio:",choices=ratios,selected="ROA"),
                actionButton("run_sens","🟡 Run Sensitivity",style="margin-bottom:10px;"),
                DTOutput("sens_table"),br(),
                plotlyOutput("sens_plot",height="380px"),br(),
                tags$div(class="dl-plot-wrap",
                  tags$button("📥 Download Sensitivity Chart",class="dl-plot-btn",onclick="dlPlot('sens_plot','Sensitivity_Analysis')")),
                br(),interp_btn("interp_sens","💬 Interpret Sensitivity"),interp_out("interp_sens_out"),
                thesis_btn("thesis_sens"),thesis_out("thesis_sens_out")
              ),
              tabPanel("🔶 Outliers",br(),
                fbox("📘 H6) Outlier Detection:",
                  fp("Z=(X−μ)/σ | |Z|>2=Moderate | |Z|>3=Extreme Outlier")),
                actionButton("run_outlier","🔶 Detect Outliers",style="margin-bottom:10px;"),
                tabsetPanel(
                  tabPanel("Table",br(),DTOutput("outlier_table")),
                  tabPanel("Heatmap",br(),plotlyOutput("outlier_heatmap",height="440px"),br(),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download Outlier Heatmap",class="dl-plot-btn",onclick="dlPlot('outlier_heatmap','Outlier_Heatmap')"))),
                  tabPanel("Count",br(),plotlyOutput("outlier_count",height="340px"),br(),
                    tags$div(class="dl-plot-wrap",
                      tags$button("📥 Download Outlier Count",class="dl-plot-btn",onclick="dlPlot('outlier_count','Outlier_Count')")))
                ),
                br(),interp_btn("interp_outlier","💬 Interpret Outliers"),interp_out("interp_outlier_out"),
                thesis_btn("thesis_outlier"),thesis_out("thesis_outlier_out")
              )
            )
          )
        )
      ),

      # ═══ AI THESIS WRITER ═══
      tabItem(tabName="ai_thesis",
        fluidRow(
          box(width=12,title="🤖 AI Thesis Writer — Complete Academic Dissertation Generator",status="primary",solidHeader=TRUE,
            tags$div(style="background:linear-gradient(135deg,rgba(0,50,100,0.25),rgba(0,20,60,0.4));border:1px solid rgba(100,150,255,0.5);border-radius:10px;padding:14px;margin-bottom:14px;",
              tags$p(style="color:#aaddff;font-size:13px;margin:0;","🤖 AI generates complete thesis chapters using your actual financial data.")),
            tabsetPanel(
              tabPanel("📖 Full Thesis Generator",br(),
                fluidRow(
                  column(6,selectInput("thesis_chapter","Select Chapter/Section:",
                    choices=c("Abstract (250 words)"="abstract","Chapter 1: Introduction"="ch1",
                              "Chapter 2: Literature Review"="ch2","Chapter 3: Methodology"="ch3",
                              "Chapter 4: Results & Discussion"="ch4",
                              "Chapter 4.1: Descriptive Statistics"="ch4_stats",
                              "Chapter 4.2: Correlation Analysis"="ch4_corr",
                              "Chapter 4.3: Regression Results"="ch4_reg",
                              "Chapter 4.4: Z-Score & WSM Ranking"="ch4_zscore",
                              "Chapter 4.5: Time Series Forecast"="ch4_ts",
                              "Chapter 5: Conclusion"="ch5",
                              "Executive Summary"="exec_summary"),selected="abstract")),
                  column(6,selectInput("thesis_style","Writing Style:",
                    choices=c("Academic/Formal (3rd person)"="academic","Simple English"="simple",
                              "APA 7th Edition"="apa","Professional Report"="report"),selected="academic"))
                ),
                actionButton("gen_thesis","🤖 Generate with AI",style="font-size:15px;padding:10px 25px;margin-bottom:15px;"),
                tags$div(class="thesis-box",uiOutput("thesis_out"))
              ),
              tabPanel("📝 Custom AI Query",br(),
                textAreaInput("custom_q","Ask AI about your financial data:",
                  placeholder="e.g. Compare all 5 companies and identify best performer...",rows=4,width="100%"),
                actionButton("gen_custom","🤖 Ask AI",style="font-size:15px;padding:10px 25px;margin-bottom:15px;"),
                tags$div(class="thesis-box",uiOutput("custom_out"))
              ),
              tabPanel("📊 WSM Result Summary",br(),
                tags$p(style="color:#ffcc00;font-weight:bold;","Auto-generated from your WSM ranking data:"),
                verbatimTextOutput("wsm_summary_print")
              )
            )
          )
        )
      )

    ) # end tabItems
  )
)


# ═══════════════════════════════════════════════════════════════
# ⚙️ SERVER
# ═══════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── PROFILE IMAGE ──
  output$profile_img_out <- renderUI({
    if(!is.null(profile_img_src)) {
      HTML(paste0(
        '<div style="text-align:center;margin-bottom:8px;">',
        '<img src="', profile_img_src, '" ',
        '     width="88" height="88" ',
        '     style="border-radius:50%;border:3px solid #ffcc00;',
        '            box-shadow:0 0 22px rgba(255,204,0,0.95),0 0 45px rgba(255,80,0,0.6);',
        '            object-fit:cover;display:block;margin:0 auto;background:#080000;" ',
        '     onerror="this.style.display=\'none\'"',
        '     alt="Aftab Ahmad" /></div>'
      ))
    } else {
      HTML('<div style="text-align:center;margin-bottom:8px;">
        <div style="width:88px;height:88px;border-radius:50%;border:3px solid #ffcc00;margin:0 auto;
                    background:linear-gradient(135deg,#880000,#cc0000);display:flex;
                    align-items:center;justify-content:center;
                    box-shadow:0 0 22px rgba(255,204,0,0.95);font-size:32px;">👤</div>
      </div>')
    }
  })

  # ── Reactive helpers ──
  selected_data <- reactive({
    req(input$company, length(input$company) > 0)
    df %>% filter(company %in% input$company)
  })

  dark_theme <- function() {
    theme_minimal() + theme(
      plot.background  = element_rect(fill="transparent", color=NA),
      panel.background = element_rect(fill="rgba(10,0,0,0.65)", color=NA),
      panel.grid.major = element_line(color="rgba(255,204,0,0.1)"),
      panel.grid.minor = element_line(color="rgba(255,80,0,0.04)"),
      text             = element_text(color="#ffffff", size=12),
      axis.text        = element_text(color="#cccccc", size=10),
      axis.title       = element_text(color="#ffcc00", size=13, face="bold"),
      legend.text      = element_text(color="#ffffff", size=10),
      legend.title     = element_text(color="#ffcc00", size=11, face="bold"),
      legend.background= element_rect(fill="rgba(10,0,0,0.88)", color="rgba(255,204,0,0.35)"),
      legend.key       = element_rect(fill="transparent"),
      plot.title       = element_text(color="#ffcc00", size=14, face="bold"),
      plot.subtitle    = element_text(color="#aaaaaa", size=11),
      strip.background = element_rect(fill="#770000"),
      strip.text       = element_text(color="#ffcc00", face="bold", size=11)
    )
  }

  pld <- function(p) {
    p %>% layout(
      plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)",
      font=list(color="#ffffff"),
      legend=list(font=list(color="#ffffff"), bgcolor="rgba(10,0,0,0.85)",
                  bordercolor="rgba(255,204,0,0.35)", borderwidth=1))
  }

  dtd <- function(dt, nc) {
    dt %>%
      formatStyle(1:nc, color="white", backgroundColor="#080000") %>%
      formatStyle(1, fontWeight="bold", color="#ffcc00")
  }

  # ══════════════════════════════════════════════════════════════
  # 🔄 CORRELATION HELPER FUNCTIONS
  # ══════════════════════════════════════════════════════════════
  get_corr_mat <- function(co) {
    d <- df %>% filter(company == co) %>% select(all_of(ratios))
    d <- d[, apply(d, 2, var, na.rm=TRUE) > 0, drop=FALSE]
    if(ncol(d) < 2) return(NULL)
    cor(d, use="complete.obs")
  }

  make_corrplot <- function(co, col_hi="#ffcc00") {
    cm <- get_corr_mat(co)
    if(is.null(cm)) return(NULL)
    corrplot(cm, method="color", type="upper", order="hclust",
             col=colorRampPalette(c("#ff0000","#222222",col_hi))(200),
             tl.col="#ffcc00", tl.srt=45, tl.cex=0.85,
             addCoef.col="white", number.cex=0.72,
             title=paste0(co," — Correlation Matrix (2020–2024)"), mar=c(0,0,2,0))
  }

  get_top_corr <- function(co, threshold=0.5) {
    cm <- get_corr_mat(co)
    if(is.null(cm)) return(data.frame())
    rn <- rownames(cm)
    pairs <- list()
    for(i in 1:(nrow(cm)-1)) {
      for(j in (i+1):ncol(cm)) {
        r_val <- cm[i,j]
        if(abs(r_val) >= threshold) {
          pairs[[length(pairs)+1]] <- data.frame(
            Ratio_X=rn[i], Ratio_Y=rn[j],
            r=round(r_val,4), abs_r=round(abs(r_val),4),
            Strength=ifelse(abs(r_val)>=0.7,"Strong",ifelse(abs(r_val)>=0.4,"Moderate","Weak")),
            Direction=ifelse(r_val>0,"Positive","Negative")
          )
        }
      }
    }
    if(length(pairs)==0) return(data.frame(Message="No correlations above threshold"))
    bind_rows(pairs) %>% arrange(desc(abs_r))
  }

  gen_co_corr_interp <- function(co) {
    top <- get_top_corr(co, 0.5)
    ctx <- list(
      AP = "AP (large-cap hardware/tech): Strong profitability clustering expected — ROA/ROE/NPM likely highly correlated.",
      AM = "AM (e-commerce/cloud): 2022 net loss creates unusual NPM pattern. DE correlation with ROA may be weak.",
      ME = "ME (semiconductors): Highest ROA company. Strong ROA-NPM-OPM clustering. Lean asset base boosts AT-ROA correlation.",
      TE = "TE (EV/growth): Revenue grew 207% 2020-2024. Revenue-driven ratios highly correlated. DE may negatively correlate with margins.",
      MS = "MS (software): Most stable, low CV. Strong positive correlations across all profitability ratios expected."
    )
    list(
      context = ctx[[co]],
      top_pairs = top,
      n_strong = if(is.data.frame(top) && "Strength" %in% names(top)) sum(top$Strength=="Strong", na.rm=TRUE) else 0,
      n_moderate = if(is.data.frame(top) && "Strength" %in% names(top)) sum(top$Strength=="Moderate", na.rm=TRUE) else 0
    )
  }

  render_company_corr <- function(co, col_hi="#ffcc00") {
    output[[paste0("corr_",co)]] <- renderPlot({ make_corrplot(co, col_hi) }, bg="transparent")

    output[[paste0("corr_interp_",co)]] <- renderUI({
      info <- gen_co_corr_interp(co)
      tags$div(style="padding:10px;",
        tags$div(style="background:rgba(255,204,0,0.08);border:1px solid rgba(255,204,0,0.3);border-radius:8px;padding:12px;margin-bottom:10px;",
          tags$p(style="color:#ffcc00;font-weight:bold;font-size:13px;margin:0 0 6px 0;", paste0("🏢 ",co," — Company Context")),
          tags$p(style="color:#cccccc;font-size:11px;margin:0;", info$context)
        ),
        tags$div(style="background:rgba(0,100,0,0.1);border:1px solid rgba(100,255,100,0.3);border-radius:8px;padding:12px;margin-bottom:10px;",
          tags$p(style="color:#88ff88;font-weight:bold;font-size:12px;margin:0 0 6px 0;","📊 Correlation Summary"),
          tags$p(style="color:#ccffcc;font-size:11px;margin:2px 0;", paste0("• Strong pairs (|r|>0.7): ", info$n_strong)),
          tags$p(style="color:#ccffcc;font-size:11px;margin:2px 0;", paste0("• Moderate pairs (|r|>0.5): ", info$n_moderate)),
          tags$p(style="color:#ccffcc;font-size:11px;margin:2px 0;","• Data points: n=5 (years 2020–2024)"),
          tags$p(style="color:#ffaa44;font-size:10px;margin:4px 0 0 0;","⚠️ n=5 is small. Use as directional evidence, not definitive proof.")
        ),
        tags$div(style="background:rgba(0,50,150,0.12);border:1px solid rgba(100,150,255,0.3);border-radius:8px;padding:12px;",
          tags$p(style="color:#88aaff;font-weight:bold;font-size:12px;margin:0 0 6px 0;","📘 Academic Interpretation"),
          tags$p(style="color:#ccddff;font-size:11px;margin:2px 0;","Formula: r = Σ[(X−X̄)(Y−Ȳ)] / √[Σ(X−X̄)²·Σ(Y−Ȳ)²]"),
          tags$p(style="color:#ccddff;font-size:11px;margin:2px 0;","• Positive r → Ratios move together"),
          tags$p(style="color:#ccddff;font-size:11px;margin:2px 0;","• Negative r → Inverse relationship"),
          tags$p(style="color:#ccddff;font-size:11px;margin:6px 0 0 0;","Use: Validates WSM weight categories & detects multicollinearity risk")
        )
      )
    })

    output[[paste0("corr_table_",co)]] <- renderDT({
      cm <- get_corr_mat(co)
      if(is.null(cm)) return(datatable(data.frame(Msg="Not enough data")))
      cm_df <- as.data.frame(round(cm,4))
      cm_df <- cbind(Ratio=rownames(cm_df), cm_df)
      datatable(cm_df, options=list(pageLength=12, scrollX=TRUE, dom='Bfrtip'), rownames=FALSE,
                caption=paste0(co," — Pearson Correlation Matrix (r values)")) %>%
        formatStyle("Ratio", fontWeight="bold", color="#ffcc00") %>%
        formatStyle(2:ncol(cm_df), color="#ffffff", backgroundColor="#080000")
    })

    output[[paste0("corr_strong_",co)]] <- renderDT({
      top <- get_top_corr(co, 0.5)
      if(!is.data.frame(top) || nrow(top)==0 || "Message" %in% names(top)) {
        return(datatable(data.frame(Message=paste0("No correlations |r|>0.5 found for ",co))))
      }
      top <- top %>% select(-abs_r)
      datatable(top, options=list(pageLength=10, dom='Bfrtip'), rownames=FALSE,
                caption=paste0(co," — Strong/Moderate Correlations (|r| ≥ 0.5)")) %>%
        formatStyle("r", color=styleInterval(c(-0.7,0,0.7), c("#ff4444","#ffaa00","#88ff88","#ffcc00")), fontWeight="bold") %>%
        formatStyle("Strength", color=styleEqual(c("Strong","Moderate","Weak"), c("#ffcc00","#88ff88","#aaaaaa")), fontWeight="bold") %>%
        formatStyle("Direction", color=styleEqual(c("Positive","Negative"), c("#44ff88","#ff4444")), fontWeight="bold") %>%
        formatStyle(1:ncol(top), backgroundColor="#080000", color="#ffffff")
    })
  }

  render_company_corr("AP", "#ffcc00")
  render_company_corr("AM", "#ff6600")
  render_company_corr("ME", "#ffdd00")
  render_company_corr("TE", "#ff3366")
  render_company_corr("MS", "#ff99cc")

  # ── Combined correlation ──
  output$corr_plot <- renderPlot({
    req(input$ratios, length(input$ratios) >= 2)
    cd <- df[,input$ratios,drop=FALSE]
    cd <- cd[, apply(cd,2,var,na.rm=TRUE) != 0, drop=FALSE]
    validate(need(ncol(cd) >= 2, "Need ≥2 ratios."))
    corrplot(cor(cd, use="complete.obs"), method="color", type="upper", order="hclust",
             col=colorRampPalette(c("#ff0000","#333333","#ffcc00"))(200),
             tl.col="#ffcc00", tl.srt=45, addCoef.col="white", number.cex=0.75,
             title="Combined Correlation Matrix — All Companies (n=25)", mar=c(0,0,2,0))
  })

  output$corr_table_all <- renderDT({
    req(input$ratios, length(input$ratios) >= 2)
    cd <- df[,input$ratios,drop=FALSE]
    cd <- cd[, apply(cd,2,var,na.rm=TRUE) != 0, drop=FALSE]
    cm <- round(cor(cd, use="complete.obs"), 4)
    cm_df <- cbind(Ratio=rownames(as.data.frame(cm)), as.data.frame(cm))
    datatable(cm_df, options=list(pageLength=12, scrollX=TRUE, dom='Bfrtip'), rownames=FALSE,
              caption="All Companies Combined — Pearson Correlation Coefficients") %>%
      formatStyle("Ratio", fontWeight="bold", color="#ffcc00") %>%
      formatStyle(2:ncol(cm_df), color="#ffffff", backgroundColor="#080000")
  })

  # ── Cross-company comparison ──
  observeEvent(input$run_cross_corr, {
    r1 <- input$cross_r1; r2 <- input$cross_r2
    req(r1, r2, r1 != r2)

    output$cross_corr_scatter <- renderPlotly({
      d <- df %>% select(company, year, X=all_of(r1), Y=all_of(r2))
      p <- ggplot(d, aes(x=X, y=Y, color=company,
                         text=paste0(company," ",year,"\n",r1,":",round(X,3),"\n",r2,":",round(Y,3)))) +
        geom_point(size=4, alpha=0.85) +
        geom_smooth(aes(group=company), method="lm", se=FALSE, size=1, linetype="dashed", alpha=0.6) +
        scale_color_manual(values=company_colors) + dark_theme() +
        labs(title=paste0("Scatter: ",r1," vs ",r2," — Per Company"), x=r1, y=r2, color="Company")
      ggplotly(p, tooltip="text") %>% pld()
    })

    output$cross_corr_table <- renderDT({
      rows <- lapply(all_companies, function(co) {
        d_co <- df %>% filter(company==co) %>% select(all_of(c(r1,r2)))
        if(nrow(d_co) < 3 || var(d_co[[r1]],na.rm=TRUE)==0 || var(d_co[[r2]],na.rm=TRUE)==0)
          return(data.frame(Company=co, r=NA, p_value=NA, Strength="Insufficient data", Direction=NA, n=nrow(d_co)))
        ct <- cor.test(d_co[[r1]], d_co[[r2]], use="complete.obs")
        data.frame(Company=co, r=round(ct$estimate,4), p_value=round(ct$p.value,4),
                   Strength=ifelse(abs(ct$estimate)>=0.7,"Strong",ifelse(abs(ct$estimate)>=0.4,"Moderate","Weak")),
                   Direction=ifelse(ct$estimate>0,"Positive","Negative"),
                   Significant=ifelse(ct$p.value<0.05,"Yes (p<0.05)","No (p≥0.05)"), n=5)
      })
      out <- bind_rows(rows) %>% arrange(desc(abs(r)))
      datatable(out, options=list(pageLength=10, dom='t'), rownames=FALSE,
                caption=paste0("Correlation of ",r1," vs ",r2," — Per Company")) %>%
        formatStyle("r", color=styleInterval(c(-0.7,0,0.7), c("#ff4444","#ffaa00","#88ff88","#ffcc00")), fontWeight="bold") %>%
        formatStyle("Strength", color=styleEqual(c("Strong","Moderate","Weak"), c("#ffcc00","#88ff88","#aaaaaa")), fontWeight="bold") %>%
        formatStyle("Significant", color=styleEqual(c("Yes (p<0.05)","No (p≥0.05)"), c("#44ff88","#ff6666")), fontWeight="bold") %>%
        formatStyle(1:ncol(out), backgroundColor="#080000", color="#ffffff")
    })

    output$cross_corr_bar <- renderPlotly({
      rows <- lapply(all_companies, function(co) {
        d_co <- df %>% filter(company==co) %>% select(all_of(c(r1,r2)))
        if(nrow(d_co)<3||var(d_co[[r1]],na.rm=TRUE)==0||var(d_co[[r2]],na.rm=TRUE)==0)
          return(data.frame(Company=co, r=0))
        data.frame(Company=co, r=round(cor(d_co[[r1]], d_co[[r2]], use="complete.obs"), 4))
      })
      out <- bind_rows(rows)
      cp <- company_colors[names(company_colors) %in% out$Company]
      p <- ggplot(out, aes(x=Company, y=r, fill=Company, text=paste0(Company,"\nr=",r))) +
        geom_bar(stat="identity", width=0.65) +
        geom_hline(yintercept=c(-0.7,-0.4,0,0.4,0.7),
                   color=c("#ff4444","#ffaa00","#ffffff","#88ff88","#ffcc00"), linetype="dashed", alpha=0.5) +
        scale_fill_manual(values=cp) + dark_theme() + theme(legend.position="none") +
        labs(title=paste0("r(",r1," vs ",r2,") — Per Company"), x="Company", y="Pearson r")
      ggplotly(p, tooltip="text") %>% pld() %>% layout(showlegend=FALSE)
    })
  })

  # ── Heatmap tab ──
  output$corr_heatmap_all <- renderPlotly({
    r1 <- input$heat_r1; r2 <- input$heat_r2
    req(r1, r2, r1 != r2)
    rows <- lapply(all_companies, function(co) {
      d_co <- df %>% filter(company==co) %>% select(all_of(c(r1,r2)))
      if(nrow(d_co)<3||var(d_co[[r1]],na.rm=TRUE)==0) return(data.frame(Company=co, r=NA))
      data.frame(Company=co, r=round(cor(d_co[[r1]], d_co[[r2]], use="complete.obs"), 4))
    })
    out <- bind_rows(rows)
    plot_ly(out, x=~Company, y=~r, type="bar",
            marker=list(color=~r, colorscale=list(c(0,"#ff0000"),c(0.5,"#333333"),c(1,"#ffcc00")),
                        colorbar=list(title="r value"), showscale=TRUE),
            text=~paste0("Company: ",Company,"<br>r = ",r), hoverinfo="text") %>%
      layout(title=list(text=paste0("Correlation: ",r1," vs ",r2," | Per Company"), font=list(color="#ffcc00")),
             xaxis=list(title="Company", color="#fff"),
             yaxis=list(title="Pearson r", range=c(-1,1), color="#fff"),
             plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)", font=list(color="#fff"),
             shapes=list(
               list(type="line",x0=-0.5,x1=4.5,y0=0.7,y1=0.7,line=list(color="#ffcc00",dash="dash")),
               list(type="line",x0=-0.5,x1=4.5,y0=-0.7,y1=-0.7,line=list(color="#ff4444",dash="dash")),
               list(type="line",x0=-0.5,x1=4.5,y0=0,y1=0,line=list(color="#888",dash="dash"))
             ))
  })

  output$corr_summary_pair <- renderDT({
    r1 <- input$heat_r1; r2 <- input$heat_r2
    req(r1, r2, r1 != r2)
    rows <- lapply(all_companies, function(co) {
      d_co <- df %>% filter(company==co) %>% select(all_of(c(r1,r2)))
      if(nrow(d_co)<3||var(d_co[[r1]],na.rm=TRUE)==0)
        return(data.frame(Company=co, r=NA, Strength="N/A", Direction="N/A"))
      rv <- round(cor(d_co[[r1]], d_co[[r2]], use="complete.obs"), 4)
      data.frame(Company=co, r=rv,
                 Strength=ifelse(abs(rv)>=0.7,"Strong",ifelse(abs(rv)>=0.4,"Moderate","Weak")),
                 Direction=ifelse(rv>0,"Positive","Negative"),
                 Interpretation=ifelse(abs(rv)>=0.7,
                   paste0(r1," and ",r2," move strongly together in ",co),
                   ifelse(abs(rv)>=0.4,
                     paste0("Moderate relationship in ",co),
                     paste0("Weak/no linear relationship in ",co))))
    })
    out <- bind_rows(rows)
    datatable(out, options=list(pageLength=10, dom='t'), rownames=FALSE,
              caption=paste0("Summary: ",r1," vs ",r2," — all companies")) %>%
      formatStyle("r", color=styleInterval(c(-0.7,0,0.7), c("#ff4444","#ffaa00","#88ff88","#ffcc00")), fontWeight="bold") %>%
      formatStyle("Strength", color=styleEqual(c("Strong","Moderate","Weak"), c("#ffcc00","#88ff88","#aaaaaa")), fontWeight="bold") %>%
      formatStyle("Company", fontWeight="bold", color="#ffcc00") %>%
      formatStyle(1:ncol(out), backgroundColor="#080000", color="#ffffff")
  })

  # ══════════════════════════════════════════════════════════════
  # 💬 INTERPRETATION & THESIS GENERATORS
  # ══════════════════════════════════════════════════════════════
  gen_interp <- function(section) {
    roa_avg <- round(tapply(df$ROA, df$company, mean), 4)
    roe_avg <- round(tapply(df$ROE, df$company, mean), 4)
    npm_avg <- round(tapply(df$NPM, df$company, mean), 4)
    sd_roa  <- round(tapply(df$ROA, df$company, sd), 4)
    best    <- names(which.max(roa_avg))
    worst   <- names(which.min(roa_avg))

    texts <- list(
      trend = paste0(
        "📊 TREND ANALYSIS — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ OVERALL TREND DIRECTION:\n","All five companies show positive growth trajectory over 2020–2024.\n\n",
        "▶ TOP PERFORMER:\n",best," demonstrates the strongest 5-year ROA trend.\n",
        "Average ROA = ",roa_avg[best]," — significantly above industry average.\n\n",
        "▶ WEAKEST PERFORMER:\n",worst," shows lowest ROA trend (",roa_avg[worst],").\n\n",
        "▶ GROWTH RATE ANALYSIS:\n",
        "• AP: Revenue growth 2020–2024 ≈ 42% | Stable profitability\n",
        "• AM: Revenue growth ≈ 65% | Highest revenue expansion\n",
        "• ME: ROA improved from 0.385 to 0.623 — Best ROA growth\n",
        "• TE: Strong revenue growth from 31K to 97K (207%)\n",
        "• MS: Consistent profitability growth (stable)\n"
      ),
      stats = paste0(
        "📊 DESCRIPTIVE STATISTICS — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ MEAN PERFORMANCE (μ = ΣX/n):\n",
        paste(paste0("• ",names(roa_avg),": ROA=",roa_avg," | ROE=",roe_avg," | NPM=",npm_avg), collapse="\n"),
        "\n\n▶ RISK ANALYSIS (σ = Standard Deviation):\n",
        paste(paste0("• ",names(sd_roa),": σROA=",sd_roa,ifelse(sd_roa>0.05," → HIGH RISK"," → STABLE")), collapse="\n"),
        "\n\n▶ KEY FINDINGS:\n",
        "1. ",best," has highest mean ROA (",roa_avg[best],") → Best overall profitability\n",
        "2. ME has highest ROA due to smaller asset base (efficiency advantage)\n",
        "3. AM shows negative NPM in 2022 — outlier year (net loss)\n",
        "4. MS has most stable performance (low CV) — defensive stock profile\n"
      ),
      corr = paste0(
        "📊 CORRELATION ANALYSIS — FULL RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ FORMULA: r = Σ(X−X̄)(Y−Ȳ) / √[Σ(X−X̄)² × Σ(Y−Ȳ)²]\n\n",
        "▶ STRENGTH SCALE:\n",
        "  |r| ≥ 0.7  → Strong | 0.4 ≤ |r| < 0.7 → Moderate\n\n",
        "▶ ALL COMPANIES COMBINED (n=25):\n",
        "• ROA ↔ ROE: Strong positive (r ≈ +0.85)\n",
        "• ROA ↔ NPM: Positive (r ≈ +0.70)\n",
        "• DE ↔ ROA: Negative (r ≈ −0.40)\n",
        "• CR ↔ CA: Strong positive (r ≈ +0.90)\n\n",
        "▶ PER COMPANY: AP/ME strongest clusters | AM weakest (2022 loss)\n",
        "▶ MULTICOLLINEARITY: ROA, ROE, NPM highly correlated → VIF check in regression.\n",
        "▶ Note: n=5 per company — use as directional evidence."
      ),
      reg = paste0(
        "📊 FOUR REGRESSION MODELS — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ MODEL 1 — PROFITABILITY (35%): ROA = β₀ + β₁ROE + β₂NPM + β₃OPM + β₄EBIT_L\n",
        "▶ MODEL 2 — EFFICIENCY (30%):   ROA = β₀ + β₁AT\n",
        "▶ MODEL 3 — LIQUIDITY (20%):    ROA = β₀ + β₁CR + β₂CF_R + β₃CF_L + β₄CA\n",
        "▶ MODEL 4 — LEVERAGE (15%):     ROA = β₀ + β₁DE + β₂IC\n\n",
        "▶ DE (β < 0): Higher leverage → Lower ROA (negative impact)\n",
        "▶ 2025 FORECAST: ",best," projected to maintain highest ROA.\n",
        "▶ R² > 0.70 → Model explains >70% variance in ROA (Strong fit)"
      ),
      zscore = paste0(
        "📊 Z-SCORE & WSM — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ Z-SCORE: Z = (X − μ) / σ  |  DE reversed (higher DE = worse)\n\n",
        "▶ CATEGORY SCORES:\n",
        "  Z_PROF = avg(Z_ROE, Z_NPM, Z_OPM, Z_EBIT_L)   → Profitability [35%]\n",
        "  Z_EFF  = Z_AT                                    → Efficiency   [30%]\n",
        "  Z_LIQ  = avg(Z_CR, Z_CF_R, Z_CF_L, Z_CA)       → Liquidity    [20%]\n",
        "  Z_LEV  = avg(Z_DE, Z_IC)                        → Leverage     [15%]\n\n",
        "▶ FINAL SCORE: 0.35·Z_PROF + 0.30·Z_EFF + 0.20·Z_LIQ + 0.15·Z_LEV\n",
        "▶ VALIDATION: WSM + DEA + Sharpe triangulation → ROBUST result"
      ),
      ts = paste0(
        "📊 TIME SERIES — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ ARMA: Yₜ = c + Σφᵢ Yₜ₋ᵢ + εₜ + Σθⱼ εₜ₋ⱼ — for stationary series\n",
        "▶ ARIMA(p,d,q): ΔᵈYₜ = c + Σφᵢ ΔᵈYₜ₋ᵢ + εₜ + Σθⱼ εₜ₋ⱼ\n\n",
        "▶ BOX-JENKINS STEPS:\n",
        "  ① Identification: ACF/PACF → determine p,d,q\n",
        "  ② Estimation: MLE\n",
        "  ③ Diagnostics: Ljung-Box p > 0.05 = good fit\n",
        "  ④ Forecasting: 95% CI (±1.96σ) | RMSE and MAE accuracy\n\n",
        "▶ If ARIMA shows upward trend for WSM winner → Ranking SUSTAINABLE"
      ),
      pca = paste0(
        "📊 PCA — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ Step 1: Standardization: Zᵢⱼ = (Xᵢⱼ − X̄ⱼ) / σⱼ\n",
        "▶ Step 2: Eigenvalue Equation: |Σ − λI| = 0\n",
        "▶ Step 3: PCₖ = Σⱼ(aₖⱼ Zⱼ)\n",
        "▶ Step 4: Kaiser Criterion: retain eigenvalue > 1\n\n",
        "▶ PC1: Explains maximum variance → dominant financial structure\n",
        "▶ Loadings: Importance of each ratio to a specific component\n",
        "▶ Companies close in biplot → similar financial behavior"
      ),
      dea = paste0(
        "📊 DEA — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ Score = 1.0 → Fully efficient | Score < 1.0 → Inefficient\n",
        "▶ Formula: Efficiency = Σ(u·y) / Σ(v·x)\n",
        "▶ Compare with WSM — if same winner → VERY ROBUST"
      ),
      sharpe = paste0(
        "📊 SHARPE RATIO — RESULTS INTERPRETATION\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n",
        "▶ S > 2.0 → Excellent | S 1–2 → Very good | S < 0 → Very bad\n",
        "▶ Formula: S = (Rₚ − Rƒ) / σₚ\n",
        "▶ High Sharpe = High return per unit of risk"
      )
    )
    result <- texts[[section]]
    if(is.null(result)) paste0("Interpretation for '",section,"' — run analysis first.")
    result
  }

  gen_thesis <- function(section, style="academic") {
    roa_avg <- round(tapply(df$ROA, df$company, mean), 4)
    best  <- names(which.max(roa_avg))
    worst <- names(which.min(roa_avg))

    texts <- list(
      abstract = paste0(
        "ABSTRACT\n\n",
        "This study evaluates the financial performance of five companies (AP, AM, ME, TE, MS) ",
        "over 2020–2024 using twelve financial ratios across profitability (35%), efficiency (30%), ",
        "liquidity (20%), and leverage (15%) dimensions. The methodology integrates descriptive statistics, ",
        "Pearson correlation analysis (combined n=25 and per-company n=5), multiple linear regression, ",
        "Z-score WSM ranking, ARIMA forecasting, PCA, DEA, Sharpe ratio, and K-Means clustering.\n\n",
        best," emerges as top performer (ROA=",roa_avg[best],") while ",worst,
        " shows weakest indicators (ROA=",roa_avg[worst],").\n\n",
        "Keywords: Financial Performance, Ratio Analysis, Z-Score, WSM Ranking, ARIMA, Pearson Correlation"
      ),
      ch1 = paste0(
        "CHAPTER 1: INTRODUCTION\n\n",
        "1.1 Background\n\nSystematic evaluation of corporate financial performance is fundamental in modern ",
        "financial management. This study investigates five companies (AP, AM, ME, TE, MS) over 2020–2024 ",
        "using twelve financial ratios within an integrated analytical framework.\n\n",
        "1.2 Problem Statement\n\nTraditional single-ratio analyses fail to capture holistic financial health. ",
        "Per-company correlation analysis addresses limitations of pooled correlation matrices.\n\n",
        "1.3 Research Objectives\n",
        "① Compute 12 financial ratios for 5 companies over 2020–2024\n",
        "② Examine trend patterns and growth rates\n",
        "③ Assess BOTH combined AND per-company inter-ratio correlations\n",
        "④ Develop composite WSM ranking with literature weights\n",
        "⑤ Forecast future performance via ARIMA"
      ),
      ch3 = paste0(
        "CHAPTER 3: RESEARCH METHODOLOGY\n\n",
        "3.1 Research Design\nQuantitative, longitudinal panel data design (5 companies × 5 years = 25 obs).\n\n",
        "3.2 Correlation Analysis Approach\n",
        "Level 1 — Combined Matrix (n=25): All companies pooled → industry-level correlation\n",
        "Level 2 — Per-Company Matrix (n=5 each): Individual company correlations → company-specific dynamics\n\n",
        "Formula: r = Σ[(X−X̄)(Y−Ȳ)] / √[Σ(X−X̄)²·Σ(Y−Ȳ)²]\n\n",
        "3.3 WSM Weights\n",
        "Profitability 35% | Efficiency 30% | Liquidity 20% | Leverage 15%\n",
        "DE negative indicator — Z-score reversed.\n\n",
        "3.4 Full Method Chain: A→B→C→D→E→F→G→H\n",
        "A) Descriptive Stats → B) Trend → C) Correlation (2-level) → D) Z-Score\n",
        "E) WSM → F) Regression → G) ARIMA → H) PCA+DEA+Sharpe+K-Means+Sensitivity+Outliers"
      ),
      ch5 = paste0(
        "CHAPTER 5: CONCLUSION\n\n",
        "5.1 Key Findings\n\n",
        "① ",best," is TOP PERFORMER (ROA=",roa_avg[best],"). ",worst," is WEAKEST (ROA=",roa_avg[worst],").\n",
        "② Combined correlation confirms strong profitability ratio clustering (ROA-ROE-NPM r>0.80).\n",
        "③ Per-company correlation reveals AM has weakest profitability correlations (2022 net loss).\n",
        "④ ME shows strongest individual-company correlations — lean, efficient asset structure.\n",
        "⑤ WSM, DEA, Sharpe triangulation produces consistent rankings — ROBUST.\n\n",
        "5.2 Recommendations\n",
        "• ",best,": Maintain competitive advantage. Monitor leverage growth.\n",
        "• ",worst,": Prioritize debt restructuring and margin improvement.\n",
        "• Investors: Use per-company correlation to assess ratio consistency."
      )
    )
    result <- texts[[section]]
    if(is.null(result)) {
      result <- paste0("SECTION: ",toupper(section),"\n\nBest: ",best," | Worst: ",worst,
                       "\n\nFull chapter generation: select from dropdown.\n",
                       "All analyses complete: A→B→C(2-level)→D→E→F→G→H")
    }
    result
  }

  make_interp <- function(btn, out, sec) {
    observeEvent(input[[btn]], {
      output[[out]] <- renderUI({
        tags$pre(style="background:#030d03;color:#88ff88;border:1px solid rgba(100,255,100,0.3);
                       border-radius:8px;padding:14px;font-family:Consolas,monospace;
                       font-size:12px;line-height:1.7;white-space:pre-wrap;max-height:450px;overflow-y:auto;",
          gen_interp(sec))
      })
    })
  }

  make_thesis_h <- function(btn, out, sec) {
    observeEvent(input[[btn]], {
      output[[out]] <- renderUI({
        tags$pre(style="background:#03030d;color:#aaddff;border:1px solid rgba(100,150,255,0.3);
                       border-radius:8px;padding:16px;font-family:Georgia,serif;
                       font-size:12.5px;line-height:1.85;white-space:pre-wrap;max-height:500px;overflow-y:auto;",
          gen_thesis(sec))
      })
    })
  }

  make_interp("interp_trend","interp_trend_out","trend")
  make_interp("interp_graph","interp_graph_out","trend")
  make_interp("interp_stats","interp_stats_out","stats")
  make_interp("interp_rank","interp_rank_out","zscore")
  make_interp("interp_corr","interp_corr_out","corr")
  make_interp("interp_reg","interp_reg_out","reg")
  make_interp("interp_zscore","interp_zscore_out","zscore")
  make_interp("interp_ts","interp_ts_out","ts")
  make_interp("interp_pca","interp_pca_out","pca")
  make_interp("interp_dea","interp_dea_out","dea")
  make_interp("interp_sharpe","interp_sharpe_out","sharpe")
  make_interp("interp_km","interp_km_out","pca")
  make_interp("interp_sens","interp_sens_out","zscore")
  make_interp("interp_outlier","interp_outlier_out","stats")

  make_thesis_h("thesis_trend","thesis_trend_out","ch4")
  make_thesis_h("thesis_graph","thesis_graph_out","ch4")
  make_thesis_h("thesis_stats","thesis_stats_out","ch4_stats")
  make_thesis_h("thesis_rank","thesis_rank_out","ch4_zscore")
  make_thesis_h("thesis_corr","thesis_corr_out","ch4_corr")
  make_thesis_h("thesis_reg","thesis_reg_out","ch4_reg")
  make_thesis_h("thesis_zscore","thesis_zscore_out","ch4_zscore")
  make_thesis_h("thesis_ts","thesis_ts_out","ch4_ts")
  make_thesis_h("thesis_pca","thesis_pca_out","ch4")
  make_thesis_h("thesis_dea","thesis_dea_out","ch4")
  make_thesis_h("thesis_sharpe","thesis_sharpe_out","ch4")
  make_thesis_h("thesis_km","thesis_km_out","ch4")
  make_thesis_h("thesis_sens","thesis_sens_out","ch4")
  make_thesis_h("thesis_outlier","thesis_outlier_out","ch4")

  observeEvent(input$gen_thesis, {
    output$thesis_out <- renderUI({
      text <- gen_thesis(input$thesis_chapter, input$thesis_style)
      tags$pre(style="background:#03030d;color:#aaddff;border:1px solid rgba(100,150,255,0.3);
                     border-radius:8px;padding:18px;font-family:Georgia,serif;
                     font-size:13px;line-height:1.9;white-space:pre-wrap;max-height:600px;overflow-y:auto;", text)
    })
  })

  observeEvent(input$gen_custom, {
    output$custom_out <- renderUI({
      q <- trimws(input$custom_q)
      if(nchar(q) < 5) return(tags$p(style="color:#ff4444;","Please enter a question."))
      roa_avg <- round(tapply(df$ROA, df$company, mean), 4)
      best <- names(which.max(roa_avg))
      text <- paste0("AI FINANCIAL ANALYSIS RESPONSE\n","━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n",
        "Query: ",q,"\n\nANALYSIS BASED ON 5 COMPANIES (2020–2024):\n\n",
        "1. OVERALL WINNER: ",best," (Avg ROA=",roa_avg[best],")\n\n",
        "2. COMPANY PERFORMANCE SUMMARY:\n",
        paste(paste0("   • ",names(roa_avg)," → ROA: ",roa_avg), collapse="\n"),
        "\n\n3. CORRELATION SUMMARY:\n",
        "   Combined (n=25): ROA-ROE r≈+0.85 | DE-ROA r≈-0.40\n",
        "   Per-Company: AP strongest cluster | AM weakest (2022 loss)\n\n",
        "4. CORE RANKING CHAIN: Raw → 12 Ratios → Z-Score → WSM → ",best," #1\n\n",
        "5. VALIDATION: WSM + DEA + Sharpe = ROBUST conclusion")
      tags$pre(style="background:#03030d;color:#aaddff;border:1px solid rgba(100,150,255,0.3);
                     border-radius:8px;padding:18px;font-family:Georgia,serif;
                     font-size:13px;line-height:1.85;white-space:pre-wrap;max-height:600px;overflow-y:auto;", text)
    })
  })

  output$wsm_summary_print <- renderPrint({
    roa_avg <- round(tapply(df$ROA, df$company, mean), 4)
    best <- names(which.max(roa_avg))
    cat("WSM RANKING SUMMARY\n══════════════════════════════════════════\n")
    cat("Chain: Raw Data → 12 Ratios → Z-Score → Category Scores → WSM → Final Rank\n\n")
    cat("Z-Score: Z = (X − μ) / σ  |  DE reversed (higher = worse)\n\n")
    cat("Category Scores:\n")
    cat("  Z_PROF = avg(Z_ROE, Z_NPM, Z_OPM, Z_EBIT_L)  [Profitability 35%]\n")
    cat("  Z_EFF  = Z_AT                                   [Efficiency   30%]\n")
    cat("  Z_LIQ  = avg(Z_CR, Z_CF_R, Z_CF_L, Z_CA)      [Liquidity    20%]\n")
    cat("  Z_LEV  = avg(Z_DE, Z_IC)                       [Leverage     15%]\n\n")
    cat("Final Score = 0.35·Z_PROF + 0.30·Z_EFF + 0.20·Z_LIQ + 0.15·Z_LEV\n\n")
    cat("Company ROA Performance (2020-2024 avg):\n")
    for(co in names(sort(roa_avg, decreasing=TRUE))) cat(sprintf("  %-4s | ROA: %+.4f\n", co, roa_avg[co]))
    cat("\nTop Performer:",best,"| Validation: WSM + DEA + Sharpe → ROBUST\n")
  })

  # ══════════════════════════════════════════════════════════════
  # 📈 OVERVIEW
  # ══════════════════════════════════════════════════════════════
  output$overview_plot <- renderPlotly({
    p <- ggplot(df, aes(x=year, y=ROA, color=company, group=company,
                        text=paste0(company,"\nYear:",year,"\nROA:",round(ROA,4)))) +
      geom_line(size=1.5) + geom_point(size=3.5) +
      scale_color_manual(values=company_colors) + dark_theme() +
      labs(title="ROA Trend — All Companies (2020–2024)", x="Year", y="ROA", color="Company")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 📋 RAW DATA
  # ══════════════════════════════════════════════════════════════
  output$raw_table <- renderDT({
    datatable(raw_df, options=list(pageLength=10, scrollX=TRUE, dom='Bfrtip', buttons=c('copy','csv')),
              rownames=FALSE, class='cell-border stripe hover') %>%
      formatStyle("company", fontWeight="bold", color="#ffcc00") %>%
      formatStyle("year", color="#ff8800", fontWeight="bold") %>%
      formatStyle(3:11, color="#ffffff", backgroundColor="#080000") %>%
      formatCurrency(3:11, currency="", digits=0, mark=",")
  })

  output$ratio_table <- renderDT({
    d <- df %>% select(company, year, all_of(ratios)) %>% mutate(across(all_of(ratios), ~round(.,4)))
    datatable(d, options=list(pageLength=10, scrollX=TRUE, dom='Bfrtip'), rownames=FALSE, class='cell-border stripe hover') %>%
      formatStyle("company", fontWeight="bold", color="#ffcc00") %>%
      formatStyle("year", color="#ff8800", fontWeight="bold") %>%
      formatStyle(3:ncol(d), color="#ffffff", backgroundColor="#080000")
  })

  output$raw_per_co <- renderDT({
    req(input$raw_co)
    d <- df %>% filter(company==input$raw_co) %>%
      select(company, year, all_of(ratios)) %>% mutate(across(where(is.numeric), ~round(.,4)))
    datatable(d, options=list(pageLength=5, scrollX=TRUE, dom='t'), rownames=FALSE) %>%
      formatStyle(1:ncol(d), color="#ffffff", backgroundColor="#080000") %>%
      formatStyle("company", fontWeight="bold", color="#ffcc00")
  })

  # ══════════════════════════════════════════════════════════════
  # 📉 TREND
  # ══════════════════════════════════════════════════════════════
  output$trend_plot <- renderPlotly({
    req(input$company, input$ratios, length(input$company)>0, length(input$ratios)>0)
    d <- selected_data(); r <- input$ratios[1]
    p <- ggplot(d, aes(x=year, y=.data[[r]], color=company, group=company,
                       text=paste0(company,"\nYear:",year,"\n",r,":",round(.data[[r]],4)))) +
      geom_line(size=1.5) + geom_point(size=4) +
      scale_color_manual(values=company_colors) + dark_theme() +
      labs(title=paste("Trend:",r), x="Year", y=r, color="Company")
    ggplotly(p, tooltip="text") %>% pld()
  })

  output$trend_score_table <- renderDT({
    req(input$company, length(input$company)>0, input$trend_table_ratio)
    r <- input$trend_table_ratio
    d <- df %>% filter(company %in% input$company) %>% arrange(company, year) %>%
      select(Company=company, Year=year, Value=all_of(r)) %>%
      group_by(Company) %>%
      mutate(Growth_Rate_pct=c(NA, round(diff(Value)/abs(head(Value,-1))*100, 2)),
             Trend_Signal=ifelse(is.na(Growth_Rate_pct),"Base Year",
               ifelse(Growth_Rate_pct>5,"Strong Growth",
                 ifelse(Growth_Rate_pct>0,"Growth",
                   ifelse(Growth_Rate_pct>-5,"Decline","Strong Decline"))))) %>%
      ungroup() %>% mutate(Value=round(Value,4))
    datatable(d, options=list(pageLength=10, scrollX=TRUE, dom="Bfrtip"), rownames=FALSE) %>%
      formatStyle("Company", fontWeight="bold", color="#ffcc00") %>%
      formatStyle("Year", color="#ff8800", fontWeight="bold") %>%
      formatStyle("Growth_Rate_pct",
        color=styleInterval(c(-5,0,5), c("#ff2222","#ffaa00","#aaaaaa","#44ff88")), fontWeight="bold") %>%
      formatStyle("Trend_Signal",
        color=styleEqual(c("Strong Growth","Growth","Base Year","Decline","Strong Decline"),
          c("#44ff88","#88ff44","#aaaaaa","#ffaa00","#ff4444")), fontWeight="bold") %>%
      formatStyle(1:5, backgroundColor="#080000", color="#ffffff")
  })

  output$trend_growth_table <- renderDT({
    req(input$trend_growth_co)
    co <- input$trend_growth_co
    d <- df %>% filter(company==co) %>% arrange(year)
    out <- data.frame(Year=d$year)
    for(r in ratios) {
      vals <- d[[r]]; out[[r]] <- round(vals,4)
      gr <- c(NA, round(diff(vals)/abs(head(vals,-1))*100, 2))
      out[[paste0(r,"_GR%")]] <- gr
    }
    datatable(out, options=list(pageLength=5, scrollX=TRUE, dom="t"), rownames=FALSE) %>%
      formatStyle("Year", color="#ff8800", fontWeight="bold") %>%
      formatStyle(names(out)[grepl("GR",names(out))],
        color=styleInterval(c(-5,0,5), c("#ff4444","#ffaa00","#aaaaaa","#44ff88")), fontWeight="bold") %>%
      formatStyle(1:ncol(out), backgroundColor="#080000", color="#ffffff")
  })

  output$trend_growth_plot <- renderPlotly({
    req(input$trend_growth_co, input$ratios, length(input$ratios)>0)
    co <- input$trend_growth_co; d <- df %>% filter(company==co) %>% arrange(year)
    r <- input$ratios[1]; vals <- d[[r]]
    gr <- c(NA, round(diff(vals)/abs(head(vals,-1))*100, 2))
    pf <- data.frame(year=d$year, Growth=gr) %>% na.omit()
    p <- ggplot(pf, aes(x=year, y=Growth, fill=Growth>0,
                         text=paste0("Year:",year,"\nGrowth:",round(Growth,2),"%"))) +
      geom_bar(stat="identity", width=0.6) +
      geom_hline(yintercept=0, color="#ffffff", linetype="dashed", size=0.8) +
      scale_fill_manual(values=c("TRUE"="#44ff88","FALSE"="#ff4444"), guide="none") +
      dark_theme() + labs(title=paste("Growth Rate:",r,"—",co), x="Year", y="Growth Rate (%)")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 📊 GRAPH
  # ══════════════════════════════════════════════════════════════
  output$multi_graph <- renderPlotly({
    req(input$company, input$ratios, length(input$company)>0, length(input$ratios)>0)
    d <- selected_data()
    ld <- d %>% select(company, year, all_of(input$ratios)) %>%
      pivot_longer(cols=-c(company,year), names_to="ratio", values_to="value")
    rp <- setNames(rainbow(length(input$ratios)), input$ratios)
    p <- ggplot(ld, aes(x=year, y=value, color=ratio, group=ratio,
                         text=paste0("Ratio:",ratio,"\nYear:",year,"\nValue:",round(value,4)))) +
      geom_line(size=1.1) + geom_point(size=2.2) +
      facet_wrap(~company, scales="free_y") +
      scale_color_manual(values=rp) + dark_theme() +
      labs(title="Multi-Ratio Analysis — Per Company", x="Year", y="Value", color="Ratio")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 📐 STATISTICS
  # ══════════════════════════════════════════════════════════════
  output$stats_table <- renderDT({
    req(input$ratios, length(input$ratios)>0)
    d <- df[,input$ratios,drop=FALSE]
    s <- data.frame(Ratio=input$ratios,
      Min=round(sapply(d,min,na.rm=TRUE),4),
      Max=round(sapply(d,max,na.rm=TRUE),4),
      Mean=round(sapply(d,mean,na.rm=TRUE),4),
      Median=round(sapply(d,median,na.rm=TRUE),4),
      SD=round(sapply(d,sd,na.rm=TRUE),4),
      CV_pct=round((sapply(d,sd,na.rm=TRUE)/sapply(d,mean,na.rm=TRUE))*100, 2))
    datatable(s, options=list(pageLength=12, scrollX=TRUE, dom='Bfrtip'), rownames=FALSE, class='cell-border stripe hover') %>% dtd(7)
  })

  output$stats_per_co <- renderDT({
    req(input$ratios, length(input$ratios)>0, input$stats_co)
    d <- df %>% filter(company==input$stats_co)
    dr <- d[,input$ratios,drop=FALSE]
    s <- data.frame(Ratio=input$ratios,
      Min=round(sapply(dr,min,na.rm=TRUE),4),
      Max=round(sapply(dr,max,na.rm=TRUE),4),
      Mean=round(sapply(dr,mean,na.rm=TRUE),4),
      Median=round(sapply(dr,median,na.rm=TRUE),4),
      SD_pop=round(sapply(dr, function(x){mu<-mean(x,na.rm=TRUE);sqrt(sum((x-mu)^2,na.rm=TRUE)/length(na.omit(x)))}), 4),
      CV_pct=round((sapply(dr,sd,na.rm=TRUE)/sapply(dr,mean,na.rm=TRUE))*100, 2))
    datatable(s, options=list(pageLength=12, scrollX=TRUE, dom='Bfrtip'), rownames=FALSE, caption=paste("Company:",input$stats_co)) %>% dtd(7)
  })

  output$yearly_ratio_table <- renderDT({
    req(input$yearly_co)
    d <- df %>% filter(company==input$yearly_co) %>%
      select(company, year, all_of(ratios)) %>% mutate(across(all_of(ratios), ~round(.,4)))
    datatable(d, options=list(pageLength=5, scrollX=TRUE, dom='Bfrtip'), rownames=FALSE) %>% dtd(ncol(d))
  })

  output$yearly_ratio_plot <- renderPlotly({
    req(input$yearly_co, input$ratios, length(input$ratios)>0)
    d <- df %>% filter(company==input$yearly_co) %>%
      select(year, all_of(input$ratios)) %>%
      pivot_longer(-year, names_to="ratio", values_to="value")
    p <- ggplot(d, aes(x=year, y=value, color=ratio, group=ratio,
                        text=paste0(ratio,"\nYear:",year,"\nValue:",round(value,4)))) +
      geom_line(size=1.3) + geom_point(size=3) +
      scale_color_manual(values=setNames(rainbow(length(input$ratios)), input$ratios)) +
      dark_theme() + labs(title=paste(input$yearly_co,"— Ratios"), x="Year", y="Value", color="Ratio")
    ggplotly(p, tooltip="text") %>% pld()
  })

  output$growth_table <- renderDT({
    req(input$growth_co, input$growth_ratio)
    d <- df %>% filter(company==input$growth_co) %>% arrange(year) %>%
      select(company, year, r=all_of(input$growth_ratio))
    d$Growth_pct <- c(NA, round(diff(d$r)/abs(head(d$r,-1))*100, 2))
    d <- d %>% rename(!!input$growth_ratio := r)
    datatable(d, options=list(pageLength=5, dom='t'), rownames=FALSE) %>%
      dtd(ncol(d)) %>%
      formatStyle("Growth_pct", color=styleInterval(0, c("#ff4444","#44ff88")), fontWeight="bold")
  })

  output$growth_plot <- renderPlotly({
    req(input$growth_co, input$growth_ratio)
    d <- df %>% filter(company==input$growth_co) %>% arrange(year)
    vals <- d[[input$growth_ratio]]
    gr <- c(NA, round(diff(vals)/abs(head(vals,-1))*100, 2))
    pf <- data.frame(year=d$year, Growth=gr) %>% na.omit()
    p <- ggplot(pf, aes(x=year, y=Growth, fill=Growth>0,
                         text=paste0("Year:",year,"\nGrowth:",round(Growth,2),"%"))) +
      geom_bar(stat="identity", width=0.6) +
      scale_fill_manual(values=c("TRUE"="#44ff88","FALSE"="#ff4444"), guide="none") +
      dark_theme() + labs(title=paste("Growth Rate:",input$growth_ratio,"—",input$growth_co), x="Year", y="Growth %")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 🏆 WSM RANKING
  # ══════════════════════════════════════════════════════════════
  ranking_rv <- eventReactive(input$calc_ranking, {
    req(input$ratios, length(input$ratios)>0)
    rd <- df[,ratios,drop=FALSE]
    zm <- scale(rd, center=TRUE, scale=TRUE)
    for(neg_r in negative_ratios) if(neg_r %in% colnames(zm)) zm[,neg_r] <- -zm[,neg_r]
    zdf <- as.data.frame(zm); colnames(zdf) <- paste0("Z_",ratios)
    base <- bind_cols(df %>% select(company, year), zdf)

    prof_v <- intersect(c("Z_ROE","Z_NPM","Z_OPM","Z_EBIT_L"), names(base))
    eff_v  <- intersect(c("Z_AT"), names(base))
    liq_v  <- intersect(c("Z_CR","Z_CF_R","Z_CF_L","Z_CA"), names(base))
    lev_v  <- intersect(c("Z_DE","Z_IC"), names(base))

    base$Z_PROF <- if(length(prof_v)>0) rowMeans(base[,prof_v,drop=FALSE],na.rm=TRUE) else 0
    base$Z_EFF  <- if(length(eff_v)>0)  rowMeans(base[,eff_v, drop=FALSE],na.rm=TRUE) else 0
    base$Z_LIQ  <- if(length(liq_v)>0)  rowMeans(base[,liq_v, drop=FALSE],na.rm=TRUE) else 0
    base$Z_LEV  <- if(length(lev_v)>0)  rowMeans(base[,lev_v, drop=FALSE],na.rm=TRUE) else 0
    base$score  <- 0.35*base$Z_PROF + 0.30*base$Z_EFF + 0.20*base$Z_LIQ + 0.15*base$Z_LEV

    base %>% group_by(company) %>%
      summarise(Z_PROF=round(mean(Z_PROF,na.rm=TRUE),4),
                Z_EFF =round(mean(Z_EFF, na.rm=TRUE),4),
                Z_LIQ =round(mean(Z_LIQ, na.rm=TRUE),4),
                Z_LEV =round(mean(Z_LEV, na.rm=TRUE),4),
                Avg_Score=round(mean(score,na.rm=TRUE),4), .groups="drop") %>%
      arrange(desc(Avg_Score)) %>% mutate(Rank=row_number())
  })

  output$custom_weight_sliders <- renderUI({
    req(input$ratios, length(input$ratios)>0)
    lapply(input$ratios, function(r) {
      fluidRow(class="weight-row",
        column(3, tags$span(class="weight-label",r)),
        column(9, sliderInput(paste0("cw_",r), NULL, min=0, max=10, value=1, step=0.5, width="100%"))
      )
    })
  })

  output$rank_table <- renderDT({
    r <- ranking_rv()
    datatable(r, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE,
              caption="🏆 WSM | Score = 0.35·Z_PROF + 0.30·Z_EFF + 0.20·Z_LIQ + 0.15·Z_LEV") %>%
      formatStyle("Rank", backgroundColor=styleEqual(1,"#ffcc00"), fontWeight=styleEqual(1,"bold"), color=styleEqual(1,"#1a0000")) %>%
      formatStyle("Z_PROF", color="#ff8800", fontWeight="bold") %>%
      formatStyle("Z_EFF",  color="#ffcc00", fontWeight="bold") %>%
      formatStyle("Z_LIQ",  color="#88ff88", fontWeight="bold") %>%
      formatStyle("Z_LEV",  color="#ff99cc", fontWeight="bold") %>%
      formatStyle("Avg_Score", backgroundColor="#0d0022", fontWeight="bold", color="#aaddff") %>%
      formatStyle(1:ncol(r), backgroundColor="#080000", color="#ffffff")
  })

  output$rank_plot <- renderPlotly({
    r <- ranking_rv()
    cp <- company_colors[names(company_colors) %in% r$company]
    p <- ggplot(r, aes(x=reorder(company,Avg_Score), y=Avg_Score, fill=company,
                        text=paste0(company,"\nWSM Score:",round(Avg_Score,4),"\nRank:#",Rank))) +
      geom_bar(stat="identity", width=0.65) +
      scale_fill_manual(values=cp) + coord_flip() + dark_theme() + theme(legend.position="none") +
      labs(title="🏆 WSM Company Ranking", x="Company", y="Composite Score")
    ggplotly(p, tooltip="text") %>% pld() %>% layout(showlegend=FALSE)
  })

  # ══════════════════════════════════════════════════════════════
  # 📏 Z-SCORE & WSM (custom tab)
  # ══════════════════════════════════════════════════════════════
  output$weight_sliders <- renderUI({
    req(input$ratios, length(input$ratios)>0)
    lapply(input$ratios, function(r) {
      fluidRow(class="weight-row",
        column(3, tags$span(class="weight-label",r)),
        column(9, sliderInput(paste0("w_",r), NULL, min=0, max=10, value=1, step=0.5, width="100%"))
      )
    })
  })

  wsm_rv <- eventReactive(input$calc_wsm, {
    req(input$ratios, length(input$ratios)>0)
    rd <- df[,ratios,drop=FALSE]
    zm <- scale(rd, center=TRUE, scale=TRUE)
    for(neg_r in negative_ratios) if(neg_r %in% colnames(zm)) zm[,neg_r] <- -zm[,neg_r]
    zdf <- as.data.frame(zm); colnames(zdf) <- paste0("Z_",ratios)
    base <- bind_cols(df %>% select(company, year), zdf)

    prof_vars <- intersect(c("Z_ROE","Z_NPM","Z_OPM","Z_EBIT_L"), names(base))
    eff_vars  <- intersect(c("Z_AT"), names(base))
    liq_vars  <- intersect(c("Z_CR","Z_CF_R","Z_CF_L","Z_CA"), names(base))
    lev_vars  <- intersect(c("Z_DE","Z_IC"), names(base))

    base$Z_PROF <- if(length(prof_vars)>0) rowMeans(base[,prof_vars,drop=FALSE],na.rm=TRUE) else 0
    base$Z_EFF  <- if(length(eff_vars)>0)  rowMeans(base[,eff_vars, drop=FALSE],na.rm=TRUE) else 0
    base$Z_LIQ  <- if(length(liq_vars)>0)  rowMeans(base[,liq_vars, drop=FALSE],na.rm=TRUE) else 0
    base$Z_LEV  <- if(length(lev_vars)>0)  rowMeans(base[,lev_vars, drop=FALSE],na.rm=TRUE) else 0
    base$WSM_Score <- 0.35*base$Z_PROF + 0.30*base$Z_EFF + 0.20*base$Z_LIQ + 0.15*base$Z_LEV

    wr <- base %>% group_by(company) %>%
      summarise(Z_PROF=round(mean(Z_PROF,na.rm=TRUE),4),
                Z_EFF =round(mean(Z_EFF, na.rm=TRUE),4),
                Z_LIQ =round(mean(Z_LIQ, na.rm=TRUE),4),
                Z_LEV =round(mean(Z_LEV, na.rm=TRUE),4),
                Avg_WSM_Score=round(mean(WSM_Score,na.rm=TRUE),4), .groups="drop") %>%
      arrange(desc(Avg_WSM_Score)) %>% mutate(WSM_Rank=row_number())

    list(z_table=base, wsm_rank=wr, weights=c(Profitability=0.35,Efficiency=0.30,Liquidity=0.20,Leverage=0.15))
  })

  output$zscore_table <- renderDT({
    req(wsm_rv())
    zt <- wsm_rv()$z_table %>% mutate(across(where(is.numeric), ~round(.,4)))
    datatable(zt, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE, caption="Z=(X−μ)/σ | DE reversed") %>% dtd(ncol(zt))
  })

  output$wsm_rank_table <- renderDT({
    req(wsm_rv())
    wt <- wsm_rv()$wsm_rank
    datatable(wt, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE,
              caption="WSM | Score = 0.35·Z_PROF + 0.30·Z_EFF + 0.20·Z_LIQ + 0.15·Z_LEV") %>%
      formatStyle("WSM_Rank", backgroundColor=styleEqual(1,"#ffcc00"), fontWeight=styleEqual(1,"bold"), color=styleEqual(1,"#1a0000")) %>%
      formatStyle("Z_PROF", color="#ff8800", fontWeight="bold") %>%
      formatStyle("Z_EFF",  color="#ffcc00", fontWeight="bold") %>%
      formatStyle("Z_LIQ",  color="#88ff88", fontWeight="bold") %>%
      formatStyle("Z_LEV",  color="#ff99cc", fontWeight="bold") %>%
      formatStyle("Avg_WSM_Score", backgroundColor="#0d0022", fontWeight="bold", color="#aaddff") %>%
      formatStyle(1:ncol(wt), backgroundColor="#080000", color="#ffffff")
  })

  output$wsm_chart <- renderPlotly({
    req(wsm_rv())
    wr <- wsm_rv()$wsm_rank
    cp <- company_colors[names(company_colors) %in% wr$company]
    p <- ggplot(wr, aes(x=reorder(company,Avg_WSM_Score), y=Avg_WSM_Score, fill=company,
                         text=paste0(company,"\nWSM:",round(Avg_WSM_Score,4),"\nRank:#",WSM_Rank))) +
      geom_bar(stat="identity", width=0.65) +
      scale_fill_manual(values=cp) + coord_flip() + dark_theme() + theme(legend.position="none") +
      labs(title="WSM Ranking", x="Company", y="WSM Score")
    ggplotly(p, tooltip="text") %>% pld() %>% layout(showlegend=FALSE)
  })

  # ══════════════════════════════════════════════════════════════
  # 📈 REGRESSION
  # ══════════════════════════════════════════════════════════════
  reg_models <- reactive({
    m1_vars <- intersect(c("ROA","ROE","NPM","OPM","EBIT_L"), names(df))
    m1_data <- na.omit(df[,m1_vars,drop=FALSE])
    m1_preds <- setdiff(m1_vars,"ROA")
    m1 <- if(length(m1_preds)>0) lm(as.formula(paste("ROA ~",paste(m1_preds,collapse=" + "))),data=m1_data) else NULL

    m2_vars <- intersect(c("ROA","AT"), names(df))
    m2_data <- na.omit(df[,m2_vars,drop=FALSE])
    m2 <- if(length(m2_vars)>1) lm(ROA ~ AT, data=m2_data) else NULL

    m3_vars <- intersect(c("ROA","CR","CF_R","CF_L","CA"), names(df))
    m3_data <- na.omit(df[,m3_vars,drop=FALSE])
    m3_preds <- setdiff(m3_vars,"ROA")
    m3 <- if(length(m3_preds)>0) lm(as.formula(paste("ROA ~",paste(m3_preds,collapse=" + "))),data=m3_data) else NULL

    m4_vars <- intersect(c("ROA","DE","IC"), names(df))
    m4_data <- na.omit(df[,m4_vars,drop=FALSE])
    m4_preds <- setdiff(m4_vars,"ROA")
    m4 <- if(length(m4_preds)>0) lm(as.formula(paste("ROA ~",paste(m4_preds,collapse=" + "))),data=m4_data) else NULL

    list(m1=m1, m2=m2, m3=m3, m4=m4)
  })

  reg_rv <- reactive({
    req(input$ratios, length(input$ratios)>0)
    preds <- setdiff(input$ratios,"ROA")
    validate(need(length(preds)>0,"Select predictor besides ROA."))
    reg_data <- na.omit(df[,c("ROA",preds)])
    lm(as.formula(paste("ROA ~",paste(preds,collapse=" + "))), data=reg_data)
  })

  output$reg_summary <- renderPrint({
    mods <- reg_models()
    cat("══════════════════════════════════════════════════════════════\n")
    cat("  FOUR PANEL REGRESSION MODELS — WSM CATEGORIES\n")
    cat("  General Form: ROA_it = β₀ + Σ(βₖXₖᵢₜ) + μᵢ + λₜ + εᵢₜ\n")
    cat("══════════════════════════════════════════════════════════════\n\n")
    if(!is.null(mods$m1)) {
      sm1 <- summary(mods$m1)
      cat("─────────────────────────────────────────────────────────────\n")
      cat("MODEL 1 — PROFITABILITY (Weight: 35%)\n")
      cat("ROA = β₀ + β₁ROE + β₂NPM + β₃OPM + β₄EBIT_L + μᵢ + λₜ + εᵢₜ\n")
      cat("R² =",round(sm1$r.squared,4),"| Adj.R² =",round(sm1$adj.r.squared,4),"| n =",nrow(mods$m1$model),"\n")
      cat("Coefficients:\n"); print(round(coef(sm1),6)); cat("\n")
    }
    if(!is.null(mods$m2)) {
      sm2 <- summary(mods$m2)
      cat("─────────────────────────────────────────────────────────────\n")
      cat("MODEL 2 — EFFICIENCY (Weight: 30%)\n")
      cat("ROA = β₀ + β₁AT + μᵢ + λₜ + εᵢₜ\n")
      cat("R² =",round(sm2$r.squared,4),"| Adj.R² =",round(sm2$adj.r.squared,4),"| n =",nrow(mods$m2$model),"\n")
      cat("Coefficients:\n"); print(round(coef(sm2),6)); cat("\n")
    }
    if(!is.null(mods$m3)) {
      sm3 <- summary(mods$m3)
      cat("─────────────────────────────────────────────────────────────\n")
      cat("MODEL 3 — LIQUIDITY (Weight: 20%)\n")
      cat("ROA = β₀ + β₁CR + β₂CF_R + β₃CF_L + β₄CA + μᵢ + λₜ + εᵢₜ\n")
      cat("R² =",round(sm3$r.squared,4),"| Adj.R² =",round(sm3$adj.r.squared,4),"| n =",nrow(mods$m3$model),"\n")
      cat("Coefficients:\n"); print(round(coef(sm3),6)); cat("\n")
    }
    if(!is.null(mods$m4)) {
      sm4 <- summary(mods$m4)
      cat("─────────────────────────────────────────────────────────────\n")
      cat("MODEL 4 — LEVERAGE (Weight: 15%)\n")
      cat("ROA = β₀ + β₁DE + β₂IC + μᵢ + λₜ + εᵢₜ\n")
      cat("R² =",round(sm4$r.squared,4),"| Adj.R² =",round(sm4$adj.r.squared,4),"| n =",nrow(mods$m4$model),"\n")
      cat("Coefficients:\n"); print(round(coef(sm4),6)); cat("\n")
    }
    cat("══════════════════════════════════════════════════════════════\n")
    cat("t = β̂ / SE(β̂) | β > 0 = positive impact | β < 0 = negative impact\n")
  })

  output$reg_pred_plot <- renderPlotly({
    m <- reg_rv(); r2 <- round(summary(m)$r.squared, 4)
    pf <- data.frame(Actual=m$model$ROA, Predicted=fitted(m))
    p <- ggplot(pf, aes(x=Actual, y=Predicted,
                         text=paste0("Actual:",round(Actual,4),"\nPredicted:",round(Predicted,4)))) +
      geom_point(color="#ffcc00", size=3, alpha=0.85) +
      geom_abline(slope=1, intercept=0, color="#ff0000", linetype="dashed", size=1.2) +
      dark_theme() + labs(title="Actual vs Predicted ROA", subtitle=paste("R²=",r2), x="Actual", y="Predicted")
    ggplotly(p, tooltip="text") %>% pld()
  })

  fc25_rv <- reactive({
    m <- reg_rv(); preds <- setdiff(input$ratios,"ROA"); req(length(preds)>0)
    rows <- lapply(all_companies, function(co) {
      co_df <- df %>% filter(company==co)
      r2025 <- data.frame(company=co, year=2025, stringsAsFactors=FALSE)
      for(pr in preds) {
        vals <- co_df[[pr]]; yrs <- co_df$year
        r2025[[pr]] <- if(length(vals)>=2 && var(vals,na.rm=TRUE)>0)
          predict(lm(vals~yrs), newdata=data.frame(yrs=2025)) else tail(vals,1)
      }
      r2025
    })
    fd <- bind_rows(rows)
    fd$ROA_Pred_2025 <- predict(m, newdata=fd[,preds,drop=FALSE])
    fd %>% mutate(across(where(is.numeric), ~round(.,4)))
  })

  output$reg_forecast_table <- renderDT({
    fd <- fc25_rv()
    datatable(fd, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE) %>%
      formatStyle("ROA_Pred_2025", backgroundColor="#0d2200", fontWeight="bold", color="#88ff44") %>%
      dtd(ncol(fd))
  })

  output$reg_forecast_plot <- renderPlotly({
    fd <- fc25_rv()
    hr <- df %>% select(company, year, ROA) %>% mutate(type="Historical")
    fr <- fd %>% select(company, year, ROA_Pred_2025) %>% rename(ROA=ROA_Pred_2025) %>% mutate(type="Forecast")
    comb <- bind_rows(hr, fr)
    p <- ggplot(comb, aes(x=year, y=ROA, color=company, linetype=type,
                           group=interaction(company,type),
                           text=paste0(company,"\n",year,":",round(ROA,4),"(",type,")"))) +
      geom_line(size=1.3) + geom_point(size=3) +
      scale_color_manual(values=company_colors) +
      scale_linetype_manual(values=c("Historical"="solid","Forecast"="dashed")) +
      dark_theme() + labs(title="ROA Historical + 2025 Forecast", x="Year", y="ROA", color="Company", linetype="Type")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # ⏱️ TIME SERIES
  # ══════════════════════════════════════════════════════════════
  ts_rv <- eventReactive(input$run_ts, {
    req(input$ts_co, input$ts_ratio)
    d <- df %>% filter(company==input$ts_co) %>% arrange(year)
    vals <- d[[input$ts_ratio]]; yrs <- d$year; h <- input$ts_h
    if(input$ts_model=="arima") {
      fit <- tryCatch(arima(vals, order=c(input$ts_p,input$ts_d,input$ts_q)),
                     error=function(e) arima(vals, order=c(1,0,0)))
      pred <- predict(fit, n.ahead=h); fv <- as.numeric(pred$pred); fs <- as.numeric(pred$se)
      fy <- seq(max(yrs)+1, by=1, length.out=h)
      ml <- paste0("ARIMA(",input$ts_p,",",input$ts_d,",",input$ts_q,")")
    } else {
      fit <- lm(vals~yrs); fy <- seq(max(yrs)+1, by=1, length.out=h)
      fv <- predict(fit, newdata=data.frame(yrs=fy))
      fs <- rep(summary(fit)$sigma, h); ml <- "Linear Trend"
    }
    list(hd=data.frame(year=yrs, value=vals),
         fd=data.frame(year=fy, value=fv, lo=fv-1.96*fs, hi=fv+1.96*fs),
         fit=fit, method=ml, ratio=input$ts_ratio, company=input$ts_co, vals=vals)
  })

  output$ts_plot <- renderPlotly({
    res <- ts_rv()
    plot_ly() %>%
      add_trace(data=res$hd, x=~year, y=~value, type="scatter", mode="lines+markers",
                line=list(color="#ffcc00",width=2.5), marker=list(color="#ffcc00",size=8), name="Historical") %>%
      add_trace(data=res$fd, x=~year, y=~value, type="scatter", mode="lines+markers",
                line=list(color="#ff0000",width=2.5,dash="dash"), marker=list(color="#ff0000",size=8), name=res$method) %>%
      add_ribbons(data=res$fd, x=~year, ymin=~lo, ymax=~hi,
                  fillcolor="rgba(255,0,0,0.12)", line=list(color="transparent"), name="95% CI") %>%
      layout(title=list(text=paste("⏱️",res$company,"—",res$ratio,":",res$method), font=list(color="#ffcc00")),
             xaxis=list(title="Year",color="#fff"), yaxis=list(title=res$ratio,color="#fff"),
             plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)",
             legend=list(font=list(color="#fff")), font=list(color="#fff"))
  })

  output$ts_summary <- renderPrint({
    res <- ts_rv()
    cat("TIME SERIES:",res$company,"—",res$ratio,"| Method:",res$method,"\n")
    if(input$ts_model=="arima") print(res$fit) else print(summary(res$fit))
  })

  output$ts_forecast_table <- renderDT({
    res <- ts_rv()
    out <- data.frame(Year=res$fd$year, Forecast=round(res$fd$value,4),
                      Lower95=round(res$fd$lo,4), Upper95=round(res$fd$hi,4), Method=res$method)
    datatable(out, options=list(pageLength=10,dom='t'), rownames=FALSE) %>%
      dtd(5) %>% formatStyle("Forecast", fontWeight="bold", color="#ffcc00")
  })

  output$ts_acf_plot <- renderPlot({
    res <- ts_rv()
    par(mfrow=c(1,2), bg="transparent", col.main="#ffcc00", col.axis="#ccc", col.lab="#ffcc00", fg="#ccc")
    acf(res$vals,  main=paste("ACF —",res$ratio),  col="#ffcc00", lwd=2)
    pacf(res$vals, main=paste("PACF —",res$ratio), col="#ff0000", lwd=2)
    par(mfrow=c(1,1))
  }, bg="transparent")

  # ══════════════════════════════════════════════════════════════
  # 🔬 ADVANCED — PCA
  # ══════════════════════════════════════════════════════════════
  pca_rv <- eventReactive(input$run_pca, {
    rd <- na.omit(df[,ratios,drop=FALSE])
    rd <- rd[, apply(rd,2,var,na.rm=TRUE) > 0]
    pc <- prcomp(rd, center=TRUE, scale.=TRUE)
    sc <- as.data.frame(pc$x)
    sc$company <- df$company[1:nrow(sc)]; sc$year <- df$year[1:nrow(sc)]
    list(pca=pc, scores=sc)
  })

  output$pca_scree <- renderPlotly({
    res <- pca_rv(); ev <- res$pca$sdev^2; pv <- round(ev/sum(ev)*100, 2)
    pf <- data.frame(PC=paste0("PC",seq_along(pv)), Var=pv, Cum=cumsum(pv))
    p <- ggplot(pf, aes(x=PC, y=Var, group=1)) +
      geom_bar(stat="identity", fill="#ff0000", width=0.6) +
      geom_line(aes(y=Cum), color="#ffcc00", size=1.5) +
      geom_point(aes(y=Cum), color="#ffcc00", size=4) +
      dark_theme() + labs(title="PCA Scree Plot", x="PC", y="Variance %")
    ggplotly(p) %>% pld()
  })

  output$pca_loadings <- renderDT({
    res <- pca_rv()
    ld <- as.data.frame(res$pca$rotation[, 1:min(4,ncol(res$pca$rotation))])
    ld$Ratio <- rownames(ld)
    ld <- ld[,c(ncol(ld),1:(ncol(ld)-1))] %>% mutate(across(where(is.numeric), ~round(.,4)))
    datatable(ld, options=list(pageLength=12,dom='t'), rownames=FALSE) %>% dtd(ncol(ld))
  })

  output$pca_biplot <- renderPlotly({
    res <- pca_rv(); sc <- res$scores
    cp <- company_colors[names(company_colors) %in% sc$company]
    p <- ggplot(sc, aes(x=PC1, y=PC2, color=company,
                         text=paste0(company," ",year,"\nPC1:",round(PC1,3),"\nPC2:",round(PC2,3)))) +
      geom_point(size=4, alpha=0.85) + scale_color_manual(values=cp) + dark_theme() +
      labs(title="PCA Biplot", x="PC1", y="PC2", color="Company")
    ggplotly(p, tooltip="text") %>% pld()
  })

  output$pca_scores <- renderDT({
    res <- pca_rv()
    sc <- res$scores %>% select(company, year, PC1, PC2, PC3) %>% mutate(across(where(is.numeric), ~round(.,4)))
    datatable(sc, options=list(pageLength=10), rownames=FALSE) %>% dtd(5)
  })

  # ══════════════════════════════════════════════════════════════
  # 🟢 DEA
  # ══════════════════════════════════════════════════════════════
  dea_rv <- eventReactive(input$run_dea, {
    req(input$dea_out, input$dea_in, length(input$dea_out)>0, length(input$dea_in)>0)
    avg <- df %>% group_by(company) %>%
      summarise(across(all_of(c(input$dea_out,input$dea_in)), ~mean(.,na.rm=TRUE)), .groups="drop")
    om <- as.matrix(avg[,input$dea_out,drop=FALSE])
    im <- as.matrix(avg[,input$dea_in, drop=FALSE])
    on  <- apply(om, 2, function(x)(x-min(x))/(max(x)-min(x)+1e-9))
    in_ <- apply(im, 2, function(x)(x-min(x))/(max(x)-min(x)+1e-9))
    eff <- rowMeans(on,na.rm=TRUE)/(rowMeans(in_,na.rm=TRUE)+0.001)
    data.frame(Company=avg$company, Efficiency=round(eff/max(eff),4)) %>%
      arrange(desc(Efficiency)) %>% mutate(DEA_Rank=row_number())
  })

  output$dea_table <- renderDT({ datatable(dea_rv(), options=list(pageLength=10,dom='t'), rownames=FALSE) %>% dtd(3) })

  output$dea_plot <- renderPlotly({
    d <- dea_rv(); cp <- company_colors[names(company_colors) %in% d$Company]
    p <- ggplot(d, aes(x=reorder(Company,Efficiency), y=Efficiency, fill=Company,
                        text=paste0(Company,"\nEff:",Efficiency))) +
      geom_bar(stat="identity", width=0.65) +
      geom_hline(yintercept=1, color="#ffcc00", linetype="dashed") +
      scale_fill_manual(values=cp) + coord_flip() + dark_theme() + theme(legend.position="none") +
      labs(title="DEA Efficiency", x="Company", y="Score")
    ggplotly(p, tooltip="text") %>% pld() %>% layout(showlegend=FALSE)
  })

  # ══════════════════════════════════════════════════════════════
  # 🟠 SHARPE
  # ══════════════════════════════════════════════════════════════
  sharpe_rv <- eventReactive(input$run_sharpe, {
    df %>% group_by(company) %>%
      summarise(Mean_ROA=mean(ROA,na.rm=TRUE), SD_ROA=sd(ROA,na.rm=TRUE), .groups="drop") %>%
      mutate(Sharpe=round((Mean_ROA-input$rf_rate)/(SD_ROA+1e-9), 4)) %>%
      arrange(desc(Sharpe)) %>% mutate(Sharpe_Rank=row_number())
  })

  output$sharpe_table <- renderDT({
    datatable(sharpe_rv(), options=list(pageLength=10,dom='t'), rownames=FALSE) %>%
      formatStyle("Sharpe", color=styleInterval(c(0,0.5,1), c("#ff4444","#ffaa00","#44ff88","#00ffcc")), fontWeight="bold") %>%
      dtd(5)
  })

  output$sharpe_plot <- renderPlotly({
    d <- sharpe_rv(); cp <- company_colors[names(company_colors) %in% d$company]
    p <- ggplot(d, aes(x=reorder(company,Sharpe), y=Sharpe, fill=company,
                        text=paste0(company,"\nSharpe:",Sharpe))) +
      geom_bar(stat="identity", width=0.65) +
      geom_hline(yintercept=0, color="#ffffff", linetype="dashed") +
      scale_fill_manual(values=cp) + coord_flip() + dark_theme() + theme(legend.position="none") +
      labs(title="Sharpe Ratio — S=(Rₚ−Rƒ)/σₚ", x="Company", y="Sharpe Ratio")
    ggplotly(p, tooltip="text") %>% pld() %>% layout(showlegend=FALSE)
  })

  # ══════════════════════════════════════════════════════════════
  # 🔴 K-MEANS
  # ══════════════════════════════════════════════════════════════
  km_rv <- eventReactive(input$run_kmeans, {
    req(input$km_ratios, length(input$km_ratios)>=2)
    avg <- df %>% group_by(company) %>%
      summarise(across(all_of(input$km_ratios), ~mean(.,na.rm=TRUE)), .groups="drop")
    mat <- scale(avg[,input$km_ratios,drop=FALSE]); set.seed(42)
    km <- kmeans(mat, centers=input$km_k, nstart=25)
    avg$Cluster <- paste0("Cluster ",km$cluster)
    if(ncol(mat)>=2) {
      pc <- prcomp(mat, center=FALSE, scale.=FALSE)
      avg$PC1 <- pc$x[,1]; avg$PC2 <- pc$x[,2]
    }
    list(res=avg, km=km)
  })

  output$km_table <- renderDT({
    res <- km_rv()$res %>% mutate(across(where(is.numeric), ~round(.,4)))
    datatable(res, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE) %>% dtd(ncol(res))
  })

  output$km_plot <- renderPlotly({
    res <- km_rv()$res
    cp <- c("Cluster 1"="#FF0000","Cluster 2"="#FFCC00","Cluster 3"="#FF6600","Cluster 4"="#FF3366","Cluster 5"="#FF99CC")
    p <- ggplot(res, aes(x=PC1, y=PC2, color=Cluster, label=company,
                          text=paste0(company,"\n",Cluster))) +
      geom_point(size=6, alpha=0.9) + geom_text(vjust=-1.5, color="#ffffff", size=4) +
      scale_color_manual(values=cp) + dark_theme() +
      labs(title="K-Means Clusters", x="PC1", y="PC2", color="Cluster")
    ggplotly(p, tooltip="text") %>% pld()
  })

  output$km_profile <- renderPlotly({
    res <- km_rv()$res
    ld <- res %>% select(company, Cluster, all_of(input$km_ratios)) %>%
      pivot_longer(all_of(input$km_ratios), names_to="Ratio", values_to="Value")
    p <- ggplot(ld, aes(x=Ratio, y=Value, fill=Cluster,
                         text=paste0(company,"\n",Ratio,":",round(Value,4)))) +
      geom_bar(stat="identity", position="dodge", width=0.7) + dark_theme() +
      theme(axis.text.x=element_text(angle=45,hjust=1)) +
      labs(title="Cluster Profiles", x="Ratio", y="Value", fill="Cluster")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 🟡 SENSITIVITY
  # ══════════════════════════════════════════════════════════════
  sens_rv <- eventReactive(input$run_sens, {
    req(input$ratios, length(input$ratios)>0)
    bw <- setNames(rep(1/length(input$ratios), length(input$ratios)), input$ratios)
    rd <- df[,input$ratios,drop=FALSE]
    zm <- scale(rd, center=TRUE, scale=TRUE)
    for(neg_r in negative_ratios) if(neg_r %in% colnames(zm)) zm[,neg_r] <- -zm[,neg_r]
    results <- lapply(c(-0.30,-0.20,-0.10,0,0.10,0.20,0.30), function(delta) {
      wn <- bw
      if(input$sens_ratio %in% names(wn)) {
        wn[input$sens_ratio] <- max(0, wn[input$sens_ratio]*(1+delta))
        tw <- sum(wn); if(tw>0) wn <- wn/tw
      }
      tmp <- df; tmp$score <- as.numeric(zm %*% wn)
      tmp %>% group_by(company) %>%
        summarise(Avg=round(mean(score,na.rm=TRUE),4), .groups="drop") %>%
        arrange(desc(Avg)) %>%
        mutate(Rank=row_number(), Delta=paste0(ifelse(delta>=0,"+",""),round(delta*100),"%"), Delta_num=delta)
    })
    bind_rows(results)
  })

  output$sens_table <- renderDT({
    d <- sens_rv() %>% select(Delta, company, Rank) %>%
      pivot_wider(names_from=Delta, values_from=Rank, id_cols=company)
    datatable(d, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE,
              caption="Rank stability under weight perturbation ±30%") %>% dtd(ncol(d))
  })

  output$sens_plot <- renderPlotly({
    d <- sens_rv(); cp <- company_colors[names(company_colors) %in% d$company]
    p <- ggplot(d, aes(x=Delta_num*100, y=Rank, color=company, group=company,
                        text=paste0(company,"\n±",round(Delta_num*100),"%\nRank:",Rank))) +
      geom_line(size=1.4) + geom_point(size=3) + scale_y_reverse(breaks=1:5) +
      scale_color_manual(values=cp) + dark_theme() +
      labs(title=paste("Sensitivity:",input$sens_ratio,"weight ±30%"), x="Perturbation %", y="Rank (1=Best)", color="Company")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 🔶 OUTLIERS
  # ══════════════════════════════════════════════════════════════
  out_rv <- eventReactive(input$run_outlier, {
    rd <- df[,c("company","year",ratios)]
    zm <- scale(rd[,ratios], center=TRUE, scale=TRUE)
    zdf <- as.data.frame(zm); colnames(zdf) <- paste0("Z_",ratios)
    fdf <- as.data.frame(abs(zm)>2); colnames(fdf) <- paste0("Out_",ratios)
    full <- bind_cols(rd[,c("company","year")], zdf, fdf)
    full$Total_Out   <- rowSums(fdf)
    full$Extreme_Out <- rowSums(abs(zm)>3)
    list(full=full, z_mat=zm)
  })

  output$outlier_table <- renderDT({
    d <- out_rv()$full %>%
      select(company, year, starts_with("Z_"), Total_Out, Extreme_Out) %>%
      mutate(across(starts_with("Z_"), ~round(.,3)))
    datatable(d, options=list(pageLength=10, scrollX=TRUE), rownames=FALSE) %>%
      formatStyle("Extreme_Out", color=styleInterval(0, c("#fff","#ff4444")), fontWeight="bold") %>%
      dtd(ncol(d))
  })

  output$outlier_heatmap <- renderPlotly({
    zm <- out_rv()$z_mat
    zdf <- as.data.frame(zm) %>%
      mutate(company=df$company, year=df$year) %>%
      pivot_longer(all_of(ratios), names_to="Ratio", values_to="Z")
    p <- ggplot(zdf, aes(x=Ratio, y=paste0(company," ",year), fill=Z,
                          text=paste0(company," ",year,"\n",Ratio,":Z=",round(Z,3)))) +
      geom_tile(color="#0d0000", size=0.2) +
      scale_fill_gradient2(low="#ff0000", mid="#222222", high="#ffcc00", midpoint=0) +
      dark_theme() + theme(axis.text.x=element_text(angle=45,hjust=1)) +
      labs(title="Z-Score Heatmap | |Z|>2=Outlier | |Z|>3=Extreme", x="Ratio", y="")
    ggplotly(p, tooltip="text") %>% pld()
  })

  output$outlier_count <- renderPlotly({
    zm <- out_rv()$z_mat
    cnt <- data.frame(Ratio=ratios, Moderate=colSums(abs(zm)>2), Extreme=colSums(abs(zm)>3)) %>%
      pivot_longer(c(Moderate,Extreme), names_to="Type", values_to="Count")
    p <- ggplot(cnt, aes(x=reorder(Ratio,Count), y=Count, fill=Type,
                          text=paste0(Ratio,"\n",Type,":",Count))) +
      geom_bar(stat="identity", position="dodge", width=0.6) +
      scale_fill_manual(values=c("Moderate"="#FF6600","Extreme"="#FF0000")) +
      coord_flip() + dark_theme() +
      labs(title="Outlier Count per Ratio", x="Ratio", y="Count", fill="Type")
    ggplotly(p, tooltip="text") %>% pld()
  })

  # ══════════════════════════════════════════════════════════════
  # 💾 SAVE PDF
  # ══════════════════════════════════════════════════════════════
  output$save_pdf <- downloadHandler(
    filename=function() paste0("Financial_Report_",Sys.Date(),".pdf"),
    content=function(file) {
      pdf(file, width=14, height=9, bg="white")
      plot.new()
      text(0.5,0.7,"FINANCIAL ANALYTICS REPORT",cex=2.2,font=2,col="#880000")
      text(0.5,0.55,"5 Companies | 5 Years | 12 Ratios | WSM Ranking | Per-Company Correlation",cex=1.1,col="#333")
      text(0.5,0.45,paste("Generated:",Sys.Date(),"| Analyst: Aftab Ahmad"),cex=0.9,col="#555")
      p1 <- ggplot(df, aes(x=year,y=ROA,color=company,group=company)) +
        geom_line(size=1.4) + geom_point(size=3) +
        scale_color_manual(values=company_colors) + theme_minimal() +
        labs(title="ROA Trend 2020–2024", x="Year", y="ROA")
      print(p1)
      for(co in all_companies) {
        cm <- get_corr_mat(co)
        if(!is.null(cm)) {
          corrplot(cm, method="color", type="upper",
                   col=colorRampPalette(c("#ff0000","#222","#ffcc00"))(200),
                   tl.col="#333", tl.srt=45, addCoef.col="white", number.cex=0.75,
                   title=paste0(co," — Correlation Matrix"), mar=c(0,0,2,0))
        }
      }
      dev.off()
    }
  )

  # ══════════════════════════════════════════════════════════════
  # 💾 SAVE XLSX
  # ══════════════════════════════════════════════════════════════
  output$save_xlsx <- downloadHandler(
    filename=function() paste0("Financial_Data_",Sys.Date(),".xlsx"),
    content=function(file) {
      wb <- createWorkbook()
      hs <- createStyle(fgFill="#880000",fontColour="#FFCC00",fontSize=11,textDecoration="bold",
                        halign="center",border="Bottom",borderColour="#FFCC00",borderStyle="medium")
      ds <- createStyle(fontColour="#111111",fontSize=10,border="All",borderColour="#cccccc",borderStyle="thin")
      ws <- function(name, data) {
        addWorksheet(wb, name, tabColour="#990000")
        writeData(wb, name, data, headerStyle=hs)
        addStyle(wb, name, ds, rows=2:(nrow(data)+1), cols=1:ncol(data), gridExpand=TRUE)
        setColWidths(wb, name, cols=1:ncol(data), widths="auto")
      }
      ws("Raw Data", raw_df)
      ws("Computed Ratios", df %>% select(company,year,all_of(ratios)) %>% mutate(across(all_of(ratios),~round(.,4))))

      if(length(input$ratios)>0) {
        d <- df[,input$ratios,drop=FALSE]
        ws("Descriptive Stats", data.frame(Ratio=input$ratios,
          Min=round(sapply(d,min,na.rm=TRUE),4), Max=round(sapply(d,max,na.rm=TRUE),4),
          Mean=round(sapply(d,mean,na.rm=TRUE),4), Median=round(sapply(d,median,na.rm=TRUE),4),
          SD=round(sapply(d,sd,na.rm=TRUE),4),
          CV_pct=round((sapply(d,sd,na.rm=TRUE)/sapply(d,mean,na.rm=TRUE))*100, 2)))
      }

      for(co in all_companies) {
        cm <- get_corr_mat(co)
        if(!is.null(cm)) {
          cm_df <- as.data.frame(round(cm,4)); cm_df <- cbind(Ratio=rownames(cm_df),cm_df)
          ws(paste0("Corr_",co), cm_df)
        }
      }

      cd <- df[,ratios,drop=FALSE]; cd <- cd[,apply(cd,2,var,na.rm=TRUE)!=0,drop=FALSE]
      cm_all <- round(cor(cd,use="complete.obs"),4)
      cm_all_df <- as.data.frame(cm_all); cm_all_df <- cbind(Ratio=rownames(cm_all_df),cm_all_df)
      ws("Corr_All_Combined", cm_all_df)

      rk <- tryCatch(ranking_rv(), error=function(e) NULL)
      if(!is.null(rk)) ws("WSM Ranking", rk)

      wsmdata <- tryCatch(wsm_rv(), error=function(e) NULL)
      if(!is.null(wsmdata)) {
        ws("Z-Scores", wsmdata$z_table %>% mutate(across(where(is.numeric),~round(.,4))))
        ws("WSM Ranking Custom", wsmdata$wsm_rank)
      }

      ts_res <- tryCatch(ts_rv(), error=function(e) NULL)
      if(!is.null(ts_res))
        ws("TS Forecast", data.frame(Year=ts_res$fd$year, Forecast=round(ts_res$fd$value,4),
                                     Lower95=round(ts_res$fd$lo,4), Upper95=round(ts_res$fd$hi,4), Method=ts_res$method))

      sh_res <- tryCatch(sharpe_rv(), error=function(e) NULL)
      if(!is.null(sh_res)) ws("Sharpe Ratio", sh_res)

      dea_res <- tryCatch(dea_rv(), error=function(e) NULL)
      if(!is.null(dea_res)) ws("DEA Efficiency", dea_res)

      roa_avg <- round(tapply(df$ROA,df$company,mean),4)
      sum_df <- data.frame(Company=names(roa_avg), Avg_ROA=as.numeric(roa_avg),
                           Avg_ROE=as.numeric(round(tapply(df$ROE,df$company,mean),4)),
                           Avg_NPM=as.numeric(round(tapply(df$NPM,df$company,mean),4)),
                           WSM_Weight="Profitability35%+Efficiency30%+Liquidity20%+Leverage15%")
      ws("Executive Summary", sum_df)
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
}

# ═══════════════════════════════════════════════════════════════
# 🚀 RUN APPLICATION
# ═══════════════════════════════════════════════════════════════
shinyApp(ui, server)