library(dplyr)

# ---------------- SAFE FUNCTION ----------------
safe_div <- function(a, b) {
  ifelse(is.na(a) | is.na(b) | b == 0, 0, a / b)
}

zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# ---------------- LOAD DATA ----------------
df <- read.csv("data.csv")
df[is.na(df)] <- 0

# ---------------- 12 RATIOS ----------------
df <- df %>%
  mutate(
    ROA = safe_div(net_income, total_assets),
    ROE = safe_div(net_income, equity),
    NPM = safe_div(net_income, revenue),
    OPM = safe_div(ebit, revenue),
    CR  = safe_div(current_assets, current_liabilities),

    CF_Revenue = safe_div(cfo, revenue),
    CF_Liabilities = safe_div(cfo, total_liabilities),

    Asset_Turnover = safe_div(revenue, total_assets),
    Debt_Equity = safe_div(total_liabilities, equity),

    Interest_Coverage = safe_div(ebit, abs(total_liabilities) + 1),
    Cash_Adequacy = safe_div(cfo, current_liabilities),

    EBIT_Liabilities = safe_div(ebit, total_liabilities)
  )

# ---------------- RATIOS LIST ----------------
ratios <- c(
  "ROA","ROE","NPM","OPM","CR",
  "CF_Revenue","CF_Liabilities",
  "Asset_Turnover","Debt_Equity",
  "Interest_Coverage","Cash_Adequacy",
  "EBIT_Liabilities"
)

# ---------------- Z SCORE ----------------
df_z <- as.data.frame(lapply(df[, ratios], zscore))

# ---------------- FINAL SCORE ----------------
df$Score <- rowMeans(df_z, na.rm = TRUE)

# ---------------- RISK CLASS ----------------
df$Risk <- ifelse(df$Score > 0.5, "SAFE",
           ifelse(df$Score > 0, "MEDIUM", "HIGH"))

# ---------------- CORRELATION MATRIX ----------------
corr_matrix <- cor(df[, ratios], use = "complete.obs")

# ---------------- COMPANY LIST ----------------
companies <- unique(df$company)