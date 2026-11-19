# =====================================================================
#  Testing Marketing Hypotheses at WSES  (IIMB case IMB 693 / data IMB 695)
#  Hypothesis tests on 1,000 sales opportunities, 2014-2018
# =====================================================================
#  Packages: readxl only (base R for all tests and charts)
#  install.packages("readxl")   # run once if needed
library(readxl)

# ---------------------------------------------------------------------
# 0. Load and prepare data
# ---------------------------------------------------------------------
path <- "IMB695-XLS-ENG 1.xlsx"        # change to your local file path
if (!file.exists(path)) path <- "wses.xlsx"
df <- read_excel(path, sheet = "Sample Data on Past Sales Leads")

names(df) <- c("OppNo", "Status", "Won", "Product", "Industry", "Region",
               "RelStrength", "CustProfit", "SalesValue", "ProfitPct",
               "JointBidPct", "LeadClass")

df$Product   <- factor(df$Product)
df$Region    <- factor(df$Region)
df$LeadClass <- factor(df$LeadClass, levels = c("E", "V", "F", "L"))
df$Outcome   <- factor(df$Won, levels = c(0, 1), labels = c("Lost", "Won"))

str(df)
summary(df)
alpha <- 0.05
cat("\nOverall win rate:", round(mean(df$Won), 4), "\n")

decide <- function(p) ifelse(p < alpha, "Reject H0", "Fail to reject H0")

# ---------------------------------------------------------------------
# H1. The chance of winning a lead is NOT the same for different products
#     H0: Outcome and Product are independent
#     H1: Outcome depends on Product
#     Test: Chi-square test of independence
# ---------------------------------------------------------------------
cat("\n========== H1: Win rate vs Product ==========\n")
tab1 <- table(df$Product, df$Outcome)
print(addmargins(tab1))
print(round(prop.table(tab1, 1), 3))          # win rate by product
h1 <- chisq.test(tab1)
print(h1)
print(round(h1$expected, 1))                  # all expected counts > 5 check
cat("Decision:", decide(h1$p.value), "\n")

# ---------------------------------------------------------------------
# H2. The chance of winning a deal in Africa is better than in the UK
#     H0: p_Africa <= p_UK
#     H1: p_Africa >  p_UK
#     Test: Two-sample test of proportions (one-tailed)
# ---------------------------------------------------------------------
cat("\n========== H2: Africa vs UK win rate ==========\n")
af <- df[df$Region == "Africa", ]
uk <- df[df$Region == "UK", ]
wins <- c(sum(af$Won), sum(uk$Won))
n    <- c(nrow(af), nrow(uk))
cat("Africa:", wins[1], "/", n[1], "=", round(wins[1] / n[1], 3), "\n")
cat("UK    :", wins[2], "/", n[2], "=", round(wins[2] / n[2], 3), "\n")
h2 <- prop.test(wins, n, alternative = "greater", correct = FALSE)
print(h2)
cat("Decision:", decide(h2$p.value), "\n")

# ---------------------------------------------------------------------
# H3. Average size of a deal is at least 8 million USD
#     H0: mu >= 8   (claim contains equality, sits in H0)
#     H1: mu <  8
#     Test: One-sample t-test (left-tailed)
# ---------------------------------------------------------------------
cat("\n========== H3: Mean deal size >= 8 Mn ==========\n")
cat("Mean:", round(mean(df$SalesValue), 3), " SD:", round(sd(df$SalesValue), 3), "\n")
h3 <- t.test(df$SalesValue, mu = 8, alternative = "less")
print(h3)
cat("Decision:", decide(h3$p.value), "\n")
# Complementary check: is there evidence the mean EXCEEDS 8?
h3b <- t.test(df$SalesValue, mu = 8, alternative = "greater")
cat("Right-tailed p-value (H1: mu > 8):", signif(h3b$p.value, 4), "\n")

# ---------------------------------------------------------------------
# H4. Average size of a deal differs across geographical locations
#     H0: mu_1 = mu_2 = ... = mu_k
#     H1: at least one regional mean differs
#     Test: One-way ANOVA (+ Welch ANOVA and Kruskal-Wallis as robustness)
# ---------------------------------------------------------------------
cat("\n========== H4: Deal size vs Region ==========\n")
print(aggregate(SalesValue ~ Region, data = df,
                FUN = function(x) c(n = length(x), mean = round(mean(x), 2),
                                    sd = round(sd(x), 2))))
h4 <- aov(SalesValue ~ Region, data = df)
print(summary(h4))
h4_welch <- oneway.test(SalesValue ~ Region, data = df, var.equal = FALSE)
print(h4_welch)
print(bartlett.test(SalesValue ~ Region, data = df))
h4_kw <- kruskal.test(SalesValue ~ Region, data = df)
print(h4_kw)
p4 <- summary(h4)[[1]][["Pr(>F)"]][1]
cat("Decision (ANOVA):", decide(p4), "\n")

# ---------------------------------------------------------------------
# H5. Winning chances are higher when relative strength is higher
#     H0: mu_RS(Won) <= mu_RS(Lost)
#     H1: mu_RS(Won) >  mu_RS(Lost)
#     Test: Two-sample Welch t-test (one-tailed) + logistic regression
# ---------------------------------------------------------------------
cat("\n========== H5: Relative strength, Won vs Lost ==========\n")
print(aggregate(RelStrength ~ Outcome, data = df, FUN = mean))
h5 <- t.test(RelStrength ~ Outcome, data = df, alternative = "less")  # Lost - Won < 0
print(h5)
cat("Decision:", decide(h5$p.value), "\n")
# Two-sided test reveals the effect runs the OPPOSITE way (won deals have lower strength)
h5_2 <- t.test(RelStrength ~ Outcome, data = df)
print(h5_2)
m5 <- glm(Won ~ RelStrength, data = df, family = binomial)
print(summary(m5)$coefficients)
cat("Odds ratio per 1-point increase in strength:", round(exp(coef(m5)[2]), 3), "\n")

# ---------------------------------------------------------------------
# H6. It is difficult to win leads from customers making low profit / losses
#     H0: mu_Profit(Won) <= mu_Profit(Lost)
#     H1: mu_Profit(Won) >  mu_Profit(Lost)
#     Test: Two-sample Welch t-test (one-tailed);
#           plus chi-square on profit bands (Loss / Low / High)
# ---------------------------------------------------------------------
cat("\n========== H6: Customer profit, Won vs Lost ==========\n")
print(aggregate(CustProfit ~ Outcome, data = df, FUN = mean))
h6 <- t.test(CustProfit ~ Outcome, data = df, alternative = "less")
print(h6)
cat("Decision:", decide(h6$p.value), "\n")
# Two-sided test: again significant, but won deals come from LOWER-profit customers
h6_2 <- t.test(CustProfit ~ Outcome, data = df)
print(h6_2)

med_pos <- median(df$CustProfit[df$CustProfit > 0])
df$ProfitBand <- cut(df$CustProfit, breaks = c(-Inf, 0, med_pos, Inf),
                     labels = c("Loss", "Low profit", "High profit"))
tab6 <- table(df$ProfitBand, df$Outcome)
print(addmargins(tab6))
print(round(prop.table(tab6, 1), 3))
h6b <- chisq.test(tab6)
print(h6b)

# ---------------------------------------------------------------------
# Bonus: does the sales team's gut-feel class actually predict wins?
# ---------------------------------------------------------------------
cat("\n========== Bonus: Lead class (gut feel) vs Outcome ==========\n")
tab7 <- table(df$LeadClass, df$Outcome)
print(addmargins(tab7))
print(round(prop.table(tab7, 1), 3))
print(chisq.test(tab7))

# ---------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------
summary_tbl <- data.frame(
  Hypothesis = c("H1 Win rate differs by product",
                 "H2 Africa win rate > UK",
                 "H3 Mean deal size >= 8 Mn",
                 "H4 Deal size differs by region",
                 "H5 Higher strength -> more wins",
                 "H6 Low-profit customers harder to win"),
  Test = c("Chi-square independence", "Two-proportion z (1-tail), z shown",
           "One-sample t (left-tail)", "One-way ANOVA",
           "Welch t (1-tail)", "Welch t (1-tail)"),
  Statistic = round(c(h1$statistic, sqrt(h2$statistic), h3$statistic,
                      summary(h4)[[1]][["F value"]][1],
                      h5$statistic, h6$statistic), 3),
  p_value = signif(c(h1$p.value, h2$p.value, h3$p.value, p4,
                     h5$p.value, h6$p.value), 4)
)
summary_tbl$Decision <- decide(summary_tbl$p_value)
summary_tbl$Note <- c("", "", "", "",
  paste0("Opposite effect: two-sided p = ", signif(h5_2$p.value, 3)),
  paste0("Opposite effect: two-sided p = ", signif(h6_2$p.value, 3)))
print(summary_tbl, row.names = FALSE)
write.csv(summary_tbl, "WSES_test_summary.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# Charts
# ---------------------------------------------------------------------
dir.create("charts", showWarnings = FALSE)
png("charts/win_by_product.png", 900, 550, res = 120)
wr <- sort(tapply(df$Won, df$Product, mean))
barplot(wr * 100, horiz = TRUE, las = 1, col = "#2E86AB", xlim = c(0, 100),
        xlab = "Win rate (%)", main = "Win rate by product")
abline(v = mean(df$Won) * 100, lty = 2)
dev.off()

png("charts/win_by_region.png", 900, 550, res = 120)
wr2 <- sort(tapply(df$Won, df$Region, mean))
barplot(wr2 * 100, horiz = TRUE, las = 1, col = "#2E86AB", xlim = c(0, 100),
        xlab = "Win rate (%)", main = "Win rate by region")
abline(v = mean(df$Won) * 100, lty = 2)
dev.off()

png("charts/sales_by_region.png", 1000, 550, res = 120)
boxplot(SalesValue ~ Region, data = df, las = 2, col = "#A8DADC",
        ylab = "Sales value (USD Mn)", xlab = "", main = "Deal size by region")
abline(h = 8, lty = 2, col = "red")
dev.off()

png("charts/strength_profit.png", 1000, 500, res = 120)
par(mfrow = c(1, 2))
boxplot(RelStrength ~ Outcome, data = df, col = c("#E76F51", "#2A9D8F"),
        main = "Relative strength", ylab = "Score")
boxplot(CustProfit ~ Outcome, data = df, col = c("#E76F51", "#2A9D8F"),
        main = "Customer profit", ylab = "USD Mn")
dev.off()

cat("\nDone. Charts saved in ./charts and summary in WSES_test_summary.csv\n")
