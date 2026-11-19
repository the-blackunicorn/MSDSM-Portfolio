# =====================================================================
#  Predicting Customer Churn at QWE Inc.  (Darden case UV6694 / data UV6696)
#  Author : Abhishek Dewangan (MSDSM/03/025)
#           abhishekd.msdsm03@iimamritsar.ac.in
#  Course : Predictive Analytics with R, IIM Amritsar
#
#  Questions answered
#   Q1. Is Richard Wall's belief about customer age and churn supported?
#   Q2. Which characteristics drive churn? (logistic regression)
#   Q3. Churn probability for customers 672, 354 and 5203
#   Q4. The 100 customers most likely to leave, with their top 3 drivers
#   Q5. How good is the model? (hold-out AUC, lift, confusion matrix)
# =====================================================================
#  install.packages(c("readxl", "pROC", "car", "ggplot2", "writexl"))
library(readxl)
library(pROC)
library(car)
library(ggplot2)
library(writexl)

set.seed(2026)
path <- "UV6696-XLS-ENG.xlsx"                  # change to your local path
if (!file.exists(path)) path <- "data.xlsx"
raw <- read_excel(path, sheet = "Case Data")

# ---------------------------------------------------------------------
# 0. Prepare data
# ---------------------------------------------------------------------
df <- data.frame(
  ID         = raw[[1]],
  Age        = raw[[2]],   # customer age in months
  Churn      = raw[[3]],   # 1 = left within two months
  CHI        = raw[[4]],   # Customer Happiness Index, Dec 2011
  CHI_chg    = raw[[5]],   # change in CHI, Nov -> Dec
  Cases      = raw[[6]],   # support cases, Dec
  Cases_chg  = raw[[7]],
  SP         = raw[[8]],   # average support priority, Dec
  SP_chg     = raw[[9]],
  Logins_chg = raw[[10]],
  Blogs_chg  = raw[[11]],
  Views_chg  = raw[[12]],
  DaysLL_chg = raw[[13]]   # change in days since last login
)
df$AgeGrp <- cut(df$Age, breaks = c(-Inf, 6, 14, Inf), right = FALSE,
                 labels = c("New (<6 m)", "Middle (6-14 m)", "Loyal (14+ m)"))
cat("Customers:", nrow(df), " Churned:", sum(df$Churn),
    " Churn rate:", round(mean(df$Churn) * 100, 2), "%\n")

# ---------------------------------------------------------------------
# Q1. Customer age and churn
# ---------------------------------------------------------------------
cat("\n===== Q1: Churn rate by age group =====\n")
tab_age <- table(df$AgeGrp, df$Churn)
print(addmargins(tab_age))
print(round(prop.table(tab_age, 1)[, 2] * 100, 2))
print(chisq.test(tab_age))
# Pairwise: is 6-14 riskier than the other two groups combined?
mid <- df$AgeGrp == "Middle (6-14 m)"
print(prop.test(c(sum(df$Churn[mid]), sum(df$Churn[!mid])),
                c(sum(mid), sum(!mid)), alternative = "greater"))

# Churn rate by single month of age (for the chart)
by_month <- aggregate(Churn ~ pmin(Age, 40), data = df, FUN = mean)
names(by_month) <- c("AgeMonth", "ChurnRate")
by_n <- aggregate(Churn ~ pmin(Age, 40), data = df, FUN = length)
by_month$n <- by_n$Churn

cat("\nChurn rate by month of age (first 40 months):\n")
print(transform(by_month, ChurnRate = round(ChurnRate * 100, 1)), row.names = FALSE)
# Month 12 = end of 12-month contracts. Is churn higher exactly then?
cat("\nMonth 12 (contract renewal) vs all other months:\n")
print(prop.test(c(sum(df$Churn[df$Age == 12]), sum(df$Churn[df$Age != 12])),
                c(sum(df$Age == 12), sum(df$Age != 12))))

# Churners vs stayers: average profile
cat("\n===== Average profile: stayed vs churned =====\n")
vars <- c("Age", "CHI", "CHI_chg", "Cases", "Cases_chg", "SP", "SP_chg",
          "Logins_chg", "Blogs_chg", "Views_chg", "DaysLL_chg")
print(round(t(aggregate(df[, vars], list(Churn = df$Churn), mean)), 2))

# ---------------------------------------------------------------------
# Q2. Logistic regression on all customers
# ---------------------------------------------------------------------
# Age enters with dummies for Wall's three groups, plus the continuous
# variables he named. Fit on 70% of customers, test on the other 30%.
form <- Churn ~ AgeGrp + CHI + CHI_chg + Cases + Cases_chg + SP + SP_chg +
  Logins_chg + Blogs_chg + Views_chg + DaysLL_chg

idx   <- sample(nrow(df), round(0.7 * nrow(df)))
train <- df[idx, ]; test <- df[-idx, ]

m_full <- glm(form, data = train, family = binomial)
cat("\n===== Q2: Full logistic model (training data) =====\n")
print(summary(m_full))
m_step <- step(m_full, direction = "both", trace = 0)
cat("\n===== Q2: Stepwise (AIC) model =====\n")
print(summary(m_step))
cat("\nVIF:\n"); print(vif(m_step))

or_tab <- data.frame(
  Variable = names(coef(m_step)),
  Coef     = round(coef(m_step), 5),
  OddsRatio= round(exp(coef(m_step)), 4),
  p_value  = signif(summary(m_step)$coefficients[, 4], 3)
)
print(or_tab, row.names = FALSE)

# Odds ratios on a meaningful scale (per typical change)
cat("\nOdds ratio per 1 SD increase (continuous variables):\n")
cont <- intersect(vars[-1], all.vars(formula(m_step)))
sd_or <- sapply(cont, function(v) unname(exp(coef(m_step)[v] * sd(df[[v]]))))
print(round(sort(sd_or), 3))

# ---------------------------------------------------------------------
# Q5. Model quality on the hold-out set
# ---------------------------------------------------------------------
test$p <- predict(m_step, test, type = "response")
roc_obj <- roc(test$Churn, test$p, quiet = TRUE)
cat("\n===== Q5: Hold-out performance (n =", nrow(test), ") =====\n")
cat("AUC:", round(auc(roc_obj), 3), "\n")

# Confusion matrix at the base churn rate as cut-off (5%) and at 10%
for (cut in c(0.05, 0.10)) {
  pred <- as.integer(test$p >= cut)
  cm <- table(Predicted = pred, Actual = test$Churn)
  cat("\nCut-off", cut, "\n"); print(cm)
  cat("Sensitivity:", round(cm[2, 2] / sum(cm[, 2]), 3),
      " Specificity:", round(cm[1, 1] / sum(cm[, 1]), 3),
      " Precision:", round(cm[2, 2] / sum(cm[2, ]), 3), "\n")
}

# Threshold sensitivity: how the trade-off moves with the cut-off
thr <- data.frame(Cutoff = c(0.03, 0.05, 0.075, 0.10, 0.15, 0.20))
thr_stats <- t(sapply(thr$Cutoff, function(cut) {
  pred <- test$p >= cut
  c(Flagged = sum(pred),
    Sensitivity = round(mean(pred[test$Churn == 1]), 3),
    Specificity = round(mean(!pred[test$Churn == 0]), 3),
    Precision   = round(mean(test$Churn[pred]), 3))
}))
thr <- cbind(thr, thr_stats)
cat("\nThreshold sensitivity:\n"); print(thr, row.names = FALSE)

# Lift by decile: what share of churners sit in the riskiest 10%?
test$decile <- cut(rank(-test$p, ties.method = "first"),
                   breaks = quantile(1:nrow(test), probs = seq(0, 1, 0.1)),
                   include.lowest = TRUE, labels = 1:10)
lift <- aggregate(Churn ~ decile, data = test,
                  FUN = function(x) c(n = length(x), churners = sum(x), rate = mean(x)))
lift <- do.call(data.frame, lift)
names(lift) <- c("Decile", "n", "Churners", "ChurnRate")
lift$CumCapture <- round(cumsum(lift$Churners) / sum(lift$Churners) * 100, 1)
lift$Lift <- round(lift$ChurnRate / mean(test$Churn), 2)
print(lift, row.names = FALSE)

# Hit rate of a "top 100" list in the hold-out set, scaled to its size
top_k <- round(100 * nrow(test) / nrow(df))
cat("Hit rate in top", top_k, "test customers:",
    round(mean(test$Churn[order(-test$p)][1:top_k]) * 100, 1),
    "% vs base", round(mean(test$Churn) * 100, 1), "%\n")

# ---------------------------------------------------------------------
# Refit on ALL customers for the final scoring
# ---------------------------------------------------------------------
m_final <- glm(formula(m_step), data = df, family = binomial)
cat("\n===== Final model (all customers) =====\n")
print(summary(m_final))
df$p_churn <- predict(m_final, df, type = "response")

# ---------------------------------------------------------------------
# Q3. Customers 672, 354, 5203
# ---------------------------------------------------------------------
cat("\n===== Q3: Specific customers =====\n")
q3 <- df[df$ID %in% c(672, 354, 5203),
         c("ID", "Age", "Churn", "CHI", "CHI_chg", "Cases", "SP",
           "Logins_chg", "Views_chg", "DaysLL_chg", "p_churn")]
q3$Percentile <- sapply(q3$p_churn, function(p) round(mean(df$p_churn <= p) * 100, 1))
q3$p_churn <- round(q3$p_churn * 100, 2)
print(q3, row.names = FALSE)
cat("Average churn probability:", round(mean(df$p_churn) * 100, 2), "%\n")

# ---------------------------------------------------------------------
# Q4. Top 100 at-risk customers and their 3 main drivers
# ---------------------------------------------------------------------
# A driver's contribution = coefficient x (customer value - average value),
# i.e. how much that variable pushes this customer's log-odds above an
# average customer. The three largest positive pushes are the drivers.
# Labels depend on direction: is the customer above or below average?
lab_hi <- c(CHI = "High happiness score", CHI_chg = "Rising CHI",
            Cases = "Many support cases", Cases_chg = "Rising support cases",
            SP = "High-priority support issues", SP_chg = "Rising support priority",
            Logins_chg = "More logins", Blogs_chg = "More blog articles",
            Views_chg = "More views", DaysLL_chg = "Longer since last login")
lab_lo <- c(CHI = "Low happiness score (CHI)", CHI_chg = "CHI falling",
            Cases = "Few support cases (low engagement)", Cases_chg = "Falling support cases",
            SP = "Low-priority support issues", SP_chg = "Falling support priority",
            Logins_chg = "Fewer logins", Blogs_chg = "Fewer blog articles",
            Views_chg = "Fewer blog views", DaysLL_chg = "Logging in more recently")
mm  <- model.matrix(m_final)
b   <- coef(m_final)
ctr <- sweep(mm, 2, colMeans(mm)) * matrix(b, nrow(mm), length(b), byrow = TRUE)
ctr <- ctr[, colnames(ctr) != "(Intercept)", drop = FALSE]
# Age dummies combine into one "customer age" driver
age_cols <- grep("^AgeGrp", colnames(ctr))
age_ctr  <- rowSums(ctr[, age_cols, drop = FALSE])
ctr <- cbind(ctr[, -age_cols, drop = FALSE], AgeGrp = age_ctr)
label_of <- function(v, row) {
  if (v == "AgeGrp") return(paste0("Age group ", sub(" \\(.*", "", df$AgeGrp[row]),
                                   " (", df$Age[row], " months)"))
  if (df[[v]][row] >= mean(df[[v]])) lab_hi[[v]] else lab_lo[[v]]
}
top_drivers <- t(sapply(seq_len(nrow(ctr)), function(i) {
  o <- order(-ctr[i, ])[1:3]
  sapply(o, function(j) label_of(colnames(ctr)[j], i))
}))

df$Driver1 <- top_drivers[, 1]; df$Driver2 <- top_drivers[, 2]; df$Driver3 <- top_drivers[, 3]

# Rank only customers still with QWE is not possible (outcome is known
# here), so the list ranks everyone by predicted probability, as the
# case asks; actual churn is shown so the list can be checked.
top100 <- df[order(-df$p_churn), ][1:100,
          c("ID", "p_churn", "Churn", "Age", "CHI", "CHI_chg", "Cases", "SP",
            "Logins_chg", "DaysLL_chg", "Driver1", "Driver2", "Driver3")]
top100$p_churn <- round(top100$p_churn * 100, 2)
top100 <- cbind(Rank = 1:100, top100)
cat("\n===== Q4: Top 100 at-risk customers (first 15 shown) =====\n")
print(head(top100, 15), row.names = FALSE)
cat("\nActual churners in the top 100:", sum(top100$Churn),
    "(", sum(top100$Churn), "% vs base rate", round(mean(df$Churn) * 100, 1), "%)\n")
cat("\nMost common drivers in the top 100:\n")
print(sort(table(gsub(" \\(\\d+ months\\)", "",
                      c(top100$Driver1, top100$Driver2, top100$Driver3))),
           decreasing = TRUE))

write_xlsx(list(Top100 = top100,
                Coefficients = or_tab,
                Lift = lift, Thresholds = thr,
                AllScores = df[order(-df$p_churn),
                               c("ID", "p_churn", "Churn", "Age", "Driver1", "Driver2", "Driver3")]),
           "QWE_Top100_AtRisk.xlsx")

# ---------------------------------------------------------------------
# Extension: add a contract-renewal flag (customer at month 12)
# ---------------------------------------------------------------------
df$Renewal12 <- as.integer(df$Age == 12)
train$Renewal12 <- as.integer(train$Age == 12); test$Renewal12 <- as.integer(test$Age == 12)
m_ren_tr <- glm(update(formula(m_step), . ~ . + Renewal12), data = train, family = binomial)
cat("\n===== Extension: + renewal-month flag =====\n")
cat("Hold-out AUC: base", round(auc(roc_obj), 3), "-> with renewal flag",
    round(auc(test$Churn, predict(m_ren_tr, test, type = "response"), quiet = TRUE), 3), "\n")
m_ren <- glm(update(formula(m_step), . ~ . + Renewal12), data = df, family = binomial)
print(summary(m_ren)$coefficients)
cat("Odds ratio for month 12:", round(exp(coef(m_ren)["Renewal12"]), 2), "\n")

# ---------------------------------------------------------------------
# Charts
# ---------------------------------------------------------------------
dir.create("charts", showWarnings = FALSE)
ggsave("charts/churn_by_age_month.png",
       ggplot(by_month, aes(AgeMonth, ChurnRate * 100)) +
         annotate("rect", xmin = 6, xmax = 14, ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "#D9534F") +
         geom_col(fill = "#2F5D8A") +
         labs(x = "Customer age (months, 40 = 40+)", y = "Churn rate (%)",
              title = "Churn rate by customer age") + theme_minimal(),
       width = 8, height = 4.5, dpi = 150)
png("charts/roc.png", 900, 800, res = 140)
plot(roc_obj, col = "#2F5D8A", lwd = 2, main = paste0("ROC, hold-out set (AUC = ", round(auc(roc_obj), 3), ")"))
dev.off()

saveRDS(list(by_month = by_month, lift = lift, thr = thr, auc = as.numeric(auc(roc_obj))), "results.rds")
cat("\nDone. Outputs: QWE_Top100_AtRisk.xlsx and ./charts\n")
