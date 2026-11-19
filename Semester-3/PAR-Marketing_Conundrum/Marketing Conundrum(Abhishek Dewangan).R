# =====================================================================
#  Marketing Head's Conundrum at WSES  (IIMB case IMB 541 / data IMB 543)
#  Author : Abhishek Dewangan (MSDSM/03/025)
#           abhishekd.msdsm03@iimamritsar.ac.in
#  Course : Predictive Analytics with R, IIM Amritsar
#
#  Part A. Predict the probability of winning a deal
#          (logistic regression and linear discriminant analysis)
#  Part B. Validate on the hold-out Test Data
#  Part C. Scenario: what if we bid with a lower profit %?
#  Part D. Cut the Procsys marketing budget with integer programming
# =====================================================================
#  install.packages(c("readxl", "MASS", "pROC", "lpSolve", "ggplot2"))
library(readxl)
library(MASS)
library(pROC)
library(lpSolve)
library(ggplot2)

# Looks for the case workbook in the working directory. If it is
# elsewhere, set path <- "full/path/to/IMB543-XLS-ENG.xlsx" instead.
cands <- c(Sys.glob("IMB543-XLS-ENG*.xlsx"), "data.xlsx")
path  <- cands[file.exists(cands)][1]
if (is.na(path)) stop("Excel file not found. Put IMB543-XLS-ENG.xlsx in ", getwd())

# ---------------------------------------------------------------------
# 0. Load and prepare
# ---------------------------------------------------------------------
tr_raw <- read_excel(path, sheet = "Training Data")
te_raw <- read_excel(path, sheet = "Test Data")

prep <- function(d) {
  seg <- do.call(rbind, strsplit(d$Segment, ","))
  data.frame(
    ID          = d[["Opportunity No."]],
    Won         = as.integer(d[["Reporting Status"]] == "Won"),
    Product     = trimws(seg[, 1]),
    Industry    = trimws(seg[, 2]),
    Region      = trimws(seg[, 3]),
    RelStrength = as.numeric(d[["Relative Strength in the segment"]]),
    CustProfit  = as.numeric(d[["Profit of customer - $Mn"]]),
    SalesValue  = as.numeric(d[["Sales Value - $Mn"]]),
    ProfitPct   = as.numeric(d[["Profit %"]]),
    JointBid    = as.numeric(d[["Joint Bid - WSES Portion"]]),
    stringsAsFactors = FALSE
  )
}
train <- prep(tr_raw)
test  <- prep(te_raw)

# Fix factor levels from the training data so the test set uses the same coding
for (v in c("Product", "Industry", "Region")) {
  lv <- names(sort(table(train[[v]]), decreasing = TRUE))   # largest group = baseline
  train[[v]] <- factor(train[[v]], levels = lv)
  test[[v]]  <- factor(test[[v]],  levels = lv)
}
cat("Training:", nrow(train), "opportunities, win rate", round(mean(train$Won) * 100, 1), "%\n")
cat("Test    :", nrow(test),  "opportunities, win rate", round(mean(test$Won) * 100, 1), "%\n")
cat("Test rows with a level unseen in training:", sum(!complete.cases(test)), "\n")

# ---------------------------------------------------------------------
# 1. Exploratory look (training data)
# ---------------------------------------------------------------------
cat("\n===== Win rate by product / region (training) =====\n")
wr <- function(v) {
  a <- aggregate(train$Won, list(train[[v]]), function(x) c(n = length(x), win = mean(x)))
  a <- do.call(data.frame, a); names(a) <- c(v, "n", "WinRate")
  a$WinRate <- round(a$WinRate * 100, 1); a[order(-a$WinRate), ]
}
print(wr("Product"), row.names = FALSE)
print(wr("Region"), row.names = FALSE)
print(wr("Industry"), row.names = FALSE)

num <- c("RelStrength", "CustProfit", "SalesValue", "ProfitPct", "JointBid")
cat("\nAverage of numeric variables, lost (0) vs won (1):\n")
print(round(t(aggregate(train[, num], list(Won = train$Won), mean)), 2))
for (v in num) cat(sprintf("%-12s Welch t p-value: %.3g\n", v, t.test(train[[v]] ~ train$Won)$p.value))

# ---------------------------------------------------------------------
# Part A1. Logistic regression
# ---------------------------------------------------------------------
f_full <- Won ~ Product + Industry + Region + RelStrength + CustProfit +
                SalesValue + ProfitPct + JointBid
m_logit_full <- glm(f_full, data = train, family = binomial)
m_logit <- step(m_logit_full, direction = "both", trace = 0)
cat("\n===== Part A1: Logistic regression (stepwise AIC) =====\n")
print(summary(m_logit))
cat("\nLikelihood-ratio test of each term:\n")
print(drop1(m_logit, test = "Chisq"))

num_in <- intersect(num, all.vars(formula(m_logit)))
# The four numeric variables almost perfectly separate wins from losses
# (hence glm's "fitted probabilities 0 or 1" warning), so 1-SD odds ratios
# are astronomically large or small. Odds ratios per unit are easier to read.
cat("\nOdds ratio per 1-unit increase:\n")
print(round(exp(coef(m_logit)[num_in]), 3))
cat("Odds ratio per +0.1 $Mn customer profit:", round(exp(coef(m_logit)["CustProfit"] * 0.1), 3), "\n")
cat("\nMcFadden pseudo R2:",
    round(1 - m_logit$deviance / m_logit$null.deviance, 3), "\n")

# ---------------------------------------------------------------------
# Part A2. Linear discriminant analysis (the method Mark used in the case)
# ---------------------------------------------------------------------
m_lda <- lda(formula(m_logit), data = train)
cat("\n===== Part A2: LDA on the same variables =====\n")
print(m_lda$prior)
cat("Wilks' lambda (MANOVA on numeric variables):\n")
print(summary(manova(as.matrix(train[, num]) ~ Won, data = train), test = "Wilks"))

# ---------------------------------------------------------------------
# Part B. Validation on the Test Data sheet
# ---------------------------------------------------------------------
test_ok <- test[complete.cases(test), ]
test_ok$p_logit <- predict(m_logit, test_ok, type = "response")
test_ok$p_lda   <- predict(m_lda, test_ok)$posterior[, "1"]

perf <- function(p, y, cut = 0.5) {
  pred <- as.integer(p >= cut)
  cm <- table(Predicted = factor(pred, 0:1), Actual = factor(y, 0:1))
  c(Accuracy    = round(mean(pred == y), 3),
    Sensitivity = round(cm["1", "1"] / sum(cm[, "1"]), 3),
    Specificity = round(cm["0", "0"] / sum(cm[, "0"]), 3),
    AUC         = round(as.numeric(auc(y, p, quiet = TRUE)), 3))
}
cat("\n===== Part B: Test-set performance (n =", nrow(test_ok), ") =====\n")
res <- rbind(Logistic = perf(test_ok$p_logit, test_ok$Won),
             LDA      = perf(test_ok$p_lda,   test_ok$Won),
             Naive_always_lose = c(round(mean(test_ok$Won == 0), 3), 0, 1, 0.5))
print(res)
cat("\nConfusion matrix, logistic, cut-off 0.5:\n")
print(table(Predicted = as.integer(test_ok$p_logit >= 0.5), Actual = test_ok$Won))

# Calibration: predicted vs actual win rate by probability band
test_ok$band <- cut(test_ok$p_logit, c(0, .2, .4, .6, .8, 1), include.lowest = TRUE)
calib <- aggregate(cbind(Predicted = p_logit, Actual = Won) ~ band, data = test_ok, FUN = mean)
calib$n <- as.vector(table(test_ok$band))
cat("\nCalibration (test set):\n"); print(transform(calib, Predicted = round(Predicted, 3), Actual = round(Actual, 3)))

# ---------------------------------------------------------------------
# Part C. Scenario: what happens to win probability if we bid leaner?
# ---------------------------------------------------------------------
cat("\n===== Part C: Profit % scenario (test set) =====\n")
if ("ProfitPct" %in% all.vars(formula(m_logit))) {
  scen <- data.frame(ProfitChange = c(-10, -5, 0, 5))
  scen$AvgWinProb <- sapply(scen$ProfitChange, function(d) {
    nd <- test_ok; nd$ProfitPct <- pmax(nd$ProfitPct + d, 0)
    round(mean(predict(m_logit, nd, type = "response")) * 100, 1)
  })
  scen$ExpProfit <- sapply(scen$ProfitChange, function(d) {
    nd <- test_ok; nd$ProfitPct <- pmax(nd$ProfitPct + d, 0)
    round(sum(predict(m_logit, nd, type = "response") * nd$SalesValue * nd$ProfitPct / 100), 1)
  })
  print(scen, row.names = FALSE)
}

# ---------------------------------------------------------------------
# Part D. Procsys budget optimisation (integer programme)
# ---------------------------------------------------------------------
# Decide which Procsys opportunities get marketing money (x = 1) so that
# the marketing budget (6% of sales value per funded deal) is minimised,
# while keeping:
#   pipeline value          >= 50% of the total pipeline
#   expected sales value    >= 90% of the total expected sales
#   expected profit         >= 90% of the total expected profit
#   expected FY15 revenue   >= 90% of the total expected FY15 revenue
opt <- read_excel(path, sheet = " Data for Optimization")
opt <- data.frame(ID = opt[["Opportunity No."]],
                  SalesValue = opt[["Sales Value - $Mn"]],
                  ProfitPct  = opt[["Profit %"]] / 100,
                  p_case     = opt[["Won"]],          # probability from the case's model
                  FY15       = opt[["FY15"]],
                  Budget     = opt[["Required Max"]], # 6% of sales value
                  Segment    = opt[["Segment"]],
                  RelStrength= opt[["Relative Strength in the segment"]],
                  CustProfit = opt[["Profit of customer - $Mn"]],
                  JointBid   = opt[["Joint Bid - WSES Portion"]])
seg <- do.call(rbind, strsplit(opt$Segment, ","))
opt$Product  <- factor(trimws(seg[, 1]), levels(train$Product))
opt$Industry <- factor(trimws(seg[, 2]), levels(train$Industry))
opt$Region   <- factor(trimws(seg[, 3]), levels(train$Region))
opt$ProfitPct100 <- opt$ProfitPct * 100
nd <- opt; nd$ProfitPct <- opt$ProfitPct100
opt$p_ours <- predict(m_logit, nd, type = "response")

solve_budget <- function(p, label) {
  n <- nrow(opt)
  A <- rbind(opt$SalesValue,
             p * opt$SalesValue,
             p * opt$SalesValue * opt$ProfitPct,
             p * opt$FY15)
  rhs <- c(0.5 * sum(opt$SalesValue),
           0.9 * sum(p * opt$SalesValue),
           0.9 * sum(p * opt$SalesValue * opt$ProfitPct),
           0.9 * sum(p * opt$FY15))
  sol <- lp("min", opt$Budget, A, rep(">=", 4), rhs, all.bin = TRUE)
  x <- sol$solution
  out <- data.frame(
    Scenario = label,
    Funded = sum(x), OutOf = n,
    Budget_before = round(sum(opt$Budget), 2),
    Budget_after  = round(sum(opt$Budget * x), 2),
    Saving_pct    = round((1 - sum(opt$Budget * x) / sum(opt$Budget)) * 100, 1),
    Pipeline_pct  = round(sum(opt$SalesValue * x) / sum(opt$SalesValue) * 100, 1),
    ExpSales_pct  = round(sum(p * opt$SalesValue * x) / sum(p * opt$SalesValue) * 100, 1),
    ExpProfit_pct = round(sum(p * opt$SalesValue * opt$ProfitPct * x) /
                          sum(p * opt$SalesValue * opt$ProfitPct) * 100, 1),
    FY15_pct      = round(sum(p * opt$FY15 * x) / sum(p * opt$FY15) * 100, 1))
  list(summary = out, x = x)
}
cat("\n===== Part D: Procsys marketing budget optimisation =====\n")
r_case <- solve_budget(opt$p_case, "Case model probabilities")
r_ours <- solve_budget(opt$p_ours, "Our logistic probabilities")
print(rbind(r_case$summary, r_ours$summary), row.names = FALSE)

opt$Fund_case <- r_case$x; opt$Fund_ours <- r_ours$x
cat("\nWho gets funded (our model), by win-probability band:\n")
opt$pband <- cut(opt$p_ours, c(0, .1, .5, .9, 1), include.lowest = TRUE,
                 labels = c("<10%", "10-50%", "50-90%", ">90%"))
print(table(Band = opt$pband, Funded = opt$Fund_ours))
cat("Agreement between the two funding plans:",
    round(mean(opt$Fund_case == opt$Fund_ours) * 100, 1), "%\n")

# Sensitivity: which constraint stops us saving more than 50%?
# Budget is 6% of sales value, so keeping >= 50% of pipeline value
# forces spending >= 50% of the budget. Relax that floor and re-solve.
pipe_sens <- function(floor) {
  p <- opt$p_ours
  A <- rbind(opt$SalesValue, p * opt$SalesValue, p * opt$SalesValue * opt$ProfitPct, p * opt$FY15)
  rhs <- c(floor * sum(opt$SalesValue), 0.9 * sum(p * opt$SalesValue),
           0.9 * sum(p * opt$SalesValue * opt$ProfitPct), 0.9 * sum(p * opt$FY15))
  x <- lp("min", opt$Budget, A, rep(">=", 4), rhs, all.bin = TRUE)$solution
  c(PipelineFloor = floor * 100, Funded = sum(x),
    Saving_pct = round((1 - sum(opt$Budget * x) / sum(opt$Budget)) * 100, 1),
    Funded_below10pct = sum(x == 1 & p < 0.10))
}
cat("\nSensitivity to the pipeline floor (our model):\n")
print(t(sapply(c(0.5, 0.4, 0.3, 0), pipe_sens)))

write.csv(opt[, c("ID", "Segment", "SalesValue", "ProfitPct", "p_case", "p_ours",
                  "Budget", "Fund_case", "Fund_ours")],
          "Procsys_funding_plan.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# Charts
# ---------------------------------------------------------------------
dir.create("charts", showWarnings = FALSE)
roc_l <- roc(test_ok$Won, test_ok$p_logit, quiet = TRUE)
roc_d <- roc(test_ok$Won, test_ok$p_lda, quiet = TRUE)
png("charts/roc.png", 900, 800, res = 140)
plot(roc_l, col = "#1F6F78", lwd = 2, main = "ROC on test data")
lines(roc_d, col = "#E07A5F", lwd = 2, lty = 2)
legend("bottomright", c(paste("Logistic AUC", round(auc(roc_l), 3)),
                        paste("LDA AUC", round(auc(roc_d), 3))),
       col = c("#1F6F78", "#E07A5F"), lwd = 2, lty = 1:2)
dev.off()
ggsave("charts/funded_vs_prob.png",
       ggplot(opt, aes(p_ours, SalesValue, colour = factor(Fund_ours, labels = c("Not funded", "Funded")))) +
         geom_point(size = 2.5) + labs(x = "Predicted win probability", y = "Sales value ($ Mn)", colour = NULL,
                                       title = "Procsys opportunities: who gets marketing money") +
         theme_minimal(), width = 7, height = 4.5, dpi = 150)

cat("\nDone. Outputs: Procsys_funding_plan.csv and ./charts\n")
