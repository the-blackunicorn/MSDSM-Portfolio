

# ---- 0. Packages, seed, log -----------------------------------------
pkgs <- c("readxl", "dplyr", "car", "lmtest", "sandwich", "rstatix", "ggplot2",
          "pROC", "PRROC", "rpart", "rpart.plot", "randomForest", "gbm")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))
set.seed(2026)
dir.create("output", showWarnings = FALSE)
sink("output/results_log.txt", split = TRUE)   # copy console output to a file
# On error (non-interactive runs only): flush the log and say so instead of stopping silently mid-log
if (!interactive()) options(warn = 1, error = function() { sink(); cat("\n*** SCRIPT STOPPED WITH AN ERROR - see message above ***\n"); q(status = 1) })
tick <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
cat("R:", R.version.string, "\n")
for (p in pkgs) cat(p, as.character(packageVersion(p)), "\n")

# ---- CONFIG: edit this block ----------------------------------------
DATA_FILE   <- "/Users/abhinavrijit/Downloads/PAR_Project_File.xlsx"   # <- your path (no extra quotes)
DATA_SHEET  <- "customer_campaign_data"
SPLIT_SHEET <- "model_split"
ID_COL      <- "customer_id"
SPLIT_COL   <- "split"                # Train / Validation / Test
GROUP_COL   <- "campaign_group"       # ANOVA factor (Control / Voucher / Free Shipping)
FACTOR2_COL <- "delivery_group"       # second factor for the two-way extension
ANOVA_Y     <- "contribution_90d_inr" # ANOVA outcome
MLR_Y       <- "net_sales_90d_inr"    # regression target
# Columns that must NEVER be regression predictors (IDs, assignments, post-assignment outcomes)
EXCLUDE     <- c("customer_id", "campaign_group", "delivery_group",
                 "repeat_purchase_90d", "orders_90d", "net_sales_90d_inr",
                 "campaign_cost_90d_inr", "contribution_90d_inr", "pilot_delivery_days",
                 "avg_order_value_180d_inr")  # last one: redundant (= sales / orders), dropped by judgement
ALPHA <- 0.05
# Business-cost assumption for the probability threshold (stated, editable):
GAIN_TP <- 500   # INR value of reaching a customer who really would repeat-buy (assumption)
COST_FP <- 100   # INR wasted on contacting someone who would not repeat-buy (assumption)
BUDGET  <- 1500000          # INR 15 lakh
MAX_TARGETS <- 1200
HOLDOUT_SHARE <- 0.10       # share of target list kept as randomised no-treatment holdout
# Costs match the data dictionary: 35 contact (all), +55 if Expedited, +180 voucher / +85 shipping
# subsidy only if the customer repeats (once per customer).
EXPEDITE_COST <- 55; CONTACT_COST <- 35; VOUCHER_COST <- 180; SHIP_COST <- 85
ACTION_ARM <- "Free Shipping"          # "Free Shipping" or "Voucher"
EXPEDITE_SHARE_PILOT  <- 0.20          # share of treated randomly upgraded if expedite NOT shown to pay (pilot only)
EXPEDITE_SHARE_PROVEN <- 0.50          # share upgraded if expedite shows a significantly positive contribution effect

fix <- function(x) if (is.null(x)) NULL else make.names(x)
ID_COL <- fix(ID_COL); GROUP_COL <- fix(GROUP_COL); FACTOR2_COL <- fix(FACTOR2_COL)
ANOVA_Y <- fix(ANOVA_Y); MLR_Y <- fix(MLR_Y); EXCLUDE <- fix(EXCLUDE); SPLIT_COL <- fix(SPLIT_COL)

# =====================================================================
# 1. LOAD + AUDIT (single load; split attached once, used by both parts)
# =====================================================================
df <- read_excel(DATA_FILE, sheet = DATA_SHEET)
names(df) <- make.names(names(df))
sp <- read_excel(DATA_FILE, sheet = SPLIT_SHEET, skip = 1)     # row 1 is a junk "Column1/Column2" header
names(sp) <- make.names(names(sp))
df <- left_join(df, sp[, c(ID_COL, SPLIT_COL)], by = ID_COL)
cat("\nRows:", nrow(df), " Cols (incl. split):", ncol(df), "\n")
cat("Missing values per column (only those > 0):\n")
print(colSums(is.na(df))[colSums(is.na(df)) > 0])
cat("Duplicate IDs:", sum(duplicated(df[[ID_COL]])), "\n")
df[[GROUP_COL]] <- factor(df[[GROUP_COL]])
if (!is.null(FACTOR2_COL)) df[[FACTOR2_COL]] <- factor(df[[FACTOR2_COL]])
cat("Split sizes:\n"); print(table(df[[SPLIT_COL]]))

# =====================================================================
# PART A: ANOVA  (Shapiro-Wilk -> Levene -> choose ANOVA -> post-hoc)
# =====================================================================
cat("\n\n================ PART A: ANOVA ================\n")
a <- df %>% filter(!is.na(.data[[ANOVA_Y]]), !is.na(.data[[GROUP_COL]]))
a$y <- a[[ANOVA_Y]]; a$g <- a[[GROUP_COL]]

# A1. Descriptives (all customers in each group, including zeros)
desc <- a %>% group_by(g) %>%
  summarise(n = n(), mean = mean(y), sd = sd(y), median = median(y),
            skew = mean((y - mean(y))^3) / sd(y)^3, .groups = "drop")
print(desc); write.csv(desc, "output/anova_descriptives.csv", row.names = FALSE)

# A2. Fit one-way model; normality is tested on RESIDUALS
fit_aov <- aov(y ~ g, data = a)
res <- residuals(fit_aov)

# A3. Shapiro-Wilk. H0: residuals normal. (R limit n<=5000 -> subsample, seeded)
res_sw <- if (length(res) > 5000) sample(res, 5000) else res
sw <- shapiro.test(res_sw)
cat("\nShapiro-Wilk on residuals: W =", round(sw$statistic, 4), " p =", signif(sw$p.value, 4), "\n")
cat("Shapiro-Wilk by group (H0: normal within group):\n")
print(a %>% group_by(g) %>%
        summarise(W = shapiro.test(if (n() > 5000) sample(y, 5000) else y)$statistic,
                  p = shapiro.test(if (n() > 5000) sample(y, 5000) else y)$p.value))
png("output/anova_qq_hist.png", 1200, 500, res = 120)
par(mfrow = c(1, 2)); qqnorm(res, main = "Q-Q plot of ANOVA residuals"); qqline(res, col = "red")
hist(res, breaks = 40, main = "Residual histogram", xlab = "residual"); par(mfrow = c(1, 1))
dev.off()

# A4. Levene (median-centred = Brown-Forsythe). H0: equal variances
lev <- leveneTest(y ~ g, data = a, center = median)
print(lev)
lev_p <- lev$`Pr(>F)`[1]
equal_var <- lev_p >= ALPHA
cat("Variances statistically similar?", equal_var, "\n")

# A5. Choose test from the assumption checks
if (equal_var) {
  cat("\n-> Classic one-way ANOVA\n"); print(summary(fit_aov))
  ss <- summary(fit_aov)[[1]]; SSb <- ss$`Sum Sq`[1]; SSt <- sum(ss$`Sum Sq`)
  k <- nlevels(a$g); MSw <- ss$`Mean Sq`[2]
  eta2 <- SSb / SSt; omega2 <- (SSb - (k - 1) * MSw) / (SSt + MSw)
  cat("eta^2 =", round(eta2, 4), " omega^2 =", round(omega2, 4), "\n")
  cat("\nTukey HSD (family = all pairwise group comparisons, FWER-adjusted):\n")
  tk <- TukeyHSD(fit_aov); print(tk)
} else {
  cat("\n-> Welch ANOVA (unequal variances)\n")
  w <- oneway.test(y ~ g, data = a, var.equal = FALSE); print(w)
  ss <- summary(fit_aov)[[1]]; eta2 <- ss$`Sum Sq`[1] / sum(ss$`Sum Sq`)
  cat("eta^2 (descriptive) =", round(eta2, 4), "\n")
  cat("\nGames-Howell post-hoc (robust to unequal variances & n):\n")
  gh <- games_howell_test(a, y ~ g); print(gh)
  write.csv(gh, "output/anova_games_howell.csv", row.names = FALSE)
}
# Pairwise Cohen's d (effect size) with Holm-adjusted Welch t-tests as a cross-check
cat("\nPairwise Cohen's d:\n"); print(cohens_d(a, y ~ g))
cat("Holm-adjusted pairwise Welch t-tests:\n")
print(pairwise.t.test(a$y, a$g, p.adjust.method = "holm", pool.sd = FALSE))
# Robustness if residuals are clearly non-normal
cat("\nKruskal-Wallis (robustness check):\n"); print(kruskal.test(y ~ g, data = a))
cat("Test family: all pairwise comparisons among", nlevels(a$g),
    "campaign groups; adjustment: Tukey HSD or Games-Howell (Holm for cross-check).\n")

# A6. Two-factor extension: group x second factor with interaction
if (!is.null(FACTOR2_COL)) {
  a$f2 <- a[[FACTOR2_COL]]
  op <- options(contrasts = c("contr.sum", "contr.poly"))   # sum-to-zero for Type III
  fit2 <- lm(y ~ g * f2, data = a)
  cat("\nTwo-way ANOVA (Type II):\n"); print(Anova(fit2, type = 2))
  cat("Type III:\n"); print(Anova(fit2, type = 3)); options(op)
  cat("Levene on cell variances:\n"); print(leveneTest(y ~ g * f2, data = a, center = median))
  cat("Heteroscedasticity-robust (HC3) two-way table:\n")
  print(Anova(fit2, type = 2, white.adjust = "hc3"))
  png("output/anova_interaction.png", 800, 550, res = 120)
  interaction.plot(a$g, a$f2, a$y, fun = mean, xlab = "Group", ylab = paste("Mean", ANOVA_Y),
                   trace.label = FACTOR2_COL, lwd = 2)
  dev.off()
}

# =====================================================================
# PART B: MULTIPLE LINEAR REGRESSION
# =====================================================================
cat("\n\n================ PART B: MLR ================\n")
m <- df   # supplied split already attached in section 1

# B2. Predictor set = baseline columns only
pred_cols <- setdiff(names(m), c(EXCLUDE, "split", SPLIT_COL, MLR_Y))
train <- m %>% filter(split == "Train")
# Preprocessing learned on TRAIN only: median-impute numerics (+ missing flag), fix factor levels
num_cols <- pred_cols[sapply(m[pred_cols], is.numeric)]
cat_cols <- setdiff(pred_cols, num_cols)
med <- sapply(train[num_cols], median, na.rm = TRUE)
lev_list <- lapply(train[cat_cols], function(x) sort(unique(as.character(x[!is.na(x)]))))
miss_flag <- num_cols[sapply(train[num_cols], function(x) anyNA(x))]
prep <- function(d) {
  for (v in num_cols) {
    if (v %in% miss_flag) d[[paste0(v, "_missing")]] <- as.integer(is.na(d[[v]]))
    d[[v]][is.na(d[[v]])] <- med[[v]]
  }
  for (v in cat_cols) {
    x <- as.character(d[[v]]); x[is.na(x) | !x %in% lev_list[[v]]] <- lev_list[[v]][1]
    d[[v]] <- factor(x, levels = lev_list[[v]])
  }
  d
}
m <- prep(m)
feat <- c(num_cols, paste0(miss_flag, "_missing"), cat_cols)
train <- m %>% filter(split == "Train"); valid <- m %>% filter(split == "Validation"); test <- m %>% filter(split == "Test")
form  <- as.formula(paste(MLR_Y, "~", paste(feat, collapse = " + ")))

# B3. Fit full OLS on Train
ols <- lm(form, data = train)
cat("\n--- OLS summary (F-test, t-tests, R2) ---\n"); print(summary(ols)); print(confint(ols))

# B4. Multicollinearity: VIF, then drop worst offender iteratively (VIF > 10)
cat("\n--- VIF ---\n")
repeat {
  v <- tryCatch(car::vif(ols), error = function(e) NULL)
  if (is.null(v)) { cat("VIF failed (aliased terms) - check perfectly collinear predictors\n"); break }
  v1 <- if (is.matrix(v)) v[, "GVIF^(1/(2*Df))"]^2 else v   # generalised VIF -> VIF scale
  print(round(sort(v1, decreasing = TRUE), 2))
  if (max(v1) <= 10) break
  drop <- names(which.max(v1)); cat("Dropping", drop, "(VIF =", round(max(v1), 1), ")\n")
  feat <- setdiff(feat, drop)
  ols  <- lm(as.formula(paste(MLR_Y, "~", paste(feat, collapse = " + "))), data = train)
}
form <- formula(ols)
cat("Condition number (kappa, unscaled predictors - not a collinearity verdict):", round(kappa(ols, exact = TRUE), 1), "\n")

# B5. Residual diagnostics
cat("\n--- Residual diagnostics ---\n")
print(shapiro.test(sample(residuals(ols), min(5000, nobs(ols)))))
print(jarque <- tseries_jb <- {
  r <- residuals(ols); n <- length(r); S <- mean(r^3) / sd(r)^3; K <- mean(r^4) / sd(r)^4
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4); c(JB = JB, p = pchisq(JB, 2, lower.tail = FALSE))
})
print(bptest(ols))                                  # Breusch-Pagan
print(bptest(ols, ~ fitted(ols) + I(fitted(ols)^2))) # White-type
print(ncvTest(ols))
print(dwtest(ols))                                  # Durbin-Watson (independence)
print(resettest(ols, power = 2:3, type = "fitted")) # RESET: functional form
cat("\nHC3 robust coefficient table (use if heteroscedastic):\n")
print(coeftest(ols, vcov = vcovHC(ols, type = "HC3")))
png("output/mlr_diagnostics.png", 1200, 1000, res = 120)
par(mfrow = c(2, 2)); plot(ols); par(mfrow = c(1, 1)); dev.off()

# B6. Influential observations
cd <- cooks.distance(ols); lev_h <- hatvalues(ols); p <- length(coef(ols)); n <- nobs(ols)
infl <- which(cd > 4 / n | lev_h > 2 * p / n)
cat("\n--- Influence ---\nCook's D > 4/n:", sum(cd > 4 / n), " | leverage > 2p/n:", sum(lev_h > 2 * p / n),
    " | max Cook's D:", round(max(cd), 4), "\n")
print(outlierTest(ols))
png("output/mlr_cooks.png", 900, 500, res = 120); plot(cd, type = "h", main = "Cook's distance"); abline(h = 4 / n, col = "red"); dev.off()
if (length(infl)) {
  ols_noinf <- update(ols, data = train[-infl, ])
  cat("Coefficient change after removing influential rows (relative %):\n")
  print(round(100 * (coef(ols_noinf) - coef(ols)) / coef(ols), 1))
}

# B7. Candidate 2: log(1+y) model with Duan smearing retransformation
ols_log <- lm(update(form, log1p(.) ~ .), data = train)
smear <- mean(exp(residuals(ols_log)))           # Duan smearing factor (train)
pred_log <- function(d) pmax(smear * exp(predict(ols_log, d)) - 1, 0)

# B8. Metrics on Validation; choose model; Test scored ONCE
metrics <- function(y, yhat, base) c(MAE = mean(abs(y - yhat)), RMSE = sqrt(mean((y - yhat)^2)),
                                     R2 = 1 - sum((y - yhat)^2) / sum((y - base)^2))
mean_base <- mean(train[[MLR_Y]])
raw_pred <- function(d) predict(ols, d)
cat("\nNegative predictions (OLS) on Validation:", sum(raw_pred(valid) < 0), "of", nrow(valid), "\n")
cmp <- rbind(
  Mean_baseline = metrics(valid[[MLR_Y]], rep(mean_base, nrow(valid)), mean_base),
  OLS_raw       = metrics(valid[[MLR_Y]], raw_pred(valid), mean_base),
  OLS_clipped0  = metrics(valid[[MLR_Y]], pmax(raw_pred(valid), 0), mean_base),
  OLS_log_smear = metrics(valid[[MLR_Y]], pred_log(valid), mean_base))
cat("\n--- Validation comparison ---\n"); print(round(cmp, 3))
write.csv(cmp, "output/mlr_validation_metrics.csv")
best <- names(which.min(cmp[-1, "RMSE"])); cat("Selected on Validation (lowest RMSE):", best, "\n")
final_pred <- switch(best, OLS_raw = raw_pred, OLS_clipped0 = function(d) pmax(raw_pred(d), 0), OLS_log_smear = pred_log)

# Refit nothing on Test; evaluate once
te <- rbind(Mean_baseline = metrics(test[[MLR_Y]], rep(mean_base, nrow(test)), mean_base),
            Selected      = metrics(test[[MLR_Y]], final_pred(test), mean_base))
cat("\n--- TEST (scored once) ---\n"); print(round(te, 3)); write.csv(te, "output/mlr_test_metrics.csv")

# B9. Predictions file (also kept in memory as mlr_out for the classification/targeting part)
out <- m %>% mutate(actual = .data[[MLR_Y]], predicted = final_pred(m)) %>%
  select(all_of(ID_COL), split, actual, predicted)
write.csv(out, "output/mlr_predictions.csv", row.names = FALSE)
mlr_out <- out
png("output/mlr_actual_vs_pred.png", 700, 600, res = 120)
plot(test[[MLR_Y]], final_pred(test), xlab = "Actual", ylab = "Predicted", main = "Test: actual vs predicted"); abline(0, 1, col = "red")
dev.off()
tick("Part 1 (ANOVA + MLR) done")

# =====================================================================
# PART 2 - re-seed so Part 2 results do not depend on how much randomness Part 1 consumed
# =====================================================================
set.seed(2026)
cat("\n\n================ PART 2 ================\n")
# =====================================================================
# 0. AUDIT: randomisation balance, missingness, skew  (all 6,000 customers)
# Balance is checked on BASELINE fields only. With ~2 x 13 tests, a few raw p < 0.05
# are expected by chance, so judge by Holm-adjusted p AND effect size
# (max standardised mean difference > 0.10, or Cramer's V > 0.10, would be a concern).
# =====================================================================
cat("\n===== 0. AUDIT =====\n")
post_cols <- c("customer_id", "campaign_group", "delivery_group", "pilot_delivery_days", "repeat_purchase_90d",
               "orders_90d", "net_sales_90d_inr", "campaign_cost_90d_inr", "contribution_90d_inr", "split")
base_cols <- setdiff(names(df), post_cols)
cat("Cell counts (campaign x delivery):\n"); print(table(df$campaign_group, df$delivery_group))
bal_one <- function(v, g) {
  x <- df[[v]]
  if (is.numeric(x)) {
    ok <- !is.na(x); m <- tapply(x[ok], g[ok], mean); s <- sqrt(mean(tapply(x[ok], g[ok], var)))
    data.frame(variable = v, type = "numeric", p = summary(aov(x[ok] ~ g[ok]))[[1]][["Pr(>F)"]][1],
               effect = unname((max(m) - min(m)) / s))                 # max standardised mean difference
  } else {
    tb <- table(x, g); ct <- suppressWarnings(chisq.test(tb))
    data.frame(variable = v, type = "categorical", p = ct$p.value,
               effect = unname(sqrt(ct$statistic / (sum(tb) * (min(dim(tb)) - 1)))))   # Cramer's V
  }
}
balance <- do.call(rbind, c(
  lapply(base_cols, function(v) cbind(factor = "campaign_group", bal_one(v, df$campaign_group))),
  lapply(base_cols, function(v) cbind(factor = "delivery_group", bal_one(v, df$delivery_group)))))
balance$p_holm <- p.adjust(balance$p, "holm")
print(balance %>% mutate(across(c(p, effect, p_holm), ~ round(.x, 4))))
cat("Raw p<", ALPHA, ":", sum(balance$p < ALPHA), "of", nrow(balance), "(about", round(ALPHA * nrow(balance), 1),
    "expected by chance) | Holm-significant:", sum(balance$p_holm < ALPHA),
    "| largest effect:", round(max(balance$effect), 3), "\n")
write.csv(balance, "output/balance_check.csv", row.names = FALSE)

cat("\nMissingness (share missing) by campaign group and delivery group:\n")
for (v in c("app_session_share", "avg_rating_180d")) {
  cat(v, "- overall", round(mean(is.na(df[[v]])), 3), "\n")
  print(round(tapply(is.na(df[[v]]), df$campaign_group, mean), 3))
  print(round(tapply(is.na(df[[v]]), df$delivery_group, mean), 3))
  cat("  chi-square p (campaign):", signif(suppressWarnings(chisq.test(table(df$campaign_group, is.na(df[[v]])))$p.value), 3),
      " (delivery):", signif(suppressWarnings(chisq.test(table(df$delivery_group, is.na(df[[v]])))$p.value), 3), "\n")
}
skw <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / sd(x)^3 }
num_all <- c(base_cols[sapply(df[base_cols], is.numeric)], "net_sales_90d_inr", "contribution_90d_inr", "pilot_delivery_days")
cat("\nSkewness (|skew| > 1 = clearly skewed):\n"); print(round(sort(sapply(df[num_all], skw), decreasing = TRUE), 2))

# =====================================================================
# 1. CHI-SQUARE: repeat purchase rate vs campaign group (all 6,000)
# H0: repeat-purchase rate is equal in Control, Voucher, Free Shipping
# H1: at least one rate differs.   alpha = 0.05
# =====================================================================
cat("\n===== 1. CHI-SQUARE =====\n")
tab <- table(df$campaign_group, df$repeat_purchase_90d); print(tab)
rates <- df %>% group_by(campaign_group) %>%
  summarise(n = n(), repeaters = sum(repeat_purchase_90d), rate = mean(repeat_purchase_90d),
            lo = prop.test(repeaters, n)$conf.int[1], hi = prop.test(repeaters, n)$conf.int[2])
print(rates)
ct <- chisq.test(tab, correct = FALSE); print(ct)
cat("Min expected count:", min(ct$expected), "\n")
cat("Cramer's V =", round(sqrt(unname(ct$statistic) / (sum(tab) * (min(dim(tab)) - 1))), 4), "\n")
cat("Pairwise 2-proportion tests, Holm-adjusted (family = 3 pairwise comparisons):\n")
print(pairwise_prop_test(tab, p.adjust.method = "holm"))
for (pr in combn(levels(df$campaign_group), 2, simplify = FALSE)) {
  t2 <- prop.test(tab[pr, ]  [, 2:1])   # successes = repeaters
  cat(pr[1], "vs", pr[2], ": diff in rate =", round(diff(rev(t2$estimate)) * 100, 2), "pp; 95% CI [",
      round(t2$conf.int[1] * 100, 2), ",", round(t2$conf.int[2] * 100, 2), "]\n")
}

# =====================================================================
# 2. WELCH TWO-SAMPLE t-TEST: pilot_delivery_days, Expedited vs Standard
# H0: mean days equal; H1: means differ (two-sided).  Independent groups
# (different customers, randomised) -> NOT a paired t-test: no customer
# is measured under both delivery options, so there are no pairs.
# =====================================================================
cat("\n===== 2. WELCH t-TEST =====\n")
print(df %>% group_by(delivery_group) %>% summarise(n = n(), mean = mean(pilot_delivery_days), sd = sd(pilot_delivery_days)))
tt <- t.test(pilot_delivery_days ~ delivery_group, data = df, var.equal = FALSE); print(tt)
cat("Difference (Expedited - Standard):", round(-diff(tt$estimate), 3), "days\n")
print(cohens_d(df, pilot_delivery_days ~ delivery_group))
cat("Levene:\n"); print(levene_test(df, pilot_delivery_days ~ delivery_group))
cat("Mann-Whitney robustness:\n"); print(wilcox.test(pilot_delivery_days ~ delivery_group, data = df))

# =====================================================================
# 2b. DOES EXPEDITED DELIVERY PAY? (delivery effect on repeat purchase and contribution)
# Faster pilot delivery (section 2) is close to a manipulation check; it says nothing about
# whether the 55 INR upgrade raises repeat buying or contribution. Contribution is already
# net of campaign cost, so compare it directly. Secondary / policy-relevant analysis.
# =====================================================================
cat("\n===== 2b. EXPEDITED vs STANDARD: OUTCOMES =====\n")
tabd <- table(df$delivery_group, df$repeat_purchase_90d)
cat("Repeat rate by delivery group:\n"); print(round(prop.table(tabd, 1), 3)); print(prop.test(tabd[, 2:1]))
cat("Contribution, Expedited vs Standard (all customers, Welch):\n")
print(t.test(contribution_90d_inr ~ delivery_group, data = df))
cells <- df %>% group_by(campaign_group, delivery_group) %>%
  summarise(n = n(), repeat_rate = mean(repeat_purchase_90d), mean_contrib = mean(contribution_90d_inr),
            se = sd(contribution_90d_inr) / sqrt(n()), .groups = "drop") %>%
  mutate(lo95 = mean_contrib - 1.96 * se, hi95 = mean_contrib + 1.96 * se)
cat("\nCell means (6 factorial cells):\n"); print(cells, width = Inf)
write.csv(cells, "output/cell_contribution.csv", row.names = FALSE)
m0 <- glm(repeat_purchase_90d ~ campaign_group + delivery_group, df, family = binomial)
m1 <- update(m0, . ~ . + campaign_group:delivery_group)
cat("\nLikelihood-ratio test, campaign x delivery interaction on repeat purchase:\n"); print(anova(m0, m1, test = "Chisq"))
cat("LR test, delivery main effect on repeat purchase:\n"); print(anova(update(m0, . ~ . - delivery_group), m0, test = "Chisq"))
dl <- do.call(rbind, lapply(levels(df$campaign_group), function(g) {
  s <- df[df$campaign_group == g, ]; tw <- t.test(contribution_90d_inr ~ delivery_group, data = s)
  data.frame(campaign = g, diff_exp_minus_std = unname(tw$estimate[1] - tw$estimate[2]),
             lo95 = tw$conf.int[1], hi95 = tw$conf.int[2], p = tw$p.value) }))
dl$p_holm <- p.adjust(dl$p, "holm")     # family = 3 within-campaign delivery comparisons
cat("\nContribution: Expedited minus Standard WITHIN each campaign group (Holm across 3):\n"); print(round(dl[, -1], 3) %>% `rownames<-`(dl$campaign))
write.csv(dl, "output/delivery_effect_contribution.csv", row.names = FALSE)
arm_dl <- dl[dl$campaign == ACTION_ARM, ]
expedite_pays <- arm_dl$lo95 > 0
EXPEDITE_SHARE <- if (expedite_pays) EXPEDITE_SHARE_PROVEN else EXPEDITE_SHARE_PILOT
cat("Expedite effect within", ACTION_ARM, ":", round(arm_dl$diff_exp_minus_std, 1), "INR [", round(arm_dl$lo95, 1), ",", round(arm_dl$hi95, 1),
    "] -> shown to pay:", expedite_pays, "-> upgrade share of treated set to", EXPEDITE_SHARE,
    if (!expedite_pays) "(small RANDOMISED pilot only, so the next experiment can measure it)" else "", "\n")

# =====================================================================
# 3. CLASSIFICATION of repeat_purchase_90d (baseline predictors only)
# =====================================================================
cat("\n===== 3. CLASSIFICATION =====\n"); tick("start classification")
excl <- c("customer_id", "campaign_group", "delivery_group", "pilot_delivery_days",
          "repeat_purchase_90d", "orders_90d", "net_sales_90d_inr", "campaign_cost_90d_inr",
          "contribution_90d_inr", "split", "avg_order_value_180d_inr")  # AOV redundant
feat0 <- setdiff(names(df), excl)
tr0 <- df %>% filter(split == "Train")
num <- feat0[sapply(df[feat0], is.numeric)]; cat_c <- setdiff(feat0, num)
med <- sapply(tr0[num], median, na.rm = TRUE)
miss <- num[sapply(tr0[num], anyNA)]
lv <- lapply(tr0[cat_c], function(x) sort(unique(x)))
prep <- function(d) {
  for (v in num) { if (v %in% miss) d[[paste0(v, "_missing")]] <- as.integer(is.na(d[[v]]))
  d[[v]][is.na(d[[v]])] <- med[[v]] }
  for (v in cat_c) d[[v]] <- factor(d[[v]], levels = lv[[v]])
  d
}
dd <- prep(df); feat <- c(num, paste0(miss, "_missing"), cat_c)
tr <- dd %>% filter(split == "Train"); va <- dd %>% filter(split == "Validation"); te <- dd %>% filter(split == "Test")
ytr <- tr$repeat_purchase_90d; yva <- va$repeat_purchase_90d; yte <- te$repeat_purchase_90d
cat("Repeat rate  Train/Val/Test:", round(c(mean(ytr), mean(yva), mean(yte)), 3), "\n")
fml <- as.formula(paste("repeat_purchase_90d ~", paste(feat, collapse = " + ")))
auc_of <- function(y, p) as.numeric(auc(roc(y, p, quiet = TRUE, direction = "<")))

# --- Baseline: majority class (always predicts the more common class)
maj <- as.integer(mean(ytr) >= 0.5)
cat("Majority-class baseline predicts", maj, "-> accuracy on Validation:", round(mean(yva == maj), 3), "\n")

# --- 3a. Logistic regression
logit <- glm(fml, data = tr, family = binomial)
p_logit <- function(d) predict(logit, d, type = "response")
cat("\nLogistic odds ratios (95% Wald CI):\n")
print(round(exp(cbind(OR = coef(logit), confint.default(logit))), 3))
# Per-1-SD odds ratios for numeric predictors (comparable scale; raw ORs for tenure/sales print as 1.000,
# and discount_rate is per full 0-1 unit). Fitted on Train SDs.
sd_tr <- sapply(tr[num], sd); cf <- summary(logit)$coefficients[num, , drop = FALSE]
or_sd <- data.frame(OR_per_SD = exp(cf[, "Estimate"] * sd_tr),
                    lo95 = exp((cf[, "Estimate"] - 1.96 * cf[, "Std. Error"]) * sd_tr),
                    hi95 = exp((cf[, "Estimate"] + 1.96 * cf[, "Std. Error"]) * sd_tr))
cat("\nLogistic odds ratios per +1 SD of each numeric predictor (95% Wald CI):\n"); print(round(or_sd, 3))
write.csv(or_sd, "output/logit_or_per_sd.csv")

# --- 3b. CART: tune cp / minbucket / maxdepth on Validation AUC, then prune
grid <- expand.grid(maxdepth = c(3, 4, 5, 6), minbucket = c(20, 50, 100), cp = c(0.0005, 0.001, 0.005))
grid$auc <- apply(grid, 1, function(g) {
  m <- rpart(fml, tr, method = "class", control = rpart.control(maxdepth = g[1], minbucket = g[2], cp = g[3]))
  auc_of(yva, predict(m, va)[, 2]) })
bc <- grid[which.max(grid$auc), ]; cat("\nCART best:", paste(names(bc), unlist(bc), collapse = ", "), "\n")
cart_full <- rpart(fml, tr, method = "class", control = rpart.control(maxdepth = bc$maxdepth, minbucket = bc$minbucket, cp = bc$cp))
# Prune on VALIDATION AUC (CV-error pruning collapsed the tree to the root, AUC 0.500). Keep >= 2 splits so it can be interpreted.
cps <- cart_full$cptable[cart_full$cptable[, "nsplit"] >= 2, "CP"]
if (!length(cps)) cps <- cart_full$cptable[, "CP"]
cp_auc <- sapply(cps, function(c) auc_of(yva, predict(prune(cart_full, c), va)[, 2]))
cat("CART prune path (cp -> validation AUC):\n"); print(round(data.frame(cp = cps, val_auc = cp_auc, nsplit = cart_full$cptable[match(cps, cart_full$cptable[, "CP"]), "nsplit"]), 4))
cart <- prune(cart_full, cps[which.max(cp_auc)])
cat("Pruned CART: splits =", max(cart$cptable[, "nsplit"]), "| validation AUC =", round(max(cp_auc), 3), "\n")
png("output/cart_tree.png", 1200, 800, res = 120); rpart.plot(cart, type = 3, extra = 104, main = "Pruned CART"); dev.off()
p_cart <- function(d) predict(cart, d)[, 2]
tick("CART done")

# --- 3c. Bagging = random forest with mtry = ALL p features (every split sees all features)
#     Random forest = mtry < p (only a random subset of features considered at EACH split)
p_all <- length(feat)
trf <- transform(tr, repeat_purchase_90d = factor(repeat_purchase_90d))
g_rf <- expand.grid(mtry = c(floor(sqrt(p_all)), floor(p_all / 3)), nodesize = c(5, 20, 50), ntree = c(300, 600))
g_rf$auc <- apply(g_rf, 1, function(g) auc_of(yva, predict(
  randomForest(fml, data = trf, mtry = g[1], nodesize = g[2], ntree = g[3]), va, type = "prob")[, 2]))
br <- g_rf[which.max(g_rf$auc), ]; cat("RF best:", paste(names(br), unlist(br), collapse = ", "), "\n")
rf  <- randomForest(fml, trf, mtry = br$mtry, nodesize = br$nodesize, ntree = br$ntree, importance = TRUE)
g_bg <- expand.grid(nodesize = c(5, 20, 50), ntree = c(300, 600))
g_bg$auc <- apply(g_bg, 1, function(g) auc_of(yva, predict(
  randomForest(fml, trf, mtry = p_all, nodesize = g[1], ntree = g[2]), va, type = "prob")[, 2]))
bb <- g_bg[which.max(g_bg$auc), ]; cat("Bagging best:", paste(names(bb), unlist(bb), collapse = ", "), "\n")
bag <- randomForest(fml, trf, mtry = p_all, nodesize = bb$nodesize, ntree = bb$ntree, importance = TRUE)
p_rf  <- function(d) predict(rf,  d, type = "prob")[, 2]
p_bag <- function(d) predict(bag, d, type = "prob")[, 2]
tick("RF + bagging done")

# --- 3d. Gradient boosting: tune depth, learning rate, number of trees on Validation
g_gb <- expand.grid(depth = c(2, 3, 4), shrink = c(0.01, 0.05, 0.1))
res <- lapply(seq_len(nrow(g_gb)), function(i) {
  m <- gbm(fml, data = as.data.frame(tr), distribution = "bernoulli", n.trees = 800, interaction.depth = g_gb$depth[i],
           shrinkage = g_gb$shrink[i], n.minobsinnode = 20, bag.fraction = 0.7, verbose = FALSE)
  ks <- c(seq(10, 100, 10), seq(150, 800, 50)); a <- sapply(ks, function(k) auc_of(yva, predict(m, as.data.frame(va), n.trees = k, type = "response")))
  data.frame(depth = g_gb$depth[i], shrink = g_gb$shrink[i], trees = ks[which.max(a)], auc = max(a)) })
res <- do.call(rbind, res); bg <- res[which.max(res$auc), ]; cat("GBM best:", paste(names(bg), unlist(bg), collapse = ", "), "\n")
gb <- gbm(fml, data = as.data.frame(tr), distribution = "bernoulli", n.trees = bg$trees, interaction.depth = bg$depth,
          shrinkage = bg$shrink, n.minobsinnode = 20, bag.fraction = 0.7, verbose = FALSE)
p_gb <- function(d) predict(gb, as.data.frame(d), n.trees = bg$trees, type = "response")
tick("GBM done")

# --- 3e. Validation comparison + threshold from business-cost assumption
models <- list(Logistic = p_logit, CART = p_cart, Bagging = p_bag, RandomForest = p_rf, GradientBoosting = p_gb)
pr_auc <- function(y, p) pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0])$auc.integral
best_thr <- function(y, p) {   # maximise expected value = GAIN_TP*TP - COST_FP*FP on Validation
  th <- seq(0.05, 0.95, 0.01); v <- sapply(th, function(t) { pred <- p >= t; GAIN_TP * sum(pred & y == 1) - COST_FP * sum(pred & y == 0) })
  th[which.max(v)] }
cls_metrics <- function(y, p, t) { pred <- as.integer(p >= t); tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0); fn <- sum(pred == 0 & y == 1)
prec <- ifelse(tp + fp == 0, NA, tp / (tp + fp)); rec <- tp / (tp + fn)
c(AUC = auc_of(y, p), PR_AUC = pr_auc(y, p), Accuracy = mean(pred == y), Precision = prec, Recall = rec,
  F1 = 2 * prec * rec / (prec + rec), Brier = mean((p - y)^2),
  Lift_top20 = mean(y[order(-p)][1:floor(.2 * length(y))]) / mean(y), Flagged_share = mean(pred), Threshold = t) }
cat("\nBreak-even probability implied by the cost assumption = COST_FP/(GAIN_TP+COST_FP) =",
    round(COST_FP / (GAIN_TP + COST_FP), 3), "(base repeat rate is ~0.31, so a LOW threshold that flags most customers is expected)\n")
val_tab <- t(sapply(names(models), function(n) { p <- models[[n]](va); cls_metrics(yva, p, best_thr(yva, p)) }))
cat("\n--- VALIDATION (threshold chosen on Validation; GAIN_TP =", GAIN_TP, ", COST_FP =", COST_FP, ") ---\n"); print(round(val_tab, 3))
cat("Majority-class baseline accuracy (Validation):", round(mean(yva == maj), 3), "\n")
write.csv(val_tab, "output/clf_validation_metrics.csv")
cat("\nValidation confusion matrices at each model's threshold (rows = actual, cols = predicted):\n")
for (n in names(models)) {
  p <- models[[n]](va); thn <- val_tab[n, "Threshold"]; cat("\n", n, " @ threshold ", thn, "\n", sep = "")
  print(table(Actual = yva, Predicted = factor(as.integer(p >= thn), levels = 0:1))) }
best <- names(which.max(val_tab[, "AUC"])); thr <- val_tab[best, "Threshold"]
cat("\nSelected model (highest validation AUC):", best, " threshold:", thr, "\n")

# --- 3f. TEST (scored once) + confusion matrix, calibration, top-20% lift
pt <- models[[best]](te); cat("\n--- TEST (once) ---\n"); print(round(cls_metrics(yte, pt, thr), 3))
cat("Confusion matrix (rows = actual, cols = predicted):\n")
print(table(Actual = yte, Predicted = factor(as.integer(pt >= thr), levels = 0:1)))
cat("Majority baseline on Test: accuracy", round(mean(yte == maj), 3), "\n")
# Second operating point: the cost-based threshold flags ~95% of customers (break-even 0.167 is far below the 0.31 base rate),
# so precision/recall/F1 there are near-trivial. Report a capacity-style cut too: flag the top 20%, cut set on VALIDATION scores.
cut20 <- unname(quantile(models[[best]](va), 0.8))
cat("\n--- TEST at top-20% operating point (cut =", round(cut20, 3), ", chosen on Validation) ---\n"); print(round(cls_metrics(yte, pt, cut20), 3))
print(table(Actual = yte, Predicted = factor(as.integer(pt >= cut20), levels = 0:1)))
cal <- te %>% mutate(p = pt, bin = ntile(p, 10)) %>% group_by(bin) %>% summarise(mean_pred = mean(p), observed = mean(repeat_purchase_90d), n = n())
print(cal); write.csv(cal, "output/calibration_deciles.csv", row.names = FALSE)
ggplot(cal, aes(mean_pred, observed)) + geom_point() + geom_line() + geom_abline(linetype = 2, colour = "red") +
  labs(title = paste("Calibration (Test):", best), x = "Mean predicted probability", y = "Observed repeat rate") + theme_minimal()
ggsave("output/calibration.png", width = 6, height = 5, dpi = 120)
top <- order(-pt)[1:floor(.2 * length(pt))]
cat("Top-20% scored: precision =", round(mean(yte[top]), 3), " lift =", round(mean(yte[top]) / mean(yte), 2), "\n")
# ROC overlay on Test
png("output/roc_test.png", 700, 600, res = 120); plot(roc(yte, pt, quiet = TRUE), main = paste("ROC (Test):", best)); dev.off()

# --- 3g. Importance (predictive only; NOT causal)
cat("\nRandom-forest importance (permutation / Gini):\n"); print(round(importance(rf)[order(-importance(rf)[, "MeanDecreaseGini"]), c("MeanDecreaseAccuracy", "MeanDecreaseGini")], 3))
cat("GBM relative influence:\n"); print(summary(gb, n.trees = bg$trees, plotit = FALSE))
cat("Importance shows predictive association, NOT that changing a variable (or giving an incentive) causes repeat purchase.\n")

# --- 3h. Prediction output file (required columns; in_sample flags Train rows, whose scores are fitted values)
all_p <- models[[best]](dd)
out <- data.frame(customer_id = dd$customer_id, split = dd$split, actual_repeat = dd$repeat_purchase_90d,
                  predicted_probability = all_p, predicted_class = as.integer(all_p >= thr),
                  actual_sales = dd$net_sales_90d_inr, in_sample = dd$split == "Train")
ms <- as.data.frame(mlr_out); names(ms)[names(ms) == "predicted"] <- "predicted_sales"   # Part 1 regression predictions (same run)
out <- left_join(out, ms[, c("customer_id", "predicted_sales")], by = "customer_id")
write.csv(out, "output/predictions.csv", row.names = FALSE)
tick("predictions.csv written")

# =====================================================================
# 4. TARGET LIST + BUDGET + INCREMENTAL-CONTRIBUTION SCENARIOS
# IMPORTANT: ranking_score = predicted repeat probability x predicted sales. It prioritises
# valuable, active customers; it is NOT a treatment effect. High-probability customers may buy
# without any incentive, so the randomised holdout below is what will measure real uplift.
# Incremental contribution below is a GROUP-LEVEL assumption taken from the experiment
# (cell mean minus Control-Standard mean), not an observed individual treatment effect.
# =====================================================================
cat("\n===== 4. TARGET LIST =====\n")
unit <- c("Free Shipping" = SHIP_COST, "Voucher" = VOUCHER_COST)[[ACTION_ARM]]
arm_rate <- mean(df$repeat_purchase_90d[df$campaign_group == ACTION_ARM])     # experiment-observed repeat rate in chosen arm
exp_cost_arm <- CONTACT_COST + arm_rate * unit
cat("Chosen arm:", ACTION_ARM, "| observed repeat rate", round(arm_rate, 3), "| expected cost per treated customer (excl. expedite) INR", round(exp_cost_arm, 2), "\n")

# Candidate pool = customers in Test + Validation (not used to fit the model) with a score
pool <- out %>% filter(split != "Train") %>% mutate(score = predicted_probability * ifelse(is.na(predicted_sales), actual_sales, predicted_sales))
pool <- pool %>% arrange(desc(score)) %>% slice_head(n = MAX_TARGETS)
n_hold <- floor(HOLDOUT_SHARE * nrow(pool)); pool$holdout <- FALSE
pool$holdout[sample(nrow(pool), n_hold)] <- TRUE       # randomised holdout: gets NO incentive, standard delivery
treated <- which(!pool$holdout)
n_treat <- length(treated); n_exp <- floor(EXPEDITE_SHARE * n_treat); n_std <- n_treat - n_exp
pool$expedite <- FALSE; pool$expedite[sample(treated, n_exp)] <- TRUE   # RANDOMISED among treated -> measurable next time
pool$proposed_action <- ifelse(pool$holdout, "Holdout (no action)", ACTION_ARM)
pool$estimated_campaign_cost <- ifelse(pool$holdout, 0, exp_cost_arm + ifelse(pool$expedite, EXPEDITE_COST, 0))
pool$worst_case_cost <- ifelse(pool$holdout, 0, CONTACT_COST + unit + ifelse(pool$expedite, EXPEDITE_COST, 0))  # everyone repeats
cat("\nHoldout vs treated comparability on score (randomisation sanity check):\n")
print(pool %>% group_by(holdout) %>% summarise(n = n(), mean_score = mean(score), mean_prob = mean(predicted_probability), expedited = sum(expedite)))

contact <- CONTACT_COST * n_treat; incent <- n_treat * arm_rate * unit; exped <- EXPEDITE_COST * n_exp
expected_total <- contact + incent + exped
worst_total <- contact + n_treat * unit + exped
budget_tab <- data.frame(
  item = c("Contact cost (all treated)", paste("Expected", ACTION_ARM, "subsidy (repeat rate x unit)"),
           paste0("Expedited upgrades (", n_exp, " customers, ", if (expedite_pays) "evidence-backed" else "randomised pilot", ")"),
           "EXPECTED TOTAL", "DOWNSIDE TOTAL (every treated customer repeats)", "Budget",
           "Unallocated headroom - expected", "Unallocated headroom - downside"),
  inr = round(c(contact, incent, exped, expected_total, worst_total, BUDGET, BUDGET - expected_total, BUDGET - worst_total)))
cat("\nTargets:", nrow(pool), "| treated", n_treat, "| holdout", n_hold, "| expedited", n_exp, "\n")
print(budget_tab, row.names = FALSE)
cat("Within INR 15 lakh even in the downside case:", worst_total <= BUDGET,
    "| share of budget used (expected / downside):", round(expected_total / BUDGET, 3), "/", round(worst_total / BUDGET, 3), "\n")
cat("NOTE: the 1,200-customer cap makes the budget non-binding; the headroom is ring-fenced for scaling and the next (larger) randomised experiment.\n")
write.csv(budget_tab, "output/budget_table.csv", row.names = FALSE)

# Incremental-contribution scenarios (INR). Baseline = Control + Standard cell (no incentive, standard delivery).
# Contribution is already net of campaign cost. Sum of CI bounds across the two cells is a conservative interval.
ctrl_std <- df$contribution_90d_inr[df$campaign_group == "Control" & df$delivery_group == "Standard"]
inc <- function(arm, del) { x <- df$contribution_90d_inr[df$campaign_group == arm & df$delivery_group == del]
tw <- t.test(x, ctrl_std); c(est = unname(tw$estimate[1] - tw$estimate[2]), lo = tw$conf.int[1], hi = tw$conf.int[2]) }
i_std <- inc(ACTION_ARM, "Standard"); i_exp <- inc(ACTION_ARM, "Expedited")
cat("\nPer-customer incremental contribution vs Control-Standard (INR, 95% CI):\n")
print(round(rbind(Standard = i_std, Expedited = i_exp), 1))
scen <- data.frame(
  scenario = c("Downside (lower 95% bound)", "Zero uplift", "Base (experiment point estimate)", "Upside (upper 95% bound)"),
  total_incremental_contribution_inr = round(c(i_std["lo"] * n_std + i_exp["lo"] * n_exp, 0,
                                               i_std["est"] * n_std + i_exp["est"] * n_exp,
                                               i_std["hi"] * n_std + i_exp["hi"] * n_exp)))
rownames(scen) <- NULL; print(scen, row.names = FALSE)
cat("Assumptions: effect is homogeneous across targeted customers and equals the experiment's group-level estimate;",
    "targeted customers (high scores) may respond differently; holdout customers contribute 0 incremental by construction;",
    "Control-Standard baseline already includes the 35 INR contact cost. These are scenarios, NOT individual treatment effects.\n")
write.csv(scen, "output/incremental_scenarios.csv", row.names = FALSE)

tc <- pool %>% transmute(customer_id, ranking_score = round(score, 2), proposed_action = proposed_action,
                         expedited_delivery = expedite, estimated_campaign_cost = round(estimated_campaign_cost, 2),
                         worst_case_cost = round(worst_case_cost, 2))
write.csv(tc, "output/target_customers.csv", row.names = FALSE)
cat("Wrote output/target_customers.csv (", nrow(tc), "rows )\n")
sink(); cat("Done. See ./output/\n")