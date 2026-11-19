# =====================================================================
#  Package Pricing at Mission Hospital  (IIMB case IMB 527 / data IMB 529)
#  Goal: predict a patient's total treatment cost AT ADMISSION so the
#        hospital can quote a fixed package price with known risk.
# =====================================================================
#  Packages: readxl, car, lmtest, MASS, ggplot2
#  install.packages(c("readxl","car","lmtest","MASS","ggplot2"))
library(readxl)
library(car)
library(lmtest)
library(MASS)
library(ggplot2)

set.seed(2026)
path <- "IMB529-XLS-ENG.xlsx"                 # change to your local path
if (!file.exists(path)) path <- "data.xlsx"
raw <- read_excel(path, sheet = "MH-Raw Data")

# ---------------------------------------------------------------------
# 1. Clean and prepare
# ---------------------------------------------------------------------
df <- data.frame(
  Age        = raw$AGE,
  Gender     = factor(raw$GENDER),
  Marital    = factor(raw$`MARITAL STATUS`),
  Complaint  = raw$`KEY COMPLAINTS -CODE`,
  Weight     = raw$`BODY WEIGHT`,
  Height     = raw$`BODY HEIGHT`,
  Pulse      = raw$`HR PULSE`,
  BP_High    = raw$`BP -HIGH`,
  BP_Low     = raw$`BP-LOW`,
  RR         = raw$RR,
  History    = raw$`PAST MEDICAL HISTORY CODE`,
  HB         = raw$HB,
  Urea       = raw$UREA,
  Creatinine = raw$CREATININE,
  Arrival    = raw$`MODE OF ARRIVAL`,
  State      = raw$`STATE AT THE TIME OF ARRIVAL`,
  Admission  = factor(raw$`TYPE OF ADMSN`),
  Cost       = raw$`TOTAL COST TO HOSPITAL`,
  LOS        = raw$`TOTAL LENGTH OF STAY`,
  LOS_ICU    = raw$`LENGTH OF STAY - ICU`,
  LOS_Ward   = raw$`LENGTH OF STAY- WARD`,
  Implant    = factor(raw$`IMPLANT USED (Y/N)`),
  ImplantCost= raw$`COST OF IMPLANT`
)

# Missing complaint / history means "none recorded"
df$Complaint[is.na(df$Complaint)] <- "None"
df$History[is.na(df$History)]     <- "None"
df$History <- tolower(df$History)             # 'Hypertension1' vs 'hypertension1'

# Group complaints into clinically meaningful buckets (some codes have 1-3 patients)
df$ComplaintGrp <- with(df, ifelse(Complaint %in% c("CAD-DVD", "CAD-TVD", "CAD-SVD", "CAD-VSD"), "CAD (coronary)",
                         ifelse(Complaint %in% c("ACHD", "OS-ASD", "PM-VSD", "other-tertalogy"), "Congenital",
                         ifelse(Complaint == "RHD", "RHD (valve)",
                         ifelse(Complaint == "None", "None", "Other")))))
df$ComplaintGrp <- relevel(factor(df$ComplaintGrp), ref = "Other")

# Past history: any recorded condition vs none
df$HistoryAny <- factor(ifelse(df$History == "none", "No", "Yes"))
# Arrival: walked in vs ambulance/transfer (only 4 transfers)
df$ArrivalGrp <- factor(ifelse(df$Arrival == "WALKED IN", "Walked in", "Ambulance/Transfer"))
# True BMI (the raw 'BMI' column is weight/height in cm, not BMI)
df$BMI <- df$Weight / (df$Height / 100)^2
df$AgeGrp <- cut(df$Age, c(-Inf, 1, 12, 18, 40, 60, Inf),
                 labels = c("<1", "1-12", "13-18", "19-40", "41-60", "60+"))
df$logCost <- log(df$Cost)

cat("Patients:", nrow(df), "\n")
cat("Missing values:\n"); print(colSums(is.na(df))[colSums(is.na(df)) > 0])

# ---------------------------------------------------------------------
# 2. Exploratory analysis
# ---------------------------------------------------------------------
cat("\n===== Cost summary (INR) =====\n")
print(summary(df$Cost))
cat("Skewness proxy (mean/median):", round(mean(df$Cost) / median(df$Cost), 2), "\n")

grp <- function(v) {
  a <- aggregate(df$Cost, list(Group = df[[v]]),
                 function(x) c(n = length(x), mean = mean(x), median = median(x)))
  a <- do.call(data.frame, a); names(a) <- c(v, "n", "Mean", "Median")
  a[order(-a$Mean), ]
}
for (v in c("ComplaintGrp", "Complaint", "Admission", "Implant", "Gender",
            "Marital", "ArrivalGrp", "HistoryAny", "AgeGrp")) {
  cat("\n--- Cost by", v, "---\n"); print(grp(v), row.names = FALSE, digits = 6)
}

for (v in c("Age", "Weight", "Height", "Pulse", "BP_High", "BP_Low", "RR", "HB", "Urea", "Creatinine", "LOS", "LOS_ICU", "LOS_Ward", "ImplantCost")) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
num <- c("Age", "Weight", "Height", "BMI", "Pulse", "BP_High", "BP_Low", "RR",
         "HB", "Urea", "Creatinine", "LOS", "LOS_ICU", "LOS_Ward", "ImplantCost")
cat("\n===== Correlation with log(Cost) =====\n")
print(round(sort(sapply(num, function(v) cor(df[[v]], df$logCost, use = "complete.obs")),
                 decreasing = TRUE), 3))

# Quick significance tests on the main drivers
cat("\n===== Group tests on log(Cost) =====\n")
print(t.test(logCost ~ Admission, data = df))
print(t.test(logCost ~ Implant, data = df))
print(summary(aov(logCost ~ ComplaintGrp, data = df)))

# ---------------------------------------------------------------------
# 3. Two models
#    Model A (explanatory): uses length of stay - known only at discharge
#    Model B (pricing):     uses only what is known at admission
# ---------------------------------------------------------------------
# Lab values with gaps (BP, urea, creatinine) are excluded from the base
# models to keep all 248 patients; checked separately below.
mA <- lm(logCost ~ LOS_ICU + LOS_Ward + Implant + ComplaintGrp + Age + Admission,
         data = df)
cat("\n===== Model A: with length of stay =====\n")
print(summary(mA))

full <- lm(logCost ~ Age + Gender + Marital + Weight + Height + Pulse + RR + HB +
             HistoryAny + ArrivalGrp + Admission + Implant + ComplaintGrp,
           data = df)
cat("\n===== Model B (full, admission-time) =====\n")
print(summary(full))
mB <- stepAIC(full, direction = "both", trace = FALSE)
cat("\n===== Model B (stepwise AIC) =====\n")
print(summary(mB))
cat("\nVIF:\n"); print(vif(mB))

# Do the lab values add anything? (complete-case comparison)
cc <- na.omit(df[, c("logCost", all.vars(formula(mB))[-1], "BP_High", "BP_Low",
                     "Urea", "Creatinine")])
base_cc <- lm(formula(mB), data = cc)
lab_cc  <- update(base_cc, . ~ . + BP_High + BP_Low + Urea + Creatinine)
cat("\nAdding lab values (n =", nrow(cc), "):\n"); print(anova(base_cc, lab_cc))

# ---------------------------------------------------------------------
# 4. Diagnostics for the pricing model
# ---------------------------------------------------------------------
cat("\n===== Diagnostics (Model B) =====\n")
print(shapiro.test(residuals(mB)))
print(bptest(mB))
cd <- cooks.distance(mB)
cat("Influential points (Cook's D > 4/n):", sum(cd > 4 / nrow(df)), "\n")
print(head(sort(cd, decreasing = TRUE), 5))

dir.create("charts", showWarnings = FALSE)
png("charts/diagnostics.png", 1200, 1000, res = 130)
par(mfrow = c(2, 2)); plot(mB); dev.off()

# ---------------------------------------------------------------------
# 5. Out-of-sample validation (80/20 split) on the rupee scale
# ---------------------------------------------------------------------
idx   <- sample(nrow(df), round(0.8 * nrow(df)))
train <- df[idx, ]; test <- df[-idx, ]
fit_tr <- lm(formula(mB), data = train)
smear  <- mean(exp(residuals(fit_tr)))       # Duan smearing: corrects log bias
pred   <- exp(predict(fit_tr, test)) * smear
rmse   <- sqrt(mean((test$Cost - pred)^2))
mape   <- mean(abs(test$Cost - pred) / test$Cost) * 100
r2_test <- cor(test$Cost, pred)^2
cat("\n===== Test-set performance (n =", nrow(test), ") =====\n")
cat("RMSE (INR):", round(rmse), "  MAPE:", round(mape, 1), "%  R2:", round(r2_test, 3), "\n")

# Same check for Model A, to show what length of stay would add
fitA_tr <- lm(formula(mA), data = train)
predA   <- exp(predict(fitA_tr, test)) * mean(exp(residuals(fitA_tr)))
cat("Model A MAPE:", round(mean(abs(test$Cost - predA) / test$Cost) * 100, 1), "%\n")

# ---------------------------------------------------------------------
# 6. From prediction to package price
#    Price = predicted cost x markup. How often would the real cost
#    exceed the quoted price (hospital loses money)?
# ---------------------------------------------------------------------
df$Pred <- exp(fitted(mB)) * mean(exp(residuals(mB)))
markups <- c(1.00, 1.10, 1.20, 1.30, 1.50)
risk <- data.frame(
  Markup          = paste0("+", (markups - 1) * 100, "%"),
  PctPatientsLoss = sapply(markups, function(m) round(mean(df$Cost > m * df$Pred) * 100, 1)),
  AvgMarginPct    = sapply(markups, function(m) round(mean((m * df$Pred - df$Cost) / (m * df$Pred)) * 100, 1))
)
cat("\n===== Package price risk =====\n"); print(risk, row.names = FALSE)

# Prediction-interval approach: quote at the 80th percentile of predicted cost
pi80 <- exp(predict(mB, df, interval = "prediction", level = 0.60)[, "upr"])
cat("Quoting at 80th percentile of prediction: patients costing more =",
    round(mean(df$Cost > pi80) * 100, 1), "%\n")

# Example quotes for typical patients
ex <- data.frame(
  Case = c("Child, congenital, elective, no implant",
           "Adult 60, CAD, elective, implant",
           "Adult 60, CAD, emergency, implant"),
  Age = c(6, 60, 60), Gender = factor(c("M", "M", "M"), levels(df$Gender)),
  Marital = factor(c("UNMARRIED", "MARRIED", "MARRIED"), levels(df$Marital)),
  Weight = c(18, 65, 65), Height = c(112, 165, 165), Pulse = c(100, 80, 80),
  RR = c(24, 20, 20), HB = c(12.5, 12.5, 12.5),
  HistoryAny = factor(c("No", "Yes", "Yes"), levels(df$HistoryAny)),
  ArrivalGrp = factor(c("Walked in", "Walked in", "Ambulance/Transfer"), levels(df$ArrivalGrp)),
  Admission = factor(c("ELECTIVE", "ELECTIVE", "EMERGENCY"), levels(df$Admission)),
  Implant = factor(c("N", "Y", "Y"), levels(df$Implant)),
  ComplaintGrp = factor(c("Congenital", "CAD (coronary)", "CAD (coronary)"), levels(df$ComplaintGrp))
)
ex$PredCost <- round(exp(predict(mB, ex)) * mean(exp(residuals(mB))))
ex$Quote80  <- round(exp(predict(mB, ex, interval = "prediction", level = 0.60)[, "upr"]))
cat("\n===== Example quotes (INR) =====\n")
print(ex[, c("Case", "PredCost", "Quote80")], row.names = FALSE)

# Compare with CGHS package rates (Exhibit 3)
cat("\n===== Actual cost vs CGHS NABH rate =====\n")
cag <- df$Cost[df$Complaint %in% c("CAD-DVD", "CAD-TVD", "CAD-SVD")]
asd <- df$Cost[df$Complaint == "OS-ASD"]
vsd <- df$Cost[df$Complaint == "PM-VSD"]
print(data.frame(Procedure = c("CAD (CABG rate 1,30,000)", "ASD (53,000)", "VSD (53,000)"),
                 n = c(length(cag), length(asd), length(vsd)),
                 MedianCost = round(c(median(cag), median(asd), median(vsd))),
                 CGHS = c(130000, 53000, 53000)), row.names = FALSE)

# ---------------------------------------------------------------------
# 7. Charts
# ---------------------------------------------------------------------
ggsave("charts/cost_hist.png",
       ggplot(df, aes(Cost / 1e5)) + geom_histogram(bins = 30, fill = "#2E7D6B") +
         labs(x = "Total cost (INR lakh)", y = "Patients", title = "Cost is right-skewed") +
         theme_minimal(), width = 7, height = 4.5, dpi = 150)
ggsave("charts/cost_by_complaint.png",
       ggplot(df, aes(reorder(ComplaintGrp, Cost, median), Cost / 1e5)) +
         geom_boxplot(fill = "#A7D3C8") + coord_flip() +
         labs(x = NULL, y = "Total cost (INR lakh)", title = "Cost by complaint group") +
         theme_minimal(), width = 7, height = 4.5, dpi = 150)
ggsave("charts/actual_vs_pred.png",
       ggplot(data.frame(Actual = test$Cost / 1e5, Pred = pred / 1e5), aes(Pred, Actual)) +
         geom_point(colour = "#2E7D6B") + geom_abline(linetype = 2) +
         labs(x = "Predicted (INR lakh)", y = "Actual (INR lakh)", title = "Test set: actual vs predicted") +
         theme_minimal(), width = 6, height = 5, dpi = 150)

write.csv(df, "mission_hospital_clean.csv", row.names = FALSE)
cat("\nDone.\n")
