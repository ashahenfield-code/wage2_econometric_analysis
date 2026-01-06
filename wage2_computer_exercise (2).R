

# =============================================================
# Wage2 Computer Exercise Script
# Dataset: wooldridge::wage2
# =============================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Packages
for (pkg in c("wooldridge", "lmtest", "sandwich", "car")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(wooldridge)
library(lmtest)
library(sandwich)
library(car)

# -----------------------------------------------------------------
# Data
# -----------------------------------------------------------------
data("wage2")
df <- wage2

# -----------------------------------------------------------------
# 1. Baseline OLS model
#    wage = beta0 + beta1*educ + beta2*exper + beta3*urban
# -----------------------------------------------------------------
m <- lm(wage ~ educ + exper + urban, data = df)

cat("\n=== Baseline OLS ===\n")
print(summary(m))

# -----------------------------------------------------------------
# 2. t-test for urban at 1% level (from summary output)
#    -> question about significance of urban
# -----------------------------------------------------------------
# (No extra code needed beyond summary(m), which reports t and p-values.)

# -----------------------------------------------------------------
# 3. Joint F-test: exper = 0 and urban = 0
# -----------------------------------------------------------------
cat("\n=== Joint F-test: exper = 0 and urban = 0 ===\n")
joint_test <- linearHypothesis(m, c("exper = 0", "urban = 0"))
print(joint_test)

# -----------------------------------------------------------------
# 4. Breusch–Pagan test for heteroskedasticity
# -----------------------------------------------------------------
cat("\n=== Breusch–Pagan test (using regressors) ===\n")
bp_test <- bptest(m)
print(bp_test)

# -----------------------------------------------------------------
# 5. White-type test: squared residuals on fitted and fitted^2
# -----------------------------------------------------------------
cat("\n=== White-type test: bptest(m, ~ fitted(m) + I(fitted(m)^2)) ===\n")
white_test <- bptest(m, ~ fitted(m) + I(fitted(m)^2))
print(white_test)

# -----------------------------------------------------------------
# 6. Robust (HC1) standard errors for OLS
# -----------------------------------------------------------------
cat("\n=== HC1 robust SEs (coeftest) ===\n")
robust_ols <- coeftest(m, vcov = vcovHC(m, type = "HC1"))
print(robust_ols)

# The robust SE for educ (question 22) is:
educ_se_robust <- robust_ols["educ", "Std. Error"]
cat("\nRobust HC1 SE for educ:", educ_se_robust, "\n")

# -----------------------------------------------------------------
# 7. Weighted Least Squares with weights = 1/educ
# -----------------------------------------------------------------
cat("\n=== WLS with weights = 1/educ ===\n")
mwls_educ <- lm(wage ~ educ + exper + urban, data = df, weights = 1/educ)
summary_wls <- summary(mwls_educ)
print(summary_wls)

# Extract coefficient and SE for educ under WLS (questions 23 & 24)
educ_coef_wls <- coef(summary_wls)["educ", "Estimate"]
educ_se_wls   <- coef(summary_wls)["educ", "Std. Error"]

cat("\nWLS coefficient on educ:", educ_coef_wls, "\n")
cat("WLS standard error for educ:", educ_se_wls, "\n")

cat("\n=== End of wage2 Computer Exercise Script ===\n")
