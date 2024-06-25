library(stats4)
library(car)
library(lmtest)
library(nortest)

setClass(
  "regLins",
  slots = list(
    coefficients = "numeric",
    fitted = "numeric",
    residuals = "numeric",
    method = "character",
    formula = "formula",
    model = "lm"
  )
)

setGeneric("regLin", function(y, X, method="kuadrat terkecil") standardGeneric("regLin"))

setMethod("regLin", 
          signature(y="numeric", X="matrix"),
          function(y, X, method="kuadrat terkecil") {
            if (method == "kuadrat terkecil") {
              obj_fun <- function(beta) {
                sum((y - X %*% beta)^2)
              }
            } else if (method == "kemungkinan") {
              obj_fun <- function(beta) {
                -sum(dnorm(y, mean = X %*% beta, sd = sd(y), log = TRUE))
              }
            } else {
              stop("Metode tidak dikenal. Gunakan 'kuadrat terkecil' atau 'kemungkinan'.")
            }
            
            init_beta <- rep(0, ncol(X))
            fit <- optim(init_beta, obj_fun)
            coefficients <- fit$par
            fitted <- as.numeric(X %*% coefficients)
            residuals <- as.numeric(y - fitted)
            
            colnames(X) <- make.names(colnames(X))
            
            if (ncol(X) == 1) {
              formula <- as.formula(paste("y ~", colnames(X)))
            } else {
              formula <- as.formula(paste("y ~", paste(colnames(X), collapse = " + ")))
            }
            
            df <- data.frame(y = y, X)
            model <- lm(formula, data = df)
            
            new("regLins", coefficients=coefficients, fitted=fitted, residuals=residuals, method=method, formula=formula, model=model)
          })

setGeneric("summary", function(object) standardGeneric("summary"))

setMethod("summary", 
          signature(object="regLins"), 
          function(object) {
            cat("Metode: ", object@method, "\n")
            cat("Koefisien:\n")
            print(object@coefficients)
            
            model <- object@model
            
            cat("\nUji F Simultan:\n")
            f_stat <- summary(model)$fstatistic
            p_value_f <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
            cat("F-statistik: ", f_stat[1], "\n")
            cat("p-value: ", p_value_f, "\n")
            
            cat("\nUji t Parsial:\n")
            coef_summary <- summary(model)$coefficients
            colnames(coef_summary) <- c("Koefisien", "Std. Error", "t-value", "p-value")
            print(coef_summary)
            
            cat("\nANOVA:\n")
            anova_table <- anova(model)
            print(anova_table)
            
            cat("\nSignifikan:\n")
            cat("Kode:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
            
            cat("\nHasil Uji Asumsi:\n")
            
            cat("1. Uji Normalitas Residual:\n")
            shapiro_test <- shapiro.test(object@residuals)
            cat("Shapiro-Wilk normality test:\n")
            print(shapiro_test)
            cat("p-value:", shapiro_test$p.value, "\n")
            if (!is.null(shapiro_test$p.value) && shapiro_test$p.value > 0.05) {
              cat("Asumsi normalitas terpenuhi\n")
            } else {
              cat("Asumsi normalitas tidak terpenuhi\n")
            }
            
            cat("\n2. Uji Homoskedastisitas:\n")
            bptest_result <- bptest(model)
            cat("Breusch-Pagan test:\n")
            print(bptest_result)
            cat("p-value:", bptest_result$p.value, "\n")
            if (!is.null(bptest_result$p.value) && bptest_result$p.value > 0.05) {
              cat("Asumsi homoskedastisitas terpenuhi\n")
            } else {
              cat("Asumsi homoskedastisitas tidak terpenuhi\n")
            }
            
            cat("\n3. Uji Autokorelasi:\n")
            dw_test <- dwtest(model)
            cat("Durbin-Watson test:\n")
            print(dw_test)
            cat("p-value:", dw_test$p.value, "\n")
            if (!is.null(dw_test$p.value) && dw_test$p.value > 0.05) {
              cat("Asumsi tidak ada autokorelasi terpenuhi\n")
            } else {
              cat("Asumsi tidak ada autokorelasi tidak terpenuhi\n")
            }
            
            cat("\n4. Uji Linieritas:\n")
            linearity_test <- ncvTest(model)
            cat("Non-constant Variance Test:\n")
            print(linearity_test)
            cat("p-value:", linearity_test$p, "\n")
            if (!is.null(linearity_test$p) && linearity_test$p > 0.05) {
              cat("Asumsi linearitas terpenuhi\n")
            } else {
              cat("Asumsi linearitas tidak terpenuhi\n")
            }
            
            cat("\n5. Uji Independensi Residual:\n")
            independence_test <- durbinWatsonTest(model)
            cat("Durbin-Watson test:\n")
            print(independence_test)
            cat("p-value:", independence_test$p, "\n")
            if (!is.null(independence_test$p) && independence_test$p > 0.05) {
              cat("Asumsi independensi residual terpenuhi\n")
            } else {
              cat("Asumsi independensi residual tidak terpenuhi\n")
            }
            
            if (length(object@coefficients) > 1) {
              cat("\n6. Uji Multikolinearitas:\n")
              vif_values <- vif(model)
              print(vif_values)
              if (all(vif_values < 10)) {
                cat("Asumsi tidak ada multikolinearitas terpenuhi\n")
              } else {
                cat("Asumsi tidak ada multikolinearitas tidak terpenuhi\n")
              }
            }
          })

setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

setMethod("plot", 
          signature(x="regLins", y="missing"), 
          function(x, y) {
            par(mfrow = c(1, 2))
            qqnorm(x@residuals, main = "Plot Q-Q Normal", pch = 19, col = "blue")
            qqline(x@residuals, col = "red")
            plot(x@fitted, x@residuals, xlab = "Nilai Fitted", ylab = "Residual", main = "Fitted vs Residuals", pch = 19, col = "blue")
            abline(h = 0, col = "red")
          })
