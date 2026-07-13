library(readxl)
data <- read_excel("C:/Users/lenovo/Desktop/dataR.xlsx")
setwd('E:/R/fb')

library(flextable)
library(MASS)

# 定义函数进行logistics回归并格式化输出
formatFit <- function(fit1){
  # 取P值
  p <- summary(fit1)$coefficients[,4]
  # 计算 Wald 值并保留三位小数
  wald <- round(summary(fit1)$coefficients[,3]^2, 3)
  
  # 计算 B 值并保留三位小数
  valueB <- round(coef(fit1), 3)
  
  # 计算 SE 值并保留三位小数
  SE <- round(summary(fit1)$coefficients[,2], 3)
  
  # OR值
  valueOR <- exp(coef(fit1))
  # OR值得95%CI
  confitOR <- exp(confint(fit1))
  
  data.frame(
    B = formatC(valueB, format = "f", digits = 3),
    SE = formatC(SE, format = "f", digits = 3),
    Wald = formatC(wald, format = "f", digits = 3),
    OR_with_CI = paste(formatC(valueOR, format = "f", digits = 3), "(", 
                       formatC(confitOR[,1], format = "f", digits = 3), "~", 
                       formatC(confitOR[,2], format = "f", digits = 3), ")", 
                       sep = ""),
    P = format.pval(p, digits = 3, eps = 0.001)
  )
}
# 定义函数进行逻辑回归分析并保存结果
runLogisticRegression <- function(data, response, predictors, direction = "both") {
  # 构建公式
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  
  # 进行逻辑回归
  fit1 <- glm(formula, data = data, family = binomial(link = "logit"))
  
  # 进行逐步回归
  step_fit <- stepAIC(fit1, direction = direction, trace = FALSE)
  
  # 格式化结果
  lj <- formatFit(step_fit)
  lj$variable <- rownames(lj)
  lj <- lj[, c(6, 1:5)]
  
  # 转换为flextable并保存为docx文件
  lj1 <- as_flextable(lj)
  save_as_docx(lj1, path = "logistic.docx")
  
  return(lj1)
}

# 使用您的数据和变量
# data是您的数据框，response是因变量名，predictors是自变量名向量
response <- "Treatment"  # 替换为您的因变量名
predictors <- c('`N/L`', '`platelet/D-Dimer`','`Venous phase`','`Arterial phase`')  # 替换为您的自变量名列表

# 使用向后逐步回归
result_backward <- runLogisticRegression(data, response, predictors, direction = "backward")
# 使用向前逐步回归
result_forward <- runLogisticRegression(data, response, predictors, direction = "forward")
# 使用双向逐步回归
result_both <- runLogisticRegression(data, response, predictors, direction = "both")
