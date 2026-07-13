#install.packages("table1")
#install.packages("boot")
#install.packages("readxl")
library(table1) 
library(boot)
library(readxl)
data <- read_excel("C:/Users/lenovo/Desktop/dataR.xlsx")
setwd('E:/R/fb')
# 修改变量名称
data$Treatment <- as.factor(data$Treatment)
data$Treatment <- 
  factor(data$Treatment, 
         levels=c(1,0),
         labels=c("surgical treatment", 
                  "Conservative treatment"))
data$sex <- as.factor(data$sex)
data$sex <- 
  factor(data$sex, 
         levels=c(1,2),
         labels=c("Male", 
                  "female sex"))
data$`Surgical history` <- as.factor(data$`Surgical history`)
data$`Surgical history` <- 
  factor(data$`Surgical history`, 
         levels=c(0,1),
         labels=c("no", 
                  "yes"))


# units()添加单位
units(data$age)       <- "years"
units(data$`Intestinal diameter`)       <- "mm"
units(data$`Intestinal wall thickness`)       <- "mm"

#自我定义所需要的统计量
my.render.cont <- function(x) {
  normality_test <- shapiro.test(x)
  
  if (normality_test$p.value > 0.05) {
    # If normally distributed, use Mean ± SD
    mean_value <- mean(x, na.rm = TRUE)
    sd_value <- sd(x, na.rm = TRUE)
    c("", "Mean (SD)" = sprintf("%.2f (&plusmn; %.2f)", mean_value, sd_value))
  } else {
    # If not normally distributed, use Mean (P25 - P75)
    mean_value <- mean(x, na.rm = TRUE)
    p25 <- quantile(x, 0.25, na.rm = TRUE)
    p75 <- quantile(x, 0.75, na.rm = TRUE)
    c("", "Mean (P25 - P75)" = sprintf("%.2f (%.2f - %.2f)", mean_value, p25, p75))
  }
}
#算P值
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    # For numeric variables, first test for normality
    normality_test <- shapiro.test(y)
    
    if (normality_test$p.value > 0.05) {
      # If normally distributed, use one-way ANOVA
      p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
    } else {
      # If not normally distributed, use Wilcoxon rank sum test (U test)
      p <- kruskal.test(y ~ g)$p.value
    }
  } else {
    # For categorical variables, use Chi-squared test
    p <- chisq.test(table(y, g))$p.value
  }
  
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

#输出表格
table1(~ sex + age + Neutrophils + lymphocyte + `N/L` + CRP +
         `platelet` + `D-Dimer` + FIB + `FIB/D-Dimer` + `platelet/D-Dimer` +
         `Intestinal diameter` + `Intestinal wall thickness` +
         `CT value` + `Arterial phase` + `Venous phase` | Treatment, 
       data = data, overall="Total", render.continuous = my.render.cont, extra.col=list(`P-value`=pvalue))


