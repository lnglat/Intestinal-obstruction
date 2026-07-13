library(pROC)
library(readxl)
setwd('E:/R/fb')
data <- read_excel("C:/Users/lenovo/Desktop/dataR.xlsx")

# 生成预测曲线
roc1<-roc(data$Treatment,data$`N/L`)
roc2<-roc(data$Treatment,data$`platelet/D-Dimer`)
roc3<-roc(data$Treatment,data$`Venous phase`)

# 计算AUC及其95%置信区间
auc1 <- auc(roc1)
auc2 <- auc(roc2)
auc3 <- auc(roc3)

ci_auc1 <- ci.auc(roc1)
ci_auc2 <- ci.auc(roc2)
ci_auc3 <- ci.auc(roc3)

# 打印AUC及其95%置信区间
print(auc1)
print(auc2)
print(auc3)
print(ci_auc1)
print(ci_auc2)
print(ci_auc3)

# 绘制曲线
plot(1 - roc1$specificities, roc1$sensitivities, type = 'l', col = 'red', lty = 1, lwd = 2, xlab = '1-specificity', ylab = 'sensitivity')
lines(1 - roc2$specificities, roc2$sensitivities, type = 'l', col = 'blue', lty = 1, lwd = 2, xlab = '1-specificity', ylab = 'sensitivity')
lines(1 - roc3$specificities, roc3$sensitivities, type = 'l', col = 'green', lty = 1, lwd = 2, xlab = '1-specificity', ylab = 'sensitivity')
abline(0, 1) # 设置参考线

# 设置图例
legend1<-paste('N/L AUC=',round(auc1,3))
legend2<-paste('platelet/D-Dimer AUC=',round(auc2,3))
legend3<-paste('pVenous phase AUC=',round(auc3,3))
legend(x=0.65,y=0.3,legend=c(legend1,legend2,legend3),fill = c('red', 'blue', 'green'),cex=0.8)

# 计算特异度、灵敏度、PPV和NPV及其95%置信区间
ci_coords1 <- ci.coords(roc1, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)
ci_coords2 <- ci.coords(roc2, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)
ci_coords3 <- ci.coords(roc3, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)

# 打印特异度、灵敏度、PPV和NPV及其95%置信区间
print(paste("N/L - Sensitivity:", round(ci_coords1$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords1$sensitivity[1], 3), "-", round(ci_coords1$sensitivity[3], 3)),
            "Specificity:", round(ci_coords1$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords1$specificity[1], 3), "-", round(ci_coords1$specificity[3], 3)),
            "PPV:", round(ci_coords1$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords1$ppv[1], 3), "-", round(ci_coords1$ppv[3], 3)),
            "NPV:", round(ci_coords1$npv[2], 3), 
            "95% CI:", paste(round(ci_coords1$npv[1], 3), "-", round(ci_coords1$npv[3], 3))))

print(paste("Platelet/D-Dimer - Sensitivity:", round(ci_coords2$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords2$sensitivity[1], 3), "-", round(ci_coords2$sensitivity[3], 3)),
            "Specificity:", round(ci_coords2$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords2$specificity[1], 3), "-", round(ci_coords2$specificity[3], 3)),
            "PPV:", round(ci_coords2$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords2$ppv[1], 3), "-", round(ci_coords2$ppv[3], 3)),
            "NPV:", round(ci_coords2$npv[2], 3), 
            "95% CI:", paste(round(ci_coords2$npv[1], 3), "-", round(ci_coords2$npv[3], 3))))

print(paste("Venous phase - Sensitivity:", round(ci_coords3$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords3$sensitivity[1], 3), "-", round(ci_coords3$sensitivity[3], 3)),
            "Specificity:", round(ci_coords3$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords3$specificity[1], 3), "-", round(ci_coords3$specificity[3], 3)),
            "PPV:", round(ci_coords3$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords3$ppv[1], 3), "-", round(ci_coords3$ppv[3], 3)),
            "NPV:", round(ci_coords3$npv[2], 3), 
            "95% CI:", paste(round(ci_coords3$npv[1], 3), "-", round(ci_coords3$npv[3], 3))))
# 将特异度和灵敏度加入图例
#legend1 <- paste('N/L\nAUC=', round(auc1, 3), '\nSensitivity=', round(coords1["sensitivity"], 3), '\nSpecificity=', round(coords1["specificity"], 3))
#legend2 <- paste('Platelet/D-Dimer\nAUC=', round(auc2, 3), '\nSensitivity=', round(coords2["sensitivity"], 3), '\nSpecificity=', round(coords2["specificity"], 3))
#legend3 <- paste('Venous phase\nAUC=', round(auc3, 3), '\nSensitivity=', round(coords3["sensitivity"], 3), '\nSpecificity=', round(coords3["specificity"], 3))

# 绘制图例
#legend(x = 0.6, y = 0.4, legend = c(legend1, legend2, legend3), col = c('red', 'blue', 'green'), lty = 1, cex = 0.8)

# 计算交互项
data$interaction_1_2 <- data$`N/L` * data$`platelet/D-Dimer`
data$interaction_1_3 <- data$`N/L` * data$`Venous phase`
data$interaction_2_3 <- data$`platelet/D-Dimer` * data$`Venous phase`
data$interaction_all <- data$`N/L` * data$`platelet/D-Dimer` * data$`Venous phase`

# 生成交互项的ROC曲线
roc_interaction_1_2 <- roc(data$Treatment, data$interaction_1_2)
roc_interaction_1_3 <- roc(data$Treatment, data$interaction_1_3)
roc_interaction_2_3 <- roc(data$Treatment, data$interaction_2_3)
roc_interaction_all <- roc(data$Treatment, data$interaction_all)

# 计算交互项的AUC及其95%置信区间
auc_interaction_1_2 <- auc(roc_interaction_1_2)
auc_interaction_1_3 <- auc(roc_interaction_1_3)
auc_interaction_2_3 <- auc(roc_interaction_2_3)
auc_interaction_all <- auc(roc_interaction_all)

ci_auc_interaction_1_2 <- ci.auc(roc_interaction_1_2)
ci_auc_interaction_1_3 <- ci.auc(roc_interaction_1_3)
ci_auc_interaction_2_3 <- ci.auc(roc_interaction_2_3)
ci_auc_interaction_all <- ci.auc(roc_interaction_all)

# 打印交互项的AUC及其95%置信区间
print(auc_interaction_1_2)
print(auc_interaction_1_3)
print(auc_interaction_2_3)
print(auc_interaction_all)
print(ci_auc_interaction_1_2)
print(ci_auc_interaction_1_3)
print(ci_auc_interaction_2_3)
print(ci_auc_interaction_all)

# 计算交互项的特异度、灵敏度、PPV和NPV及其95%置信区间
ci_coords_interaction_1_2 <- ci.coords(roc_interaction_1_2, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)
ci_coords_interaction_1_3 <- ci.coords(roc_interaction_1_3, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)
ci_coords_interaction_2_3 <- ci.coords(roc_interaction_2_3, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)
ci_coords_interaction_all <- ci.coords(roc_interaction_all, "best", ret = c("sensitivity", "specificity", "ppv", "npv"), conf.level = 0.95)

# 打印交互项的特异度、灵敏度、PPV和NPV及其95%置信区间
print(paste("N/L * Platelet/D-Dimer - Sensitivity:", round(ci_coords_interaction_1_2$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_2$sensitivity[1], 3), "-", round(ci_coords_interaction_1_2$sensitivity[3], 3)),
            "Specificity:", round(ci_coords_interaction_1_2$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_2$specificity[1], 3), "-", round(ci_coords_interaction_1_2$specificity[3], 3)),
            "PPV:", round(ci_coords_interaction_1_2$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_2$ppv[1], 3), "-", round(ci_coords_interaction_1_2$ppv[3], 3)),
            "NPV:", round(ci_coords_interaction_1_2$npv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_2$npv[1], 3), "-", round(ci_coords_interaction_1_2$npv[3], 3))))

print(paste("N/L * Venous phase - Sensitivity:", round(ci_coords_interaction_1_3$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_3$sensitivity[1], 3), "-", round(ci_coords_interaction_1_3$sensitivity[3], 3)),
            "Specificity:", round(ci_coords_interaction_1_3$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_3$specificity[1], 3), "-", round(ci_coords_interaction_1_3$specificity[3], 3)),
            "PPV:", round(ci_coords_interaction_1_3$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_3$ppv[1], 3), "-", round(ci_coords_interaction_1_3$ppv[3], 3)),
            "NPV:", round(ci_coords_interaction_1_3$npv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_1_3$npv[1], 3), "-", round(ci_coords_interaction_1_3$npv[3], 3))))

print(paste("Platelet/D-Dimer * Venous phase - Sensitivity:", round(ci_coords_interaction_2_3$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_2_3$sensitivity[1], 3), "-", round(ci_coords_interaction_2_3$sensitivity[3], 3)),
            "Specificity:", round(ci_coords_interaction_2_3$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_2_3$specificity[1], 3), "-", round(ci_coords_interaction_2_3$specificity[3], 3)),
            "PPV:", round(ci_coords_interaction_2_3$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_2_3$ppv[1], 3), "-", round(ci_coords_interaction_2_3$ppv[3], 3)),
            "NPV:", round(ci_coords_interaction_2_3$npv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_2_3$npv[1], 3), "-", round(ci_coords_interaction_2_3$npv[3], 3))))

print(paste("N/L * Platelet/D-Dimer * Venous phase - Sensitivity:", round(ci_coords_interaction_all$sensitivity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_all$sensitivity[1], 3), "-", round(ci_coords_interaction_all$sensitivity[3], 3)),
            "Specificity:", round(ci_coords_interaction_all$specificity[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_all$specificity[1], 3), "-", round(ci_coords_interaction_all$specificity[3], 3)),
            "PPV:", round(ci_coords_interaction_all$ppv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_all$ppv[1], 3), "-", round(ci_coords_interaction_all$ppv[3], 3)),
            "NPV:", round(ci_coords_interaction_all$npv[2], 3), 
            "95% CI:", paste(round(ci_coords_interaction_all$npv[1], 3), "-", round(ci_coords_interaction_all$npv[3], 3))))




#我需要将上述print的结果以表格的形式全部输出到一个word中，表格第一行的行名分为~,第一列列名为~
library(flextable)
library(officer)

# 创建数据框保存结果
results <- data.frame(
  Metric = c("N/L", "Platelet/D-Dimer", "Venous phase", 
             "N/L * Platelet/D-Dimer", "N/L * Venous phase", 
             "Platelet/D-Dimer * Venous phase", 
             "N/L * Platelet/D-Dimer * Venous phase"),
  Sensitivity = c(
    paste(round(ci_coords1$sensitivity[2], 3), " (", round(ci_coords1$sensitivity[1], 3), "-", round(ci_coords1$sensitivity[3], 3), ")", sep=""),
    paste(round(ci_coords2$sensitivity[2], 3), " (", round(ci_coords2$sensitivity[1], 3), "-", round(ci_coords2$sensitivity[3], 3), ")", sep=""),
    paste(round(ci_coords3$sensitivity[2], 3), " (", round(ci_coords3$sensitivity[1], 3), "-", round(ci_coords3$sensitivity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_2$sensitivity[2], 3), " (", round(ci_coords_interaction_1_2$sensitivity[1], 3), "-", round(ci_coords_interaction_1_2$sensitivity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_3$sensitivity[2], 3), " (", round(ci_coords_interaction_1_3$sensitivity[1], 3), "-", round(ci_coords_interaction_1_3$sensitivity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_2_3$sensitivity[2], 3), " (", round(ci_coords_interaction_2_3$sensitivity[1], 3), "-", round(ci_coords_interaction_2_3$sensitivity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_all$sensitivity[2], 3), " (", round(ci_coords_interaction_all$sensitivity[1], 3), "-", round(ci_coords_interaction_all$sensitivity[3], 3), ")", sep="")
  ),
  Specificity = c(
    paste(round(ci_coords1$specificity[2], 3), " (", round(ci_coords1$specificity[1], 3), "-", round(ci_coords1$specificity[3], 3), ")", sep=""),
    paste(round(ci_coords2$specificity[2], 3), " (", round(ci_coords2$specificity[1], 3), "-", round(ci_coords2$specificity[3], 3), ")", sep=""),
    paste(round(ci_coords3$specificity[2], 3), " (", round(ci_coords3$specificity[1], 3), "-", round(ci_coords3$specificity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_2$specificity[2], 3), " (", round(ci_coords_interaction_1_2$specificity[1], 3), "-", round(ci_coords_interaction_1_2$specificity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_3$specificity[2], 3), " (", round(ci_coords_interaction_1_3$specificity[1], 3), "-", round(ci_coords_interaction_1_3$specificity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_2_3$specificity[2], 3), " (", round(ci_coords_interaction_2_3$specificity[1], 3), "-", round(ci_coords_interaction_2_3$specificity[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_all$specificity[2], 3), " (", round(ci_coords_interaction_all$specificity[1], 3), "-", round(ci_coords_interaction_all$specificity[3], 3), ")", sep="")
  ),
  PPV = c(
    paste(round(ci_coords1$ppv[2], 3), " (", round(ci_coords1$ppv[1], 3), "-", round(ci_coords1$ppv[3], 3), ")", sep=""),
    paste(round(ci_coords2$ppv[2], 3), " (", round(ci_coords2$ppv[1], 3), "-", round(ci_coords2$ppv[3], 3), ")", sep=""),
    paste(round(ci_coords3$ppv[2], 3), " (", round(ci_coords3$ppv[1], 3), "-", round(ci_coords3$ppv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_2$ppv[2], 3), " (", round(ci_coords_interaction_1_2$ppv[1], 3), "-", round(ci_coords_interaction_1_2$ppv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_3$ppv[2], 3), " (", round(ci_coords_interaction_1_3$ppv[1], 3), "-", round(ci_coords_interaction_1_3$ppv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_2_3$ppv[2], 3), " (", round(ci_coords_interaction_2_3$ppv[1], 3), "-", round(ci_coords_interaction_2_3$ppv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_all$ppv[2], 3), " (", round(ci_coords_interaction_all$ppv[1], 3), "-", round(ci_coords_interaction_all$ppv[3], 3), ")", sep="")
  ),
  NPV = c(
    paste(round(ci_coords1$npv[2], 3), " (", round(ci_coords1$npv[1], 3), "-", round(ci_coords1$npv[3], 3), ")", sep=""),
    paste(round(ci_coords2$npv[2], 3), " (", round(ci_coords2$npv[1], 3), "-", round(ci_coords2$npv[3], 3), ")", sep=""),
    paste(round(ci_coords3$npv[2], 3), " (", round(ci_coords3$npv[1], 3), "-", round(ci_coords3$npv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_2$npv[2], 3), " (", round(ci_coords_interaction_1_2$npv[1], 3), "-", round(ci_coords_interaction_1_2$npv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_1_3$npv[2], 3), " (", round(ci_coords_interaction_1_3$npv[1], 3), "-", round(ci_coords_interaction_1_3$npv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_2_3$npv[2], 3), " (", round(ci_coords_interaction_2_3$npv[1], 3), "-", round(ci_coords_interaction_2_3$npv[3], 3), ")", sep=""),
    paste(round(ci_coords_interaction_all$npv[2], 3), " (", round(ci_coords_interaction_all$npv[1], 3), "-", round(ci_coords_interaction_all$npv[3], 3), ")", sep="")
  ),
  AUC = c(
    paste(round(auc1, 3), " (", round(ci_auc1[1], 3), "-", round(ci_auc1[3], 3), ")", sep=""),
    paste(round(auc2, 3), " (", round(ci_auc2[1], 3), "-", round(ci_auc2[3], 3), ")", sep=""),
    paste(round(auc3, 3), " (", round(ci_auc3[1], 3), "-", round(ci_auc3[3], 3), ")", sep=""),
    paste(round(auc_interaction_1_2, 3), " (", round(ci_auc_interaction_1_2[1], 3), "-", round(ci_auc_interaction_1_2[3], 3), ")", sep=""),
    paste(round(auc_interaction_1_3, 3), " (", round(ci_auc_interaction_1_3[1], 3), "-", round(ci_auc_interaction_1_3[3], 3), ")", sep=""),
    paste(round(auc_interaction_2_3, 3), " (", round(ci_auc_interaction_2_3[1], 3), "-", round(ci_auc_interaction_2_3[3], 3), ")", sep=""),
    paste(round(auc_interaction_all, 3), " (", round(ci_auc_interaction_all[1], 3), "-", round(ci_auc_interaction_all[3], 3), ")", sep="")
  )
)

# 转置数据框以符合表格格式
results_t <- as.data.frame(t(results))
colnames(results_t) <- results$Metric
results_t <- results_t[-1,]

# 创建表格
ft <- flextable(results_t)
ft <- add_header_row(ft, values = c(" ", colnames(results_t)), colwidths = c(1, rep(1, ncol(results_t))))

# 保存到Word
doc <- read_docx() %>% 
  body_add_flextable(value = ft)

print(doc, target = "E:/R/fb/ROC_Results.docx")
