library(readxl)
data <- read_excel("C:/Users/lenovo/Desktop/dataR.xlsx")
setwd('E:/R/fb')


library(psych)
library(dplyr)
library(officer)
library(flextable)

# 选择响应变量和预测变量的索引
response_var <- "Treatment" # 响应变量的索引
predictor_vars <- c('N/L', 'platelet/D-Dimer','Venous phase','Arterial phase')  # 预测变量的索引

# 使用 dplyr 选择列
selected_data <- data %>% select(any_of(c(response_var, predictor_vars)))

# 进行相关性检验
res <- corr.test(selected_data[, predictor_vars], selected_data[, response_var], use="pairwise",
                 method = "pearson", adjust = "holm",
                 alpha=0.05)

# 获取相关系数矩阵和 p 值矩阵
cmt <- res$r
pmt <- res$p

# 转换为数据框
cmt <- as.data.frame(cmt)
pmt <- as.data.frame(pmt)

# 提取变量名称
predictor_names <- colnames(selected_data)[-1] 
response_name <- colnames(selected_data)[1]

# 将相关系数和 p 值格式化
result_table <- data.frame(
  变量 = predictor_names,
  相关系数 = round(cmt[, 1], 3),
  p值 = format(round(pmt[, 1], 3), scientific = FALSE)
)

# 打印结果表格
print(result_table)

# 创建Word文档
doc <- read_docx()

# 将结果表格转换为flextable
ft <- flextable(result_table)

# 添加表格到文档
doc <- body_add_flextable(doc, value = ft)

# 保存文档
print(doc, target = "相关性分析.docx")
