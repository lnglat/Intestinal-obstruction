# #Intestinal-obstruction
# 项目背景
早期识别绞窄的挑战在于临床表现可能在不可逆损伤发生后才显现。因此，虽然保守治疗通常是在没有缺血或穿孔的情况下开始的，但手术干预的时机仍然是一个有争议的问题.本项目旨在研究评估肠梗阻患者中性粒细胞与淋巴细胞比值、血小板与D-二聚体比值及CT值在预测是否需要早期手术干预的作用.

# 技术栈
**编程语言**: [R](https://www.r-project.org/)
**主要R包/框架**:
数据处理与操作:
dplyr - 提供 %>% 管道操作符及 select()、filter()、mutate() 等数据变换函数，是数据清洗的核心。
readxl - 用于无缝导入 Excel (.xlsx, .xls) 文件，无需依赖外部环境。

统计分析与建模:
MASS - 提供大量的统计函数，如线性判别分析 (lda())、稳健线性回归 (rlm()) 等，是经典统计工具箱。
psych - 专注于心理测量学，但广泛应用于一般数据分析，提供描述性统计、因子分析、信度分析 (alpha()) 等功能。

可视化与结果呈现:
pROC - 用于构建和可视化ROC曲线，并计算曲线下面积 (AUC)，是诊断试验性能评估的专用工具。
flextable - 用于生成和美化表格，尤其擅长将 R 数据框转换为 Word、PowerPoint 或 HTML 报告中的专业级表格。

报告自动化与导出:
officer - 与 flextable 配合使用，用于直接操作 Microsoft Word、PowerPoint 和 Excel 文档，实现分析报告的自动化生成

# 使用方法
使用R和SPSS进行基线数据整理与描述性统计；对具有统计学意义的指标进行相关性分析构建二元logistic回归模型识别手术干预的独立预测因子；使用ROC曲线分析评估模型判别能力

#  结果
<img width="444" height="425" alt="image" src="https://github.com/user-attachments/assets/600966b0-6082-454e-acdf-94166cc11635" />
<img width="1311" height="323" alt="image" src="https://github.com/user-attachments/assets/ec343e50-0251-48ec-829f-0af5a312e82e" />
<img width="956" height="605" alt="image" src="https://github.com/user-attachments/assets/5de81a84-d625-467c-be9c-2f2f4d264ebf" />
NLR、platelet/D-Dimer、Venous phase的CT值能够提供更为精确且全面的评估，为临床上治疗肠梗阻患者提供恰当的方案。这些指标联合应用还能在预测肠道缺血方面展现出更佳性能。‘Platelet/D-Dimer + Venous phase’模型3在诊断性能上优于其他模型，具有最高的敏感性（0.738）和较高的特异性（0.745），以及最高的AUC（0.779），表明其综合表现最佳。





