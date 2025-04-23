# ========================
# SCM稳健性检验分析脚本
# 生成日期：2024-06-05
# 最后检查：2025-03-02
# ========================

# 初始化设置 ---------------------------------------------------------------
# 设置图形输出为PDF格式
#pdf.options(width = 8, height = 6, paper = "a4")
#options(device = "pdf")

# 加载必要库 ---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(gsynth)
library(panelView)

# 数据准备 ----------------------------------------------------------------
# 加载主数据集
data_NMVOC <- read_rds("~/science-ynn-NMVOC/regression_input_ynn.RDS") #下面NOX对应2处，分别是生成ac1和ac6

# 加载自定义函数
source("~/science-code/00_oecd_project_functions.R")

# 定义样本分组 ------------------------------------------------------------
AC1 <- c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Croatia",        
         "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany",        
         "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Japan",         
         "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "New Zealand",
         "Norway", "Poland", "Portugal", "Romania", "Slovak Republic", "Slovenia",
         "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

AC6 <- c("Argentina", "Brazil", "Chile", "Colombia", "China", "Costa Rica", "India",
         "Indonesia", "Mexico", "Peru", "Russia", "Saudi Arabia", "South Africa",
         "South Korea", "Turkey")

# 数据预处理 ---------------------------------------------------------------
# AC1子集 (2000-2021)
ac1 <- data_NMVOC %>% 
  subset(country %in% AC1) %>% 
  subset(year >= 2000)

# AC6子集 (2000-2021)
ac6 <- data_NMVOC %>% 
  subset(country %in% AC6) %>% 
  subset(year >= 2000)

# 控制变量定义 -------------------------------------------------------------
buildings_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"
electricity_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_E_2005 + ETS_E_2018"
industry_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"
transport_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + MEPS_T_2009"

# 电力部门分析 - AC1 ------------------------------------------------------
#pdf("~/science-ynn/electricity_analysis_ac1.pdf", width = 10, height = 8)  # 开启PDF设备
pdf("~/science-ynn-NMVOC/electricity_analysis_ac1.pdf")
# 数据准备
data_ac1 <- ac1

# 创建处理变量
data_ac1$treated <- 0
data_ac1$treated <- ifelse(data_ac1$country == "Czech Republic" & data_ac1$year >= 2005, 1, data_ac1$treated)
data_ac1$treated <- ifelse(data_ac1$country == "France" & data_ac1$year >= 2011, 1, data_ac1$treated)
data_ac1$treated <- ifelse(data_ac1$country == "Germany" & data_ac1$year >= 2017, 1, data_ac1$treated)
data_ac1$treated <- ifelse(data_ac1$country == "Hungary" & data_ac1$year >= 2012, 1, data_ac1$treated)

data_ac1$treated <- ifelse(data_ac1$country == "Japan" & data_ac1$year >= 2017, 1, data_ac1$treated)
data_ac1$treated <- ifelse(data_ac1$country == "Netherlands" & data_ac1$year >= 2013, 1, data_ac1$treated)

data_ac1$treated <- ifelse(data_ac1$country == "Norway" & data_ac1$year >= 2020, 1, data_ac1$treated)
data_ac1$treated <- ifelse(data_ac1$country == "Portugal" & data_ac1$year >= 2006, 1, data_ac1$treated)
data_ac1$treated <- ifelse(data_ac1$country == "Slovak Republic" & data_ac1$year >= 2005, 1, data_ac1$treated)

#data_ac1$treated <- ifelse(data_ac1$country == "United Kingdom" & data_ac1$year >= 2015, 1, data_ac1$treated)  ##co2的
#data_ac1$treated <- ifelse(data_ac1$country == "Sweden" & data_ac1$year >= 2005, 1, data_ac1$treated)
#data_ac1$treated <- ifelse(data_ac1$country == "Portugal" & data_ac1$year >= 2019, 1, data_ac1$treated)
#data_ac1$treated <- ifelse(data_ac1$country == "Norway" & data_ac1$year >= 2012, 1, data_ac1$treated)
#data_ac1$treated <- ifelse(data_ac1$country == "New Zealand" & data_ac1$year >= 2009, 1, data_ac1$treated)

# 可视化处理状态
panelview(electricity_heat_NMVOC ~ treated, #
          data = data_ac1,  
          index = c("country", "year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)

# 运行gsynth模型
system.time(
  out_ac1 <- gsynth(
    log_electricity_heat_NMVOC ~ treated + hdd + cdd + lgdp + lpop + lgdp_sq + ETS_E_2005 + ETS_E_2018,
    data = data_ac1,
    index = c("country", "year"),
    force = "two-way", #包含个体和时间双固定效应
    CV = TRUE,
    r = c(0, 3), #允许0-3个潜在因子（数据驱动选择最优数）
    se = TRUE,
    inference = "parametric",
    nboots = 1000, #Bootstrap次数，影响置信区间精度
    parallel = FALSE,
    estimator = "ife"
  )
)

# 生成分析图表
countries <- c("Czech Republic", "France", "Germany", "Hungary", "Japan", "Netherlands", 
               "Norway", "Portugal", "Slovak Republic")
# 循环生成图表
for (country in countries) {
  # 生成处理效应图
  plot(out_ac1, 
       type = "gap", 
       id = country, 
       main = paste(country, "Gap (T =", 
                    unique(data$year[data$country == country & data$treated == 1]), ")"), set.par = FALSE,
       cex.main = 3,        # 主标题放大1.5倍
       ylab = "Gap",
       cex.lab = 3,         # 覆盖全局设置确保生效
       cex.axis = 3)
  
  # 生成反事实对照图
  plot(out_ac1, 
       type = "ct", 
       id = country, 
       main = paste(country, "Counterfactual"), set.par = FALSE,
       cex.main = 3,
       xlab = "Year",
       ylab = "Counterfactual Value",
       cex.lab = 3,
       cex.axis = 3)
}

dev.off()  # 关闭图形设备

# 电力部门分析 - AC6 ------------------------------------------------------
#pdf("~/science-ynn/electricity_analysis_ac6_1.pdf", width = 10, height = 8)  # 开启PDF设备,设置会表小
pdf("~/science-ynn-NMVOC/electricity_analysis_ac6.pdf")
# 数据准备
data_ac6 <- ac6

# 创建处理变量
data_ac6$treated <- 0
data_ac6$treated <- ifelse(data_ac6$country == "Brazil" & data_ac6$year >= 2016, 1, data_ac6$treated)
data_ac6$treated <- ifelse(data_ac6$country == "Chile" & data_ac6$year >= 2010, 1, data_ac6$treated)
data_ac6$treated <- ifelse(data_ac6$country == "Indonesia" & data_ac6$year >= 2009, 1, data_ac6$treated)
data_ac6$treated <- ifelse(data_ac6$country == "Peru" & data_ac6$year >= 2006, 1, data_ac6$treated)
data_ac6$treated <- ifelse(data_ac6$country == "Turkey" & data_ac6$year >= 2015, 1, data_ac6$treated)
# data_ac6$treated <- ifelse(data_ac6$country == "Brazil" & data_ac6$year >= 2016, 1, data_ac6$treated)
# data_ac6$treated <- ifelse(data_ac6$country == "Colombia" & data_ac6$year >= 2011, 1, data_ac6$treated)

# 可视化处理状态
panelview(log_electricity_heat_NMVOC ~ treated, #NOX
          data = data_ac6,  
          index = c("country", "year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)

# 运行gsynth模型
system.time(
  out_ac6 <- gsynth(
    log_electricity_heat_NMVOC ~ treated + hdd + cdd + lgdp + lpop + lgdp_sq, #NOX
    data = data_ac6,
    index = c("country", "year"),
    force = "two-way",
    CV = TRUE,
    r = c(0, 3),
    se = TRUE,
    inference = "parametric",
    nboots = 1000,
    parallel = FALSE,
    estimator = "ife"
  )
)

# 生成分析图表
# 生成分析图表
countries <- c("Brazil", "Chile", "Indonesia", "Peru", "Turkey")
# 循环生成图表
for (country in countries) {
  # 生成处理效应图
  plot(out_ac6, 
       type = "gap", 
       id = country, 
       main = paste(country, "Gap (T =", 
                    unique(data$year[data$country == country & data$treated == 1]), ")"), set.par = FALSE,
       cex.main = 3,        # 主标题放大1.5倍
       ylab = "Gap",
       cex.lab = 3,         # 覆盖全局设置确保生效
       cex.axis = 3)
  
  # 生成反事实对照图
  plot(out_ac6, 
       type = "ct", 
       id = country, 
       main = paste(country, "Counterfactual"), set.par = FALSE,
       cex.main = 3,
       xlab = "Year",
       ylab = "Counterfactual Value",
       cex.lab = 3,
       cex.axis = 3)
}

dev.off()  # 关闭PDF设备

# 会话信息 ----------------------------------------------------------------
# sessionInfo()
#
# 工业部门分析--发达 AC1
pdf("~/science-ynn-NMVOC/Industry_analysis_ac1.pdf")  # 开启PDF设备
## AC1数据预处理
data <- ac1
data$treated <- 0
data$treated <- ifelse(data$country == "Canada" & data$year >= 2008, 1, data$treated)
data$treated <- ifelse(data$country == "Czech Republic" & data$year >= 2015, 1, data$treated)
data$treated <- ifelse(data$country == "Denmark" & data$year >= 2005, 1, data$treated)
data$treated <- ifelse(data$country == "Hungary" & data$year >= 2005, 1, data$treated)

data$treated <- ifelse(data$country == "Netherlands" & data$year >= 2013, 1, data$treated)
data$treated <- ifelse(data$country == "Portugal" & data$year >= 2017, 1, data$treated)
data$treated <- ifelse(data$country == "Romania" & data$year >= 2007, 1, data$treated)
data$treated <- ifelse(data$country == "Sweden" & data$year >= 2007, 1, data$treated)

#data$treated <- ifelse(data$country == "Czech Republic" & data$year >= 2010, 1, data$treated)
#data$treated <- ifelse(data$country == "France" & data$year >= 2017, 1, data$treated)
#data$treated <- ifelse(data$country == "Ireland" & data$year >= 2009, 1, data$treated)
#data$treated <- ifelse(data$country == "Italy" & data$year >= 2013, 1, data$treated)
#data$treated <- ifelse(data$country == "Romania" & data$year >= 2009, 1, data$treated)

## 可视化预处理：绘图重点展示 treated 的处理状态（0/1）及其时间分布，
#但此处因变量（log_industry_NOX）仅用于标识数据范围，不参与绘图逻辑。
panelview(log_industry_NMVOC ~ treated, data = data,  #
          index = c("country","year"), #定义面板数据的单位和时间维度
          pre.post = TRUE, #区分处理前（Pre-Treatment）和处理后（Post-Treatment）阶段。
          axis.adjust = TRUE,#调整y轴（国家名称）的字体大小。
          cex.axis.y = 8)

## 模型拟合
system.time(
  out <- gsynth(log_industry_NMVOC ~ treated +
                  lgdp + lpop + lgdp_sq + hdd + cdd + 
                  ETS_I_2005 + ETS_I_2018 + 
                  MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015,
                data = data, 
                index = c("country","year"), force = "two-way", 
                CV = TRUE, r = c(0, 3), se = TRUE, 
                inference = "parametric", nboots = 1000, 
                parallel = FALSE, estimator = "ife")
)

## 结果可视化（并排显示两张图）
#par(mfrow = c(1,2), cex.axis = 3, font.axis = 5) # 设置画布布局
## 结果可视化（并排显示两张图）
countries <- c("Canada", "Czech Republic", "Denmark", "Hungary", "Netherlands", "Portugal", "Romania","Sweden")
# 循环生成图表
for (country in countries) {
  # 生成处理效应图
  plot(out, 
       type = "gap", 
       id = country, 
       main = paste(country, "Gap (T =", 
                    unique(data$year[data$country == country & data$treated == 1]), ")"), set.par = FALSE,
       cex.main = 3,        # 主标题放大1.5倍
       ylab = "Gap",
       cex.lab = 3,         # 覆盖全局设置确保生效
       cex.axis = 3)
  
  # 生成反事实对照图
  plot(out, 
       type = "ct", 
       id = country, 
       main = paste(country, "Counterfactual"), set.par = FALSE,
       cex.main = 3,
       xlab = "Year",
       ylab = "Counterfactual Value",
       cex.lab = 3,
       cex.axis = 3)
}
dev.off()  # 关闭PDF设备

#工业部门发展======================= AC6 Analysis =======================#
## AC6数据预处理
pdf("~/science-ynn-NMVOC/Industry_analysis_ac6.pdf")
data <- ac6
data$treated <- 0
data$treated <- ifelse(data$country == "Chile" & data$year >= 2020, 1, data$treated)
data$treated <- ifelse(data$country == "China" & data$year >= 2017, 1, data$treated)
data$treated <- ifelse(data$country == "Indonesia" & data$year >= 2008, 1, data$treated)
data$treated <- ifelse(data$country == "South Africa" & data$year >= 2006, 1, data$treated)
data$treated <- ifelse(data$country == "Turkey" & data$year >= 2008, 1, data$treated)

## 可视化预处理
panelview(log_industry_NMVOC ~ treated, data = data,  
          index = c("country","year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)

## 模型拟合
system.time(
  out <- gsynth(log_industry_NMVOC ~ treated +
                  lgdp + lpop + lgdp_sq + hdd + cdd,
                data = data, 
                index = c("country","year"), force = "two-way", 
                CV = TRUE, r = c(0, 3), se = TRUE, 
                inference = "parametric", nboots = 1000, 
                parallel = FALSE, estimator = "ife")
)

## 结果可视化（并排显示两张图）
countries <- c("Chile","China", "Indonesia", "South Africa", "Turkey")
# 循环生成图表
for (country in countries) {
  # 生成处理效应图
  plot(out, 
       type = "gap", 
       id = country, 
       main = paste(country, "Gap (T =", 
                    unique(data$year[data$country == country & data$treated == 1]), ")"), set.par = FALSE,
       cex.main = 3,        # 主标题放大1.5倍
       ylab = "Gap",
       cex.lab = 3,         # 覆盖全局设置确保生效
       cex.axis = 3)
  
  # 生成反事实对照图
  plot(out, 
       type = "ct", 
       id = country, 
       main = paste(country, "Counterfactual"), set.par = FALSE,
       cex.main = 3,
       xlab = "Year",
       ylab = "Counterfactual Value",
       cex.lab = 3,
       cex.axis = 3)
}
dev.off()  # 关闭PDF设备

###交通部门======================= AC1 Analysis =======================
# Transport Sector AC1 Analysis Script
pdf("~/science-ynn-NMVOC/Transport_ac1.pdf")   # 坐标轴刻度加粗

# 加载数据（假设ac1数据已存在）
data <- ac1  # 请替换为实际数据加载代码

# 创建交错处理变量
data$treated <- 0

data$treated <- ifelse(data$country == "Australia" & data$year >= 2016, 1, data$treated)
data$treated <- ifelse(data$country == "Canada" & data$year >= 2017, 1, data$treated)
data$treated <- ifelse(data$country == "Czech Republic" & data$year >= 2015, 1, data$treated)
data$treated <- ifelse(data$country == "Hungary" & data$year >= 2011, 1, data$treated)
data$treated <- ifelse(data$country == "Ireland" & data$year >= 2011, 1, data$treated)
#
data$treated <- ifelse(data$country == "Italy" & data$year >= 2007, 1, data$treated)
data$treated <- ifelse(data$country == "Japan" & data$year >= 2016, 1, data$treated)
data$treated <- ifelse(data$country == "New Zealand" & data$year >= 2018, 1, data$treated)
data$treated <- ifelse(data$country == "Sweden" & data$year >= 2015, 1, data$treated)
data$treated <- ifelse(data$country == "United Kingdom" & data$year >= 2005, 1, data$treated)
data$treated <- ifelse(data$country == "United States" & data$year >= 2008, 1, data$treated)

#data$treated <- ifelse(data$country == "Austria" & data$year >= 2006, 1, data$treated)
#data$treated <- ifelse(data$country == "Denmark" & data$year >= 2020, 1, data$treated)
#data$treated <- ifelse(data$country == "Germany" & data$year >= 2005, 1, data$treated)
#data$treated <- ifelse(data$country == "Hungary" & data$year >= 2011, 1, data$treated)
#data$treated <- ifelse(data$country == "Ireland" & data$year >= 2015, 1, data$treated)
#data$treated <- ifelse(data$country == "Norway" & data$year >= 2016, 1, data$treated)
#data$treated <- ifelse(data$country == "Poland" & data$year >= 2013, 1, data$treated)
#data$treated <- ifelse(data$country == "Romania" & data$year >= 2005, 1, data$treated)
#data$treated <- ifelse(data$country == "Spain" & data$year >= 2009, 1, data$treated)
#data$treated <- ifelse(data$country == "Sweden" & data$year >= 2016, 1, data$treated)
#data$treated <- ifelse(data$country == "United States" & data$year >= 2008, 1, data$treated)

# 查看处理效应可视化
panelview(log_transport_NMVOC ~ treated, 
          data = data,  
          index = c("country","year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)

# 运行广义合成控制
system.time(
  out <- gsynth(log_transport_NMVOC ~ treated + hdd + cdd + lgdp + lpop + lgdp_sq + MEPS_T_2009,
    data = data, 
    index = c("country","year"), 
    force = "two-way", 
    CV = TRUE, 
    r = c(0, 3), 
    se = TRUE, 
    inference = "parametric", 
    nboots = 1000, 
    parallel = FALSE, 
    estimator = "ife"
  )
)

# 生成PDF报告
countries <- c("Australia", "Canada", "Czech Republic", "Hungary", "Ireland", "Italy",
               "Japan", "New Zealand", "Sweden", "United Kingdom", "United States")

# 配置PDF输出

# 循环生成图表
for (country in countries) {
  # 生成处理效应图
  plot(out, 
       type = "gap", 
       id = country, 
       main = paste(country, "Gap (T =", 
                    unique(data$year[data$country == country & data$treated == 1]), ")"),set.par = FALSE,
       cex.main = 5,        # 主标题放大1.5倍
       ylab = "Gap",
       cex.lab = 5,         # 覆盖全局设置确保生效
       cex.axis = 5)

  # 生成反事实对照图
  plot(out, 
       type = "ct", 
       id = country, 
       main = paste(country, "Counterfactual"), set.par = FALSE,
       cex.main = 5,
       xlab = "Year",
       ylab = "Counterfactual Value",
       cex.lab = 5,
       cex.axis = 5)
}

dev.off()  # 关闭图形设备

# 可选：保存模型结果
#saveRDS(out, "Transport_AC1_Model.rds")

###交通部门======================= # AC6 分析 ----------------------------------------------------------------
# 创建输出目录
#output_dir <- "~/science-ynn/transport"
# 加载数据
pdf("~/science-ynn-NMVOC/Transport_ac6.pdf")  # A4横向尺寸

data <- ac6

# 创建处理变量
data$treated <- 0

data$treated <- ifelse(data$country == "China" & data$year >= 2009, 1, data$treated)
data$treated <- ifelse(data$country == "Colombia" & data$year >= 2006, 1, data$treated)

#data$treated <- ifelse(data$country == "Colombia" & data$year >= 2008, 1, data$treated)
#data$treated <- ifelse(data$country == "Mexico" & data$year >= 2020, 1, data$treated)
#data$treated <- ifelse(data$country == "South Korea" & data$year >= 2008, 1, data$treated)

# 可视化处理状态（PNG格式）
panelview(log_transport_NMVOC ~ treated, 
          data = data,  
          index = c("country","year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)

# 运行广义合成控制模型
system.time(
  out <- gsynth(log_transport_NMVOC ~ treated + hdd + cdd + lgdp + lpop + lgdp_sq,
                data = data, 
                index = c("country","year"), 
                force = "two-way", 
                CV = TRUE, 
                r = c(0, 3), 
                se = TRUE, 
                inference = "parametric", 
                nboots = 1000, 
                parallel = FALSE, 
                estimator = "ife")
)

# 生成分析图表）-------------------------------------------------
countries <- c("China", "Colombia")
for (country in countries) {
  # 生成处理效应图
  plot(out, 
       type = "gap", 
       id = country, 
       main = paste(country, "Gap (T =", 
                    unique(data$year[data$country == country & data$treated == 1]), ")"),
       cex.main = 1.5,        # 主标题放大1.5倍
       ylab = "Gap",
       cex.lab = 1.5,         # 覆盖全局设置确保生效
       cex.axis = 1.5)
  
  # 生成反事实对照图
  plot(out, 
       type = "ct", 
       id = country, 
       main = paste(country, "Counterfactual"), 
       cex.main = 1.5,
       xlab = "Year",
       ylab = "Counterfactual Value",
       cex.lab = 1.5,
       cex.axis = 1.5)
}

  dev.off()  # 关闭当前PNG设备

# 重要说明 ----------------------------------------------------------------
##### Buildings Sector## AC1 =======================  AC1 建筑部门
# 加载必要的包
library(panelView)
library(gsynth)
library(ggplot2)
  pdf("~/science-ynn-NMVOC/Buildings_ac1.pdf")  

  data <- ac1
  # 创建处理变量
  data$treated <- 0
  
  data$treated <- ifelse(data$country == "Canada" & data$year >= 2005, 1, data$treated)
  data$treated <- ifelse(data$country == "Hungary" & data$year >= 2018, 1, data$treated)
  data$treated <- ifelse(data$country == "Italy" & data$year >= 2011, 1, data$treated)
  data$treated <- ifelse(data$country == "Portugal" & data$year >= 2010, 1, data$treated)
  data$treated <- ifelse(data$country == "Slovak Republic" & data$year >= 2005, 1, data$treated)
  # 查看处理效应可视化
  panelview(log_buildings_NMVOC ~ treated, 
            data = data,  
            index = c("country","year"), 
            pre.post = TRUE,
            axis.adjust = TRUE,
            cex.axis.y = 8)
  
  # 运行广义合成控制
  system.time(
    out <- gsynth(log_buildings_NMVOC ~ treated 
                  + hdd + cdd + lgdp + lpop + lgdp_sq
                  + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009,
                  data = data, 
                  index = c("country","year"), 
                  force = "two-way", 
                  CV = TRUE, 
                  r = c(0, 3), 
                  se = TRUE, 
                  inference = "parametric", 
                  nboots = 1000, 
                  parallel = FALSE, 
                  estimator = "ife"
    )
  )
  
  # 生成PDF报告

  countries <- c("Canada", "Hungary", "Italy", "Portugal", "Slovak Republic")
  
  # 配置PDF输出
  
  # 循环生成图表
  for (country in countries) {
    # 生成处理效应图
    plot(out, 
         type = "gap", 
         id = country, 
         main = paste(country, "Gap (T =", 
                      unique(data$year[data$country == country & data$treated == 1]), ")"),
         cex.main = 1.5,        # 主标题放大1.5倍
         ylab = "Gap",
         cex.lab = 5,         # 覆盖全局设置确保生效
         cex.axis = 5)
    
    # 生成反事实对照图
    plot(out, 
         type = "ct", 
         id = country, 
         main = paste(country, "Counterfactual"), 
         cex.main = 1.5,
         xlab = "Year",
         ylab = "Counterfactual Value",
         cex.lab = 5,
         cex.axis = 5)
  }
  
  dev.off()  # 关闭图形设备
#==================================================================================  
  ##### Buildings Sector## AC6 =======================
  # 加载必要的包
  library(panelView)
  library(gsynth)
  library(ggplot2)
  
  pdf("~/science-ynn-NMVOC/Buildings_ac6.pdf")
  
  # 修改全局图形参数，增大边距、标签和刻度字体并加粗
  #par(mfrow = c(1, 2), 
   #   mar = c(6, 6, 4, 2),       # 较大下、左、上、右边距
    #  oma = c(2, 2, 2, 0), 
     # cex.lab = 2.5,             # 坐标轴标签字体放大（相对于默认值）
      #cex.axis = 2.5,            # 坐标轴刻度字体放大
      #font.lab = 2,              # 坐标轴标签加粗
      #font.axis = 2)             # 坐标轴刻度加粗
  data <- ac6
  # 创建处理变量
  data$treated <- 0
  data$treated <- ifelse(data$country == "Chile" & data$year >= 2005, 1, data$treated)
  data$treated <- ifelse(data$country == "Indonesia" & data$year >= 2015, 1, data$treated)
  data$treated <- ifelse(data$country == "Russia" & data$year >= 2009, 1, data$treated)
  data$treated <- ifelse(data$country == "South Africa" & data$year >= 2010, 1, data$treated)
  data$treated <- ifelse(data$country == "South Korea" & data$year >= 2016, 1, data$treated)
  
  # 可视化处理效应：panelview函数中也增加了cex.axis和cex.lab参数
  panelview(log_buildings_NMVOC ~ treated, 
            data = data,  
            index = c("country","year"), 
            pre.post = TRUE,
            axis.adjust = TRUE,
            cex.lab = 8)     # 调整坐标轴标签字体大小
  
  # 运行广义合成控制
  system.time(
    out <- gsynth(log_buildings_NMVOC ~ treated 
                  + hdd + cdd + lgdp + lpop + lgdp_sq,
                  data = data, 
                  index = c("country","year"), 
                  force = "two-way", 
                  CV = TRUE, 
                  r = c(0, 3), 
                  se = TRUE, 
                  inference = "parametric", 
                  nboots = 1000, 
                  parallel = FALSE, 
                  estimator = "ife"
    )
  )
  
  # 指定需要绘图的国家
  countries <- c("Chile", "Indonesia", "Russia", "South Africa", "South Korea")
  
  # 循环生成每个国家的图表
  for (country in countries) {
    par(cex.lab=4, cex.axis=3.5, font.lab=2, font.axis=2, lwd=2)
     # 生成处理效应图：gap图
    plot(out, 
         type = "gap", 
         id = country, 
         main = paste(country, "Gap (T =", 
                      unique(data$year[data$country == country & data$treated == 1]), ")"),
         cex.main = 2,        # 主标题字体较大
         ylab = "Gap",
         cex.lab = 4,       # 坐标轴标签字体
         cex.axis = 3.5)      # 坐标轴刻度字体
    
    # 生成反事实对照图：counterfactual图
    plot(out, 
         type = "ct", 
         id = country, 
         main = paste(country, "Counterfactual"), 
         cex.main = 2,
         xlab = "Year",
         ylab = "Counterfactual Value",
         cex.lab = 4,
         cex.axis = 3.5)
  }
  
  dev.off()  # 关闭图形设备
#======================================================================================
  

  
#============================================================================
# ###############设置高清图形的默认参数
options(device = function(file, width = 7, height = 5, ...) {
  png(file, width = width*300, height = height*300, res = 300, ...)
})

#=======================
# Buildings Sector - AC1
#=======================
output_dir <- "~/science-ynn/buildings"  #以下代码生成的是单图png图片
# 数据处理
data <- ac1
data$treated <- 0

data$treated <- ifelse(data$country == "Czech Republic" & data$year >= 2005, 1, data$treated)
data$treated <- ifelse(data$country == "Finland" & data$year >= 2021, 1, data$treated)

data$treated <- ifelse(data$country == "Greece" & data$year >= 2009, 1, data$treated)
data$treated <- ifelse(data$country == "Hungary" & data$year >= 2018, 1, data$treated)
data$treated <- ifelse(data$country == "Ireland" & data$year >= 2014, 1, data$treated)
data$treated <- ifelse(data$country == "Norway" & data$year >= 2010, 1, data$treated)
data$treated <- ifelse(data$country == "Poland" & data$year >= 2010, 1, data$treated)
data$treated <- ifelse(data$country == "Slovak Republic" & data$year >= 2011, 1, data$treated)
#data$treated <- ifelse(data$country == "Australia" & data$year >= 2019, 1, data$treated)
#data$treated <- ifelse(data$country == "Czech Republic" & data$year >= 2005, 1, data$treated)
#data$treated <- ifelse(data$country == "Denmark" & data$year >= 2020, 1, data$treated)
#data$treated <- ifelse(data$country == "Greece" & data$year >= 2012, 1, data$treated)
#data$treated <- ifelse(data$country == "Ireland" & data$year >= 2015, 1, data$treated)
#data$treated <- ifelse(data$country == "New Zealand" & data$year >= 2018, 1, data$treated)
#data$treated <- ifelse(data$country == "Norway" & data$year >= 2015, 1, data$treated)
#data$treated <- ifelse(data$country == "Poland" & data$year >= 2019, 1, data$treated)
#data$treated <- ifelse(data$country == "Slovak Republic" & data$year >= 2011, 1, data$treated)
# 可视化处理状态png
png(file.path(output_dir,"AC1_treatment_status.png"), width = 10, height = 6, units = "in", res = 300)
panelview(log_buildings_NOX ~ treated, data = data,  
          index = c("country","year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)
dev.off()

# 运行模型
set.seed(123) # 保证可重复性
system.time(
  out_ac1 <- gsynth(log_buildings_NOX ~ treated 
                    + hdd + cdd + lgdp + lpop + lgdp_sq
                    + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009, #+ MEPS_Appliances_2013,
                    data = data, 
                    index = c("country","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 3), se = TRUE, 
                    inference = "parametric", nboots = 1000, 
                    parallel = TRUE, estimator = "ife")
)

# 保存AC1图形
countries_ac1 <- c("Czech Republic", "Finland", "Greece", "Hungary", "Ireland",
                    "Norway", "Poland", "Slovak Republic")

for (country in countries_ac1) {
  
  # 在绘图前设置图形参数
  par(
    cex.axis = 5,      # 坐标轴刻度标签放大1.2倍
    font.axis = 2,       # 坐标轴刻度字体加粗（2代表bold）
    family = "sans"      # 设置字体类型（可选）
  )
  # Gap图
  png(file.path(output_dir,paste0("AC1_", gsub(" ", "_", country), "_gap.png")), width = 3000, height = 2000, res = 300)
  plot(out_ac1, type = "gap", id = country, main = paste(country, "Gap (T =", unique(data$year[data$country == country & data$treated == 1]), ")"), cex.main = 5,
       col.main = "black",
       cex.lab = 5,    # 轴标签放大1.3倍
       font.lab = 2)     # 轴标签加粗
  dev.off()
  
  
  # 反事实图
  png(file.path(output_dir,paste0("AC1_", gsub(" ", "_", country), "_ct.png")), width = 3000, height = 2000, res = 300)
  plot(out_ac1, type = "ct", id = country, main = paste(country, "Counterfactual"), cex.main = 5,
       font.main = 2,    # 标题加粗
       col.main = "black",
       cex.lab = 5,    # 轴标签放大1.3倍
       font.lab = 2)     # 轴标签加粗
  dev.off()
}

#=======================
# Buildings Sector - AC6
#=======================

# 数据处理
data <- ac6
data$treated <- 0

data$treated <- ifelse(data$country == "Argentina" & data$year >= 2017, 1, data$treated)
data$treated <- ifelse(data$country == "Chile" & data$year >= 2014, 1, data$treated)
data$treated <- ifelse(data$country == "Peru" & data$year >= 2005, 1, data$treated)
data$treated <- ifelse(data$country == "South Africa" & data$year >= 2010, 1, data$treated)
data$treated <- ifelse(data$country == "Turkey" & data$year >= 2013, 1, data$treated)
#data$treated <- ifelse(data$country == "Argentina" & data$year >= 2017, 1, data$treated)
#data$treated <- ifelse(data$country == "China" & data$year >= 2018, 1, data$treated)
#data$treated <- ifelse(data$country == "Colombia" & data$year >= 2013, 1, data$treated)
#data$treated <- ifelse(data$country == "Peru" & data$year >= 2006, 1, data$treated)
#data$treated <- ifelse(data$country == "South Africa" & data$year >= 2010, 1, data$treated)
#data$treated <- ifelse(data$country == "Turkey" & data$year >= 2013, 1, data$treated)

# 可视化处理状态
png(file.path(output_dir,"AC6_treatment_status.png"), width = 10, height = 6, units = "in", res = 300)
panelview(log_buildings_NOX ~ treated, data = data,  
          index = c("country","year"), 
          pre.post = TRUE,
          axis.adjust = TRUE,
          cex.axis.y = 8)
dev.off()

# 运行模型
system.time(
  out_ac6 <- gsynth(log_buildings_NOX ~ treated 
                    + hdd + cdd + lgdp + lpop + lgdp_sq,
                    data = data, 
                    index = c("country","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 3), se = TRUE, 
                    inference = "parametric", nboots = 1000, 
                    parallel = TRUE, estimator = "ife")
)

# 保存AC6图形
countries_ac6 <- c("Argentina", "Chile", "Peru", "South Africa", "Turkey")

for (country in countries_ac6) {
  # Gap图
  png(file.path(output_dir,paste0("AC6_", gsub(" ", "_", country), "_gap.png")), width = 3000, height = 2000, res = 300)
  plot(out_ac6, type = "gap", id = country, main = paste(country, "Gap (T =", unique(data$year[data$country == country & data$treated == 1]), ")"), cex.main = 5)
  dev.off()
  
  # 反事实图
  png(file.path(output_dir,paste0("AC6_", gsub(" ", "_", country), "_ct.png")), width = 3000, height = 2000, res = 300)
  plot(out_ac6, type = "ct", id = country, main = paste(country, "Counterfactual"), cex.main = 5)
  dev.off()
}  
 
