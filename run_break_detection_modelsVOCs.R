# Master Runs#01_run_break_detection_models
#install.packages("tidyverse")
#install.packages("devtools")
#install.packages("gets")
#install.packages("getspanel")
#install.packages("here")
#install.packages("doParallel")
#install.packages("readxl")
#install.packages("gdata")
#install.packages("cowplot")
#install.packages("foreach")
library(tidyverse)
library(devtools)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)
library(cowplot)
library(foreach)
# Country groupings

#Developed economies = AC1
#Developing economies = AC6

AC6 <- read_excel("~/science-code/country_groupings.xlsx", sheet = 2)
#从名为 "country_groupings.xlsx" 的Excel文件中读取第二个工作表的数据，并将其存储在变量 AC6 中。
AC1 <- read_excel("~/science-code/country_groupings.xlsx") %>% pull(Developed) %>% unlist
#读取同一Excel文件的默认工作表，提取名为 "Developed" 的列，转化为向量并存储在 AC1 中。

for(i in 1:nrow(AC6)){
  temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist  #在循环中，提取当前行的 "countries" 列，按逗号分隔字符串，并将结果转换为向量，存储在 temp 中。
  mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))               #将 temp 的内容移动（或复制）到一个新的变量，变量名由 "AC6_" 和当前行的 "cat_abbrev" 列的值组合而成。
}
##这个for循环目的是遍历AC6中的每个国家组，按国家组名称将各国提取出来并创建新的变量。

#dfi <- readRDS("~/science-code/break_detection_regression_input.RDS")  
input_data <- readRDS("~/science-ynn-NMVOC/regression_input_ynn.RDS")
#data_no_na <- na.omit(input_data)
#cleaned_data <- data_no_na[apply(data_no_na, 1, function(x) all(is.finite(x))), ]
#exclude countries that don't have a minimum number of emissions 
excl_all <- input_data %>% group_by(country) %>% 
  summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique
  #summarise(excl_test = mean(logtotal_NMVOC)) %>% filter(excl_test < 10) %>% pull(country) %>% unique
#从左到右执行管道中的每个步骤，将管道的最终结果赋值给excl_all
#计算每个国家的总排放量（对数）的均值，并将其存储在 excl_test 列中。filter(excl_test < 10)：筛选出均值小于10的国家。pull(country)：提取这些国家的名称。unique：去重，得到唯一的国家名称。

df_excl <- input_data %>% filter(!(country %in% excl_all))
#从 dfi 中筛选出不在 excl_all 列表中的国家，将结果存储在 df_excl 中。
AC6_1 <- df_excl %>% filter(!(country %in% AC1)) %>% select(country)%>% distinct()
# 从df_excl 中筛选出不在 AC1 列表中的国家。select(country)：只选择国家这一列。distinct()：去重，确保每个国家只出现一次。

AC6_2 <- AC6_1$country  #区别在一个数据框一个是值
samples <- mget(c("AC1", "AC6_2"))


#specifcy basic model forms

tot_controls <- c(" ~ lgdp + lpop + lgdp_sq + hdd + cdd")

tot_core_deps <- c("log_buildings_NMVOC",
                   "log_electricity_heat_NMVOC", 
                   "log_industry_NMVOC",
                   "log_transport_NMVOC")
#原来的数据变量log_buildings_co2 改为现有的lbuild_NMVOC，log_electricity_heat_co2改为lelec_heat_NMVOC；log_industry_co2及log_transport_co2
tot_base_forms <- paste0(rep(tot_core_deps, each = length(tot_controls)), tot_controls)
#用于将多个字符或对象连接在一起，不使用任何分隔符

# specify sector-specific controls (including EU dummies as we're looking at developed economies, all EU countries are in this group)
total_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013 + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018 + MEPS_T_2009 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

buildings_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"

electricity_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_E_2005 + ETS_E_2018"

industry_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

transport_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + MEPS_T_2009"


eu_control_forms <- c(
  paste0("log_buildings_NMVOC", buildings_controls),
  paste0("log_electricity_heat_NMVOC", electricity_controls), 
  paste0("log_industry_NMVOC", industry_controls),
  paste0("log_transport_NMVOC", transport_controls))

#create an ensemble of models to run

tot_forms <- c(tot_base_forms, eu_control_forms)
#cleaned_data <- input_data[complete.cases(input_data) & apply(input_data, 1, function(x) all(is.finite(x))), ]
# Incorporating linear country-specific time trends
df_trends <- input_data %>% mutate(country = as.factor(country),
                             trend = year - 1999,
              )

df_restr_trends <- df_excl %>% mutate(country = as.factor(country),
                                      trend = year - 1999,
                                      trend_sq = trend^2)

# Base forms
tot_forms_trends <- c(paste0(tot_forms, " + country:trend"))


##run models for developing economies
rel_forms_trends <- tot_forms_trends[1:4]

# 创建一个集群，使用除了一个核心外的所有核心
cl <- makeCluster(detectCores() - 1)    
registerDoParallel(cl)

#run models with getspanel

results_developing <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6_2"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

### run for developed economies

#only keep final models
rel_forms_trends <- tot_forms_trends[5:8]


#run models with getspanel

results_developed <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

##combine results
results = rbind(results_developed, results_developing)

#save isatpanel objects and metadata
saveRDS(results,"~/science-ynn-NMVOC/Break_detection_results_ynn.RDS")

stopCluster(cl)#停止集群（完成后）

