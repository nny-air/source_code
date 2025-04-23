
library(tidyverse)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)
# 加载自定义函数
source("~/science-code/00_oecd_project_functions.R")
conflict_prefer("filter", "dplyr")
# Country groupings
# Developing vs Developed: 4 x AC6 & AC1
AC6 <- read_excel(here('~/science-code/country_groupings.xlsx'), sheet = 2)
AC1 <- read_excel(here('~/science-code/country_groupings.xlsx')) %>% pull(Developed) %>% unlist
for(i in 1:nrow(AC6)){
  temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist
  mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))
}
AC6_HICs <- c(AC6_HICs, "Russia")

AC6_all <- c(AC6_HICs, AC6_LICs, AC6_LMICs, AC6_UMICs)

# Combine
samples <- mget(c("AC1", "AC6_all"))

dfi <- readRDS(here("~/science-ynn-CO/regression_input_ynn.RDS"))  %>% 
  filter(year >= 2000)

# Exclude countries according to minimum emissions restriction
excl_all <- dfi %>% group_by(country) %>% 
  summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique

# Additional control variables for urban population and population^2 to be used in robustness checks below
additional_controls <- readRDS(here('~/science-code/additional_control_variables.RDS'))

df_excl <- dfi %>% 
  filter(!(country %in% excl_all) & country != "Israel") %>% 
  # Incorporating linear country-specific time trends
  mutate(country = as.factor(country),
         trend = year - 1999) %>% 
  left_join(., additional_controls, by = c("country", "year"))

###############################################################################
# MAIN SPECIFICATION WITH PARAMETER VARIATIONS

# The following block runs the principal specification as well as the additional 
# robustness checks that vary the false detection rate, 
# impulse indicator saturation, and ghg vs co2 emissions

standard_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + country:trend"

total_controls <- " + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013 + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018 + MEPS_T_2009 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

buildings_controls <- " + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"

electricity_controls <- " + ETS_E_2005 + ETS_E_2018"

industry_controls <- " + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

transport_controls <- " + MEPS_T_2009"

standard_forms <- c(
  #paste0("logtotal_NOX", standard_controls, total_controls),
  paste0("log_buildings_CO", standard_controls, buildings_controls),
  paste0("log_electricity_heat_CO", standard_controls, electricity_controls), 
  paste0("log_industry_CO", standard_controls, industry_controls),
  paste0("log_transport_CO", standard_controls, transport_controls))

#cl <- makeCluster(30)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
standard_models <- foreach(f = standard_forms, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(TRUE, FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6_all", "AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01, 0.001), .combine = rbind) %dopar% {
    dat <- df_excl %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
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

stopCluster(cl)

standard_models %>% 
  saveRDS(here("~/science-ynn-CO/main_model_result_parameter_variation1.RDS"))
##===============
results = standard_models
results$sector = str_to_title(sapply(strsplit(results$source, "_"), function(x) x[2])) #提取results$source的sector部分
library(doParallel)
#install.packages("ISAT")
policy_out <- foreach(i = 1:nrow(results), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  print(paste("Processing row:", i))  # 打印正在处理的行信息，方便跟踪进度
  library(stats)  # 确保加载 stats 包
  library(ISAT)
  library(tidyverse)
  library(getspanel)
  # 获取 breaks_list 的结果  ##这一行代码创建一个 tibble（数据框）对象 models，每次循环都会返回一个新的 tibble，并将其拼接到最终的 policy_out 中。
  models = tibble(country_sample = results$country_sample[i],##results 数据框的 country_sample 列中的第 i 行
                  sector = results$sector[i],
                  out = list(get_breaks_list(results$is[[i]])),##访问 results 数据框中的 is 列的第 i 行的元素，get_breaks_list 函数，将 results$is[[i]] 传递给它
                  is =  list(results %>% slice(i) %>% pull(is) %>% first))  #与results$is[[i]]结果一样貌似，但可能是为了确保返回的仍然是一个标量而使用了 first
} #使用policy_out$out[[1]]查看断点第1/8的结果信息
policy_out$out[[1]]
#library(dplyr)
# 假设 policy_out 是一个包含多个数据框的列表
# 使用 do.call 和 rbind 将它们合并成一个大的数据框
#policy_out_combined <- do.call(rbind, policy_out)
#output_file <- "~/science-ynn/policy_out_output.csv"
# 将合并后的数据框写入 CSV 文件
#write.csv(policy_out_combined, file = output_file, row.names = FALSE)
