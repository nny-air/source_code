#match policy to breaks based on different confidence intervals

source('~/science-code/00_oecd_project_functions.R')

#read preprocessed oecd policy data
oecd_grouped = read.csv('~/science-ynn-CO/OECD_data_preprocessed.csv')

#set the color palette for the policies 
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324","#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- unique(oecd_grouped$Policy_name)
color_dict = palette


## Load the break detection results

results = readRDS('~/science-ynn-CO/Break_detection_results_ynn.RDS')
#results = readRDS('~/science-code/Break_detection_results.RDS')
##add sector to results 
results$sector = str_to_title(sapply(strsplit(results$source, "_"), function(x) x[2]))


########## format the output of the break analysis, match with policy data in different ways for further analysis 

#basic reformatting based on the getspanel package 
library(doParallel)
install.packages("ISAT") #下面并行计算报错改成单独计算的
#cl <- makeCluster(detectCores() - 1) # 使用计算机核心数减1
#registerDoParallel(cl) #确保在并行计算之前注册了并行后端。并行后端的选择可能会影响任务的执行方式和资源的分配。
#clusterExport(cl, list("results", "get_breaks_list", "oecd_grouped"))
policy_out <- foreach(i = 1:nrow(results), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  print(paste("Processing row:", i))  # 打印正在处理的行信息
  library(stats)  # 确保加载 stats 包
  library(ISAT)
  library(tidyverse)
  library(getspanel)
  # 获取 breaks_list 的结果
  models = tibble(country_sample = results$country_sample[i],
                  sector = results$sector[i],
                  out = list(get_breaks_list(results$is[[i]])),
                  is =  list(results %>% slice(i) %>% pull(is) %>% first))
}
#stopCluster(cl)#在并行任务完成后，不要忘记停止并行集群
#class(get_breaks_list(results$is[[i]]))

##compute mean of effect sizes to report in study  

all_together = data.frame()

for(i in 1:nrow(policy_out)){
  sector_data = policy_out$out[[i]]
  sector_data$sector = policy_out$sector[i]
  all_together = rbind(all_together, sector_data)
}

##count number of breaks per sector to report in study

for(i in 1:4){
  print(paste("The number of breaks in ",policy_out$sector[i]," is:",nrow(policy_out$out[[i]])+nrow(policy_out$out[[i+4]])))
}

#[1] "The number of breaks in  Buildings  is: 18"
#[1] "The number of breaks in  Electricity  is: 14"
#[1] "The number of breaks in  Industry  is: 16"
#[1] "The number of breaks in  Transport  is: 29"

#translate into % (still needs to be multiplied with 100 of course)

all_together$coef_percent = exp(all_together$coef)-1

mean(all_together$coef_percent)#-0.2689675

#get average treatment effect size by sector 

all_together %>% group_by(sector) %>% summarize(mean_effect_size = mean(coef_percent))
# sector      mean_effect_size
#1 Buildings             -0.316
#2 Electricity           -0.273
#3 Industry              -0.322
#4 Transport             -0.208
#get number of breaks by country group 
developed_countries = read.csv('~/science-code/country_groupings.csv')
developed_countries$ISO = countrycode(developed_countries$Developed,origin='country.name',destination='iso3c')
all_together$High_income = 0
all_together[all_together$country_code %in% developed_countries$ISO,'High_income'] = 1

all_together %>% group_by(High_income) %>% count()
#High_income     n
#<dbl> <int>
#           0    27
#           1    50
#
#match policies based on different time intervals (statistical interval as extracted from break detection method (policy_match),
# 2 year fixed (policy_match_2y), 3 year fixed (policy_match_3y))

policy_match <- foreach(i = 1:nrow(policy_out), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(country_sample = results$country_sample[i],
                  sector = results$sector[i],
                  policy_match = list(match_oecd_policies(oecd_grouped, policy_out$out[[i]],module=policy_out$sector[i])),
                  policy_match_2y = list(match_oecd_policies(oecd_grouped, policy_out$out[[i]],module=policy_out$sector[i],fixed_interval=2)),
                  policy_match_3y = list(match_oecd_policies(oecd_grouped, policy_out$out[[i]],module=policy_out$sector[i],fixed_interval=3)))
  
}


##merge all into one object

policy_out$policy_match = policy_match$policy_match
policy_out$policy_match_2y = policy_match$policy_match_2y
policy_out$policy_match_3y = policy_match$policy_match_3y

#save -> This version is used in Fig. 2 and 3!

saveRDS(policy_out,"~/science-ynn-CO/Policy_out.RDS")

##generate filtered version for break overlap (used in counting and comparisons) 
##in this version two breaks are merged if the confidence interval of one break is fully contained in the 
##confidence interval of the other 

policy_out_filtered <- foreach(i = 1:nrow(results), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(country_sample = results$country_sample[i],
                  sector = results$sector[i],
                  out = list(filter_break_overlap(policy_out$out[[i]])),
                  is =  list(results %>% slice(i) %>% pull(is) %>% first))
}

policy_match_filtered <- foreach(i = 1:nrow(policy_out), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(country_sample = results$country_sample[i],
                  sector = results$sector[i],
                  policy_match = list(match_oecd_policies(oecd_grouped, policy_out_filtered$out[[i]],module=policy_out_filtered$sector[i])),
                  policy_match_2y = list(match_oecd_policies(oecd_grouped, policy_out_filtered$out[[i]],module=policy_out_filtered$sector[i],fixed_interval=2)),
                  policy_match_3y = list(match_oecd_policies(oecd_grouped, policy_out_filtered$out[[i]],module=policy_out_filtered$sector[i],fixed_interval=3)))
}

policy_out_filtered$policy_match = policy_match_filtered$policy_match
policy_out_filtered$policy_match_2y = policy_match_filtered$policy_match_2y
policy_out_filtered$policy_match_3y = policy_match_filtered$policy_match_3y

saveRDS(policy_out_filtered,"~/science-ynn-CO/Policy_out_filtered.RDS")
#下面将包含匹配断点的政策信息的列表policy_out_filtered$policy_match_2y中8元素写入到一个csv格式文件
# 加载必要的包
library(dplyr)
# 假设 policy_out_filtered$policy_match_2y 是一个包含多个数据框的列表
# 使用 do.call 和 rbind 将它们合并成一个大的数据框
policy_match_2y_combined <- do.call(rbind, policy_out_filtered$policy_match_2y)

# 指定输出文件路径（可以修改路径和文件名）
output_file <- "~/science-ynn-CO/policy_match_2y_CO.csv"

# 将合并后的数据框写入 CSV 文件
write.csv(policy_match_2y_combined, file = output_file, row.names = FALSE)

# 输出提示信息
cat("CSV 文件已成功保存到:", output_file, "\n")
