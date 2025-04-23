#match policy to breaks based on different confidence intervals
source('~/science-code/00_oecd_project_functions.R')

#read preprocessed oecd policy data
oecd_grouped = read.csv('~/science-ynn/OECD_data_preprocessed.csv')
# 修改 'oecd_grouped$Policy_name_fig_2_3' 中的 "label " 为 "label"
oecd_grouped$Policy_name_fig_2_3 <- gsub("label ", "label", oecd_grouped$Policy_name_fig_2_3)
#set the color palette for the policies 
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324","#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")

names(palette) <- unique(oecd_grouped$Policy_name_fig_2_3)
color_dict = palette

## Load the break detection results

results = readRDS('~/science-ynn/Break_detection_results_ynn.RDS')
#results = readRDS('~/science-code/Break_detection_results.RDS')
##add sector to results 
results$sector = str_to_title(sapply(strsplit(results$source, "_"), function(x) x[2]))
#提取results$source的sector部分

########## format the output of the break analysis, match with policy data in different ways for further analysis 

#basic reformatting based on the getspanel package 
library(doParallel)
install.packages("ISAT") #下面并行计算报错改成单独计算的
#cl <- makeCluster(detectCores() - 1) # 使用计算机核心数减1
#registerDoParallel(cl) #确保在并行计算之前注册了并行后端。并行后端的选择可能会影响任务的执行方式和资源的分配。
#foreach 函数的作用是迭代 results 数据框的每一行，逐行执行后面的代码。nrow(results)：获取 results 数据框的行数，迭代从 1 到 nrow(results)。
##.combine = rbind：指定结果按行绑定（rbind），即每次循环返回的 models 将按行拼接到 policy_out 中。
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

#[1] "The number of breaks in  Buildings  is: 22"
#[1] "The number of breaks in  Electricity  is: 27"
#[1] "The number of breaks in  Industry  is: 11"
#[1] "The number of breaks in  Transport  is: 18"

#translate into % (still needs to be multiplied with 100 of course)

all_together$coef_percent = exp(all_together$coef)-1

mean(all_together$coef_percent)#-0.2419024

#get average treatment effect size by sector 

all_together %>% group_by(sector) %>% summarize(mean_effect_size = mean(coef_percent))
# sector      mean_effect_size
#1 Buildings             -0.221
#2 Electricity           -0.301
#3 Industry              -0.258
#4 Transport             -0.169
#get number of breaks by country group 
developed_countries = read.csv('~/science-code/country_groupings.csv')
developed_countries$ISO = countrycode(developed_countries$Developed,origin='country.name',destination='iso3c')
all_together$High_income = 0
all_together[all_together$country_code %in% developed_countries$ISO,'High_income'] = 1

all_together %>% group_by(High_income) %>% count()

#match policies based on different time intervals (statistical interval as extracted from break detection method (policy_match),
# 2 year fixed (policy_match_2y), 3 year fixed (policy_match_3y))

policy_match <- foreach(i = 1:nrow(policy_out), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped) 
  models = tibble(country_sample = results$country_sample[i],#.combine = rbind意味着将每次循环的结果合并成一个大的数据框，使用 rbind（按行合并）。
                  sector = results$sector[i],#提取当前行对应的 sector（行业或模块）信息
                  policy_match = list(match_oecd_policies(oecd_grouped, policy_out$out[[i]],module=policy_out$sector[i])),#policy_match 是根据默认的时间区间tci来匹配政策的结果
                  policy_match_2y = list(match_oecd_policies(oecd_grouped, policy_out$out[[i]],module=policy_out$sector[i],fixed_interval=2)),
                  policy_match_3y = list(match_oecd_policies(oecd_grouped, policy_out$out[[i]],module=policy_out$sector[i],fixed_interval=3)))
  #tibble：这行代码创建了一个新的 tibble（一种数据框），并为每个字段赋值。这个数据框用于存储每次循环中计算得到的匹配政策结果  
}
  #循环体的结束。每次循环会生成一个新的 models 数据框，其中包含了当前国家和模块的匹配政策结果。每次生成的数据会被 foreach 汇总，最终形成一个包含所有国家和模块的 policy_match 数据框。

##merge all into one object

policy_out$policy_match = policy_match$policy_match
policy_out$policy_match_2y = policy_match$policy_match_2y
policy_out$policy_match_3y = policy_match$policy_match_3y

#save -> This version is used in Fig. 2 and 3!

saveRDS(policy_out,"~/science-ynn/Policy_out.RDS")

##generate filtered version for break overlap (used in counting and comparisons) 
##in this version two breaks are merged if the confidence interval of one break is fully contained in the 
##confidence interval of the other 
#foreach 循环：针对 results 中的每一行数据，执行一系列操作。
policy_out_filtered <- foreach(i = 1:nrow(results), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(country_sample = results$country_sample[i],
                  sector = results$sector[i],
                  out = list(filter_break_overlap(policy_out$out[[i]])),
                  is =  list(results %>% slice(i) %>% pull(is) %>% first))
}
#models：对于每一行数据，创建一个 tibble，其中包含：
#country_sample 和 sector：来自 results 数据框的当前行数据。
#out：应用 filter_break_overlap 函数处理过后的 policy_out$out[[i]] 数据，这一步是将断点的重叠情况处理为没有重叠的结果。
#is：从 results 中提取出对应的 is 值（似乎是一些指标或标记）

policy_match_filtered <- foreach(i = 1:nrow(policy_out), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %do% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(country_sample = results$country_sample[i],
                  sector = results$sector[i],
                  policy_match = list(match_oecd_policies(oecd_grouped, policy_out_filtered$out[[i]],module=policy_out_filtered$sector[i])),
                  policy_match_2y = list(match_oecd_policies(oecd_grouped, policy_out_filtered$out[[i]],module=policy_out_filtered$sector[i],fixed_interval=2)),
                  policy_match_3y = list(match_oecd_policies(oecd_grouped, policy_out_filtered$out[[i]],module=policy_out_filtered$sector[i],fixed_interval=3)))
}
#foreach 循环：再次遍历 policy_out 中的每一行数据。
#policy_match：通过 match_oecd_policies 函数将处理过的 policy_out_filtered$out[[i]] 进行政策匹配，得到基于断点数据的匹配结果。
#policy_match_2y 和 policy_match_3y：分别是使用固定间隔（2年和3年）进行的政策匹配，与之前的 policy_match 方式类似。
#fixed_interval=2 和 fixed_interval=3 使得匹配使用了相应的固定时间区间（2年或3年）来进行匹配。

policy_out_filtered$policy_match = policy_match_filtered$policy_match
policy_out_filtered$policy_match_2y = policy_match_filtered$policy_match_2y
policy_out_filtered$policy_match_3y = policy_match_filtered$policy_match_3y
#将上面的 policy_match_filtered 中得到的匹配结果赋值给 policy_out_filtered 中对应的列（policy_match, policy_match_2y, policy_match_3y），完成最终的政策匹配操作。
saveRDS(policy_out_filtered,"~/science-ynn/Policy_out_filtered.RDS")

#将包含匹配断点的政策信息的列表policy_out_filtered$policy_match_2y中8元素写入到一个csv格式文件
# 加载必要的包
library(dplyr)
# 假设 policy_out_filtered$policy_match_2y 是一个包含多个数据框的列表
# 使用 do.call 和 rbind 将它们合并成一个大的数据框
policy_match_2y_combined <- do.call(rbind, policy_out_filtered$policy_match_2y)

# 指定输出文件路径（可以修改路径和文件名）
output_file <- "~/science-ynn/policy_match_2y.csv"

# 将合并后的数据框写入 CSV 文件
write.csv(policy_match_2y_combined, file = output_file, row.names = FALSE)

# 输出提示信息
cat("CSV 文件已成功保存到:", output_file, "\n")

