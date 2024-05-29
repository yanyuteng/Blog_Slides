# Yu-Teng's Diary Analysis
# Author: Yu-Teng Yan
# 1st Time: 18/04/2024 - 22/04/2024



###################################################################################################
###################################################################################################
####### 1. Environment ########
library(tidyverse)
library(lubridate)
library(ggplot2)
setwd('/Users/yuteng/Blog_Hexo/source/_posts')



###################################################################################################
###################################################################################################
####### 2. Diary Data Input ########
# Improved date parsing to handle ranges and provide appropriate dates
parse_dates <- function(date_str, default_date) {
  if (str_detect(date_str, "-")) {
    date_range <- str_extract_all(date_str, "[0-9]{4}\\.[0-9]{1,2}\\.[0-9]{1,2}")[[1]]
    if (length(date_range) == 2) {
      dates <- seq(as.Date(date_range[1], format="%Y.%m.%d"), as.Date(date_range[2], format="%Y.%m.%d"), by = "day")
      return(dates)
    } else {
      warning("Date range is not correctly formatted: ", date_str)
      return(as.Date(default_date))
    }
  } else {
    date <- as.Date(date_str, format="%Y.%m.%d", quiet = TRUE)
    if (is.na(date)) {
      warning("Date parsing failed for: ", date_str)
      return(as.Date(default_date))
    } else {
      return(date)
    }
  }
}

# Processing a single diary file with consideration for links and date ranges
process_diary <- function(filename) {
  lines <- read_lines(filename)
  metadata_start <- which(lines == "---")[1]
  metadata_end <<- which(lines == "---")[2]
  metadata <- lines[(metadata_start + 1):(metadata_end - 1)]
  meta_list <- map_chr(metadata, ~str_split(.x, ": ")[[1]][2])
  names(meta_list) <- map_chr(metadata, ~str_split(.x, ": ")[[1]][1])
  
  default_date <- as.Date(meta_list["date"], format = "%Y-%m-%d")
  
  content_start <- metadata_end + 1
  entries <- str_subset(lines[content_start:length(lines)], "^### ")
  diary_entries <- map_dfr(entries, ~{
    parts <- str_split(.x, " ", 3)[[1]]
    date_part <- parts[2]
    title <- if (length(parts) > 2) parts[3] else ""
    dates <- parse_dates(date_part, default_date)
    content_index <- which(lines == .x) + 1
    if (length(content_index) == 0 || is.na(content_index)) {
      return(tibble(Date = dates, Title = title, Content = NA_character_))
    }
    next_empty_line_index <- which(lines[content_index:length(lines)] == "")[1]
    end_index <- if (!is.na(next_empty_line_index)) {
      next_empty_line_index + content_index - 2
    } else {
      length(lines)
    }
    content <- paste(lines[content_index:end_index], collapse = "\n")
    map_dfr(dates, ~tibble(Date = .x, Title = title, Content = content))
  })
  
  return(diary_entries)
}

# Read all diary files and combine data
file_paths <- dir(pattern = "^Diary_.*\\.md$")
# Sort files to prioritize files with non-numeric suffixes
numeric_suffix <- grepl("\\d+$", sub("\\.md$", "", file_paths))
file_paths <- file_paths[order(numeric_suffix)]  # FALSE values (non-numeric) first

diary_data_list <- map(file_paths, process_diary)
diary_data <- bind_rows(diary_data_list) %>%
  group_by(Date) %>%
  summarise(Content = first(Content), Title = first(Title), .groups = 'drop')

# diary_data[which(diary_data$Date=='2021-10-02'),] #观察单日的Diary_京畿古刹访录与Diary_2021优先级
# diary_data[which(diary_data$Date=='2015-01-13'),] #观察多日的Diary_2015是否被Diary_旅行日志逐日数据替换
rm(list=setdiff(ls(), c('diary_data')))
diary_data <- diary_data[,c(1,3,2)]



###################################################################################################
###################################################################################################
####### 3. Log Data Input ########
# 处理单个日记文件
process_log <- function(filename) {
  lines <- read_lines(filename)
  content <- lines[(which(lines == "---")[2] + 1):length(lines)]
  
  entries <- str_subset(content, "^### ")
  log_entries <- map_dfr(entries, ~{
    parts <- str_split(.x, " ", 3)[[1]]
    date <- as.Date(parts[2], format="%Y.%m.%d")
    title <- if (length(parts) > 2) parts[3] else ""
    content_index <- which(content == .x) + 1
    next_empty_line_index <- which(content[content_index:length(content)] == "")[1]
    end_index <- if (!is.na(next_empty_line_index)) {
      next_empty_line_index + content_index - 2
    } else {
      length(content)
    }
    content_text <- paste(content[content_index:end_index], collapse = "\n")
    tibble(Date = date, Title = title, Content = content_text)
  })
  
  return(log_entries)
}

# 读取所有日记文件并合并数据
files <- c("C_Prose_博客日志.md", "M_Prose_统计暨编程问题日志.md")
log_data_list <- map(files, process_log)
log_data <- bind_rows(log_data_list) %>%
  group_by(Date) %>%
  summarise(Title = first(Title), Content = paste(Content, collapse = "\n"), .groups = 'drop')

# 检查重复的日期
duplicated_dates <- log_data %>%
  add_count(Date) %>%
  filter(n > 1) %>%
  pull(Date)

if (length(duplicated_dates) > 0) {
  print(paste("重复的日期包括:", paste(duplicated_dates, collapse=", ")))
} else {
  print("没有重复的日期。")
}

rm(list=setdiff(ls(), c('diary_data','log_data')))



###################################################################################################
###################################################################################################
####### 4. Diary Data + Log Data Input ########
dt_diary <- full_join(log_data, diary_data, by = "Date", suffix = c(".log", ".diary")) # 使用 full_join 来合并这两个数据框

# 合并 Title 和 Content 列，但不包括 NA 值
dt_diary <- dt_diary %>%
  mutate(Title = ifelse(!is.na(Title.log) & !is.na(Title.diary), paste(Title.log, Title.diary, sep = " / "),
                        ifelse(!is.na(Title.log), Title.log, 
                               ifelse(!is.na(Title.diary), Title.diary, NA))),
         Content = ifelse(!is.na(Content.log) & !is.na(Content.diary), paste(Content.log, Content.diary, sep = " / "),
                          ifelse(!is.na(Content.log), Content.log, 
                                 ifelse(!is.na(Content.diary), Content.diary, NA))))

dt_diary <- select(dt, Date, Title, Content) # 清理不再需要的列

colnames(dt_diary) <- c('date','title','content')
dt_diary$category1 <- 'Diary'



###################################################################################################
###################################################################################################
####### 5. Post Data Input ########
# 读取所有Markdown文件，排除以“Diary_”开头的文件和指定的多日期文档
file_paths <- list.files(pattern = "\\.md$")
file_paths <- file_paths[!grepl("^Diary_", file_paths)]
file_paths <- file_paths[!file_paths %in% c("C_Prose_博客日志.md", "M_Prose_统计暨编程问题日志.md")]

# 定义处理单个Markdown文件的函数
process_post <- function(file_path) {
  file_content <- read_lines(file_path, skip_empty_rows = FALSE)
  
  # 检查文件是否正常读取
  if (length(file_content) == 0) {
    return(data.frame())
  }
  
  # 提取metadata
  metadata_indices <- which(file_content == "---")
  if (length(metadata_indices) < 2) {
    return(data.frame())
  }
  metadata_content <- file_content[(metadata_indices[1] + 1):(metadata_indices[2] - 1)]
  metadata <- list(file = file_path)
  
  # 分析每行元数据
  current_key <- NULL
  tag_count <- 0
  for (line in metadata_content) {
    if (grepl(":", line)) {
      key_value <- str_split(line, ":", 2, simplify = TRUE)
      current_key <- trimws(key_value[1])
      if (current_key != "categories" && current_key != "tags") {
        metadata[[current_key]] <- trimws(key_value[2])
      }
    } else if ((current_key == "categories" || current_key == "tags") && grepl("^\\s+-\\s+", line)) {
      if (current_key == "categories") {
        category_index <- sum(grepl("category", names(metadata))) + 1
        metadata[[paste0("category", category_index)]] <- trimws(sub("^\\s+-\\s+", "", line))
      } else if (current_key == "tags" && tag_count < 3) {
        tag_count <- tag_count + 1
        metadata[[paste0("tag", tag_count)]] <- trimws(sub("^\\s+-\\s+", "", line))
      }
    }
  }
  
  content <- paste(file_content[(metadata_indices[2] + 1):length(file_content)], collapse = "\n") # 提取Content，即metadata区块之后的所有内容
  
  metadata_df <- as.data.frame(t(unlist(metadata)), stringsAsFactors = FALSE) # 将metadata转换为单行DataFrame
  
  combined_df <- cbind(metadata_df, content = content) # 组合metadata和内容
  
  combined_df
}

# 应用函数到所有文件并合并结果
dt_doc <- bind_rows(map(file_paths, process_post), .id = "id")

# 转换日期时间为日期
dt_doc$date <- ymd_hms(dt_doc$date)  # 解析日期时间字符串
dt_doc$date <- as.Date(dt_doc$date)  # 转换为日期
# 检查重复的日期
duplicates <- dt_doc %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  filter(count > 1)
print(duplicates) # 打印重复的日期和它们的计数



###################################################################################################
###################################################################################################
####### 6. dt_diary + dt_doc input  ########
dt <- full_join(dt_doc, dt_diary, by = "date", suffix = c(".doc", ".diary"))

# 合并 title、content 和 category1 列，但不包括 NA 值
dt <- dt %>%
  mutate(title = ifelse(!is.na(title.doc) & !is.na(title.diary), paste(title.doc, title.diary, sep = " / "),
                        ifelse(!is.na(title.doc), title.doc, 
                               ifelse(!is.na(title.diary), title.diary, NA))),
         content = ifelse(!is.na(content.doc) & !is.na(content.diary), paste(content.doc, content.diary, sep = " / "),
                          ifelse(!is.na(content.doc), content.doc, 
                                 ifelse(!is.na(content.diary), content.diary, NA))),
         category1 = ifelse(!is.na(category1.doc) & !is.na(category1.diary), paste(category1.doc, category1.diary, sep = " / "),
                            ifelse(!is.na(category1.doc), category1.doc, 
                                   ifelse(!is.na(category1.diary), category1.diary, NA))))

# 保留需要的列，包括额外的标签和分类列
dt <- select(dt, date, title, content, category1, category2, tag1, tag2, tag3, mathjax)


# 分析标题使用情况的函数
analyze_headings <- function(content) {
  # 确定每个文档的一级标题格式
  first_heading_pattern <- str_extract(content, "(# |## |### )")
  if (is.na(first_heading_pattern)) {
    return(c(title_level1 = 0, title_level2 = 0, title_level3 = 0))
  }
  
  # 根据第一个标题的格式，确定一级、二级和三级标题的格式
  if (first_heading_pattern == "# ") {
    primary = "# "
    secondary = "## "
    tertiary = "### "
  } else if (first_heading_pattern == "## ") {
    primary = "## "
    secondary = "### "
    tertiary = "# "
  } else {
    primary = "### "
    secondary = "# "
    tertiary = "## "
  }
  
  # 计算各级标题的数量
  num_primary = str_count(content, paste0("\n", primary))
  num_secondary = str_count(content, paste0("\n", secondary))
  num_tertiary = str_count(content, paste0("\n", tertiary))
  
  return(c(title_level1 = num_primary, title_level2 = num_secondary, title_level3 = num_tertiary))
}

# 应用到数据框中的每个 content 列
dt_doc <- dt_doc %>%
  mutate(headings = map(content, analyze_headings)) %>%
  unnest_wider(headings)

dt <- dt %>%
  mutate(headings = map(content, analyze_headings)) %>%
  unnest_wider(headings)

rm(list=setdiff(ls(), c('dt','dt_diary','dt_doc')))
save.image("~/Desktop/Blog_Analysis/data.RData")



###################################################################################################
###################################################################################################
####### 7. Data Content Clean ########
load("/Users/yuteng/Desktop/Blog_Analysis/data.RData")
# 处理字符段函数
clean_content <- function(df) {
  df %>% mutate(
    content = gsub("####|\\*|\\[.*?\\]\\(.*?\\)|>|<[^>]+>", "", content),  # 移除Markdown符号、链接和HTML标签
    content = gsub("[\r\n\t]+", " ", content),  # 替换换行符、回车符和制表符为一个空格
    content = gsub("\\s{2,}", " ", content),  # 将两个以上连续的空格替换成一个空格
    content = gsub("^\\s+|\\s+$", "", content)  # 移除字符串开头和结尾的空格
  )
}
# <：标记 HTML 标签的开始。
# [^>]+：匹配一个或多个不是 > 的字符，即标签内的所有内容。
# >：标记 HTML 标签的结束。

# 处理日期数据的函数
process_date_data <- function(df, date_col = "date", content_col = "content") {
  df %>% mutate(
    date = as.Date(.[[date_col]]),
    year = year(.[[date_col]]),
    month = month(.[[date_col]], label = TRUE),
    day = day(.[[date_col]]),
    weekday = wday(.[[date_col]], label = TRUE, week_start = 1),  # 确保周一为每周的开始
    character_count = as.numeric(nchar(.[[content_col]]))  # 使用动态文本列名称, 确保每个月的每一天都被包括进来，并计算每天的总字符数
  )
}

dt <- clean_content(dt); dt <- process_date_data(dt)
dt <- dt %>% filter(year(date) != 2014)

dt_diary <- clean_content(dt_diary); dt_diary <- process_date_data(dt_diary)
dt_diary <- dt_diary %>% filter(year(date) != 2014)

dt_doc <- clean_content(dt_doc); dt_doc <- process_date_data(dt_doc)
dt_doc <- dt_doc %>% filter(year(date) != 2014)


#dt[which(dt$date=='2017-01-13'),]$content #观察是否剔除HTML语言等无关信息
#df <- dt %>% filter(str_detect(category1, "Diary")) #因合并问题，很难取代纯Diary数据
#dt = dt %>% sample_n(10)
save.image("~/Desktop/Blog_Analysis/data_c.RData")



###################################################################################################
###################################################################################################
####### 8. Data Analysis Heatmap & Line ########
library(extrafont)
setwd('~/Desktop/Blog_Analysis')
load("~/Desktop/Blog_Analysis/data_c.RData")

####### 8.1 heatmap diary ########
ac_diary <- dt_diary %>%
  group_by(year, month, day) %>%
  summarize(activity = sum(character_count), .groups = 'drop') %>%
  complete(year, month, day, fill = list(activity = 0)) %>%  # 填充每月的每一天，即使活动量为0
  mutate(date = make_date(year, month, day)) %>%  # 创建一个新的日期列
  filter(date <= make_date(year, month, days_in_month(make_date(year, month, 1)))) %>% #在过滤条件中检查日期的有效性
  arrange(year, desc(month), day) %>% # 按年和月（反向）排序
  filter(date <= as.Date("2024-04-21")) 

ggplot(data = ac_diary, aes(x = day, y = month, 
                                  fill = factor(cut(activity, 
                                                    breaks = c(-1, 0, 100, 300, 1200, Inf),
                                                    labels = c("No Activity, 0", "Low, 1-100", "Moderate, 101-300", "High, 301-1200", "Extreme, 1201+"))))) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("#ebedf0", "#c6e48b", "#239a3b", "#00441b", "#ff0000"),
    guide = guide_legend(title = "Character Count")) +
  scale_x_continuous(breaks = seq(0, 30, by = 5), expand = c(0.05, 0)) + 
  #geom_vline(xintercept = seq(min(ac_diary$day), max(ac_diary$day), by = 10), linetype = "longdash", color = "black") + 
  facet_wrap(~ year, ncol = 3) +
  labs(x = "Day of Month", y = "Month", title = "Diary Heatmap Based on Character Count") +
  theme_minimal() +
  theme(
    text = element_text(family = "Bradley Hand", size = 12),  # 使用特定字体
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(angle = 0),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray"),
    axis.ticks.length.x = unit(-0.05,'cm'), 
    axis.ticks.x = element_line(colour = "gray", linewidth = 0.5),  # 设置刻度线长度，负值表示向内
    axis.ticks.length.y = unit(-0.05,'cm'), 
    axis.ticks.y = element_line(colour = "gray", linewidth = 0.5))

ggsave("heatmap_ac_diary.png", width = 16, height = 8, dpi = 300)


####### 8.2 heatmap dt ########
ac <- dt %>%
  group_by(year, month, day) %>%
  summarize(activity = sum(character_count), .groups = 'drop') %>%
  complete(year, month, day, fill = list(activity = 0)) %>%  # 填充每月的每一天，即使活动量为0
  mutate(date = make_date(year, month, day)) %>%  # 创建一个新的日期列
  filter(date <= make_date(year, month, days_in_month(make_date(year, month, 1)))) %>% #在过滤条件中检查日期的有效性
  arrange(year, desc(month), day) %>% # 按年和月（反向）排序
  filter(date <= as.Date("2024-04-21")) 

ggplot(data = ac, aes(x = day, y = month, 
                            fill = factor(cut(activity, 
                                              breaks = c(-1, 0, 300, 1000, 5000, Inf),
                                              labels = c("No Activity, 0", "Low, 1-300", "Moderate, 301-1000", "High, 1001-5000", "Extreme, 5001+"))))) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("#ebedf0", "#c6e48b", "#239a3b", "#00441b", "#ff0000"),
    guide = guide_legend(title = "Character Count")) +
  scale_x_continuous(breaks = seq(0, 30, by = 5), expand = c(0.05, 0)) + 
  facet_wrap(~ year, ncol = 3) + 
  labs(x = "Day of Month", y = "Month", title = "Post Heatmap Based on Character Count") +
  theme_minimal() +
  theme(
    text = element_text(family = "Bradley Hand", size = 12),  # 使用特定字体
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(angle = 0),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray"),
    axis.ticks.length.x = unit(-0.05,'cm'), 
    axis.ticks.x = element_line(colour = "gray", linewidth = 0.5),  # 设置刻度线长度，负值表示向内
    axis.ticks.length.y = unit(-0.05,'cm'), 
    axis.ticks.y = element_line(colour = "gray", linewidth = 0.5))

ggsave("heatmap_ac_dt.png", width = 16, height = 8, dpi = 300)


####### 8.3 Line diary ########
# 按季度聚合数据
ac_diary_quarterly <- dt_diary %>%
  mutate(quarter = floor_date(date, "quarter")) %>%
  group_by(quarter) %>%
  summarise(character_count = sum(character_count, na.rm = TRUE))

# Plotting quarterly character count with trend line
ggplot(ac_diary_quarterly, aes(x = quarter, y = character_count / 10000)) +
  geom_point() +  # Points for each quarter
  ggalt::geom_xspline(spline_shape = -0.3, size = 1) + #Smooth Line
  #geom_line(group = 1) +  # Line connecting the points
  geom_smooth(color = 'red', method = "loess") +  # Smooth trend line using loess method
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # 每年显示一个标签
  #scale_x_date(date_breaks = "3 months", date_labels = "%Y Q%q") +
  labs(
    title = "Diary Quarterly Character Count",
    x = "Quarter",
    y = "Total Character Count / 10,000"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Bradley Hand", size = 12),  # Specific font style
    axis.text.x = element_text(angle = 0, hjust = 1),  # Horizontal x-axis labels
    axis.text.y = element_text(angle = 0),  # Horizontal y-axis labels
    strip.background = element_blank(),  # Remove background of facet labels
    panel.spacing = unit(0.1, "lines"),  # Spacing between panels
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "gray"), # Gray color for axis lines
    axis.ticks.length.x = unit(-0.1,'cm'), 
    axis.ticks.x = element_line(colour = "gray", linewidth = 0.5),  # 设置刻度线长度，负值表示向内
    axis.ticks.length.y = unit(-0.1,'cm'), 
    axis.ticks.y = element_line(colour = "gray", linewidth = 0.5) 
  )

ggsave("Line_ac_diary.png", width = 16, height = 8, dpi = 300)

####### 8.4 Line dt ########
# 按季度聚合数据
ac_quarterly <- dt %>%
  mutate(quarter = floor_date(date, "quarter")) %>%
  group_by(quarter) %>%
  summarise(character_count = sum(character_count, na.rm = TRUE))

# Plotting quarterly character count with trend line
ggplot(ac_quarterly, aes(x = quarter, y = character_count / 10000)) +
  geom_point() +  # Points for each quarter
  #ggalt::geom_xspline(spline_shape = -0.3, size = 1) +
  geom_line(group = 1) +  # Line connecting the points
  geom_smooth(color = 'red', method = "loess") +  # Smooth trend line using loess method
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # 每年显示一个标签
  #scale_x_date(date_breaks = "3 months", date_labels = "%Y Q%q") +
  labs(
    title = "Post Quarterly Character Count",
    x = "Quarter",
    y = "Total Character Count / 10,000"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Bradley Hand", size = 12),  # Specific font style
    axis.text.x = element_text(angle = 0, hjust = 1),  # Horizontal x-axis labels
    axis.text.y = element_text(angle = 0),  # Horizontal y-axis labels
    strip.background = element_blank(),  # Remove background of facet labels
    panel.spacing = unit(0.1, "lines"),  # Spacing between panels
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "gray"),  # Gray color for axis lines
    axis.ticks.length.x = unit(-0.1,'cm'), 
    axis.ticks.x = element_line(colour = "gray", linewidth = 0.5),  # 设置刻度线长度，负值表示向内
    axis.ticks.length.y = unit(-0.1,'cm'), 
    axis.ticks.y = element_line(colour = "gray", linewidth = 0.5)
  )

ggsave("Line_ac_dt.png", width = 16, height = 8, dpi = 300)



###################################################################################################
###################################################################################################
####### 9. Data Analysis Word Cloud (dt) ########
library(wordcloud2)
#library(gridExtra)
library(reticulate)
use_condaenv("nlp", required = TRUE) # 指定Python环境
jieba <- import("jieba") # 导入jieba库

setwd('~/Desktop/Blog_Analysis')
load("~/Desktop/Blog_Analysis/data_c.RData")

# 加载或定义停用词列表
stopwords_dir <- "/Users/yuteng/Desktop/DATA_CN/DATA_BigDt/NLP_停用词语库" # 设置文件夹路径
files <- list.files(stopwords_dir, full.names = TRUE, pattern = "\\.txt$") # 列出所有文本文件
stopwords_list <- unlist(lapply(files, readLines, encoding = "UTF-8")) # 读取所有文件并合并内容
stopwords <- unique(stopwords_list) # 去除重复的停用词
#length(stopwords);head(stopwords) # 查看停用词数量和部分内容

# 修改批量分词函数，添加停用词处理
batch_content_cut <- function(texts, stopwords) {
  python_code <- sprintf("
def batch_cut(texts, stopwords):
    import jieba
    results = []
    for text in texts:
        cut_words = [word for word in jieba.cut(text, cut_all=False) if word not in stopwords]
        results.append(cut_words)
    return results
", paste(shQuote(stopwords), collapse=", "))
  
  py_run_string(python_code) # 运行 Python 代码
  
  results <- py$batch_cut(texts, stopwords) # 调用 Python 函数并将结果转为 R 的向量
  lapply(results, as.character)  # 将每个分词结果转换为字符向量
}

####### 9.1 word data ########
# 将内容分成较小的批次并应用分词
batch_size <- 100  # 根据需要调整批次大小
n <- nrow(dt)
batch_indices <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

# 应用批量分词并直接使用结果
dt$content_cut <- do.call(c, lapply(batch_indices, function(indices) {
  batch_content_cut(dt$content[indices], stopwords)
}))

# # 检查结果长度是否与原数据行数一致
# if (length(dt$content_cut) != nrow(dt)) {
#   stop("Error: The length of the processed data does not match the original data frame.")
# }

dt$content_cut <- lapply(dt$content_cut, function(words) {
  Filter(function(word) nchar(word) > 1, words)
})

# 统计每年的词频
word_counts <- dt %>%
  unnest(content_cut) %>%
  group_by(year, word = content_cut) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# 选择每年前30个高频词
top_words_by_year <- word_counts %>%
  group_by(year) %>%
  slice_max(order_by = count, n = 30) %>%
  ungroup()

str(top_words_by_year) # 查看数据结构
top_words_by_year <- top_words_by_year %>% rename(freq = count, word = word) # 生成词云需要的列名要符合wordcloud2的要求
top_words_by_year$freq <- as.numeric(as.character(top_words_by_year$freq)) # 确保 freq 列是数值型

# 清理数据，确保只包含word和freq两列
clean_data <- function(data) {
  required_columns <- c("word", "freq")
  data <- data[, required_columns, drop = FALSE]  # 仅保留必要的列
  return(data)
}

# 生成词云
cloud_list <- lapply(split(top_words_by_year, top_words_by_year$year), function(data) {
  clean_data <- clean_data(data)  # 清理数据
  if (nrow(clean_data) > 0 && all(colnames(clean_data) %in% c("word", "freq"))) {
    try(
      wordcloud2(clean_data, size = 1, fontFamily = "STFangSong", color = rep_len( c("red","black"), nrow(clean_data)), #"random-dark", #shape = 'circle', 
                 minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1), 
      silent = TRUE
    )
  } else {
    NULL  # 如果数据不合适，则返回NULL
  }
})

rm(list=setdiff(ls(), c('dt','dt_diary','dt_doc','cloud_list','top_words_by_year')))

####### 9.2 word pic ########
library(webshot) # 为每个词云创建图像文件
# wordcloud2 应该返回 HTML widget 类型的对象，而非常规的 grid 图形对象（grob）。
# 如果 wordcloud2 返回的是 HTML widget，我们不能使用 gridExtra::grid.arrange 来显示它们，因为这个函数是为 grob 设计的。

lapply(seq_along(cloud_list), function(i) {
  html_file <- tempfile(fileext = ".html") # 创建一个临时的 HTML 文件
  htmlwidgets::saveWidget(cloud_list[[i]], html_file, selfcontained = TRUE)

  image_file <- paste0("wordcloud_", i, ".png") # 指定输出图像文件名
  webshot(html_file, file = image_file, zoom = 1, delay = 1) # 使用 webshot 生成截图, 添加 zoom 参数 #cliprect = c(250, 250, 500, 300)
  # 如果需要更精确的控制，可以尝试添加 cliprect 参数
  # webshot(html_file, file = image_file, cliprect = c("top", "left", "width", "height"))
  image_file # 返回生成的图像文件名
})

library(magick) # 合并图像
image_list <- lapply(1:9, function(i) image_read(paste0("wordcloud_", i, ".png")))

# 加载和标注每个图像
annotated_images <- lapply(2015:2024, function(year) {
  image_path <- paste0("wordcloud_", year - 2014, ".png")
  img <- image_read(image_path)
  img <- image_annotate(img, text = as.character(year), size = 50, gravity = "north", location = "+100+25", color = "black", font = "Bradley Hand")
})

# 使用 image_montage 合并图像
combined_image <- image_montage(
  do.call(c, annotated_images),
  tile = "3x4",  # 3列4行布局
  geometry = "+5+5"  # 图像之间的间隔
)

# 查看合并后的图像
print(combined_image)

# 保存合并后的图像
image_write(combined_image, "combined_wordcloud.png")

# 调试代码
# library(webshot)
# library(htmlwidgets)
#
# # 假设 wordcloud_1 对应的是 cloud_list 中的第一个元素
# html_file <- tempfile(fileext = ".html")
# saveWidget(cloud_list[[9]], html_file, selfcontained = TRUE)
# 
# # 指定输出图像文件名
# image_file <- "wordcloud_1.png"
# 
# # 使用 webshot 生成截图，开始时可以使用默认的 zoom
# webshot(html_file, file = image_file, zoom = 1, delay = 1)
# 
# # 如果需要更精确的控制，尝试添加 cliprect 参数
# # cliprect 接受四个数字，分别表示矩形区域的 top, left, width, height
# # webshot(html_file, file = image_file, cliprect = c(250, 250, 500, 300))
# 
# # 如果在 RStudio 中运行，可以直接打开图片查看效果
# if (file.exists(image_file)) {
#   utils::browseURL(image_file)
# }



###################################################################################################
###################################################################################################
####### 10. Data Analysis Word Cloud (dt_diary) ########
library(wordcloud2)
#library(gridExtra)
library(reticulate)
use_condaenv("nlp", required = TRUE) # 指定Python环境
jieba <- import("jieba") # 导入jieba库

setwd('~/Desktop/Blog_Analysis')
load("~/Desktop/Blog_Analysis/data_c.RData")

# 加载或定义停用词列表
stopwords_dir <- "/Users/yuteng/Desktop/DATA_CN/DATA_BigDt/NLP_停用词语库" # 设置文件夹路径
files <- list.files(stopwords_dir, full.names = TRUE, pattern = "\\.txt$") # 列出所有文本文件
stopwords_list <- unlist(lapply(files, readLines, encoding = "UTF-8")) # 读取所有文件并合并内容
stopwords <- unique(stopwords_list) # 去除重复的停用词
#length(stopwords);head(stopwords) # 查看停用词数量和部分内容

# 修改批量分词函数，添加停用词处理
batch_content_cut <- function(texts, stopwords) {
  python_code <- sprintf("
def batch_cut(texts, stopwords):
    import jieba
    results = []
    for text in texts:
        cut_words = [word for word in jieba.cut(text, cut_all=False) if word not in stopwords]
        results.append(cut_words)
    return results
", paste(shQuote(stopwords), collapse=", "))
  
  py_run_string(python_code) # 运行 Python 代码
  
  results <- py$batch_cut(texts, stopwords) # 调用 Python 函数并将结果转为 R 的向量
  lapply(results, as.character)  # 将每个分词结果转换为字符向量
}

####### 10.1 word data ########
# 将内容分成较小的批次并应用分词
batch_size <- 100  # 根据需要调整批次大小
n <- nrow(dt_diary)
batch_indices <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

# 应用批量分词并直接使用结果
dt_diary$content_cut <- do.call(c, lapply(batch_indices, function(indices) {
  batch_content_cut(dt_diary$content[indices], stopwords)
}))

# # 检查结果长度是否与原数据行数一致
# if (length(dt$content_cut) != nrow(dt)) {
#   stop("Error: The length of the processed data does not match the original data frame.")
# }

dt_diary$content_cut <- lapply(dt_diary$content_cut, function(words) {
  Filter(function(word) nchar(word) > 1, words)
})

# 统计每年的词频
word_counts <- dt_diary %>%
  unnest(content_cut) %>%
  group_by(year, word = content_cut) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

# 选择每年前30个高频词
top_words_by_year <- word_counts %>%
  group_by(year) %>%
  slice_max(order_by = count, n = 30) %>%
  ungroup()

str(top_words_by_year) # 查看数据结构
top_words_by_year <- top_words_by_year %>% rename(freq = count, word = word) # 生成词云需要的列名要符合wordcloud2的要求
top_words_by_year$freq <- as.numeric(as.character(top_words_by_year$freq)) # 确保 freq 列是数值型

# 清理数据，确保只包含word和freq两列
clean_data <- function(data) {
  required_columns <- c("word", "freq")
  data <- data[, required_columns, drop = FALSE]  # 仅保留必要的列
  return(data)
}

# 生成词云
cloud_list <- lapply(split(top_words_by_year, top_words_by_year$year), function(data) {
  clean_data <- clean_data(data)  # 清理数据
  if (nrow(clean_data) > 0 && all(colnames(clean_data) %in% c("word", "freq"))) {
    try(
      wordcloud2(clean_data, size = 1, fontFamily = "STFangSong", color = rep_len( c("red","black"), nrow(clean_data)), #"random-dark", #shape = 'circle', 
                 minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1), 
      silent = TRUE
    )
  } else {
    NULL  # 如果数据不合适，则返回NULL
  }
})

rm(list=setdiff(ls(), c('dt','dt_diary','dt_doc','cloud_list','top_words_by_year')))

####### 10.2 word pic ########
library(webshot) # 为每个词云创建图像文件
# wordcloud2 应该返回 HTML widget 类型的对象，而非常规的 grid 图形对象（grob）。
# 如果 wordcloud2 返回的是 HTML widget，我们不能使用 gridExtra::grid.arrange 来显示它们，因为这个函数是为 grob 设计的。

lapply(seq_along(cloud_list), function(i) {
  html_file <- tempfile(fileext = ".html") # 创建一个临时的 HTML 文件
  htmlwidgets::saveWidget(cloud_list[[i]], html_file, selfcontained = TRUE)
  
  image_file <- paste0("wordcloud_", i, ".png") # 指定输出图像文件名
  webshot(html_file, file = image_file, zoom = 1, delay = 1) # 使用 webshot 生成截图, 添加 zoom 参数 #cliprect = c(250, 250, 500, 300)
  # 如果需要更精确的控制，可以尝试添加 cliprect 参数
  # webshot(html_file, file = image_file, cliprect = c("top", "left", "width", "height"))
  image_file # 返回生成的图像文件名
})

library(magick) # 合并图像
image_list <- lapply(1:9, function(i) image_read(paste0("wordcloud_", i, ".png")))

# 加载和标注每个图像
annotated_images <- lapply(2015:2024, function(year) {
  image_path <- paste0("wordcloud_", year - 2014, ".png")
  img <- image_read(image_path)
  img <- image_annotate(img, text = as.character(year), size = 50, gravity = "north", location = "+100+25", color = "black", font = "Bradley Hand")
})

# 使用 image_montage 合并图像
combined_image <- image_montage(
  do.call(c, annotated_images),
  tile = "3x4",  # 3列4行布局
  geometry = "+5+5"  # 图像之间的间隔
)

# 查看合并后的图像
print(combined_image)



###################################################################################################
###################################################################################################
####### 11. Data Analysis Senti (dt_diary) ########
# Ref: https://mp.weixin.qq.com/s/_Z9drxk1-cZ5VbXRi7Z8Rw
# 参考上文，我进行了较大的魔改，此处没有使用Gpt，而是直接coding了。
####### 11.1 Environment ########
library(tidyverse)
library(jsonlite)
library(httr)
source('/Users/yuteng/Desktop/DATA_CN/DATA_Key/Key.R')
# baidu_sentiment_api <- "https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials&client_id=your_api_id&client_secret=your_api_key"
lst <- fromJSON(baidu_sentiment_api) 
lst$access_token

bd_senti <- function(x, access_token = lst$access_token) {
  if (is.null(x) || x == "") {
    return(tibble(text = NA, confidence = NA, negative_prob = NA, positive_prob = NA, sentiment = NA))
  }
  
  # 移除所有反斜杠
  cleaned_text <- gsub("\\\\", "", x, fixed = TRUE)
  # 将清洗后的文本转为JSON
  cleaned_text <- toJSON(list(text = cleaned_text), auto_unbox = TRUE)
  
  response <- POST(
    url = paste0("https://aip.baidubce.com/rpc/2.0/nlp/v1/sentiment_classify?charset=UTF-8&access_token=", access_token),
    add_headers('Content-Type' = 'application/json', 'Accept' = 'application/json'),
    body = cleaned_text
  )
  
  res <- content(response, as = "parsed", type = "application/json")
  
  if (!is.null(res$items)) {
    res$items %>%
      transpose() %>%
      as_tibble() %>%
      unnest(cols = c(confidence, negative_prob, positive_prob, sentiment)) %>%
      mutate(text = x) %>%
      select(text, everything())
  } else {
    tibble(text = x, confidence = NA, negative_prob = NA, positive_prob = NA, sentiment = NA)
  }
}

####### 11.2 Analysis ########
# # Save Method 1
# dt_diary_res1 <- dt_diary %>%
#   mutate(res = map(content, bd_senti))
# dt_diary_res2 <- dt_diary_res1 %>% unnest(res)

# Save Method 2
dir.create("sentires")

dt_diary <- dt_diary %>% mutate(id = row_number())
tempres <- lapply(1:nrow(dt_diary), function(x){
  try({
    dt_diary %>%
      slice(x) %>%
      mutate(res = map(content, bd_senti)) %>% #调用baidu api
      write_rds(paste0("sentires/", x, ".rds"))
    })
  })

dt_diary_res3 <- lapply(fs::dir_ls("sentires"), readr::read_rds) %>%
  bind_rows() %>%
  unnest(res) # 合并结果

rm(list=setdiff(ls(), c('dt','dt_diary','dt_doc',
                        #'dt_diary_res1','dt_diary_res2',
                        'tempres','dt_diary_res3')))
save.image("~/Desktop/Blog_Analysis/data_s.RData")

#' ####### 11.3 Analysis NA ########
#' # 在意识到\可能导致json转译出错后，已经补全了11.2的识别代码，因此这部分是一次性代码
#' safe_bd_senti <- function(x, access_token = lst$access_token) {
#'   if (is.null(x) || x == "") {
#'     return(tibble(text = NA, confidence = NA, negative_prob = NA, positive_prob = NA, sentiment = NA))
#'   }
#'   
#'   # 移除所有反斜杠
#'   cleaned_text <- gsub("\\\\", "", x, fixed = TRUE)
#'   # 将清洗后的文本转为JSON
#'   cleaned_text <- toJSON(list(text = cleaned_text), auto_unbox = TRUE)
#'   
#'   response <- POST(
#'     url = paste0("https://aip.baidubce.com/rpc/2.0/nlp/v1/sentiment_classify?charset=UTF-8&access_token=", access_token),
#'     add_headers('Content-Type' = 'application/json', 'Accept' = 'application/json'),
#'     body = cleaned_text
#'   )
#'   
#'   res <- content(response, as = "parsed", type = "application/json")
#'   
#'   if (!is.null(res$items)) {
#'     return(res$items %>%
#'              transpose() %>%
#'              as_tibble() %>%
#'              unnest(cols = c(confidence, negative_prob, positive_prob, sentiment)) %>%
#'              mutate(text = x) %>%
#'              select(text, everything()))
#'   } else {
#'     return(tibble(text = x, confidence = NA, negative_prob = NA, positive_prob = NA, sentiment = NA))
#'   }
#' }
#' 
#' # 1.确保每个需要重新分析的文本都是因为API调用失败（sentiment为NA）
#' rows_to_reanalyze <- dt_diary_res3 %>% filter(is.na(sentiment))
#' 
#' # 2.重新调用API进行情感分析
#' reanalyzed_results <- map_dfr(rows_to_reanalyze$text, ~ safe_bd_senti(.x, access_token = lst$access_token))
#' 
#' # 3.识别重新分析的id，并按dt_diary_res3写成rds的格式重新输出
#' rows_to_reanalyze.r <- rows_to_reanalyze %>% 
#'   left_join(reanalyzed_results,by = "text") %>% 
#'   mutate(
#'     sentiment = coalesce(sentiment.x, sentiment.y),
#'     confidence = coalesce(confidence.x, confidence.y),
#'     negative_prob = coalesce(negative_prob.x, negative_prob.y),
#'     positive_prob = coalesce(positive_prob.x, positive_prob.y)
#'   ) %>%
#'   select(-contains(".x"), -contains(".y"))  # 移除临时列
#' 
#' rows_to_reanalyze.r$res <- NA
#' rows_to_reanalyze.r <- rows_to_reanalyze.r[-which(is.na(rows_to_reanalyze.r$sentiment)==TRUE),]
#' 
#' for (i in 1:nrow(rows_to_reanalyze.r)) {
#'   rows_to_reanalyze.r$res[i] <- list(
#'     tibble(
#'       text = rows_to_reanalyze.r$text[i],
#'       confidence = rows_to_reanalyze.r$confidence[i],
#'       negative_prob = rows_to_reanalyze.r$negative_prob[i],
#'       positive_prob = rows_to_reanalyze.r$positive_prob[i],
#'       sentiment = rows_to_reanalyze.r$sentiment[i]
#'     )
#'   )
#' }
#' 
#' rows_to_reanalyze.r <- rows_to_reanalyze.r[,-c(11:15)]
#' 
#' tempres2 <- lapply(1:nrow(rows_to_reanalyze.r), function(x){
#'   try({
#'     # 提取当前行的数据
#'     current_row <- rows_to_reanalyze.r %>%
#'       slice(x)
#'     
#'     # 获取当前行的ID
#'     current_id <- current_row$id
#'     
#'     # 写入RDS文件
#'     write_rds(current_row, paste0("sentires/", current_id, ".rds"))
#'   })
#' })
#' 
#' dt_diary_res4 <- lapply(fs::dir_ls("sentires"), readr::read_rds) %>%
#'   bind_rows() %>%
#'   unnest(res) # 合并结果
#' 
#' rm(list=setdiff(ls(), c('dt','dt_diary','dt_doc',
#'                         #'dt_diary_res1','dt_diary_res2',
#'                         'tempres','dt_diary_res3',
#'                         'tempres2','dt_diary_res4')))
#' save.image("~/Desktop/Blog_Analysis/data_s.RData")

####### 11.3 Plot ########
setwd('~/Desktop/Blog_Analysis')
load("~/Desktop/Blog_Analysis/data_s.RData")
rm(list=setdiff(ls(), c('dt_diary_res4')))

library(tidyverse)
library(lubridate)

# 确保date列是日期格式
dt_diary_res4$date <- as.Date(dt_diary_res4$date)

# 重新编码情绪值并剔除NA
dt_diary_res4 <- dt_diary_res4 %>%
  filter(!is.na(sentiment)) %>%  # 剔除情绪值为NA的行
  mutate(sentiment = recode(sentiment, `0` = -1, `1` = 0, `2` = 1)) %>% 
  mutate(quarter = floor_date(date, "quarter"))

# 创建季度字段
# dt_diary_res4$quarter <- quarter(dt_diary_res4$date, with_year = TRUE)

# 计算每个季度的平均情绪值
quarterly_sentiment <- dt_diary_res4 %>%
  group_by(quarter) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE))

ggplot(quarterly_sentiment, aes(x = quarter, y = mean_sentiment)) +
  geom_point() +  # Points for each quarter
  ggalt::geom_xspline(spline_shape = -0.3, size = 1) +
  #geom_line(group = 1) +  # Line connecting the points
  geom_smooth(color = 'red', method = "loess") +  # Smooth trend line using loess method
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # 每年显示一个标签
  geom_hline(yintercept = 0, linetype = "longdash", color = "black") + 
  labs(
    title = "Quarterly Sentiment Change",
    x = "Quarter",
    y = "Average Sentiment Value"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Bradley Hand", size = 12),  # Specific font style
    axis.text.x = element_text(angle = 0, hjust = 1),  # Horizontal x-axis labels
    axis.text.y = element_text(angle = 0),  # Horizontal y-axis labels
    strip.background = element_blank(),  # Remove background of facet labels
    panel.spacing = unit(0.1, "lines"),  # Spacing between panels
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "gray"),  # Gray color for axis lines
    axis.ticks.length.x = unit(-0.1,'cm'), 
    axis.ticks.x = element_line(colour = "gray", linewidth = 0.5),  # 设置刻度线长度，负值表示向内
    axis.ticks.length.y = unit(-0.1,'cm'), 
    axis.ticks.y = element_line(colour = "gray", linewidth = 0.5)
  )

ggsave("Line_sentiment_diary.png", width = 16, height = 8, dpi = 300)

####### 11.4 双坐标轴 Plot ########
# load("~/Desktop/Blog_Analysis/data_c.RData")
# # 按季度聚合数据
# ac_diary_quarterly <- dt_diary %>%
#   mutate(quarter = floor_date(date, "quarter")) %>%
#   group_by(quarter) %>%
#   summarise(character_count = sum(character_count, na.rm = TRUE))
# 
# rm(list=setdiff(ls(), c('ac_diary_quarterly','quarterly_sentiment')))
# df <- left_join(ac_diary_quarterly, quarterly_sentiment, by='quarter')

setwd('~/Desktop/Blog_Analysis')
load("/Users/yuteng/Desktop/Blog_Analysis/data_plot.RData")

# 预计算转换系数
scale_factor <- max(df$mean_sentiment) / max(df$character_count)

# 创建绘图
p <- ggplot(df, aes(x = quarter)) +
  geom_point(aes(y = mean_sentiment, color='Average Sentiment')) +  # 主y轴的点
  ggalt::geom_xspline(aes(y = mean_sentiment, color='Average Sentiment'), spline_shape = -0.3, size = 1) +  # 主y轴的曲线
  geom_smooth(aes(y = mean_sentiment, color='Average Sentiment'), method = "lm", se = FALSE) +  # 主y轴的平滑线
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_hline(yintercept = 0, linetype = "longdash", color = "black") +
  labs(
    title = "Quarterly Sentiment Change",
    x = "Quarter",
    y = "Average Sentiment Value",
    color = "Indicator"  # 设置图例的标题
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Bradley Hand", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text.y = element_text(angle = 0),
    strip.background = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray"),
    axis.ticks.length.x = unit(-0.1,'cm'),
    axis.ticks.x = element_line(colour = "gray", linewidth = 0.5),
    axis.ticks.length.y = unit(-0.1,'cm'),
    axis.ticks.y = element_line(colour = "gray", linewidth = 0.5)
  )

# 添加次y轴，scale_y_continuous添加一个次y轴
p <- p + scale_y_continuous(
  sec.axis = sec_axis(~ . / scale_factor / 10000, name = "Total Character Count / 10,000")
) +
  ggalt::geom_xspline(aes(y = character_count * scale_factor, color = "Character Count"), spline_shape = -0.3, size = 1) +  # 次y轴的曲线
  geom_smooth(aes(y = character_count * scale_factor, color = "Character Count"), method = "lm", se = FALSE) +  # 次y轴的平滑线
  geom_point(aes(y = character_count * scale_factor, color = "Character Count"))  # 次y轴的点

# 保存图像
ggsave("Line_ac_sentiment_diary.png", plot = p, width = 16, height = 8, dpi = 300)

cor(df$character_count, df$mean_sentiment, method = "pearson") # 计算相关系数
df$character_count = df$character_count/10000
model <- lm(mean_sentiment ~ character_count, data = df)
model_poly <- lm(mean_sentiment ~ character_count + I(character_count^2), data = df)
summary(model_poly)


