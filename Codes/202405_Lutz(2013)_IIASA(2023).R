# Replicative Research: Can Below-replacement Fertility Be Desirable (2013)
# Organization: RUC
# Author: Yuteng Yan
# Time: 20/05/2024



###################################################################################################
###################################################################################################
####### 1. Building Environment ########
rm(list=ls())
library(tidyverse)
library(wcde)
load('~/Desktop/3_家庭结构研究/Project_Support_Ratio/IIASA_Dt_EU.RData')


###################################################################################################
###################################################################################################
####### 2. Data ########
dt = get_wcde(indicator = 'pop', country_name = c("Finland", "Germany"),
              pop_age = 'all', pop_edu = 'four', scenario = 1:5)
df.i = get_wcde(indicator = "tfr",
                country_name = c("Finland", "Germany"),
                scenario = 1:5)
df <- data.frame(
  Variable = c('edc','edw','penc','lmen','lmex','slw'),
  #c("Education cost", "Productivity weight", "Pension cost", "Labour market entry age",
  #"Labour market exit age", "Share of life-years Gained spent working"),
  No_Education = c(1, 1, 1, 15, 57, 1),
  Primary = c(1.18, 1.25, 1, 15, 57, 1),
  Secondary = c(1.29, 1.5, 1, 19, 61, 1),
  Tertiary = c(1.36, 2, 1, 26, 65, 1)
)

save.image("~/Desktop/3_家庭结构研究/Project_Support_Ratio/IIASA_Dt_EU.RData")

# 创建一个函数将年龄组展开为1岁组
expand_age_group <- function(dt) {
  dt %>%
    mutate(age_group = case_when(
      age == "0--4"  ~ list(0:4),
      age == "5--9"  ~ list(5:9),
      age == "10--14" ~ list(10:14),
      age == "15--19" ~ list(15:19),
      age == "20--24" ~ list(20:24),
      age == "25--29" ~ list(25:29),
      age == "30--34" ~ list(30:34),
      age == "35--39" ~ list(35:39),
      age == "40--44" ~ list(40:44),
      age == "45--49" ~ list(45:49),
      age == "50--54" ~ list(50:54),
      age == "55--59" ~ list(55:59),
      age == "60--64" ~ list(60:64),
      age == "65--69" ~ list(65:69),
      age == "70--74" ~ list(70:74),
      age == "75--79" ~ list(75:79),
      age == "80--84" ~ list(80:84),
      age == "85--89" ~ list(85:89),
      age == "90--94" ~ list(90:94),
      age == "95--99" ~ list(95:99),
      age == "100+"  ~ list(100)
    )) %>%
    unnest(cols = c(age_group)) %>%
    mutate(pop = if_else(age != "100+", pop / 5, pop)) %>%
    select(-age) %>%
    rename(age = age_group)
}

# 展开年龄组
dt_expanded <- expand_age_group(dt)



###################################################################################################
###################################################################################################
####### 3. Analysis ########
####### 3.1 EWC ########
# 计算pop0-5, pop6-10, pop11-18, pop19-25的总和
pop_preschool <- dt_expanded %>% group_by(scenario,name,year) %>% filter(age >= 0 & age <= 15, education %in% c("Under 15", "No Education")) %>% summarise(total = sum(pop))
pop_prim <- dt_expanded %>% group_by(scenario,name,year) %>% filter(age >= 0 & age <= 15, education == "Primary") %>% summarise(total = sum(pop))
pop_sec <- dt_expanded %>% group_by(scenario,name,year) %>% filter(age >= 10 & age <= 18, education == "Secondary") %>% summarise(total = sum(pop))
pop_tert <- dt_expanded %>% group_by(scenario,name,year) %>% filter(age >= 10 & age <= 25, education == "Post Secondary") %>% summarise(total = sum(pop))

# 获取ed0cost, ed1cost, ed2cost, ed3cost
ed0cost <- df %>% filter(Variable == "edc") %>% select(No_Education) %>% pull()
ed1cost <- df %>% filter(Variable == "edc") %>% select(Primary) %>% pull()
ed2cost <- df %>% filter(Variable == "edc") %>% select(Secondary) %>% pull()
ed3cost <- df %>% filter(Variable == "edc") %>% select(Tertiary) %>% pull()

# 合并所有分组后的数据，并计算 EWC
EWC <- pop_preschool %>%
  left_join(pop_prim, by = c("scenario", "name", "year"), suffix = c(".preschool", ".prim")) %>%
  left_join(pop_sec, by = c("scenario", "name", "year"), suffix = c("", ".sec")) %>%
  left_join(pop_tert, by = c("scenario", "name", "year"), suffix = c("", ".tert")) 
colnames(EWC)[6] <- 'total.sec'

EWC = EWC %>% group_by(scenario,name,year) %>% 
  mutate(EWC = total.preschool * ed0cost + total.prim * ed1cost + total.sec * ed2cost + total.tert * ed3cost)

####### 3.2 EWW ########
# 计算 pop16-57, pop19-61, pop26-65 的总和，按 scenario, name, year 分组
pop_prim_16_57 <- dt_expanded %>% 
  group_by(scenario, name, year) %>% 
  filter(age >= 16 & age <= 57, education %in% c("No Education", "Primary")) %>% 
  summarise(total = sum(pop))

pop_sec_19_61 <- dt_expanded %>% 
  group_by(scenario, name, year) %>% 
  filter(age >= 19 & age <= 61, education == "Secondary") %>% 
  summarise(total = sum(pop))

pop_tert_26_65 <- dt_expanded %>% 
  group_by(scenario, name, year) %>% 
  filter(age >= 26 & age <= 65, education == "Post Secondary") %>% 
  summarise(total = sum(pop))

# 获取 ed1weight, ed2weight, ed3weight
ed1weight <- df %>% filter(Variable == "edw") %>% select(Primary) %>% pull()
ed2weight <- df %>% filter(Variable == "edw") %>% select(Secondary) %>% pull()
ed3weight <- df %>% filter(Variable == "edw") %>% select(Tertiary) %>% pull()

# 合并所有分组后的数据，并计算 EWW
EWW <- pop_prim_16_57 %>%
  left_join(pop_sec_19_61, by = c("scenario", "name", "year"), suffix = c(".prim", ".sec")) %>%
  left_join(pop_tert_26_65, by = c("scenario", "name", "year"), suffix = c("", ".tert")) 
colnames(EWW)[6] = 'total.tert'
EWW = EWW %>% group_by(scenario,name,year) %>% 
  mutate(EWW = total.prim * ed1weight + total.sec * ed2weight + total.tert * ed3weight)

####### 3.3 R ########
# 计算 pop58+, pop62+, pop66+ 的总和，按 scenario, name, year 分组
pop_prim_58_plus <- dt_expanded %>% 
  group_by(scenario, name, year) %>% 
  filter(age >= 58, education %in% c("No Education", "Primary")) %>% 
  summarise(total = sum(pop))

pop_sec_62_plus <- dt_expanded %>% 
  group_by(scenario, name, year) %>% 
  filter(age >= 62, education == "Secondary") %>% 
  summarise(total = sum(pop))

pop_tert_66_plus <- dt_expanded %>% 
  group_by(scenario, name, year) %>% 
  filter(age >= 66, education == "Post Secondary") %>% 
  summarise(total = sum(pop))

# 获取 pencost
pencost <- df %>% filter(Variable == "penc") %>% select(No_Education) %>% pull()

# 合并所有分组后的数据，并计算 R
R <- pop_prim_58_plus %>%
  left_join(pop_sec_62_plus, by = c("scenario", "name", "year"), suffix = c(".prim", ".sec")) %>%
  left_join(pop_tert_66_plus, by = c("scenario", "name", "year"), suffix = c("", ".tert")) 
colnames(R)[6] = 'total.tert'

R = R %>% group_by(scenario,name,year) %>%
  mutate(R = total.prim * pencost + total.sec * pencost + total.tert * pencost)

####### 3.4 EWSR ########
# 合并 EWC, EWW 和 R，并计算 EWSR
EWSR <- EWC %>%
  left_join(EWW, by = c("scenario", "name", "year")) %>%
  left_join(R, by = c("scenario", "name", "year")) %>%
  mutate(EWSR = EWW / (EWC + R)) %>%
  select(scenario, name, year, EWSR)


# 格式化 period 字段，保留前四位
df.i <- df.i %>%
  mutate(period = ifelse(period == "2095-2100", "2095", substr(period, 1, 4)))
df.2095 <- df.i %>% filter(period == "2095")
df.2100 <- df.2095 %>% mutate(period = "2100") # 复制 2095-2100 对应的行，并将 period 改为 2100
df.i <- bind_rows(df.i, df.2100) # 将 2100 行合并回原数据
colnames(df.i)[4] <- 'year'
df.i$year <- as.numeric(df.i$year)
df.i <- df.i %>% select("scenario","name","year",'tfr')

EWSR <- left_join(EWSR,df.i,by=c("scenario","name","year"))
rm(list=setdiff(ls(), 'EWSR'))
#save.image("~/Desktop/3_家庭结构研究/Project_Support_Ratio/EWSR_EU.RData")



###################################################################################################
###################################################################################################
####### 4. Plot ########
####### 4.1 3D Plot ########
library(plotly) # 创建3D图表
load("~/Desktop/3_家庭结构研究/Project_Support_Ratio/EWSR_EU.RData")
dp <- EWSR %>% filter(name=='Finland')

plot_ly(data = dp, x = ~tfr, y = ~year, z = ~EWSR, 
        type = 'scatter3d', mode = 'lines+markers') %>% #color = ~name
  layout(
    scene = list(
      xaxis = list(title = 'TFR'),
      yaxis = list(title = 'Year'),
      zaxis = list(title = 'Support')
    ),
    title = '3D Plot of EWSR and TFR Over Years'
  )

library(mgcv)
mod <- gam(EWSR ~ te(tfr) + te(year) + ti(tfr, year), data=dp)

tfr.seq = seq(min(dp$tfr), max(dp$tfr), by = 0.01)
#tfr.seq = seq(0, 5, by = 0.01)
year.seq = seq(min(dp$year), max(dp$year), by = 5)

predfun <- function(x,y){
  newdat <- data.frame(tfr = x, year = y)
  predict(mod, newdata=newdat)
}
fit <- outer(tfr.seq, year.seq, Vectorize(predfun))

plot_ly() %>% 
  #add_markers(x = ~dp$tfr, y=dp$year, z=dp$EWSR) %>% 
  add_surface(x = ~tfr.seq, y = ~year.seq, z = t(fit))

plot_ly() %>% 
  add_surface(x = ~tfr.seq, y = ~year.seq, z = t(fit)) %>%
  layout(
    title = "3D Surface Plot",
    scene = list(
      xaxis = list(
        title = "TFR",
        titlefont = list(size = 12),
        tickfont = list(size = 10),
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        mirror = TRUE,
        range = c(max(tfr.seq), min(tfr.seq))  # 倒转x轴
      ),
      yaxis = list(
        title = "Year",
        titlefont = list(size = 12),
        tickfont = list(size = 10),
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        mirror = TRUE
      ),
      zaxis = list(
        title = "Support",
        titlefont = list(size = 12),
        tickfont = list(size = 10),
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        mirror = TRUE
      ),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.5)
      )
    ),
    annotations = list(
      list(
        x = 1, y = 1, text = "Source: IIASA(2023)", 
        showarrow = FALSE, xref = 'paper', yref = 'paper',
        xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
        font = list(size = 10)
      )
    )
  )


####### 4.2 Line Plot ########
selected_years <- c(2030, 2050, 2075, 2100)
dp_selected <- dp %>% filter(year %in% selected_years)

# 创建曲线图
ggplot(dp_selected, aes(x = tfr, y = EWSR, color = as.factor(year), group = year)) +
  #geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_point(size = 1) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("2030" = "blue", "2050" = "green", "2075" = "red", "2100" = "black")) +
  labs(x = "TFR", y = "Support", color = "Year") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),  # Specific font style, family = "Bradley Hand",
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

dp_selected = data.frame(fit)
dp_selected$tfr = tfr.seq
dp_selected = dp_selected %>% select(X3,X7,X12,X17,tfr) #2030, 2050, 2075, 2100
dp_selected = dp_selected %>% gather(key= year,value = EWSR, 1:4)
dp_selected = within(dp_selected,{
  year[year=='X3'] <- 2030; year[year=='X7'] <- 2050
  year[year=='X12'] <- 2075; year[year=='X17'] <- 2100
  year = as.numeric(year)
})

ggplot(dp_selected, aes(x = tfr, y = EWSR, color = as.factor(year), group = year)) +
  #geom_smooth(method = "loess", se = FALSE, size = 1) +
  geom_point(size = 1) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("2030" = "blue", "2050" = "green", "2075" = "red", "2100" = "black")) +
  labs(x = "TFR", y = "Support", color = "Year") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),  # Specific font style, family = "Bradley Hand",
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
