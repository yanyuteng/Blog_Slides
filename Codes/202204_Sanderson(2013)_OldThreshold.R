# Replicative Research: The Characteristics Approach to the Measurement of Population Aging (2013)
# Organization: RUC
# Author: Yuteng Yan
# Time: 17/04/2022 - 18/04/2022

# ———————————— Ouline ———————————— #
# 1. Analysis Environment #
# 2. Example #
## 2.1 Median Age ##
## 2.2 Case in Sanderson & Scherbov (2013) ## 
# 3. Real Case_Beijing Census #
## 3.1 Find Prospective Old Threshold (by ex, tx and mx) ## 
## 3.2 Find Prospective Old Prop (by ex, tx and mx) ##
# ———————————— End ———————————— #


###################################################################################################
###################################################################################################
####### 1. Analysis Environment ########
rm(list=ls())
options(scipen = 50)
library(pacman)
p_load(tidyverse,readxl)

# Inputing a set of x and y values (equal lenght) and a single x value 
# to interpolate the y value of, this function uses the splinefun function
# from the stats package as it's workhorse
FunSpline <- function(x, y, threshold = 15, method="monoH.FC",  ties = mean) {
  func = splinefun(x, y, method=method,  ties = ties)
  func(threshold)
  }
# FunSpline Func from Maja Založnik(2018)

# Median age is calculated with this function based on the population by age group ("agegroups") 
# and the size of the age groups in years ("agesize")
medage<-function(agegroups,agesize){
  cat<-cumsum(agegroups)
  fish<-(cat[length(agegroups[,]),1])/2
  foot<-t(agegroups)
  foot<-array(0,length(foot))
  hand<-foot
  for (i in 2:length(foot)){if(cat[i,1]>fish) {foot[i]<-cat[i-1,1]}}
  for (i in 2:length(hand)){if(cumsum(foot[i-1])==0) {hand[i]<-foot[i]}}
  leg<-seq(0,length(agegroups[,]),1)
  arm<-array(0,length(foot))
  for (i in 2:length(hand)){if(hand[i]!=0) {arm[i]<-leg[i]}}
  head<-array(0,length(foot))
  for (i in 2:length(hand)){if(arm[i]!=0) {head[i]<-agegroups[i,1]}}
  air<-fish-sum(hand)
  water<-(air/sum(head))+sum(arm)
  water*agesize
}
# medage Func from Eddie (2018)


###################################################################################################
###################################################################################################
####### 2. Example ####### 
########## 2.1 Median Age ##########
# source('/Users/yuteng/Desktop/Mortality Project/_Tech/Median Age/median age.R')

# 2.1.1 5th Beijing Male
lt = read_xlsx('~/Desktop/Mortality Project/3_校正后普查变迁趋势/LTWilmoth5th_Male_Prov_5year.xlsx',
               sheet = 1,range = cell_limits(c(1,2)))
lt2 = as.data.frame(read_xlsx('~/Desktop/Mortality Project/1_历普分区域数据/2_五普分省生命表.xlsx',
                              sheet = 1,range = cell_limits(c(2,1),c(NA,3))))
# Median age
a1 = medage(lt2[2],1)
# Life expectancy at median age
a2 = FunSpline(lt$x,lt$ex,a1) 
# Standardized median age (Beijing)
a3 = FunSpline(lt$ex,lt$x,a2) #作为基准组


# 2.1.2 6th Beijing Male
lt = read_xlsx('~/Desktop/Mortality Project/3_校正后普查变迁趋势/LTWilmoth6th_Male_Prov_5year.xlsx',
               sheet = 1,range = cell_limits(c(1,2)))
lt2 = as.data.frame(read_xlsx('~/Desktop/Mortality Project/1_历普分区域数据/3_六普分省生命表.xlsx',
                              sheet = 1,range = cell_limits(c(2,1),c(NA,3))))
b1 = medage(lt2[2],1)
b2 = FunSpline(lt$x,lt$ex,b1) 
b3 = FunSpline(lt$ex,lt$x,a2) #按北京2000年年龄中位数预期余寿标准化


# 2.1.3 7th Beijing Male
lt = read_xlsx('~/Desktop/Mortality Project/3_校正后普查变迁趋势/LTWilmoth7th_Male_Prov_5year.xlsx',
               sheet = 1,range = cell_limits(c(1,2)))
lt2 = as.data.frame(read_xlsx('~/Desktop/Mortality Project/2_七普分区域数据/Census7th_Prov_5year_5m0.xlsx',
                              sheet = 1,range = cell_limits(c(1,2),c(NA,5))))
lt2 = lt2 %>% mutate(across(2:4, as.integer))
c1 = medage(lt2[3],5) 
c2 = FunSpline(lt$x,lt$ex,c1) 
c3 = FunSpline(lt$ex,lt$x,a2) #按北京2000年年龄中位数预期余寿标准化


# 2.1.4 Cbind
a = cbind(a1,a2,a3); b = cbind(b1,b2,b3); c = cbind(c1,c2,c3)
dt = as.data.frame(rbind(a,b,c)); colnames(dt) <- c('median_age','le_median_age','s_median_age')
dt$year = c(2000,2010,2020);dt[,c(4,1:3)]

dt <- dt %>% 
  gather('class','index',1:3) %>% 
  mutate(across(class,as.factor))
dt$class <- factor(dt$class,levels = c('median_age','le_median_age','s_median_age'))
dt %>% 
  ggplot(aes(x=year,y=index,group=class,color=class))+
  geom_line(position = position_dodge(width = .5), size = 1)+
  geom_point(position = position_dodge(width = .5), size = 1)+
  theme_bw()+
  theme(text = element_text(family='STKaiti',size=20,face="bold"),
        axis.text.y=element_text(size=20),
        axis.text.x = element_text(size=20,angle = 45, hjust = 1), 
        legend.position = "right") +
  labs(x='年份',y='年龄(年份)',color="年龄指标",
       title="2000年至2020年北京市中位年龄")


########## 2.2 Case in Sanderson & Scherbov(2013) ##########
# 2.2.1 Test1
# yc = x
# y = ?
c = matrix(c(3,3.2,3.5,3.6),2,2);c
x = matrix(c(118.4,135.2),1,2);x
y = x %*% solve(c);y

# 2.2.2 Test2
# 单一年份求逆矩阵，只有21*21一个特征值矩阵
# yc = x
# step1 c = ?
y = matrix(seq(65,67,0.1),1,21);y
x = matrix(seq(15.2,14.2,-0.05),1,21);x
# y-1*y*c = y-1*x ; c = y-1*x
c = MASS::ginv(y) %*% x
y %*% c
# step2 y = ?
# y*c*c-1 = x*c-1; y=x*c-1
y0 = 15 %*% MASS::ginv(c[,which(x==15)])[which(x==15)];y0

# 多年求逆矩阵，实际是每一年一个逆矩阵特征值，有21*21的6个特征值矩阵，即需生成list存储
# 注 seq函数生成数值不是整数，需要round
# yc = x
# step1 c = ?
y = matrix(0,21,6)
for (i in 1:6){y[,i] <- round(seq(65,67,0.1),2)};rm(i); y = t(y); y
x = matrix(0,21,6)
for (i in 1:6){x[,i] <- round(seq(14.8+0.2*i,13.8+0.2*i,-0.05),2)};rm(i); x = t(x); x
# y-1*y*c = y-1*x ; c = y-1*x
c = list()
for (i in 1:6){c[[i]] = MASS::ginv(t(matrix(y[i,]))) %*% t(matrix(x[i,]))};c[[1]]
d = matrix(0,21,6)
for (i in 1:6){d[,i] = t(y[i,] %*% c[[i]])}; d

# step2 y = ?
# y*c*c-1 = x*c-1; y=x*c-1
y0 = matrix(0,6,1)
n=0
for (i in 1:6){
  y0[i,1] = 15 %*% MASS::ginv(c[[i]][,(which(x==15)[i]-n*21)])[which(x==15)[i]-n*21]
  n=n+1
}
y0


# 2.2.3 Test3
t <- matrix(0,21,7)
t[,7] <- seq(65,67,0.1)
for (i in 1:6){t[,i] <- seq(14.8+0.2*i,13.8+0.2*i,-0.05)};rm(i)
t <- t[,c(7,1:6)]

t <- as.data.frame(t)
colnames(t) <- c('age','t1','t2','t3','t4','t5','t6');t

c <- t %>% gather('t','k',2:7)
c %>% 
  group_by(t) %>% 
  summarise(RLE_15=FunSpline(k,age,15)) %>%
  rename(Year=t, a_ages=RLE_15)



###################################################################################################
###################################################################################################
####### 3. Real Case_Beijing Census #######
# C value(t65/t20, 5m65) 设定为2000年65岁时的值,比较2000、2010、2020年
# step1, use splines to get old age threshold, inverse to find a value;
# step2, interpolate old age thresholds for single years



########## 3.1 Find Prospective Old Threshold (by ex, tx and mx) ##########
lt <- read_xlsx('~/Desktop/Mortality Project/3_校正后普查变迁趋势/LTWilmoth5th_Male_Prov_5year.xlsx',
                sheet = 1,
                range = cell_limits(c(1,2)))
for (i in 1:24){
  lt[i,11] = lt[i,9]/lt[6,9] #t65/t20
  #lt2[i,11] = lt[i,9]/(lt[6,9]-lt[i,9]) #t65/(t20-t65)
};rm(i);colnames(lt)[11] <- c('tx_t20')

#ot.ex5 = 65
ot.tx5 = as.numeric(lt[15,11])
#ot.tx5 = FunSpline(lt$tx_t20,lt$x,ot.tx5) #65
ot.mx5 = as.numeric(lt[15,3])
#ot.mx5 = FunSpline(lt$mx,lt$x,ot.mx5) 65
# t65/t20 == 0.2219715; 5m65 == 0.02536705
t1= as.data.frame(matrix(rep(65,3),1,3));colnames(t1) = c('ex15','mx65','tx65')
t1


lt <- read_xlsx('~/Desktop/Mortality Project/3_校正后普查变迁趋势/LTWilmoth6th_Male_Prov_5year.xlsx',
                sheet = 1,
                range = cell_limits(c(1,2)))
for (i in 1:24){
  lt[i,11] = lt[i,9]/lt[6,9]
  #lt2[i,11] = lt[i,9]/(lt[6,9]-lt[i,9]) #t65/(t20-t65)
};rm(i);colnames(lt)[11] <- c('tx_t20')

#old.threshold
ot.ex6 <- lt %>% summarise(ex15=FunSpline(ex,x,15)) #RLE_15
ot.tx6 <- lt %>% summarise(tx65=FunSpline(tx_t20,x,ot.tx5))
ot.mx6 <- lt %>% summarise(mx65=FunSpline(mx,x,ot.mx5))
t2 = cbind(ot.ex6,ot.mx6,ot.tx6);t2


lt <- read_xlsx('~/Desktop/Mortality Project/3_校正后普查变迁趋势/LTWilmoth7th_Male_Prov_5year.xlsx',
                sheet = 1,
                range = cell_limits(c(1,2)))
for (i in 1:24){
  lt[i,11] = lt[i,9]/lt[6,9]
  #lt2[i,11] = lt[i,9]/(lt[6,9]-lt[i,9]) #t65/(t20-t65)
};rm(i);colnames(lt)[11] <- c('tx_t20')

#old.threshold
ot.ex7 <- lt %>% summarise(ex15=FunSpline(ex,x,15)) #RLE_15
ot.tx7 <- lt %>% summarise(tx65=FunSpline(tx_t20,x,ot.tx5))
ot.mx7 <- lt %>% summarise(mx65=FunSpline(mx,x,ot.mx5))
t3=cbind(ot.ex7,ot.mx7,ot.tx7);t3


t = rbind(t1,t2,t3)
t$year=c(2000,2010,2020)
t %>% 
  gather('class','index',1:3) %>% 
  ggplot(aes(x=year,y=index,group=class,color=class))+
  geom_line(position = position_dodge(width = .1), size = 1)+
  geom_point(position = position_dodge(width = 0.1), size = 2)+
  theme_bw()+
  theme(text = element_text(family='STKaiti',size=20,face="bold"),
        axis.text.y=element_text(size=20),
        axis.text.x = element_text(size=20,angle = 45, hjust = 1), 
        legend.position = "right") +
  labs(x='年份',y='年龄(年份)',color="年龄指标",
       title="2000年至2020年北京市老龄阈值(未插值)")


dt = expand.grid(year = seq(2000, 2020, 1))
dt = left_join(dt,t,by='year')
dt$ex15 = FunSpline(dt$year, dt$ex15, dt$year)
dt$mx65 =FunSpline(dt$year, dt$mx65, dt$year)
dt$tx65 =FunSpline(dt$year, dt$tx65, dt$year)
dt %>% 
  gather('class','index',2:4) %>% 
  ggplot(aes(x=year,y=index,group=class,color=class))+
  geom_line(position = position_dodge(width = .1), size = 1)+
  geom_point(position = position_dodge(width = 0.1), size = 2)+
  theme_bw()+
  theme(text = element_text(family='STKaiti',size=20,face="bold"),
        axis.text.y=element_text(size=20),
        axis.text.x = element_text(size=20,angle = 45, hjust = 1), 
        legend.position = "right") +
  labs(x='年份',y='年龄(年份)',color="年龄指标",
       title="2000年至2020年北京市老龄阈值(插值)")



########## 3.2 Find Prospective Old Prop (by ex) ##########
########### 3.2.1 Base 5th Census ###########
lt2 = as.data.frame(read_xlsx('~/Desktop/Mortality Project/1_历普分区域数据/2_五普分省生命表.xlsx',
                              sheet = 1,range = cell_limits(c(2,1),c(NA,3))))
colnames(lt2) = c('age','male','female')

lt2[101,1] = 100; lt2[,1] <- as.numeric(lt2[,1])
lta = lt2 %>% 
  mutate(age_group_end = age) %>% 
  mutate(cum_pop = cumsum(male))  %>% 
  select(-male, -female) %>%
  bind_rows(dt %>%
              filter(year==2000) %>%
              select(ex15) %>%
              rename(age_group_end=ex15)) %>%
  mutate(dt %>%
           filter(year==2000) %>%
           select(ex15) %>%
           rename(threshold_ex = ex15)) %>%
  arrange(age_group_end) %>%
  mutate(cum_pop = FunSpline(age_group_end, cum_pop, age_group_end))

a = lta %>% 
  mutate(over_65 = ifelse(age_group_end <= 65 , "under", "over"),
         over_ex = ifelse(age_group_end <= threshold_ex, "under", "over")) %>%
  
  group_by(over_65) %>% 
  mutate(pop_u65 = max(cum_pop),
         pop_u64 = nth(cum_pop,-3)) %>% #2000年指标一致
  group_by(over_ex) %>% 
  mutate(pop_uex = max(cum_pop),
         pop_uex1 = nth(cum_pop,-3)) %>% #2000年指标一致
  ungroup() %>%
  summarise(pop_all = last(cum_pop),
            pop_u65 = first(pop_u65),
            pop_u64 = first(pop_u64),
            pop_uex = first(pop_uex),
            pop_uex1 = first(pop_uex1))

a1 = as.data.frame(a)


########### 3.2.2 Find 6th Census ###########
lt2 = as.data.frame(read_xlsx('~/Desktop/Mortality Project/1_历普分区域数据/3_六普分省生命表.xlsx',
                              sheet = 1,range = cell_limits(c(2,1),c(NA,3))))
colnames(lt2) = c('age','male','female')

lt2[101,1] = 100; lt2[,1] <- as.numeric(lt2[,1])
lta = lt2 %>% 
  mutate(age_group_end = age) %>% 
  mutate(cum_pop = cumsum(male))  %>% 
  select(-male, -female) %>%
  bind_rows(dt %>%
              filter(year==2010) %>%
              select(ex15) %>%
              rename(age_group_end=ex15)) %>%
  mutate(dt %>%
           filter(year==2010) %>%
           select(ex15) %>%
           rename(threshold_ex = ex15)) %>%
  arrange(age_group_end) %>%
  mutate(cum_pop = FunSpline(age_group_end, cum_pop, age_group_end))

a = lta %>% 
  mutate(over_65 = ifelse(age_group_end <= 65 , "under", "over"),
         over_ex = ifelse(age_group_end <= threshold_ex, "under", "over")) %>%
  
  group_by(over_65) %>% 
  mutate(pop_u65 = max(cum_pop),
         pop_u64 = nth(cum_pop,-2)) %>% 
  group_by(over_ex) %>% 
  mutate(pop_uex = max(cum_pop),
         pop_uex1 = nth(cum_pop,-2)) %>%
  ungroup() %>%
  summarise(pop_all = last(cum_pop),
            pop_u65 = first(pop_u65),
            pop_u64 = first(pop_u64),
            pop_uex = first(pop_uex),
            pop_uex1 = first(pop_uex1))

a2 = as.data.frame(a)


########### 3.2.3 Find 7th Census ###########
lt2 = as.data.frame(read_xlsx('~/Desktop/Mortality Project/2_七普分区域数据/Census7th_Prov_1year.xlsx',
                              sheet = 1,range = cell_limits(c(1,2),c(NA,5))))
lt2 = lt2 %>% mutate(across(2:4, as.integer)) %>% select(age,pop_male,pop_female)
colnames(lt2) = c('age','male','female')
lt2[101,1] = 100; lt2[,1] <- as.numeric(lt2[,1])

lta = lt2 %>% 
  mutate(age_group_end = age) %>% 
  mutate(cum_pop = cumsum(male))  %>% 
  select(-male, -female) %>%
  bind_rows(dt %>%
              filter(year==2020) %>%
              select(ex15) %>%
              rename(age_group_end=ex15)) %>%
  mutate(dt %>%
           filter(year==2020) %>%
           select(ex15) %>%
           rename(threshold_ex = ex15)) %>%
  arrange(age_group_end) %>%
  mutate(cum_pop = FunSpline(age_group_end, cum_pop, age_group_end))

a = lta %>% 
  mutate(over_65 = ifelse(age_group_end <= 65 , "under", "over"),
         over_ex = ifelse(age_group_end <= threshold_ex, "under", "over")) %>%
  
  group_by(over_65) %>% 
  mutate(pop_u65 = max(cum_pop),
         pop_u64 = nth(cum_pop,-2)) %>% 
  group_by(over_ex) %>% 
  mutate(pop_uex = max(cum_pop),
         pop_uex1 = nth(cum_pop,-2)) %>%
  ungroup() %>%
  summarise(pop_all = last(cum_pop),
            pop_u65 = first(pop_u65),
            pop_u64 = first(pop_u64),
            pop_uex = first(pop_uex),
            pop_uex1 = first(pop_uex1))


########### 3.2.4 Plot age_65 & age_REL ###########
a3 = as.data.frame(a)

a = rbind(a1,a2,a3);a

b1 = 1-a[,3]/a[,1]
b2 = 1-a[,5]/a[,1]
b = as.data.frame(t(rbind(b1,b2)));b$year=c(2000,2010,2020)
colnames(b) = c('age_65','age_REL','year')

b %>% 
  gather('class','index',1:2) %>% 
  ggplot(aes(x=year,y=index*100,group=class,color=class))+
  geom_line(position = position_dodge(width = .1), size = 1)+
  geom_point(position = position_dodge(width = 0.1), size = 2)+
  theme_bw()+
  theme(text = element_text(family='STKaiti',size=20,face="bold"),
        axis.text.y=element_text(size=20),
        axis.text.x = element_text(size=20,angle = 45, hjust = 1), 
        legend.position = "right") +
  labs(x='年份',y='老龄人口比重(%)',color="老龄人口\n指标",
       title="2000年至2020年北京市老龄化程度")

# lt.s <- lt_abridged2single(nMx = lt$mx,Age = lt$x,Sex = 'm')
# ot.s <- lt.s %>% summarise(RLE_15=FunSpline(ex,Age,15))
