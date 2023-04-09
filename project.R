#前回の処理が影響しないように、ここでけしておく
rm(list = ls(all.names = TRUE))

#分析に必要なパッケージは最初に読んでおく
library(magrittr)
library(dplyr)#データフレーム処理には読み込んでおくと便利
library(reshape2)
library(ggplot2)
library(tidyr)
library(rlang)
library(recipes)
library(assertr)
library(assertthat)
library(effsize)
library(tidyverse)
library(magrittr)
library(data.table)
library(lme4)
library(car)
library(multcomp)
library(ggeffects)
library(ggplot2)
library(ggsci)
library(scales)


setwd("E:/pragmatics/jikkendai")
getwd()

dat <- fread("E:/pragmatics/jikkendai/jikkendai_seijin.csv", header=TRUE)
dat <- fread("E:/pragmatics/jikkendai/jikkendai_pooled.csv", header=TRUE)
dat <- fread("E:/pragmatics/jikkendai/sugunikesu.csv", header=TRUE)


##まずは誤答率の計算

#不正解試行のあぶり出し

#Exp1の不正解率
dat[dat$first_target == 0 &
      ((dat$inserted_position == "last" & dat$retention_task !=2) |
         (dat$inserted_position == "middle" & dat$retention_task !=1)),]

dat[dat$first_target == 0, ] %>% nrow()



#Exp2の不正解率
dat[dat$inserted_position == "middle" &
      ((dat$inserted_position == "last" & dat$retention_task !=2) |
         (dat$inserted_position == "middle" & dat$retention_task !=1)),]

nrow %>% dat[dat$inserted_position == "middle", ]


#Exp3の不正解率

dat[dat$inserted_position == "last" &
      ((dat$inserted_position == "last" & dat$retention_task !=2) |
         (dat$inserted_position == "middle" & dat$retention_task !=1)),]

nrow %>% dat[dat$inserted_position == "last", ]




dat[(dat$inserted_position == "last" & dat$retention_task !=2) |
      (dat$inserted_position == "middle" & dat$retention_task !=1),]


dat <- subset(dat,
              (dat$inserted_position == "last" & dat$retention_task == 2) |
                (dat$inserted_position == "middle" & dat$retention_task == 1))
dat
dat %>% nrow()



#==================================odd刺激・直近・その他だけを考える・この命令は完成====================================


dat1 <- dat[dat$first_target==0,]

dat1 <- mutate(dat1, category = 3)

dat1$category[(dat1$second_target ==4 &
                 
                 rowSums(dat1[, paste0("c", 1:9)]) ==1 &
                 dat1$"c4" ==1
) | 
  (dat1$second_target ==5 &
     rowSums(dat1[, paste0("c", 1:9)]) == 1 &
     dat1$"c5" ==1) |
  (dat1$second_target ==6 &
     rowSums(dat1[, paste0("c", 1:9)]) == 1 &
     dat1$"c6" == 1) |
  (dat1$second_target ==7 &
     rowSums(dat1[, paste0("c", 1:9)]) == 1 &
     dat1$"c7" == 1) |
  (dat1$second_target ==8 &
     rowSums(dat1[, paste0("c", 1:9)]) == 1 &
     dat1$"c8" == 1)] <- 1

dat1$category[rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                dat1$"c9" == 1] <-2


dat1$category[dat1$second_target == 9 & dat1$category ==2] <-1


dat1 <- mutate(dat1, cat = if_else(category == 1, 1, 0))

n <-3
dat1[dat1$second_target ==4 & dat1$category == n,] %>% nrow()
dat1[dat1$second_target ==5 & dat1$category == n,] %>% nrow()
dat1[dat1$second_target ==6 & dat1$category == n,] %>% nrow()
dat1[dat1$second_target ==7 & dat1$category == n,] %>% nrow()
dat1[dat1$second_target ==8 & dat1$category == n,] %>% nrow()
dat1[dat1$second_target ==9 & dat1$category == n,] %>% nrow()

dat1$second_target <- as.factor(dat1$second_target)
l2 <- glm(cat ~ exponential + age + exponential : age, data = dat1, family = binomial)
summary(l2)

Anova(l2)

summary(glht(l2, linfct = mcp(second_target = "Tukey")))

plot(ggpredict(l2, terms = c("exponential", "age")), rawdata = T)

a <- as.numeric(dat1$second_target)

dat1 <- mutate(dat1, exponential = exp(as.numeric(dat1$second_target)))



#=========================oddが２つ条件・lastだけを考える================


dat1 <- dat[dat$first_target!=0 & dat$inserted_position =="last",]

dat1 <- mutate(dat1, category = 4)



dat1$category[dat1$inserted_position == "last" &
                ((dat1$first_target != 0 & dat1$second_target == 7 &
                    rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                    dat1$"c7" == 1) | 
                   (dat1$first_target != 0 & dat1$second_target ==8 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                      dat1$"c8" == 1) |
                   (dat1$first_target != 0 & dat1$second_target ==9 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 1  &
                      dat1$"c9" == 1))]<-1

dat1$category[dat1$inserted_position == "last" & 
                ((dat1$first_target == 4 & dat1$second_target == 7 &
                    rowSums(dat1[, paste0("c", 1:9)]) == 2 &
                    dat1$"c4" == 1
                  &dat1$"c7" == 1) | 
                   (dat1$first_target == 4 & dat1$second_target ==8 &
                      rowSums(dat1[, paste0("c", 1:9)]) ==  2 &
                      dat1$"c4" == 1
                    &dat1$"c8" == 1) |
                   (dat1$first_target == 4 & dat1$second_target ==9 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 2 &
                      dat1$"c4" == 1
                    &dat1$"c9" == 1) |
                   (dat1$first_target == 5 & dat1$second_target ==7 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 2 &
                      dat1$"c5" == 1
                    &dat1$"c7" == 1) |
                   (dat1$first_target == 5 & dat1$second_target ==8 &
                      rowSums(dat1[, paste0("c", 1:9)]) ==  2 &
                      dat1$"c4" == 1
                    &dat1$"c8" == 1) |
                   (dat1$first_target == 5 & dat1$second_target == 9 &
                      rowSums(dat1[, paste0("c", 1:9)]) ==  2 &
                      dat1$"c5" == 1
                    &dat1$"c9" == 1) |
                   (dat1$first_target == 6 & dat1$second_target == 7 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 2 &
                      dat1$"c6" == 1
                    &dat1$"c7" == 1) |
                   (dat1$first_target == 6 & dat1$second_target == 8 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 2 &
                      dat1$"c6" == 1
                    &dat1$"c8" == 1) |
                   (dat1$first_target == 6 & dat1$second_target == 9 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 2 &
                      dat1$"c6" == 1
                    &dat1$"c9" == 1))] <- 2


dat1$category[dat1$inserted_position == "last" &
                dat1$second_target != 9 &
                rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                dat1$"c9" == 1] <-3



dat1$category[dat1$inserted_position == "last" &
                ((dat1$first_target == 4 &
                    rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                    dat1$"c4" == 1) | 
                   (dat1$first_target == 5 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                      dat1$"c5" == 1) |
                   (dat1$first_target == 6 &
                      rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                      dat1$"c6" == 1))]<-5





dat1 <- mutate(dat1, cat = if_else(category == 1, 1, 0))

d1 <- dat1[dat1$first_target == 4,]

#==================


d1$second_target <- as.factor(d1$second_target)
l1 <- glm(cat ~ second_target, data = d1, family = binomial)
summary(l1)

Anova(l1)

summary(glht(l1, linfct = mcp(second_target = "Tukey")))
plot(ggpredict(l1), rawdata = T)


l0 <- glm(cat ~ 1, data = d1, family = binomial)


#ベイズファクターの求め方。-1/2をかけて、対数をはずす。比を計算する。

BIC(l1)
BIC(l0)

BIC(l0)*-1/2
exp(BIC(l1)*(-1/2))
exp(BIC(l0)*(-1/2))

exp((BIC(l0)-BIC(l1))/2)#これがベイズファクター

n <- 5
d1[d1$second_target ==7 & d1$category ==n,] %>% nrow()
d1[d1$second_target ==8 & d1$category ==n,] %>% nrow()
d1[d1$second_target ==9 & d1$category ==n,] %>% nrow()

#===============================



#================oddが2つでmiddleに挿入されているとき====================


dat1 <- dat[dat$first_target!=0 & dat$inserted_position =="middle",]

dat1 <- mutate(dat1, category = 3)


dat1$category[((dat1$first_target == 4 &
                  rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                  dat1$"c4" == 1) | 
                 (dat1$first_target == 5 &
                    rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                    dat1$"c5" == 1) |
                 (dat1$first_target == 6 &
                    rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                    dat1$"c6" == 1))] <- 1

dat1$category[rowSums(dat1[, paste0("c", 1:9)]) == &
                dat1$"c6" == 1] <- 2

dat1$category[dat1$first_target==6 & dat1$category ==2]<-1
d1 <- dat1[dat1$second_target == 9,]
d1 <- mutate(d1, cat = if_else(category == 1, 1, 0))

dat1_adults <- dat1[dat1$age == "adults",]
dat1_children <- dat1[dat1$age == "children",]




d1$first_target <- as.factor(d1$first_target)

d1 <- mutate(d1, pos_contrast = ifelse(first_target == 4, -1, ifelse(first_target == 5, 0, 1)))
d1 <- mutate(d1, pos_contrast_step = ifelse(first_target == 4, -0.5, ifelse(first_target == 5,-0.5,1)))##4と#5は変わらないと過程しているから同じ値を入れる

l1 <- glm(cat ~ pos_contrast, data = subset(d1,d1$age =="children"), family = binomial)
l2 <- glm(cat ~ pos_contrast_step, data = subset(d1,d1$age =="children"), family = binomial)

summary(l1)#linear
summary(l2)#段階

Anova(l1)
Anova(l2)

summary(glht(l1, linfct = mcp(first_target = "Tukey")))

plot(ggpredict(l1), rawdata = T)#linearはpositionによって上がっていっている
plot(ggpredict(l2), rawdata = T)#段階は#4と#5は同じで#6でだけ上がっている


#AICが低いほどあてはまりがいい。


n <-3
d1[d1$first_target ==4 & d1$category == n,] %>% nrow()
d1[d1$first_target ==5 & d1$category == n,] %>% nrow()
d1[d1$first_target ==6 & d1$category == n,] %>% nrow()




write.csv(d1,"E:/pragmatics/data.csv")






#==============oddが2つ、Distracted、Non-Distractedで6th以降の選択があったかなかったかを検討する

f <- 6
s <- 9

test_data <- dat[dat$first_target == f &
                   dat$second_target == s,]

test_data <- mutate(test_data, cat = 1)

#一つでも#7以降に反応があった＝1
#cat 0は7以降に反応がなかった
test_data$cat[(test_data$"c7"==0 &
                 test_data$"c8" ==0 &
                 test_data$"c9"== 0)] <- 0

if (all(test_data$cat[test_data$inserted_position == "last"]==1)){
  test_data$cat[test_data$inserted_position == "last" & test_data$cat == 1][1] <-0
}


test_data$inserted_position <- as.factor(test_data$inserted_position)
l2 <- glm(cat ~ inserted_position, data = test_data, family = binomial)
summary(l2)
Anova(l2)

summary(glht(l2, linfct = mcp(inserted_position = "Tukey")))
plot(ggpredict(l2), rawdata = T)


#===============================



a<-rnorm(100, 1, 0.1)
b<-rnorm(100, 2, 0.1)

write.csv(a,"E:/pragmatics/jasp_data_a.csv")
write.csv(b,"E:/pragmatics/jasp_data_b.csv")