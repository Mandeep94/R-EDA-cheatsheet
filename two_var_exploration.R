library(ggplot2)
library(gridExtra)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
qplot(x=age, y=friend_count, data=pf)


ggplot(aes(x=age, y=friend_count), data=pf)+geom_point() +
  xlim(13,90)

summary(pf$age)

ggplot(aes(x=age, y=friend_count), data=pf)+
  geom_jitter (alpha=1/20) +
  xlim(13,90)

ggplot(aes(x=age, y=friend_count), data=pf)+
  geom_point(alpha=1/20, position = position_jitter(h=0)) +
  xlim(13,90)+
  coord_trans(y='sqrt')

ggplot(aes(x=age, y=friendships_initiated), data=pf)+
  geom_point(alpha=1/20, position = position_jitter(h=0))+
  coord_trans(y='sqrt')+
  xlim(13,90)

library(dplyr)

age_groups<-group_by(pf, age)
pf.fc_by_age<-summarise(age_groups,
          friend_count_mean=mean(friend_count),
          friend_count_median=median(friend_count),
          n=n())

pf.fc_by_age<-arrange(pf.fc_by_age, age)

head(pf.fc_by_age)

pf.fc_by_age <-pf %>% 
  group_by(age) %>%
  summarise(friend_count_mean=mean(friend_count),
          friend_count_median=median(friend_count),
          n=n()) %>%
  arrange(age)
head(pf.fc_by_age, 20)
ggplot(aes(x=age, y=friend_count_mean), data=pf.fc_by_age)+geom_line()


ggplot(aes(x=age, y=friend_count), data=pf)+
  coord_cartesian(xlim = c(13, 90), ylim = c(0,1000))+
  geom_point(alpha=0.05, position = position_jitter(h=0), 
             color='orange')+
  coord_trans(y='sqrt')+
  geom_line(stat='summary', fun.y=mean)+
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.1),
            linetype=2, color='blue')+
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.9),
            linetype=2, color='blue')+
  geom_line(stat='summary', fun.y=quantile, fun.args=list(probs=0.5),
            color='blue')

cor.test(pf$age, pf$friend_count, method = 'pearson')

with(pf, cor.test(age, friend_count, method = 'pearson'))
with(subset(pf, pf$age<=70), cor.test(age, friend_count, method = 'spearman'))


ggplot(aes(x=www_likes_received+1, y=likes_received+1), data=pf)+
  geom_jitter(alpha=0.05)+
  xlim(0,25000)+
  ylim(1,50000)+
  coord_trans(x='sqrt',y='log10')

ggplot(aes(x=www_likes_received+1, y=likes_received+1), data=pf)+
  geom_point()+
  xlim(0, quantile(pf$www_likes_received, 0.95))+
  ylim(0, quantile(pf$likes_received, 0.95))+
  geom_smooth(method='lm', color='red')


cor.test(pf$www_likes_received, pf$likes_received)


library(alr3)
data(Mitchell)

ggplot(aes(x=Month, y=Temp), data=Mitchell)+
  geom_point()

cor.test(Mitchell$Month, Mitchell$Temp)

ggplot(aes(x=Month, y=Temp), data=Mitchell)+
  geom_point()+
  scale_x_discrete(breaks=seq(0,203,12))+
  geom_line()


pf$age_with_months<-pf$age+(12-pf$dob_month)/12
pf.fc_age_with_months <-pf %>% 
  group_by(age_with_months) %>%
  summarise(friend_count_mean=mean(friend_count),
          friend_count_median=median(friend_count),
          n=n()) %>%
  arrange(age_with_months)

head(pf.fc_age_with_months)

p1<-ggplot(aes(x=age, y=friend_count_mean), 
       data=subset(pf.fc_by_age, age<71))+
  geom_line()+
  geom_smooth()

p2<-ggplot(aes(x=age_with_months, y=friend_count_mean), 
       data=subset(pf.fc_age_with_months, age_with_months<71))+
  geom_line()+
  geom_smooth()

p3<-ggplot(aes(x=round(age/5)*5, y=friend_count_mean), 
       data=subset(pf.fc_by_age, age<71))+
  geom_line(stat='summary', fun.y=mean)

grid.arrange(p1,p2,p3,ncol=1)  



data(diamonds)
names(diamonds)
diamonds$volume <- diamonds$x*diamonds$y*diamonds$z
ggplot(aes(x=volume, y=price), data=diamonds)+
  geom_point()

v<-subset(diamonds, diamonds$volume>0 & diamonds$volume<800)
cor.test(v$price, v$volume)
range(diamonds$depth)

ggplot(aes(x=volume, y=price), data=v)+
  geom_point(alpha=1/20)+
  geom_smooth()


diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

p1<-ggplot(aes(x=clarity, y=mean_price), data=diamonds_mp_by_clarity)+
  geom_bar(stat = "identity")
p2<-ggplot(aes(x=color, y=mean_price), data=diamonds_mp_by_color)+
  geom_bar(stat = "identity")
grid.arrange(p1,p2,ncol=1)
