install.packages("car")
install.packages("MASS")
install.packages("lattice")
install.packages("memisc")
install.packages("scales")
install.packages('RColorBrewer')


library(ggplot2)
library(gridExtra)
library(dplyr)
library(alr3)
library(reshape2)
library(GGally)
library(scales)
library(memisc)
library(lattice)
library(MASS)
library(car)
library(RColorBrewer)

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
ggplot(aes(x=gender, y=age), data=subset(pf, !is.na(gender)))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom='point', shape=4)
ggplot(aes(x=age, y=friend_count), data=subset(pf, !is.na(gender)))+
  geom_line(aes(color=gender), stat='summary', fun.y=median)


pf.fc_by_age_gender <- 
  pf %>% 
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count=mean(friend_count),
            median_friend_count=median(friend_count),
            n=n()) %>%
  ungroup() %>%
  arrange(age)
pf.fc_by_age_gender

ggplot(aes(x=age, y=median_friend_count), data=pf.fc_by_age_gender)+
  geom_line(aes(color=gender))


pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, 
                                  age~gender,
                                  value.var = 'median_friend_count')
pf.fc_by_age_gender.wide

ggplot(aes(x=age, y=female/male), data=pf.fc_by_age_gender.wide)+
  geom_line()+
  geom_hline(yintercept = 1, alpha=0.3, linetype=2)


pf$year_joined<- floor(2014-pf$tenure/365)
names(pf)
table(pf$year_joined)
pf$year_joined.bucket <- cut(pf$year_joined, c(2004, 2009, 2011, 2012, 2014))
table(pf$year_joined.bucket, useNA = 'ifany')
ggplot(aes(x=age, y=friend_count), data=subset(pf, !is.na(gender)))+
  geom_line(aes(color=year_joined.bucket), stat='summary', fun.y=mean)+
  geom_line(stat='summary', fun.y=mean, linetype=2)

with(subset(pf, tenure>=1), summary(friend_count/tenure))

ggplot(aes(x=30*round(tenure/30), y=friendships_initiated/tenure), data=subset(pf, tenure>=1))+
  geom_line(aes(color=year_joined.bucket), stat='summary', fun.y=mean)+
  geom_smooth(aes(color=year_joined.bucket))


yo<-read.csv("yogurt.csv")
str(yo)
yo$id <- factor(yo$id)


ggplot(aes(x=price), data=yo)+
  geom_histogram(binwidth = 10)
summary(yo)
table(yo$price)
yo<- transform(yo, all.purchases=strawberry+blueberry+pina.colada+plain+mixed.berry)
ggplot(aes(x=all.purchases), data=yo)+
  geom_histogram(binwidth = 1, color='blue', fill='blue')
ggplot(aes(x=time, y=price), data=yo)+
  geom_jitter(alpha=1/5, shape=21, fill=I('#F79420'))
set.seed(420)
sample.ids<-sample(levels(yo$id), 10)
ggplot(aes(x=time ,y=price),
       data=subset(yo, id %in% sample.ids))+
  facet_wrap(~id)+
  geom_line()+
  geom_point(aes(size=all.purchases), pch=1)


theme_set(theme_minimal(20))
set.seed(1836)
pf_subset<-pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])
cor.test(pf_subset$age, pf_subset$mobile_likes)


nci<-read.table('nci.tsv')
colnames(nci)<-c(1:64)
colnames(nci)
nci.long.samp<-melt(as.matrix(nci[1:200,]))
names(nci.long.samp)<-c('gene', 'case', 'value')
head(nci.long.samp)

ggplot(aes(y=gene, x=case, fill=value), data=nci.long.samp)+
  geom_tile()+
  scale_fill_gradientn(colors=colorRampPalette(c('blue', 'red'))(100))

ggplot(aes(x=log10(price)), data=diamonds)+
  geom_histogram(aes(color=cut))+
  facet_wrap(~color)

ggplot(aes(x=table, y=price), data=diamonds)+
  geom_point(aes(color=cut, type='qual'))

ggplot(aes(x=volume, y=price), data=diamonds)+
  coord_trans(y='log10')+
  xlim(0, quantile(diamonds$volume, 0.99))+
  geom_point(aes(color=clarity))+
  scale_color_brewer(type = 'div')

diamonds$price_carat<- diamonds$price/diamonds$carat
ggplot(aes(x=cut, y=price_carat), data=diamonds)+
  geom_jitter(aes(color=color))+
  facet_wrap(~clarity)+
  scale_color_brewer(type = 'div')

ggplot(aes(x=carat, y=price), data=diamonds)+
  xlim(0, quantile(diamonds$carat, 0.99))+
  ylim(0, quantile(diamonds$price, 0.99))+
  geom_point(shape=21, fill=I('#F79420'))


set.seed(20022012)
diamond_sample <- diamonds[sample(1:length(diamonds$price), 10000),]
ggpairs(diamond_sample, lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

p1<-ggplot(aes(x=price), data=diamonds)+
  geom_histogram(binwidth = 100)
p2<-ggplot(aes(x=log10(price)), data=diamonds)+
  geom_histogram(binwidth = 0.01)
grid.arrange(p1, p2, ncol=2)

cuberoot_trans <- function() trans_new('cuberoot',
                                       transform = function(x) x^(1/3),
                                       inverse = function(x) x^(3))
ggplot(aes(x=carat, y=price), data=diamonds)+
  geom_jitter(alpha=0.5, size =0.75)+
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3))+
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000))+
  ggtitle("Price (log10) by Cube-Root of Carat")

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color=color), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', reverse = FALSE,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')

m1<-lm(I(log10(price))~I(carat^(1/3)), data=diamonds)
m2 <- update(m1, ~.+carat)
m3<-  update(m2, ~.+cut)
m4<-  update(m3, ~.+color)
m5 <-  update(m4, ~.+clarity)
mtable(m1, m2, m3, m4, m5)
