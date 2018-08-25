library(ggplot2)

fb<-read.csv('pseudo_facebook.tsv', sep='\t')

qplot(x=dob_day, data=fb)

qplot(x=dob_day, data=fb)+
 scale_x_continuous(breaks=1:31)

qplot(x=dob_day, data=fb)+
  scale_x_continuous(breaks=1:31)+
  facet_wrap(~dob_month, ncol=3)


names(fb)

qplot(x=friend_count, data=fb, xlim=c(0,1000))


qplot(x=friend_count, data=fb, binwidth=25)+
  scale_x_continuous(limits = c(0,1000), breaks = seq(1,1000, 50))


ggplot(aes(x = friend_count), data = fb) +
  geom_histogram(binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))


qplot(x=friend_count, data=fb, binwidth=25)+
  scale_x_continuous(limits = c(0,1000), breaks = seq(1,1000, 50))+
  facet_wrap(~gender)


qplot(x=friend_count, data=subset(fb, !is.na(gender)), binwidth=25)+
  scale_x_continuous(limits = c(0,1000), breaks = seq(1,1000, 50))+
  facet_wrap(~gender)



table(fb$gender)
by(fb$friend_count, fb$gender, summary)


qplot(x=tenure/365, data=fb, binwidth=0.25, 
      xlab = 'Numbers of years using facebook',
      ylab = 'Number of users in the sample',
      color=I('black'), fill=I('#099DD9'))+
  scale_x_continuous(breaks = seq(1,7,1), limits = c(0,7))


qplot(x=age, data=fb, binwidth=1,
      color=I('black'), fill='#FF0000')+
  scale_x_discrete(breaks=seq(0,113,5))





#Transforming data


qplot(x=friend_count, data=fb)

summary(fb$friend_count)
summary(log10(fb$friend_count+1))
summary(sqrt(fb$friend_count))


qplot(x=log10(friend_count), data=fb)
qplot(x=sqrt(friend_count), data=fb)


library(gridExtra)

p1<-qplot(x=friend_count, data=fb)
p2<-qplot(x=log10(friend_count), data=fb)
p3<-qplot(x=sqrt(friend_count), data=fb)
grid.arrange(p1, p2, p3, ncol=1)


p1<-ggplot(aes(x=friend_count), data = fb)+
  geom_histogram()
p2<-p1+scale_x_log10()
p3<-p1+scale_x_sqrt()
grid.arrange(p1, p2, p3, ncol=1)


logScale<-qplot(x=log10(friend_count), data=fb)
countScale<-ggplot(aes(x=friend_count), data = fb)+
  geom_histogram()+scale_x_log10()
grid.arrange(logScale, countScale, ncol=2)




#frequency hist


qplot(x=friend_count, y=..count../sum(..count..), 
      data=subset(fb, !is.na(gender)), 
      xlab = 'Friend count',
      ylab = 'Proportions of users with that friend count',
      binwidth=10, geom = 'freqpoly', color=gender)+
  scale_x_continuous(limits = c(0,1000), breaks = seq(1,1000, 50))

summary(fb$likes)


qplot(x=likes,data=subset(fb, !is.na(gender)), 
      xlab = 'likes', geom = 'freqpoly', color=gender)+
  scale_x_continuous()+scale_x_log10()


qplot(x=gender, y=friend_count, data=subset(fb, !is.na(gender)), 
      geom='boxplot')+
  coord_cartesian(ylim = c(0,250))


by(fb$friendships_initiated, fb$gender, summary)


qplot(x=gender, y=friendships_initiated, 
      data=subset(fb, !is.na(gender)), geom='boxplot')+
  coord_cartesian(ylim = c(0,200))



