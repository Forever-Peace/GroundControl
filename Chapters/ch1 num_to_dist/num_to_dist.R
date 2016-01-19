df = read.csv("rushing_data_stack.csv") #changing the working directory may be necessary to load this data

df_rb = df[df$FantPos=='RB',]

df_rb$week <- factor(df_rb$week)
df_rb$down <- factor(df_rb$down)
df_rb$year <- factor(df_rb$year)
df_rb$FantPos <- factor(df_rb$FantPos)
df_rb$full_name <- factor(df_rb$full_name)

library('ggplot2')
library('reshape2')
library("dplyr")

#summary statistics
names(sort(-table(df_rb$rushing_yds)))[1] #mode
mean(df_rb$rushing_yds) #mean across runs
aggregate(df_rb$rushing_yds, by=list(df_rb$year), FUN=mean) #mean across years.
mean(aggregate(df_rb$rushing_yds, by=list(df_rb$full_name), FUN=mean)[[2]]) #mean accross players
median(df_rb$rushing_yds) #median
fivenum(df_rb$rushing_yds) #min 1st median 3rd max
quantile(df_rb$rushing_yds,c(0.1,0.9)) #top and bottom 10%

#Histogram
run_hist<-ggplot(df_rb, aes(rushing_yds)) + geom_histogram(fill='blue',alpha = 0.2,binwidth = 1,aes(y=..count../sum(..count..)))+ coord_cartesian(xlim=c(-10, 20))+ylab ("Proportion")+xlab ("Rushing Yards") +
  geom_vline(xintercept = mean(df_rb$rushing_yds),size=1.5)+geom_text(aes(mean(df_rb$rushing_yds),y=0.14,label = round(mean(df_rb$rushing_yds),digits=4), hjust = -0.15)) +
  labs(title="Run Distribution")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+
  scale_y_continuous(breaks=seq(0, 0.15, 0.05))+scale_x_continuous(breaks=seq(-10, 20, 5))

#Density Plot
density<-ggplot(df_rb, aes(rushing_yds)) + geom_density(fill='blue',alpha = 0.2,adjust=1.85)+ coord_cartesian(xlim=c(-10, 20))+ylab ("Probability")+xlab ("Rushing Yards") +
  geom_vline(xintercept = mean(df_rb$rushing_yds),size=1.5)+geom_text(aes(mean(df_rb$rushing_yds),y=0.12,label = round(mean(df_rb$rushing_yds),digits=4), hjust = -0.15)) +
  labs(title="Run Distribution")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+
  scale_y_continuous(breaks=seq(0, 0.15, 0.05))+scale_x_continuous(breaks=seq(-10, 20, 5))

#Cumulative Density
cum_density<- ggplot(df_rb, aes(rushing_yds))+stat_ecdf(alpha = 0.6,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death") + xlab ("Rushing Yards") + scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  labs(title="Cumulative Run Distribution")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+
  scale_x_continuous(breaks=seq(-10, 20, 5))

#Cumulative Density Table
df_rb_rundist <- dcast(df_rb, 1 ~ rushing_yds, length)

df_rb_rundist_table <- prop.table(as.table(as.matrix(df_rb_rundist[,-1])),1)
df_rb_rundist <- as.data.frame.matrix(df_rb_rundist_table) 
df_rb_rundist_cum <- data.frame(t(apply(df_rb_rundist,1,cumsum)))
round(subset(df_rb_rundist_cum, select = X.5:X15),digits=4)

#Expected yards left after gaining at least x yards
yardsleft_mean = data.frame()
for (i in 1:31) {
  yardsleft_mean[i,1] <- i-11
  yardsleft_mean[i,2] <-  mean(df_rb[df_rb["rushing_yds"]>=(i-11),]$rushing_yds)-i+11
}
  
yardsleft_median = data.frame()
for (i in 1:31) {
  yardsleft_median[i,1] <- i-11
  yardsleft_median[i,2] <-  median(df_rb[df_rb["rushing_yds"]>=(i-11),]$rushing_yds)-i+11
}

yardsleft_mean <- rename(yardsleft_mean, yards_gained=V1)
yardsleft_mean <- rename(yardsleft_mean, yards_left=V2)
yardsleft_median <- rename(yardsleft_median, yards_gained=V1)
yardsleft_median <- rename(yardsleft_median, yards_left=V2)

yardsleft_mean_plot <- ggplot(yardsleft_mean, aes(x = yards_gained, y = yards_left)) + geom_point(size=4) +
  coord_cartesian(xlim=c(-11, 21), ylim=c(0,15))+ylab ("Yards Left (Mean)")+xlab ("Yards Gained So Far") +
  labs(title="Yards Left per Yards Gained So Far (Mean)")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+
  scale_y_continuous(breaks=seq(0, 15, 2))+scale_x_continuous(breaks=seq(-10, 20, 5))

yardsleft_median_plot <- ggplot(yardsleft_median, aes(x = yards_gained, y = yards_left)) + geom_point(size=4) +
  coord_cartesian(xlim=c(-11, 21), ylim=c(0,15))+ylab ("Yards Left (Median)")+xlab ("Yards Gained So Far") +
  labs(title="Yards Left per Yards Gained So Far (Median)")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+
  scale_y_continuous(breaks=seq(0, 15, 2))+scale_x_continuous(breaks=seq(-10, 20, 5))
