df = read.csv("rushing_data_stack.csv")

library('ggplot2')
library('reshape2')
library("dplyr")
library('boot')
library('ggthemes')

df_rb = df[df$FantPos=='RB',]

df_rb$week <- factor(df_rb$week)
df_rb$down <- factor(df_rb$down)
df_rb$year <- factor(df_rb$year)
df_rb$FantPos <- factor(df_rb$FantPos)
df_rb$full_name <- factor(df_rb$full_name)

#Downs
downs_density<-ggplot(df_rb[df_rb$down!=4,], aes(rushing_yds, fill = down)) + geom_density(alpha = 0.2,adjust=1.35)+ coord_cartesian(ylim=c(0,.15),xlim=c(-10, 20))+theme_fivethirtyeight()+ylab ("Proportion")+xlab ("Rushing Yards") +
  labs(title="Run Distribution by Down")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
downs_density



samplemean <- function(x, d) {
  return(mean(x[d]))
}

boot_1down <- boot(df_rb[df_rb$down==1,]$rushing_yds, samplemean, R=5000)
boot_2down <- boot(df_rb[df_rb$down==2,]$rushing_yds, samplemean, R=5000)
boot_3down <- boot(df_rb[df_rb$down==3,]$rushing_yds, samplemean, R=5000)
boot_4down <- boot(df_rb[df_rb$down==4,]$rushing_yds, samplemean, R=5000)
boot.ci(boot_1down, type = "basic")
boot.ci(boot_2down, type = "basic")
boot.ci(boot_3down, type = "basic")
boot.ci(boot_4down, type = "basic")

#Yards to go. 
library("psych")
describeBy(df_rb$yards_to_go,df_rb$down)
detach("package:psych", unload=TRUE)

togo_gam<-ggplot(df_rb, aes(yards_to_go, rushing_yds))+coord_cartesian(xlim=c(0,15), ylim=c(2,6))+theme_fivethirtyeight()+
  stat_smooth(method="gam", formula=y~s(x,k=10), size=2)+ylab ("Yards per Carry")+xlab ("Yards to go (for first down)")+
  labs(title="YPC by Yards to Go")+theme(plot.title = element_text(size = rel(2)))+scale_x_reverse()+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
togo_gam

#Yards til goal.
tilgoal_gam<-ggplot(df_rb, aes(yards_til_goal, rushing_yds))+coord_cartesian(xlim=c(0,100), ylim=c(0,6))+theme_fivethirtyeight()+
  stat_smooth(method="gam", formula=y~s(x,k=20), size=2)+ylab ("Yards per Carry")+xlab ("Yards from Goal")+
  labs(title="YPC by Yards from Goal")+theme(plot.title = element_text(size = rel(2)))+scale_x_reverse()+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
tilgoal_gam

tilgoal_gam_25<-ggplot(df_rb, aes(yards_til_goal, rushing_yds))+coord_cartesian(xlim=c(0,25), ylim=c(0,6))+theme_fivethirtyeight()+
  stat_smooth(method="gam", formula=y~s(x,k=25), size=2)+ylab ("Yards per Carry")+xlab ("Yards from Goal")+
  labs(title="YPC by Yards from Goal (red zone)")+theme(plot.title = element_text(size = rel(2)))+scale_x_reverse()+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
tilgoal_gam_25

resample_yardmeans_goalmargin <- function(yards,samplesize,cutoff)
{
  yardmeans = numeric(10000)
  for (i in 1:10000) {
    yardsamp = sample(yards, size=samplesize, replace=T)
    yardmeans[i] = mean(replace(yardsamp, yardsamp>cutoff, cutoff))
  }
  yardmeans
}

goalcut_effect = numeric(20)
for (i in seq(1,20)) {
  goalcut_effect[i] <- mean(resample_yardmeans_goalmargin(df_rb$rushing_yds, 1000, i))
}

ydstilgoal_effect = numeric(20)
for (i in seq(1,20)) {
  ydstilgoal_effect[i] <- mean(df_rb[df_rb$yards_til_goal==i,]$rushing_yds)
}

(ydstilgoal_effect-goalcut_effect)/goalcut_effect #marginal defense effect

ydstilgoal_plotting <- data.frame(yards_til_goal=seq(1,20), goalcut_effect, ydstilgoal_effect, marginal_defense_perc = (ydstilgoal_effect-goalcut_effect)/goalcut_effect)

ydstilgoal_plot <- ggplot(data = ydstilgoal_plotting)+geom_line(aes(yards_til_goal,goalcut_effect), colour="blue", size=2.5)+stat_smooth(method="loess", aes(yards_til_goal,ydstilgoal_effect), colour="red", size=2.5, se=FALSE)+
  ylab ("Yards per Carry")+xlab ("Yards from Goal")+theme_fivethirtyeight()+
  labs(title="Expected YPC after cutoff adjustment (blue) and Actual YPC (red)")+theme(plot.title = element_text(size = rel(2)))+coord_cartesian(xlim=c(0,20), ylim=c(0,4))+scale_x_reverse()+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
ydstilgoal_plot

#Scorediff
ggplot(df_rb, aes(score_dif, rushing_yds))+coord_cartesian(xlim=c(-30,30), ylim=c(3,6))+
  stat_smooth(method="gam", formula=y~s(x,k=10), size=2)+theme_fivethirtyeight()+
  ylab ("Yards per Carry")+xlab ("Score Differential")+theme(axis.title.y = element_text(angle=90))+
  labs(title="YPC by score differential")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))

ggplot(df_rb[df_rb$quarter%in%c("Q2","Q4"),], aes(score_dif, rushing_yds))+coord_cartesian(xlim=c(-30,30), ylim=c(3,6))+
  stat_smooth(method="gam", formula=y~s(x,k=10), aes(color=factor(quarter)), size=2)+theme_fivethirtyeight()+
  ylab ("Yards per Carry")+xlab ("Score Differential")+theme(axis.title.y = element_text(angle=90))+
  labs(title="YPC by score differential")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+scale_color_discrete(name = "Quarter")

#GAM - down distance
downdist_gam<-gam(rushing_yds~s(yards_to_go,k=10,by=down),data=df_rb)
summary(downdist_gam)
plot(downdist_gam, xlim=c(0,15), ylim = c(-4,4))

downdistgoal_gam<-gam(rushing_yds~s(yards_to_go,k=10,by=down)+s(yards_til_goal, k=20),data=df_rb)
summary(downdistgoal_gam)
plot(downdistgoal_gam, xlim=c(0,15), ylim = c(-4,4))

plot(downdistgoal_gam, xlim=c(20,0), ylim = c(-4,4), select=5, xlab = "Yards from Goal", ylab = "Marginal Difference from Average YPC", main="Marginal YPC Effect attributable to Field Position ")
plot(downdistgoal_gam, xlim=c(15,0), ylim = c(-4,4), select=1, xlab = "Yards to Go (for first down)", ylab = "Marginal Difference from Average YPC", main="Marginal YPC Effect attributable to 1st down and distance")
plot(downdistgoal_gam, xlim=c(15,0), ylim = c(-4,4), select=3, xlab = "Yards to Go (for first down)", ylab = "Marginal Difference from Average YPC", main="Marginal YPC Effect attributable to 3rd down and distance")


qplot(yards_til_goal, ..count.., data=df_rb, geom="density", fill=down, position="fill", xlim=c(0,100), adjust=0.30, xlab = "Field Position", ylab = "Proportion of Rushing Attempts", main = "Proportion of Rushing Attempts by Down and Field Position")


library("mgcv")
a <- gamm(rushing_yds~s(yards_to_go,k=10,by=down)+s(yards_til_goal, k=20), random= list(full_name = ~1), data=df_rb[df_rb$down!=4,])
anova.gam(a$gam)
gam.check(a$gam)
random.effects(a$lme)
plot(a$gam, xlim=c(0,15), ylim = c(-2,2))

summary(lm(rushing_yds~yards_to_go*down+yards_til_goal+full_name, data=df_rb))
