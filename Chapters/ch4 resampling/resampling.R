df = read.csv("rushing_data_stack.csv")

library('ggplot2')
library('reshape2')
library("dplyr")

df_rb = df[df$FantPos=='RB',]

df_rb$week <- factor(df_rb$week)
df_rb$down <- factor(df_rb$down)
df_rb$year <- factor(df_rb$year)
df_rb$FantPos <- factor(df_rb$FantPos)
df_rb$full_name <- factor(df_rb$full_name)

df_rb <- merge(df_rb, dcast(df_rb, pos_team+year ~ "teamyear_atts", length), by=c("pos_team","year"))
df_rb <- merge(df_rb, dcast(df_rb, full_name+year ~ "playeryear_atts", length), by=c("full_name","year"))
df_rb$usage <- df_rb$playeryear_atts / df_rb$teamyear_atts
df_rb <- merge(df_rb, data.frame(summarize(group_by(df_rb, full_name), max.usage = max(usage))), by=c("full_name"))

#resample average yards per carry
resample_yardmeans <- function(yards,samplesize)
{
  yardmeans = numeric(10000)
  for (i in 1:10000) {
    yardmeans[i] = mean(sample(yards, size=samplesize, replace=T))
  }
  yardmeans
}

yardmeans10 <- resample_yardmeans(df_rb$rushing_yds,10)
sd(yardmeans10)
quantile(yardmeans10, c(.025, .50, .975))
quantile(yardmeans10, c(.25, .50, .75))

#Footrace
playercomp_resample <- function(player1, player2, samplesize)
{
  yardmeansdf <- data.frame(matrix(ncol = 2, nrow = 10000))
  colnames(yardmeansdf) <- c(player1, player2)
  yardmeansdf[,1] <- resample_yardmeans(df_rb[df_rb$full_name==player1,]$rushing_yds,samplesize)
  yardmeansdf[,2] <- resample_yardmeans(df_rb[df_rb$full_name==player2,]$rushing_yds,samplesize)
  yardmeansdf
}

yardmeansdf <- playercomp_resample("Marshawn Lynch","Trent Richardson",1)
count(yardmeansdf[which(yardmeansdf[,1]>yardmeansdf[,2]),])/10000 #player 1 wins
count(yardmeansdf[which(yardmeansdf[,2]>yardmeansdf[,1]),])/10000 #player 2 wins
count(yardmeansdf[which(yardmeansdf[,2]==yardmeansdf[,1]),])/10000 #tie

yardmeansdf <- playercomp_resample("Marshawn Lynch","Trent Richardson",11)
count(yardmeansdf[which(yardmeansdf[,1]>yardmeansdf[,2]),])/10000 #player 1 wins
count(yardmeansdf[which(yardmeansdf[,2]>yardmeansdf[,1]),])/10000 #player 2 wins
count(yardmeansdf[which(yardmeansdf[,2]==yardmeansdf[,1]),])/10000 #tie

yardmeansdf <- playercomp_resample("Marshawn Lynch","Trent Richardson",100)
count(yardmeansdf[which(yardmeansdf[,1]>yardmeansdf[,2]),])/10000 #player 1 wins
count(yardmeansdf[which(yardmeansdf[,2]>yardmeansdf[,1]),])/10000 #player 2 wins
count(yardmeansdf[which(yardmeansdf[,2]==yardmeansdf[,1]),])/10000 #tie

yardmeansdf <- playercomp_resample("Marshawn Lynch","Trent Richardson",130) #takes roughly 130 carries to say Lynch > TRich
count(yardmeansdf[which(yardmeansdf[,1]>yardmeansdf[,2]),])/10000 #player 1 wins
count(yardmeansdf[which(yardmeansdf[,2]>yardmeansdf[,1]),])/10000 #player 2 wins
count(yardmeansdf[which(yardmeansdf[,2]==yardmeansdf[,1]),])/10000 #tie

#CLT
yardmeans10 <- resample_yardmeans(df_rb$rushing_yds,10)
yardmeans15 <- resample_yardmeans(df_rb$rushing_yds,15)
yardmeans20 <- resample_yardmeans(df_rb$rushing_yds,20)
yardmeans25 <- resample_yardmeans(df_rb$rushing_yds,25)
yardmeans30 <- resample_yardmeans(df_rb$rushing_yds,30)
yardmeans40 <- resample_yardmeans(df_rb$rushing_yds,40)
yardmeans50 <- resample_yardmeans(df_rb$rushing_yds,50)
yardmeans75 <- resample_yardmeans(df_rb$rushing_yds,75)
yardmeans100 <- resample_yardmeans(df_rb$rushing_yds,100)
yardmeans150 <- resample_yardmeans(df_rb$rushing_yds,150)
yardmeans200 <- resample_yardmeans(df_rb$rushing_yds,200)
yardmeans250 <- resample_yardmeans(df_rb$rushing_yds,250)
yardmeans300 <- resample_yardmeans(df_rb$rushing_yds,300)
yardmeans350 <- resample_yardmeans(df_rb$rushing_yds,350)
yardmeans400 <- resample_yardmeans(df_rb$rushing_yds,400)

qplot(yardmeans10, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="10 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans15, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="15 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans20, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="20 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans25, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="25 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans30, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="30 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans40, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="40 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans50, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="50 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans75, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="75 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans100, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="100 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans150, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="150 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans200, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="200 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans250, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="250 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans300, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="300 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans350, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="350 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))
qplot(yardmeans400, geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),xlab="Yards per Carry",main="400 carries")+coord_cartesian(xlim=c(-1, 10),ylim=c(0,1.3))

quantile(yardmeans10, c(.025, .50, .975))
quantile(yardmeans15, c(.025, .50, .975))
quantile(yardmeans20, c(.025, .50, .975))
quantile(yardmeans25, c(.025, .50, .975))
quantile(yardmeans30, c(.025, .50, .975))
quantile(yardmeans40, c(.025, .50, .975))
quantile(yardmeans50, c(.025, .50, .975))
quantile(yardmeans75, c(.025, .50, .975))
quantile(yardmeans100, c(.025, .50, .975))
quantile(yardmeans150, c(.025, .50, .975))
quantile(yardmeans200, c(.025, .50, .975))
quantile(yardmeans250, c(.025, .50, .975))
quantile(yardmeans300, c(.025, .50, .975))
quantile(yardmeans350, c(.025, .50, .975))
quantile(yardmeans400, c(.025, .50, .975))

quantile(yardmeans10, c(.25, .75))
quantile(yardmeans15, c(.25, .75))
quantile(yardmeans20, c(.25, .75))
quantile(yardmeans25, c(.25, .75))
quantile(yardmeans30, c(.25, .75))
quantile(yardmeans40, c(.25, .75))
quantile(yardmeans50, c(.25, .75))
quantile(yardmeans75, c(.25, .75))
quantile(yardmeans100, c(.25, .75))
quantile(yardmeans150, c(.25, .75))
quantile(yardmeans200, c(.25, .75))
quantile(yardmeans250, c(.25, .75))
quantile(yardmeans300, c(.25, .75))
quantile(yardmeans350, c(.25, .75))
quantile(yardmeans400, c(.25, .75))

sd(yardmeans10)
sd(yardmeans15)
sd(yardmeans20)
sd(yardmeans25)
sd(yardmeans30)
sd(yardmeans40)
sd(yardmeans50)
sd(yardmeans75)
sd(yardmeans100)
sd(yardmeans150)
sd(yardmeans200)

#Karlos Williams
yardmeans93 <- resample_yardmeans(df_rb$rushing_yds,93)
length(yardmeans93[yardmeans93>=5.6])/10000

#Footrace plots
yardmeansdf10 <- playercomp_resample("Marshawn Lynch","Trent Richardson",10)
yardmeansdf15 <- playercomp_resample("Marshawn Lynch","Trent Richardson",15)
yardmeansdf20 <- playercomp_resample("Marshawn Lynch","Trent Richardson",20)
yardmeansdf25 <- playercomp_resample("Marshawn Lynch","Trent Richardson",25)
yardmeansdf30 <- playercomp_resample("Marshawn Lynch","Trent Richardson",30)
yardmeansdf40 <- playercomp_resample("Marshawn Lynch","Trent Richardson",40)
yardmeansdf50 <- playercomp_resample("Marshawn Lynch","Trent Richardson",50)
yardmeansdf75 <- playercomp_resample("Marshawn Lynch","Trent Richardson",75)
yardmeansdf100 <- playercomp_resample("Marshawn Lynch","Trent Richardson",100)
yardmeansdf150 <- playercomp_resample("Marshawn Lynch","Trent Richardson",150)
yardmeansdf200 <- playercomp_resample("Marshawn Lynch","Trent Richardson",200)
yardmeansdf250 <- playercomp_resample("Marshawn Lynch","Trent Richardson",250)
yardmeansdf300 <- playercomp_resample("Marshawn Lynch","Trent Richardson",300)
yardmeansdf350 <- playercomp_resample("Marshawn Lynch","Trent Richardson",350)
yardmeansdf400 <- playercomp_resample("Marshawn Lynch","Trent Richardson",400)

plot_footrace <- function(df,df_title)
{
  qplot(value, data = data.frame(melt(df)), geom="density",fill=factor(variable),size=I(1.5),alpha=I(0.3),xlab="Yards Per Carry", ylab="Density", main=df_title)+coord_cartesian(xlim=c(-1, 10),ylim=c(0,2))+geom_vline(xintercept = mean(df[[1]]),size=1.5)+geom_text(aes(mean(yardmeansdf400[[1]]),y=1.5,label = round(mean(df[[1]]),digits=2), hjust = -0.19))+geom_vline(xintercept = mean(df[[2]]),size=1.5)+geom_text(aes(mean(df[[2]]),y=1.6,label = round(mean(df[[2]]),digits=2), hjust = -0.19))
}

plot_footrace(yardmeansdf10,"10 carries each")
plot_footrace(yardmeansdf15,"15 carries each")
plot_footrace(yardmeansdf20,"20 carries each")
plot_footrace(yardmeansdf25,"25 carries each")
plot_footrace(yardmeansdf30,"30 carries each")
plot_footrace(yardmeansdf40,"40 carries each")
plot_footrace(yardmeansdf50,"50 carries each")
plot_footrace(yardmeansdf75,"75 carries each")
plot_footrace(yardmeansdf100,"100 carries each")
plot_footrace(yardmeansdf150,"150 carries each")
plot_footrace(yardmeansdf200,"200 carries each")
plot_footrace(yardmeansdf250,"250 carries each")
plot_footrace(yardmeansdf300,"300 carries each")
plot_footrace(yardmeansdf350,"350 carries each")
plot_footrace(yardmeansdf400,"400 carries each")

count(yardmeansdf400[which(yardmeansdf400[,1]>yardmeansdf400[,2]),])/10000 #player 1 wins
count(yardmeansdf400[which(yardmeansdf400[,2]>yardmeansdf400[,1]),])/10000 #player 2 wins
count(yardmeansdf400[which(yardmeansdf400[,2]==yardmeansdf400[,1]),])/10000 #tie

#Gain a first?
resample_firstdown <- function(yards) #simple draw 3
{
  yardtots <- data.frame(matrix(ncol = 2, nrow = 10000))
  colnames(yardtots) <- c("total yards", "longest run")
  for (i in 1:10000) {
    run_samp <- sample(yards, size=3, replace=T)
    yardtots[i,1] = sum(run_samp)
    yardtots[i,2] = max(run_samp)
  }
  yardtots
}

firstdowns_Lynch <- resample_firstdown(df_rb[df_rb$full_name=="Marshawn Lynch",]$rushing_yds)
length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])/10000
length(firstdowns_Lynch[firstdowns_Lynch[2]==4&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]==4&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]==4,][[1]]) #If your longest run is N, you get the first down X% of the time
length(firstdowns_Lynch[firstdowns_Lynch[2]==5&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]==5&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]==5,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]==6&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]==6&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]==6,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]==7&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]==7&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]==7,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]==8&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]==8&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]==8,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]==9&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]==9&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]==9,][[1]])

length(firstdowns_Lynch[firstdowns_Lynch[2]>=5&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=5&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]>=5,][[1]]) #If your longest run is AT LEAST N, you get the first down X% of the time
length(firstdowns_Lynch[firstdowns_Lynch[2]>=6&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=6&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]>=6,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=7&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=7&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]>=7,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=8&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=8&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]>=8,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=9&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=9&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[2]>=9,][[1]])

length(firstdowns_Lynch[firstdowns_Lynch[2]>=5&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=5&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]]) #First downs included a longest run of at least N, X% of the time.
length(firstdowns_Lynch[firstdowns_Lynch[2]>=6&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=6&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=7&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=7&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=8&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=8&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=9&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=9&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])
length(firstdowns_Lynch[firstdowns_Lynch[2]>=10&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=10&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])

length(firstdowns_Lynch[firstdowns_Lynch[2]>=0&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=0&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])-length(firstdowns_Lynch[firstdowns_Lynch[2]>=8&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=8&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]]) #Only X% of first downs had a longest run between 0 and 8.
length(firstdowns_Lynch[firstdowns_Lynch[2]>=0&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=0&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]])-length(firstdowns_Lynch[firstdowns_Lynch[2]>=9&firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>=9&firstdowns_Lynch[2]>9,][[1]])/length(firstdowns_Lynch[firstdowns_Lynch[1]>9|firstdowns_Lynch[2]>9,][[1]]) #Only X% of first downs had a longest run between 0 and 9.


firstdowns_pl <- resample_firstdown(df_rb$rushing_yds)
length(firstdowns_pl[firstdowns_pl[1]>9|firstdowns_pl[2]>9,][[1]])/10000

firstdowns_pl <- resample_firstdown(df_rb[df_rb$full_name=="LeSean McCoy",]$rushing_yds)
length(firstdowns_pl[firstdowns_pl[1]>9|firstdowns_pl[2]>9,][[1]])/10000

firstdowns_pl <- resample_firstdown(df_rb[df_rb$full_name=="Frank Gore",]$rushing_yds)
length(firstdowns_pl[firstdowns_pl[1]>9|firstdowns_pl[2]>9,][[1]])/10000

firstdowns_pl <- resample_firstdown(df_rb[df_rb$full_name=="John Kuhn",]$rushing_yds)
length(firstdowns_pl[firstdowns_pl[1]>9|firstdowns_pl[2]>9,][[1]])/10000

firstdowns_pl <- resample_firstdown(df_rb[df_rb$full_name=="Adrian Peterson",]$rushing_yds)
length(firstdowns_pl[firstdowns_pl[1]>9|firstdowns_pl[2]>9,][[1]])/10000

firstdowns_pl <- resample_firstdown(df_rb[df_rb$full_name=="Jamaal Charles",]$rushing_yds)
length(firstdowns_pl[firstdowns_pl[1]>9|firstdowns_pl[2]>9,][[1]])/10000


resample_firstdown_adj <- function(df)
{
  yardtots <- data.frame(matrix(ncol = 2, nrow = 10000))
  colnames(yardtots) <- c("total yards", "longest run")
  for (i in 1:10000) {
    b=0
    c=0
    a <- sample(df[df$down==1,]$rushing_yds, size=1, replace=T)
    if(a < 10) b <- sample(df[df$down==2&df$yards_to_go==I(10-a),]$rushing_yds, size=1, replace=T)
    tot <- a+b
    if(tot < 10) c <- sample(df[df$down==3&df$yards_to_go==I(10-a),]$rushing_yds, size=1, replace=T)
    tot <- tot+c
    yardtots[i,1] = tot
    yardtots[i,2] = max(a,b,c)
  }
  yardtots
}

adjusted_firsts <- resample_firstdown_adj(df_rb)
length(which(adjusted_firsts[,1]>9))/10000 #proportion of firsts
summary(adjusted_firsts[which(adjusted_firsts[,1]>9),][2]) #longest runs for first-down series
qplot(adjusted_firsts[which(adjusted_firsts[,1]>9),][[2]], geom="density",fill=I("skyblue4"),size=I(1.5),alpha=I(0.3),main="Where First Downs Come From",xlab="Longest run of the series", ylab="Proportion")+coord_cartesian(xlim=c(0, 20),ylim=c(0,0.15))

length(adjusted_firsts[adjusted_firsts[,2]>=6&adjusted_firsts[,1]>9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=6&adjusted_firsts[,1]<=9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=6&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]>=6&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]>=6&adjusted_firsts[,1]<=9,][[1]]))

length(adjusted_firsts[adjusted_firsts[,2]==4&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]==4&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]==4&adjusted_firsts[,1]<=9,][[1]]))
length(adjusted_firsts[adjusted_firsts[,2]==6&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]==6&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]==6&adjusted_firsts[,1]<=9,][[1]]))
length(adjusted_firsts[adjusted_firsts[,2]==8&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]==8&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]==8&adjusted_firsts[,1]<=9,][[1]]))

length(adjusted_firsts[adjusted_firsts[,2]>=5&adjusted_firsts[,1]>9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=5&adjusted_firsts[,1]<=9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=5&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]>=5&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]>=5&adjusted_firsts[,1]<=9,][[1]]))

length(adjusted_firsts[adjusted_firsts[,2]>=4&adjusted_firsts[,1]>9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=4&adjusted_firsts[,1]<=9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=4&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]>=4&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]>=4&adjusted_firsts[,1]<=9,][[1]]))

length(adjusted_firsts[adjusted_firsts[,2]>=3&adjusted_firsts[,1]>9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=3&adjusted_firsts[,1]<=9,][[1]])
length(adjusted_firsts[adjusted_firsts[,2]>=3&adjusted_firsts[,1]>9,][[1]])/(length(adjusted_firsts[adjusted_firsts[,2]>=3&adjusted_firsts[,1]>9,][[1]])+length(adjusted_firsts[adjusted_firsts[,2]>=3&adjusted_firsts[,1]<=9,][[1]]))
