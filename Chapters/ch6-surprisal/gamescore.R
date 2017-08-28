df = read.csv("rushing_data_stack.csv")


# Load Packages -----------------------------------------------------------

library('ggplot2')
library('reshape2')
library("dplyr")
library('ggthemes')
library('entropy')


# Munge starting data -----------------------------------------------------

df_rb = df[df$FantPos=='RB',] #restrict to RBs only

#Turn discrete variables into factors
df_rb$week <- factor(df_rb$week)
df_rb$down <- factor(df_rb$down)
df_rb$year <- factor(df_rb$year)
df_rb$FantPos <- factor(df_rb$FantPos)
df_rb$full_name <- factor(df_rb$full_name)

df_rb$nameweekyear <- paste(df_rb$full_name, df_rb$year, df_rb$week, sep="_")
df_rb <- merge(df_rb, dcast(df_rb, full_name+week+year ~ "nameweekyear_atts", length), by=c("full_name","year", "week")) #Find weekly carries.



# How to illustrate a single game? ----------------------------------------

#Sample size problem for run distributions
density<-ggplot(df_rb, aes(rushing_yds)) + geom_density(fill='blue',alpha = 0.2,adjust=1.85)+ coord_cartesian(xlim=c(-10, 20), ylim=c(0,.16))+ylab ("Probability")
density + geom_density(mapping = aes(rushing_yds), data = df_rb[df_rb$nameweekyear=="David Johnson_2015_2",],fill='red',alpha = 0.2) +
  theme(legend.title = element_text(size=16, face="bold"))+ggtitle("David Johnson week 2, 2015")+
  theme_fivethirtyeight()+ylab ("Probability Density")+xlab ("Rushing Yards")+
  labs(title="David Johnson week 2, 2015")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))

ggplot(data=df_rb[df_rb$nameweekyear=="David Johnson_2015_2",], aes(rushing_yds)) + 
  stat_ecdf(alpha = 0.8,size=1.4) + coord_cartesian(xlim=c(-10, 20)) + ylab("Proportion of Run Death")+xlab ("Rushing Yards")+ scale_y_reverse(breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))+
  theme(legend.title = element_text(size=16, face="bold"))+theme_fivethirtyeight()+
  labs(title="David Johnson week 2, 2015")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))

#Possible solution: "rug" plots.
RunRug <- function(player1,week1,year1)
{
  dfsub <- df_rb[df_rb$full_name==player1,]
  dfsub <- dfsub[dfsub$week==week1&dfsub$year==year1,]
  dfsub$rushjitter <- dfsub$rushing_yds+rnorm(length(dfsub$rushing_yds),sd=0.1)
  ggplot(dfsub, aes(rushjitter))+
    geom_segment(mapping=aes(x=rushjitter, y=0, xend=rushjitter, yend=0.4), col="coral3",alpha=0.7)+
    geom_vline(size=1.75,xintercept=mean(dfsub$rushing_yds))+
    theme_fivethirtyeight()+xlab ("Yardage gained on runs")+
    coord_fixed(ratio=3,xlim=c(-5, 20))+
    labs(title=paste0("Rushing Output: ",player1," week ", week1))+theme(plot.title = element_text(size = rel(1.5)))+
    theme(axis.title=element_text(size="16"))+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

RunRug("David Johnson", 3, 2015)
RunRug("David Johnson", 14, 2015)
RunRug("David Johnson", 2, 2015)
RunRug("David Johnson", 15, 2015) #But still has problems.

#Runprobs for all runs from 1 to 10 yards (used for table)
length(df_rb[df_rb$rushing_yds==1,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==2,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==3,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==4,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==5,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==6,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==7,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==8,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==9,]$nameweekyear)/length(df_rb$nameweekyear)
length(df_rb[df_rb$rushing_yds==10,]$nameweekyear)/length(df_rb$nameweekyear)


# Converting distance to probability --------------------------------------

#Kernal Density Estimation
kde_avg <- density(df_rb$rushing_yds, adjust = 5, n=121, from = -20, to = 100)
df_kde <- data.frame(rushing_yds = kde_avg$x, density = kde_avg$y)
df_kde$runprob <- df_kde$density / sum(df_kde$density)

#Plot KDE
ggplot(df_kde, aes(x=rushing_yds, y=runprob))+geom_line(size=2)+
  theme(legend.title = element_text(size=16, face="bold"))+ggtitle("Run Probability for Yards Gained")+
  theme_fivethirtyeight()+ylab ("Probability")+xlab ("Yards")+
  labs(title="Run Probability for Yards Gained")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))+coord_cartesian(xlim=c(-10,40))

#Combine "mediocre and bad" runs into a single category
lowprob <- sum(df_kde[0:(which.max(df_kde$runprob)-1),]$runprob)
df_kde[0:(which.max(df_kde$runprob)-1),]$runprob <- lowprob

#Add KDE probabilities to database
df_rb <- merge(df_rb, df_kde, by=c("rushing_yds"))

df_rb$yardname <- df_rb$rushing_yds
df_rb[df_rb$yardname<2,]$yardname <- 1 #Rounds bad runs into single category. Numbers determined by shortest yardage below the max probable run.
df_rb$yardname <- factor(df_rb$yardname) #Turns numerical distance values into names of the distances, and saves it as a factor.
df_kde <- df_kde[df_kde$rushing_yds>0,] #Restricts the KDE to same range.

#Plot run probabilities
probvector <- df_kde[df_kde$rushing_yds%in%unique(df_rb$yardname),]$runprob

ggplot(df_kde[df_kde$rushing_yds>1,], aes(x=rushing_yds, y=runprob))+geom_line(size=2)+
  theme(legend.title = element_text(size=16, face="bold"))+ggtitle("Run Probability for Yards Gained")+
  theme_fivethirtyeight()+ylab ("Probability")+xlab ("Yards")+
  labs(title="Run Probability for Yards Gained")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))+coord_cartesian(xlim=c(-10,40))

ggplot(df_kde[df_kde$rushing_yds>1,], aes(x=rushing_yds, y=runprob))+geom_line(size=2)+
  theme(legend.title = element_text(size=16, face="bold"))+ggtitle("Run Probability for Yards Gained")+
  theme_fivethirtyeight()+ylab ("Probability")+xlab ("Yards")+
  labs(title="Run Probability for Yards Gained")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))+coord_cartesian(xlim=c(-10,40))+
  geom_point(data=df_kde[df_kde$rushing_yds==1,], size=5)


# Generalized Game Surprisal Calculation ----------------------------------

probvector <- df_kde[df_kde$rushing_yds%in%unique(df_rb$yardname),]$runprob

findprob <- function(nameweekyearID) #define function to take a player-game as an input and extract the Game Probability using the multinomial distribution and the probability vector estimated from the KDE.
{
  a<-tapply(df_rb[df_rb$nameweekyear==nameweekyearID,]$yardname,df_rb[df_rb$nameweekyear==nameweekyearID,]$yardname,length) #this uses a trick: "yardname" is just "rushing_yds" turned from a number into a name. This function counts the number of times each run distance occurred. Later, we can apply the probability vector to this set when sampling from the multinomial distribution. 
  a[is.na(a)]<-0
  dmultinom(a,prob=probvector)
}

df_s<-dcast(df_rb, full_name+week+year+homefield+defense+Age+nameweekyear+nameweekyear_atts ~ "nameweekyear_prob", value.var = "nameweekyear", fun.aggregate = function(x) findprob(x)) #Apply the game probability function to the database
df_s$surp <- -log2(df_s$nameweekyear_prob) #convert game probability to Game Surprisal.

#Plot Game Surprisal
ggplot(df_s, aes(x=nameweekyear_atts, y=nameweekyear_prob))+stat_smooth(method="loess", size=2, span = 0.3)+
  theme(legend.title = element_text(size=16, face="bold"))+ggtitle("Game Probability over Carries")+
  theme_fivethirtyeight()+ylab ("Game Probability")+xlab ("Carries")+
  labs(title="Game Probability over Carries")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))

ggplot(df_s, aes(x=nameweekyear_atts, y=surp))+stat_smooth(method="loess", size=2, span = 0.3)+
  theme(legend.title = element_text(size=16, face="bold"))+ggtitle("Game Surprisal over Carries")+
  theme_fivethirtyeight()+ylab ("Game Surprisal (in bits)")+xlab ("Carries")+
  labs(title="Game Surprisal over Carries")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))



#Account for league-average Surprisal ----------------------------------

#Account using LOESS local regression. This is biased by the fact that better players are given more carries (but could still be useful).
#note: does not appear in chapter.
surp_loess <- loess(surp ~ nameweekyear_atts, data = df_s, span=1/3) #loess estimation
df_s$surp_loess <- surp_loess$fitted #applies "average game" values to database.
df_s$surp_loessmarg <- df_s$surp - df_s$surp_loess #finds how much above or below average every game was.

#Account using league-average simulations. THIS IS HOW WE CALCULATE GAME SCORE IN THE CHAPTER (not loess).
#resampled surprisal at each number of carries
library('doParallel') #the simulation is massive so we're doing this in parallel.
cl <- makeCluster(4)  #SET TO NUMBER OF CORES YOU WANT TO USE
registerDoParallel(cl) 

#define a function to calculate game surprisal, given a database to sample from (we'll put in df_rb$rushing_yds for "yards") and a number of carries.
resample_yards <- function(yards,samplesize) 
{
  yardsample = numeric(150000) #populate a vector to fill with game samples. This determines number of simulated games.
  for (i in 1:150000) { #if number of simulated games was changed above, change it here too.
    a  = sample(yards, size=samplesize, replace=T) #sample some carries at random.
    b = tapply(a, a, length) #use the name counting trick.
    b[is.na(b)]<-0
    yardsample[i] <- -log2(dmultinom(b,prob=probvector)) #calculates game surprisal for the sample.
  }
  yardsample #returns vector of simulated Game Surprisal values.
}

#Define function to apply the game surprisal simulator to every carry size from 1 to 40.
resample_db <- numeric(40)
resample_db <- foreach(n = 1:40, .combine=c) %dopar% {
  list(resample_yards(df_rb$yardname, n))
}

stopCluster(cl) #end the CPU cluster.

resample_db_mean <- data.frame(unlist(lapply(resample_db, mean))) #find average of the simulated games at each carry size.
resample_db_mean <- add_rownames(resample_db_mean, "nameweekyear_atts")
names(resample_db_mean)[2] <- "surp_avg"
df_s <- merge(df_s, resample_db_mean, by="nameweekyear_atts") #add these "league average" simulated values to the database.
df_s$surp_marg <- df_s$surp - df_s$surp_avg #calculate how much above or below average each game was from the simulated mean.

entropy(df_kde$runprob,unit="log2") #"average surprisal" is another name for "entropy". So...
mean(df_s[df_s$nameweekyear_atts==1,]$surp_avg) #mean simulated surprisal should be similar to entropy of run probability distribution. This double-checks our work.

#Add YPC of each week to dabase as comparison.
df_s <- merge(df_s, dcast(df_rb, nameweekyear~"YPC", value.var = "rushing_yds", fun.aggregate = function(x) mean(x)), by="nameweekyear")


# Plot Game Surprisal and Game Score values -------------------------------

ggplot(df_s, aes(x=nameweekyear_atts, y=surp))+geom_point(color="gray70")+
  geom_line(aes(x=nameweekyear_atts, y=surp_avg), size=1.75, color="dodgerblue3")+
  theme_fivethirtyeight()+ylab ("Game Surprisal (in bits)")+xlab ("Game Rushing Attempts")+
  labs(title="Game Surprisal")+theme(plot.title = element_text(size = rel(2)))+
  theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))

probvsleague <- function(player1)
{
  ggplot(df_s, aes(x=nameweekyear_atts, y=surp))+geom_point(color="gray70")+
    geom_line(aes(x=nameweekyear_atts, y=surp_avg), size=1.75, color="dodgerblue3")+
    geom_point(data=df_s[df_s$full_name==player1,],color="coral3",size=4)+
    stat_smooth(data=df_s[df_s$full_name==player1,], method="loess", size=1.75, span = 0.65, color="coral3", se=FALSE)+
    theme_fivethirtyeight()+ylab ("Game Surprisal (in bits)")+xlab ("Game Rushing Attempts")+
    labs(title=paste0("Surprising Games: ",player1))+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
}

probvsleague_year <- function(player1,year)
{
  ggplot(df_s, aes(x=nameweekyear_atts, y=surp))+geom_point(color="gray80")+
    geom_line(aes(x=nameweekyear_atts, y=surp_avg), size=1.75, color="dodgerblue3")+
    geom_point(data=df_s[df_s$full_name==player1,],color="coral3",size=4)+
    geom_point(data=df_s[df_s$full_name==player1&df_s$year==year,],color="darkslategray4",shape=18,size=6.5)+
    stat_smooth(data=df_s[df_s$full_name==player1,], method="loess", size=1.75, span = 0.65, color="coral3", se=FALSE)+
    theme_fivethirtyeight()+ylab ("Game Surprisal (in bits)")+xlab ("Game Rushing Attempts")+
    labs(title=paste0("Surprising Games: ",player1," ",year))+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
}

probvsleague("LeSean McCoy")
probvsleague("Frank Gore")
probvsleague("Trent Richardson")
probvsleague("Jonathan Stewart")
probvsleague("Joique Bell")
probvsleague("Devontae Booker")
probvsleague("Ezekiel Elliott")
probvsleague("Jamaal Charles")


probvsleague_year("Melvin Gordon", 2016)
probvsleague_year("David Johnson", 2016)
probvsleague_year("LeSean McCoy", 2016)
probvsleague_year("Mike Gillislee", 2016)
probvsleague_year("Doug Martin", 2016)
probvsleague_year("Jamaal Charles", 2010)


#Density plots of Game Score
probvsleague_den <- function(player1)
{
  ggplot(df_s[df_s$nameweekyear_atts>2,], aes(surp_marg))+
    geom_density(fill='blue',alpha = 0.2, adjust=1.15)+coord_cartesian(xlim=c(-15, 15), ylim=c(0,0.25))+
    geom_rug(data=df_s[df_s$full_name==player1,],col="coral3",alpha=0.6)+
    #geom_vline(col="coral3",alpha=.6,xintercept=df_s[df_s$full_name==player1,]$surp_marg)+
    geom_density(data=df_s[df_s$full_name==player1,],fill="coral3", alpha=0.2)+
    theme_fivethirtyeight()+ylab ("Relative Density")+xlab ("Game Score (in bits)")+
    labs(title=paste0("Surprising Games: ",player1))+theme(plot.title = element_text(size = rel(2)))+
    theme(axis.title=element_text(size="16"))+theme(axis.title.y = element_text(angle=90))
}

probvsleague_den("Jamaal Charles")
probvsleague_den("Trent Richardson")
probvsleague_den("Adrian Peterson")
probvsleague_den("Marshawn Lynch")
probvsleague_den("Tevin Coleman")
probvsleague_den("Jeremy Langford")
probvsleague_den("Matt Jones")


# Exploring and Validating Game Score ----------------------------------------------------

#List Game Score values over career
df_s_avg<-dcast(df_s, full_name ~ "surp_marg", value.var = "surp_marg", fun.aggregate = function(x) mean(x)) #take the "dumb mean" of yearly Game Score averages.
df_s_avg<-merge(df_s_avg, dcast(df_s, full_name ~ "nameweekyear_atts", value.var = "nameweekyear_atts", fun.aggregate = function(x) sum(x)), by = "full_name") #add yearly carries
df_s_avg100 <- df_s_avg[df_s_avg$nameweekyear_atts>100,] #restrict to 100+ carry seasons

#Year by Year stability
##calculate dumb mean of Game Score and YPC and Game Surprisal etc values.
df_years <- aggregate(df_s, by=list(df_s$full_name,df_s$year), FUN=mean, na.rm=TRUE)
df_years[c("nameweekyear", "week", "homefield", "defense")] <- NULL
df_years<-merge(df_years, aggregate(df_s$nameweekyear_atts, by=list(df_s$full_name,df_s$year), FUN=sum, na.rm=TRUE), by=c("Group.1","Group.2"))
names(df_years)[names(df_years)=="x"] <- "atts_year"
df_years$full_name <- df_years$Group.1
df_years$year <- df_years$Group.2
df_years[c("nameweekyear", "week", "homefield", "defense", "Group.1", "Group.2", "nameweekyear_atts")] <- NULL

##Calculate regular mean of YPC values for each year.
df_ypc <- aggregate(df_rb[,c("full_name","year","rushing_yds")], by=list(df_rb$full_name,df_rb$year), FUN=mean, na.rm=TRUE)
df_ypc$full_name <- df_ypc$Group.1
df_ypc$year <- df_ypc$Group.2
df_ypc[c("Group.1", "Group.2")] <- NULL
names(df_ypc)[names(df_ypc)=="rushing_yds"] <- "YPC_year"
df_years <- merge(df_years, df_ypc, by=c("full_name", "year"))

##Calculate mean carries per game for each year.
df_carriesweek <- dcast(df_rb, full_name+week+year ~ "nameweekyear_atts", length)
df_carriesweek <- aggregate(df_carriesweek, by=list(df_carriesweek$full_name,df_carriesweek$year), FUN=mean, na.rm=TRUE)
df_carriesweek$full_name <- df_carriesweek$Group.1
df_carriesweek$year <- df_carriesweek$Group.2
df_carriesweek[c("Group.1", "Group.2", "week")] <- NULL
names(df_carriesweek)[names(df_carriesweek)=="nameweekyear_atts"] <- "carriespergame_year"
df_years <- merge(df_years, df_carriesweek, by=c("full_name", "year"))

##combine above data frames and arrange year-to-year comparisons
df_years_100car <- df_years[df_years$atts_year>=100,] #restrict to years with 100+ carries
yearcount <- count(df_years_100car,vars = full_name)
yearcount <- yearcount[yearcount$n>=2,] #find players with at least 2 years of 100+ carries.
df_years_100car <- df_years_100car[df_years_100car$full_name%in%yearcount$vars,]
df_years_100car$year <- as.numeric(levels(df_years_100car$year))[df_years_100car$year]
df_years_100car$year2 <- df_years_100car$year-1 #Derive consecutive years for those players.
df_years_100car <- merge(df_years_100car, df_years_100car, by.x=c("full_name", "year"), by.y=c("full_name", "year2")) #make database of consecutive 100+ carry years.

#Calculate year-to-year correlations for efficiency stats.
cor(df_years_100car$YPC.x, df_years_100car$YPC.y)
cor(df_years_100car$YPC_year.x, df_years_100car$YPC_year.y)
cor(df_years_100car$surp_marg.x, df_years_100car$surp_marg.y)

cor(df_years_100car$YPC.x, df_years_100car$YPC.y, method="spearman")
cor(df_years_100car$YPC_year.x, df_years_100car$YPC_year.y, method="spearman")
cor(df_years_100car$surp_marg.x, df_years_100car$surp_marg.y, method="spearman")

#Calculate year-to-year correlations for volume stats, using a 20-carry cutoff instead of 100-carry.
df_years_20car <- df_years[df_years$atts_year>=20,]
yearcount <- count(df_years_20car,vars = full_name)
yearcount <- yearcount[yearcount$n>=2,]
df_years_20car <- df_years_20car[df_years_20car$full_name%in%yearcount$vars,]
df_years_20car$year <- as.numeric(levels(df_years_20car$year))[df_years_20car$year]
df_years_20car$year2 <- df_years_20car$year-1
df_years_20car <- merge(df_years_20car, df_years_20car, by.x=c("full_name", "year"), by.y=c("full_name", "year2"))
cor(df_years_20car$atts_year.x, df_years_20car$atts_year.y)
cor(df_years_20car$surp.x, df_years_20car$surp.y)
cor(df_years_20car$carriespergame_year.x, df_years_20car$carriespergame_year.y)
cor(df_years_20car$atts_year.x, df_years_20car$atts_year.y, method="spearman")
cor(df_years_20car$surp.x, df_years_20car$surp.y, method="spearman")
cor(df_years_20car$carriespergame_year.x, df_years_20car$carriespergame_year.y, method="spearman")
