#THIS SCRIPT JUST CALCULATES GAME SCORE, WITHOUT ANY OF THE PLOTTING FUNCTIONS ETC FOUND IN THE CHAPTER.
#Simply change the filepath here to load the csv (found in the Ground Control github page), the run the entire script. It will take a while.

df = read.csv("rushing_data_stack.csv") #point this to the location of the csv from the github.

# Load Packages -----------------------------------------------------------

library('reshape2')
library("dplyr")
library('doParallel') #the simulation is massive so we're doing this in parallel.

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

# Converting distance to probability --------------------------------------

#Kernal Density Estimation
kde_avg <- density(df_rb$rushing_yds, adjust = 5, n=121, from = -20, to = 100)
df_kde <- data.frame(rushing_yds = kde_avg$x, density = kde_avg$y)
df_kde$runprob <- df_kde$density / sum(df_kde$density)

#Combine "mediocre and bad" runs into a single category
lowprob <- sum(df_kde[0:(which.max(df_kde$runprob)-1),]$runprob)
df_kde[0:(which.max(df_kde$runprob)-1),]$runprob <- lowprob

#Add KDE probabilities to database
df_rb <- merge(df_rb, df_kde, by=c("rushing_yds"))

df_rb$yardname <- df_rb$rushing_yds
df_rb[df_rb$yardname<2,]$yardname <- 1 #Rounds bad runs into single category. Numbers determined by shortest yardage below the max probable run.
df_rb$yardname <- factor(df_rb$yardname) #Turns numerical distance values into names of the distances, and saves it as a factor.
df_kde <- df_kde[df_kde$rushing_yds>0,] #Restricts the KDE to same range.

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

#Account for league-average Surprisal ----------------------------------
#Account using league-average simulations. THIS IS HOW WE CALCULATE GAME SCORE IN THE CHAPTER (not loess).
#resampled surprisal at each number of carries
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
df_s$gamescore <- df_s$surp - df_s$surp_avg #calculate how much above or below average each game was from the simulated mean.

summary(df_s$gamescore) #there are you Game Score values!





