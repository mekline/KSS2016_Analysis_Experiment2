#This reads in the *.dat data from a forced-choice
#experiment (TVW at the moment!)

#Note: Data cleaning & participant selection (see Participants_TSW for details for fussouts/other exclusions)
#Descriptives & analsyis start line 50

library(reshape)
library(stringr)
library(ggplot2)
library(testit)
library(ggthemes)
library(bootstrap)

assert("Make sure R is pointed at the right working directory", sum(str_detect(dir(), "Data_E2"))==1)
#See setwd() or Session > Set Working Directory > To Source File Location 


#DATA LOADING & SHAPING

####Load Datafiles
#Load in the data from the TVW files

mydata <- data.frame(NULL)
max_participant = 131

for(f in 1:max_participant) {
	filename = paste('Data_E2/TSW_', f, '.dat', sep='')
	tryCatch({
		tmp <- read.table(filename, header=FALSE, sep=" ")
		names(tmp) <- c("Subject", "Condition", "Trial.Number", "MUSH", "First.Item", "Second.Item",
			 "First.Side", "Causal.Side", "Response.Causal", "Choice.Causal")
		mydata <- rbind(mydata, tmp)
		}, 
		error = function(ex) {},
		finally = {})
}

#Load in the data about participants and pretest/touch performance
pdata <- data.frame(NULL)
pdata <- read.table('Participants_TSW.csv', header=TRUE, sep=',')
mydata <- merge(mydata, pdata, by=c("Subject"))

#Make sure conditions are labeled correctly
mydata <- mydata[mydata$Experiment != "switch-pilot",]

#THROW OUT people who didn't get included!


#Optional - keep those who just failed pretest!!
mydata[mydata$Exclude.Reason == "FAILED PRETEST",]$Include <- 0

mydata <- mydata[mydata$Include == 1,]
mydata <- mydata[!is.na(mydata$Condition),]




################################################
#####DEMOGRAPHICS

#means days old - Note, it counts subjects not trials
collapsed <- mydata[!duplicated(mydata$Subject),]
transitive <- collapsed[collapsed$Condition=="Transitive",]
intransitive <- collapsed[collapsed$Condition=="Intransitive",]
happen <- collapsed[collapsed$Condition=="Happen",]

length(collapsed$Days.Old)
mean(collapsed$Days.Old, na.rm = TRUE) 
length(transitive$Days.Old)
mean(transitive$Days.Old)
length(intransitive$Days.Old)
mean(intransitive$Days.Old) 
length(happen$Days.Old)
mean(happen$Days.Old) 

foo <- subset(collapsed, select=c("Subject", "Days.Old"))
min(foo$Days.Old)
max(foo$Days.Old)

#Number of girls - Note, make sure it counts subjects, not trials!
foo <- subset(collapsed, select=c("Subject", "Gender"))
nrow(foo[foo$Gender=="F",])
nrow(foo[foo$Gender=="M",])




################################################
#######ANALYSIS!

#aggregate the choices in a new dataframe: how many causal choices did you make, 0, 1, or 2
sum.na.rm <- function(x) { sum(x,na.rm=T) }
my.sd <- function(x) {sd(x)/sqrt(length(x))}

Scores <- aggregate(mydata$Choice.Causal, by=list(mydata$Subject), sum.na.rm)
names(Scores) <- c("Subject", "CausalScore")
collapsed <- merge(collapsed,Scores, by=c("Subject"))

with(collapsed, tapply(CausalScore, list(Condition), mean, na.rm=TRUE), drop=TRUE)
with(collapsed, tapply(CausalScore, list(Condition), my.sd), drop=TRUE)

#Check whether each group is different from chance, with wilcox test
wilcox.test(collapsed[collapsed$Condition=="Transitive",]$CausalScore, mu=1, exact=FALSE)
wilcox.test(collapsed[collapsed$Condition=="Intransitive",]$CausalScore, mu=1, exact=FALSE)
wilcox.test(collapsed[collapsed$Condition=="Happen",]$CausalScore, mu=1, exact=FALSE)

#And is transitive different from intransitive?
wilcox.test(collapsed[collapsed$Condition=="Transitive",]$CausalScore, collapsed[collapsed$Condition=="Intransitive",]$CausalScore, exact=FALSE, paired = FALSE)
#And is transitive different from happen?
wilcox.test(collapsed[collapsed$Condition=="Transitive",]$CausalScore, collapsed[collapsed$Condition=="Happen",]$CausalScore, exact=FALSE, paired = FALSE)
#And is happen different from intransitive?
wilcox.test(collapsed[collapsed$Condition=="Happen",]$CausalScore, collapsed[collapsed$Condition=="Intransitive",]$CausalScore, exact=FALSE, paired = FALSE)

#Time for bootstrapped confidence intervals around the means of the 3 conditions!
graphdata <- data.frame(NULL)
happen.boot.mean = bootstrap(collapsed[collapsed$Condition=="Happen",]$CausalScore, 1000, mean)
graphdata <- rbind(graphdata,quantile(happen.boot.mean$thetastar, c(0.025, 0.975)))
trans.boot.mean = bootstrap(collapsed[collapsed$Condition=="Transitive",]$CausalScore, 1000, mean)
graphdata <- rbind(graphdata,quantile(trans.boot.mean$thetastar, c(0.025, 0.975)))
intrans.boot.mean = bootstrap(collapsed[collapsed$Condition=="Intransitive",]$CausalScore, 1000, mean)
graphdata <- rbind(graphdata,quantile(intrans.boot.mean$thetastar, c(0.025, 0.975)))

names(graphdata) <- c("LowCI","HighCI")

graphdata$CondName <- c("Manipulation Check", "Transitive", "Intransitive")

#New 3/16/16 make some pretty ggplot graphs 
graphdata$Mean[1] <- mean(collapsed[collapsed$Condition=="Happen",]$CausalScore)
graphdata$Mean[2] <- mean(collapsed[collapsed$Condition=="Transitive",]$CausalScore)
graphdata$Mean[3] <- mean(collapsed[collapsed$Condition=="Intransitive",]$CausalScore)

graphdata$ToOrder <- c(1,2,3)
barcolors = c("steelblue2", "steelblue4", "steelblue3") #Dumb bug! I am overriding R's default order and the colors don't go with :(
p<- ggplot(graphdata, aes(x=CondName, y=Mean, fill=CondName, group=CondName)) +
  geom_bar(stat="identity",) +
  aes(x=reorder(CondName, ToOrder)) +
  scale_fill_manual(values=barcolors) +
  geom_errorbar(aes(ymin=LowCI, ymax=HighCI), colour="black", width=.1) +
  coord_cartesian(ylim=c(0,2))+  
  #theme_set(theme_gray(base_size = 14))+
  #ggtitle(title)+
  ylab("Mean Causal Choices")+
  xlab("")+
  theme(legend.position="none")+
  geom_hline(aes(yintercept=1), color="black", linetype="dashed")

p
ggsave(filename="E2.jpg", plot=p, width=6, height=4)







#And try all those as T tests, even though that's toootally wrong
#Check whether each group is different from chance, with wilcox test
t.test(collapsed[collapsed$Condition=="Transitive",]$CausalScore, mu=1, exact=FALSE)
t.test(collapsed[collapsed$Condition=="Intransitive",]$CausalScore, mu=1, exact=FALSE)
t.test(collapsed[collapsed$Condition=="Happen",]$CausalScore, mu=1, exact=FALSE)

#And is transitive different from intransitive?
t.test(collapsed[collapsed$Condition=="Transitive",]$CausalScore, collapsed[collapsed$Condition=="Intransitive",]$CausalScore, exact=FALSE, paired = FALSE)
#And is transitive different from happen?
t.test(collapsed[collapsed$Condition=="Transitive",]$CausalScore, collapsed[collapsed$Condition=="Happen",]$CausalScore, exact=FALSE, paired = FALSE)




#Compare 3s and 4s
threes <- collapsed[collapsed$Age.Years == 3,]
fours <- collapsed[collapsed$Age.Years == 4,]

with(threes, tapply(CausalScore, list(Condition), mean, na.rm=TRUE), drop=TRUE)
with(fours, tapply(CausalScore, list(Condition), mean, na.rm=TRUE), drop=TRUE)

wilcox.test(threes[threes$Condition=="Transitive",]$CausalScore, fours[fours$Condition=="Transitive",]$CausalScore, exact=FALSE)
wilcox.test(threes[threes$Condition=="Intransitive",]$CausalScore, fours[fours$Condition=="Intransitive",]$CausalScore, exact=FALSE)
wilcox.test(threes[threes$Condition=="Happen",]$CausalScore, fours[fours$Condition=="Happen",]$CausalScore, exact=FALSE)

#Check whether each group is different from chance, with wilcox test
wilcox.test(threes[threes$Condition=="Transitive",]$CausalScore, mu=1, exact=FALSE)
wilcox.test(threes[threes$Condition=="Intransitive",]$CausalScore, mu=1, exact=FALSE)
wilcox.test(threes[threes$Condition=="Happen",]$CausalScore, mu=1, exact=FALSE)

#And is transitive different from intransitive?
wilcox.test(threes[threes$Condition=="Transitive",]$CausalScore, threes[threes$Condition=="Intransitive",]$CausalScore, exact=FALSE)

#Check whether each group is different from chance, with wilcox test
wilcox.test(fours[fours$Condition=="Transitive",]$CausalScore, mu=1, exact=FALSE)
wilcox.test(fours[fours$Condition=="Intransitive",]$CausalScore, mu=1, exact=FALSE)
wilcox.test(fours[fours$Condition=="Happen",]$CausalScore, mu=1, exact=FALSE)

#And is transitive different from intransitive?
wilcox.test(fours[fours$Condition=="Transitive",]$CausalScore, fours[fours$Condition=="Intransitive",]$CausalScore, exact=FALSE)





















################################################
#Old ANALYSIS
##ANALYSIS - DESCRIPTIVES
#How did they do on causal questions?

trials.CQ <- nrow(mydata)
corr.CQ <- nrow(mydata[mydata$Choice.Causal == 1,])
corr.CQ/trials.CQ

#How about noncausal?

trials.NCQ <- nrow(mydata)
corr.NCQ <- nrow(mydata[mydata$Choice.NC == 0,])
corr.NCQ/trials.NCQ

#Overall?

(corr.CQ + corr.NCQ)/(trials.CQ + trials.NCQ)

#What about 1st vs. 2nd question?

yesfirst <- mydata[mydata$Yesfirst == 1,]
nofirst <- mydata[mydata$Yesfirst == 0,]

#First questions are yesfirst causal and nofirst NC
trials.CQ <- nrow(yesfirst)
corr.CQ <- nrow(yesfirst[yesfirst$Choice.Causal == 1,])
corr.CQ/trials.CQ

trials.NCQ <- nrow(nofirst)
corr.NCQ <- nrow(nofirst[nofirst$Choice.NC == 0,])
corr.NCQ/trials.NCQ

(corr.CQ + corr.NCQ)/(trials.CQ + trials.NCQ)

#Second questions are yesfirst NC, nofirst C

trials.NCQ <- nrow(yesfirst)
corr.NCQ <- nrow(yesfirst[yesfirst$Choice.NC == 0,])
corr.NCQ/trials.NCQ

trials.CQ <- nrow(nofirst)
corr.CQ <- nrow(nofirst[nofirst$Choice.Causal == 1,])
corr.CQ/trials.CQ

(corr.CQ + corr.NCQ)/(trials.CQ + trials.NCQ)










#################################

mydata$Causal.First <- mydata$First.Side==mydata$Causal.Side

trials.cfirst <- nrow(oldsters[oldsters$Causal.First == TRUE,])
trials.cfirst
trials.clast <- nrow(oldsters[oldsters$Causal.First == FALSE,])
trials.clast

corr.cfirst <- sum(oldsters[oldsters$Causal.First == TRUE,]$Causal.Choice)
corr.cfirst/trials.cfirst

corr.clast <- sum(oldsters[oldsters$Causal.First == FALSE,]$Causal.Choice)
corr.clast/trials.clast

(corr.cfirst+corr.clast)/(trials.cfirst+trials.clast)

unique(oldsters$Subject)








trials.cfirst <- nrow(mydata[mydata$Causal.First == TRUE,])
trials.cfirst
trials.clast <- nrow(mydata[mydata$Causal.First == FALSE,])
trials.clast

corr.cfirst <- sum(mydata[mydata$Causal.First == TRUE,]$Causal.Choice)
corr.cfirst/trials.cfirst

corr.clast <- sum(mydata[mydata$Causal.First == FALSE,]$Causal.Choice)
corr.clast/trials.clast

(corr.cfirst+corr.clast)/(trials.cfirst+trials.clast)



