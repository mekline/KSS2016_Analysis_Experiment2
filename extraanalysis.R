library(reshape)

mydata <- data.frame(NULL)

mydata <- read.table('check_sentences.csv', header=TRUE, sep=",")

c_sentences <- aggregate(mydata$Sentence., by=list(mydata$Cond.), mean)
c_sentences <- aggregate(mydata$Sentence., by=list(mydata$Cond.), sum)

%Here's the wrongish Chi Square test
sents <- c(40,33,8,15)
dim(sents) <- c(2,2)
chisq.test(sents)

%Here's the righter MacNemar test
peoplescores <- aggregate(mydata$Sentence., by=list(mydata$Cond., mydata$Subj), sum)
names(peoplescores) <- c("Cond", "Subj", "Score")

tmp <- cast(peoplescores, Subj~Cond)

marginals <- c(nrow(tmp[tmp$C < 1 & tmp$N < 1,]),
				nrow(tmp[tmp$C > 0 & tmp$N < 1,]),
				nrow(tmp[tmp$C < 1 & tmp$N > 0,]),
				nrow(tmp[tmp$C > 0 & tmp$N > 0,]))
dim(marginals) <- c(2,2)

mcnemar.test(marginals)

%Here's the Wilcox test - right? Only 3 levels, seems weird

C <- peoplescores[peoplescores$Cond == "C",]
NC <- peoplescores[peoplescores$Cond == "N",]

wilcox.test(C$Score, NC$Score)

