R at Aobayama 20221205 by Ryosuke TAJIMA 
==============  

# Reading and Handling dataset
```R  
getwd() # checking current directory
setwd("****") # setting the using directory

# reading dataset
d<-read.table("data.txt", header=T)
head(d)
d

# checking column
rep<-d$Rep  
rep<-d$rep # distinguishing between capital and small

#checking the class 
class(d$Rep)
class(d$SDW)  

# extracting from datasets  
sd<-subset(d,d$Entry=="Nerica1")
sd<-subset(d,d$Rep=="R1")

# calculating indices
SRL_L1<-d$TRLD_L1/d$RWD_L1
SRL_L2<-d$TRLD_L2/d$RWD_L2
SRL_L3<-d$TRLD_L3/d$RWD_L3
SRL_L4<-d$TRLD_L4/d$RWD_L4
SRL_All<-d$TRLD/d$RWD

# changing Nan, Inf to NA
SRL_L1<-ifelse(is.nan(SRL_L1), NA, SRL_L1)
SRL_L1<-ifelse(is.infinite(SRL_L1), NA, SRL_L1)
SRL_L2<-ifelse(is.nan(SRL_L2), NA, SRL_L2)
SRL_L2<-ifelse(is.infinite(SRL_L2), NA, SRL_L2)
SRL_L3<-ifelse(is.nan(SRL_L3), NA, SRL_L3)
SRL_L3<-ifelse(is.infinite(SRL_L3), NA, SRL_L3)
SRL_L4<-ifelse(is.nan(SRL_L4), NA, SRL_L4)
SRL_L4<-ifelse(is.infinite(SRL_L4), NA, SRL_L4)
SRL_All<-ifelse(is.nan(SRL_All), NA, SRL_All)
SRL_All<-ifelse(is.infinite(SRL_All), NA, SRL_All)

# adding indices
d2<-cbind(d,SRL_L1,SRL_L2,SRL_L3,SRL_L4,SRL_All)  
```  
  
# Analysis
```R  
# extracting from datasets  
d3<-subset(d2,d2$Entry!="SasanishkiSub")
d3<-subset(d3,d3$Entry!="HaenukiSub")

# ANOVA  
result<-aov(SDW~Entry,d3) #one-way ANOVA  
summary(result)

# Multiple comparison: Tukey  
result<-aov(SDW~Entry,d3)
TukeyHSD(result)  

install.packages("multcomp", dependencies = T)
library(multcomp)
d3$Entry<-as.factor(d3$Entry)
result<-aov(SDW~Entry,d3)
TukeyHSD(result)  
Tukey<-glht(result, linfct=mcp(Entry="Tukey"))  
summary(Tukey)  
cld(Tukey, level = 0.05, decreasing = TRUE) # adding alphabet
  
# Correlation  
cor(d3$SDW, d3$TRLD)  
allcor<-cor(na.omit(d3[,3:63]))  
  
# Regression  
result<-lm(SDW~TRLD, d3)  
summary(result)  
```  
  
# All analyses
```R  
AllAnova<-c()
CN<-colnames(d3)
for (i in 3:63) {
    result<-aov(d3[,i]~Entry,d3)  
    one<-c(CN[i],summary(result)[[1]][[5]][[1]])  
    AllAnova<-rbind(AllAnova,one)
}
AllAnova<-data.frame(Index=AllAnova[,1], Pvalue=as.numeric(AllAnova[,2]))  
Asterisk<-ifelse(AllAnova$Pvalue<0.05,"*","ns")
AllAnova<-cbind(AllAnova,Asterisk)
```  
  
# Mean (Average) and Standard error
```R  
install.packages("dplyr", dependencies = T)  
library(dplyr)  
dMean <-d3 %>%
    group_by(Entry) %>%
    summarise(across(where(is.numeric), mean, na.rm=TRUE))
dMean<-data.frame(dMean)
  
dSD <-d3 %>%
    group_by(Entry) %>%
    summarise(across(where(is.numeric), sd, na.rm=TRUE))
dSD<-data.frame(dSD)
dSE<-cbind(dSD[,1], dSD[,2:62]/sqrt(4))

write.table(dMean, file="dMean.csv", sep=",", row.names=F)
write.table(dSE, file="dSE.csv", sep=",", row.names=F)
```  
  
# Checking data with figures
```R  
#Correlation
plot(d3[,3:7])
plot(d3$SDW, d3$TRL)  
  
#Boxplot
boxplot(SDW~Entry,d3)  
```  