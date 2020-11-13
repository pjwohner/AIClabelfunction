setwd("c:/Laymon_YBCU_2020/KRV_SPOTMAPPING_YBCU_and_SGCN")
#########Load data for all analyses###########################################
laymon <- read.table("no0to3_20ha_pha.txt", header=TRUE, 
                     sep="", na.strings="NA", dec=".", strip.white=TRUE)
attach(laymon)
names(laymon)

library(ggplot2)
library("nnet")
library(AICcmodavg)
library(lme4)
library(fabricatr)
##song sparrow and common yellowthroat we use normal distribution
##check all variables first to see what is best

wohnerAIC_labels<-function(model_name, n){
llist <- ""
  for(i in 1:n)
      {
    
            if (i <10) {
            llist[i] <- paste(model_name,"0",as.character(i),sep = "") 
            }
            else
            {
            llist[i] <- paste(model_name, as.character(i),sep = "")
              
            }
    
      }
#llist[n+1] <= "pnull"
return(c(llist,"PNull"))
}

lrNames<-wohnerAIC_labels("Mod",40)
lrNames

response<-SOSP20
pmod01<-lmer(response~ X0to1+(1|site))
pmod02<-lmer(response~ X1to2+(1|site))
pmod03<-lmer(response~ X2to3+(1|site))
pmod04<-lmer(response~ X3to4+(1|site))
pmod05<-lmer(response~ X4to5+(1|site))
pmod06<-lmer(response~ X5to6+(1|site))
pmod07<-lmer(response~ X6to7+(1|site))
pmod08<-lmer(response~ X7to8+(1|site))
pmod09<-lmer(response~ X8to9+(1|site))
pmod10<-lmer(response~ X9to10+(1|site))
pmod11<-lmer(response~ X10to11+(1|site))
pmod12<-lmer(response~ X11to12+(1|site))
pmod13<-lmer(response~ X12to13+(1|site))
pmod14<-lmer(response~ X13to14+(1|site))
pmod15<-lmer(response~ X14to15+(1|site))
pmod16<-lmer(response~ grassper+(1|site))
pmod17<-lmer(response~ ccper+(1|site))
pmod18<-lmer(response~ propwill*type+(1|site))
pmod19<-lmer(response~ treespha+(1|site))
pmod20<-lmer(response~ treessd+(1|site))
pmod21<-lmer(response~ wilpha+(1|site))
pmod22<-lmer(response~ wilsd+(1|site))
pmod23<-lmer(response~ propwill+(1|site))
pmod24<-lmer(response~ ht+(1|site))
pmod25<-lmer(response~ poly(ht,2)+(1|site))
pmod26<-lmer(response~ type*ht+(1|site))
pmod27<-lmer(response~ bam2pha+(1|site))
pmod28<-lmer(response~ dbh+(1|site))
pmod29<-lmer(response~ dbhsd+(1|site))
pmod30<-lmer(response~ PClow+(1|site))
pmod31<-lmer(response~ PChigh+(1|site))
pmod32<-lmer(response~ PCmid+(1|site))
pmod33<-lmer(response~ poly(age,2)+(1|site))
pmod34<-lmer(response~ age+(1|site))
pnull<-lmer(response~ 1+(1|site))
#Create list of all the models
lrModels<- (aic("pmod",34))
lrModels<- (list(pmod01, pmod02, pmod03, pmod04, pmod05,pmod06,pmod07,
                 pmod08, pmod09,pmod10, pmod11,pmod12,pmod13, pmod14,pmod15,pmod16,pmod17, pmod18,
                 pmod19, pmod20,pmod21,pmod22,pmod23, pmod24,pmod25,pmod26,pmod27, pmod28,
                 pmod29, pmod30,pmod31, pmod32,pmod33, pmod34,pnull))
lrNames <- c("Mod01", "Mod02", "Mod03", "Mod04", "Mod05","Mod06","Mod07","Mod08", "Mod09", 
             "Mod10","Mod11","Mod12", "Mod13", "Mod14","Mod15","Mod16","Mod17", "Mod18",
             "Mod19","Mod20","Mod21","Mod22", "Mod23", "Mod24","Mod25","Mod26","Mod27", "Mod28",
             "Mod29","Mod30","Mod31", "Mod32","Mod33","Mod34","Null")

##model selection table based on AICc
aictab(cand.set = lrModels, modnames = lrNames)
aicWt<-aictab(cand.set=lrModels, modnames=lrNames, sort=TRUE, c.hat=1)
aicWt