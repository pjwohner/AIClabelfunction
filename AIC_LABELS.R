library(ggplot2)
library(AICcmodavg)
library(lme4)
rm(list = ls())
setwd("c:/Laymon_YBCU_2020/KRV_SPOTMAPPING_YBCU_and_SGCN")

#########Load data for all analyses###########################################
laymon <- read.table("no0to3_20ha_pha.txt", header=TRUE, 
                     sep="", na.strings="NA", dec=".", strip.white=TRUE)
attach(laymon)

##FUNCTION for labeling and collecting models
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
lrNames<-wohnerAIC_labels("Mod", 7)

####  Analysis
response<-SOSP20
lrModels<-list(
pmod01<-lmer(response~ X0to1+(1|site),REML = FALSE),
pmod02<-lmer(response~ X1to2+(1|site),REML = FALSE),
pmod03<-lmer(response~ X2to3+(1|site),REML = FALSE),
pmod04<-lmer(response~ X3to4+(1|site),REML = FALSE),
pmod05<-lmer(response~ X4to5+(1|site),REML = FALSE),
pmod06<-lmer(response~ X5to6+(1|site),REML = FALSE),
pmod07<-lmer(response~ X6to7+(1|site),REML = FALSE),
pnull<-lmer(response~ 1+(1|site),REML = FALSE)
)

##model selection table based on AICc
aictab(cand.set = lrModels, modnames = lrNames, sort = TRUE, c.hat=1)

modavg(cand.set=lrModels, parm="X6to7", modnames=lrNames, conf.level = 0.85,
       second.ord = TRUE, nobs = NULL, exclude = list("type:propwill"), 
       warn = TRUE,uncond.se = "revised")

confint(pmod07,level = 0.85)
