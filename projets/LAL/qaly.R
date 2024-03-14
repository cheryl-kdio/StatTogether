rm(list=ls())

require(survival)
require(gdata)
require(stringr)
require(xtable)

#Read Data 
library(ggplot2)
library(scales)
require(gridExtra)

library(ggplot2)
library(scales)
library(tableone)
library(ggplot2)
library(dplyr)
library(gtsummary)


#setwd("/Users/schevret/documents/enseignement/ensai/2023-2024/sujet qaly")
qaly <- read.csv2("projets/LAL/graaphR_ensai.csv", encoding = "latin1", na.strings = c("", " ", "NA", "NI"))
names(qaly)
dim(qaly)

qaly$death.dt <- as.Date(as.character(qaly$deathdt),format="%Y-%m-%d")
qaly$rando.dt <- as.Date(as.character(qaly$randodt),format="%Y-%m-%d")
qaly$max.dt <- as.Date(as.character(qaly$Datemax),format="%Y-%m-%d")
qaly$rec.dt <- as.Date(as.character(qaly$relapsdt),format="%Y-%m-%d")

qaly$finctdt   <- apply(qaly[,c("C1dt","C2dt" , "C3dt" , "C4dt", "Inter1dt" )],1,max,na.rm=T)
qaly$finct.dt <- as.Date(as.character(qaly$finctdt),format="%Y-%m-%d")
qaly$arr.dt <- as.Date(as.character(qaly$arrpremadt),format="%Y-%m-%d")
qaly$bmt.dt <- as.Date(as.character(qaly$bmtdt),format="%Y-%m-%d")

    

qaly$suivi  <- as.numeric(qaly$max.dt-qaly$rando.dt)
qaly$survie <- as.numeric(qaly$death.dt-qaly$rando.dt)
qaly$delfinct <- as.numeric(qaly$finct.dt-qaly$rando.dt)
qaly$delarr <- as.numeric(qaly$arr.dt-qaly$rando.dt)
qaly$delfinct[which(qaly$delfinct==0)]<- qaly$delarr[which(qaly$delfinct==0)]
qaly$finct<- rep(1,length(qaly$delfinct))
qaly$delrec<- as.numeric(qaly$rec.dt-qaly$rando.dt)
qaly$delpfs<- qaly$suivi
qaly$delpfs[!is.na(qaly$delrec)]<-qaly$delrec[!is.na(qaly$delrec)]
qaly$delbmt <- as.numeric(qaly$bmt.dt-qaly$rando.dt)

qaly$delfinct2 <- qaly$delfinct
qaly$delfinct2[!is.na(qaly$delbmt)]<- pmax(qaly$delfinct[!is.na(qaly$delbmt)],qaly$delbmt[!is.na(qaly$delbmt)])
qaly$finct2 <- qaly$finct
qaly$finct2[!is.na(qaly$delbmt)]<-1

###

table1=
  qaly %>%
  select(R1, suivi, survie, delpfs, dc, pfs ) %>%
  
  tbl_summary(by=R1,
              type = list(suivi ~ "continuous")
  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(pvalue_fun = function(p) sprintf("%.3f", p))
table1


require(survival)

ct  <- survfit(Surv(delfinct,finct)~1,data=qaly)
ct2  <- survfit(Surv(delfinct2,finct2)~1,data=qaly)
sv  <- survfit(Surv(suivi,dc)~1,data=qaly)
efs <- survfit(Surv(delpfs,pfs)~1, data=qaly)

require(survminer)
s1<- ggsurvplot(sv, data = qaly,
                conf.int = F,        # Add the 95% confidence band
                risk.table = TRUE,      # Add risk table
                tables.height = 0.2,
                legend.labs=c("OS"),
                tables.theme = theme_cleantable(),
                palette = "#619CFF",
                ggtheme =  theme_classic())

efs1<- ggsurvplot(efs, data = qaly,
                  conf.int = F,        # Add the 95% confidence band
                  risk.table = TRUE,      # Add risk table
                  tables.height = 0.2,
                  legend.labs=c("EFS"),
                  tables.theme = theme_cleantable(),
                  palette="#00BA38",
                  ggtheme = theme_classic())

ct1<- ggsurvplot(ct, data = qaly,
                 conf.int = F,           # Add the 95% confidence band
                 risk.table = TRUE,      # Add risk table
                 tables.height = 0.2,
                 legend.labs=c("CT"),
                 tables.theme = theme_cleantable(),
                 palette="#F8766D",
                 ggtheme = theme_classic())

ct.2<- ggsurvplot(ct2, data = qaly,
                 conf.int = F,           # Add the 95% confidence band
                 risk.table = TRUE,      # Add risk table
                 tables.height = 0.2,
                 legend.labs=c("CT2"),
                 tables.theme = theme_cleantable(),
                 palette="#E69F00",
                 ggtheme = theme_classic())

efs1$table <- efs1$table +
  theme(plot.title = element_blank())

plot.all <- s1$plot + 
  geom_step(data = ggplot_build(efs1$plot)$data[[1]], aes(x, y, color="EFS")) + 
  geom_point(data = ggplot_build(efs1$plot)$data[[3]], aes(x, y, color="EFS"), shape=3)+ 
  geom_step(data = ggplot_build(ct1$plot)$data[[1]], aes(x, y, color="CT"))+ 
  geom_step(data = ggplot_build(ct.2$plot)$data[[1]], aes(x, y, color="CT2"))+ 
  scale_color_manual(values=c("#00BA38", "#619CFF", "#F8766D", "#E69F00"))

plot.all
