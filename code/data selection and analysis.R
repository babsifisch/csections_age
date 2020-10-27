#################
#Analysis


#Read in data, load R packages. Variable names and levels are containted in document "variablenliste_Safe Center_fischer.xlsx"
#in folder summary_data.

setwd("M://Barbara_Fischer//2019//Datensatz")
geburtsdaten=read.csv('geburten_familie_fischer.csv',header=T, sep=';')

library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(effects)
library(pscl)
library(dplyr)
library(tidyr)

geb=tbl_df(geburtsdaten)

#package MASS also has a function select, which overwrites dplyr function select.
select <- dplyr::select

#define those columns as factors that do not contain numerical values
factorcols<-c("C_WO_GKZ_GEBSTAND_AKT_0","C_GESCHL_K_0","C_LEBTOT_0","C_MEHRL_GESCHL","C_LEGITIM_0","C_STAATSANG_V_P_0","C_STAATSANG_M_P_0","C_STAATSANG_K_P_0","C_FAMSTAND_M_0","C_ERWERB_V_ALT_0","C_ERWERB_M_ALT_0","C_BERUFST_V_ALT_0","C_BERUFST_M_ALT_0","C_BILDUNG_V_ALT_0","C_BILDUNG_M_ALT_0","C_GO_STAAT_V_0","C_GO_STAAT_M_0","C_GO_STAAT_K_0","C_GEBEND_0","C_GEBURTSORTTYP_0","C_AUFENTHALT_0","J_SCHWDAUERW_vollendet","C_SCHWDAUERT_0","C_APG01_0","C_APG05_0","C_APG10_0","PaarID","J_GO_BDL")

geb[factorcols]<-lapply(geb[factorcols],as.factor)

geb_oldvars<- geb %>% rename(GEBJAHR=C_GEBJAHR_K_0,GEBMONAT=C_GEBMONAT_K_0,ALTM=C_ALTER_M_0,GESCHLECHT=C_GESCHL_K_0,LEBEND_TOT=C_LEBTOT_0,MEHRL=C_MEHRL_GESCHL_0,LEGITIMITAET=C_LEGITIM_0,STAATTV=C_STAATSANG_V_P_0,STAATTM=C_STAATSANG_M_P_0,STAATTK=C_STAATSANG_K_P_0,FAMST=C_FAMSTAND_M_0,BILDV=C_BILDUNG_V_ALT_0,BILDM=C_BILDUNG_M_ALT_0,ENTBINDUNGSART=C_GEBEND_0,ENTBINDUNGSORT=C_GEBURTSORTTYP_0,AUFENTHALT=C_AUFENTHALT_0,SCHWDAUER_W=J_SCHWDAUERW_vollendet,SCHWDAUER_T=C_SCHWDAUERT_0,erbdl=J_GO_BDL,KLINIKTYP=J_KLINIKTYP,EINLEITUNG=C_GEBEINL_0,GEBLAGE=C_GEBLAGE_0,GEBGEWICHT=J_gewicht_k,GEBLAENGE=P_LAENGE_K,GEBURTENFOLGE_INS=C_GEBF_GES_0,GEBURTENFOLGE_LEB=C_GEBF_LEB_0,RAUCHERIN=C_RAUCHERIN_0,GROESSEM=C_GROESSE_M_0,GEWICHTM_B=C_GEWBEG_M_0,GEWICHTM_E=C_GEWEND_M_0
)

#choose only years >=1995 as only these have mode of births. only life births
births1_old<-geb_oldvars %>% filter(GEBJAHR>=1995) %>%  filter(LEBEND_TOT==1) %>%  select(GEBJAHR,BILDM,ALTM,PaarID,ENTBINDUNGSART,ENTBINDUNGSORT, GEBURTENFOLGE_LEB,GEBURTENFOLGE_INS,MEHRL,SCHWDAUER_W,SCHWDAUER_T,KLINIKTYP,EINLEITUNG,GEBLAGE,GEBGEWICHT,GEBLAENGE,GEWICHTM_B,GEWICHTM_E,GROESSEM,RAUCHERIN)


############################
#life births, still births, multiples;


#number of life births vs. still born infants. Very few still births
births_life_dead_total<-summary(geb_oldvars$LEBEND_TOT)
write.csv(births_life_dead_total,"births_life_dead_total.csv")

#total number of life births per year
birthsperyear <- births1_old %>% group_by(GEBJAHR) %>% summarise(count=n())
write.csv(birthsperyear,"birthsperyear.csv")

#percentage of multiples
multiples <- geb_oldvars %>% filter(GEBJAHR>=1995) %>% group_by(GEBJAHR) %>% summarise(single=sum(MEHRL==1 | MEHRL==10),count=n()) %>% mutate(multiples=(count-single)/count)
write.csv(multiples,"multiples.csv")

#total number of cases, multiples
multiples_cases=summary(as.factor(geb_oldvars$MEHRL))
write.csv(multiples_cases,"multiples_cases.csv")
percentagemultiples <- geb_oldvars %>% filter(GEBJAHR>=1995)  %>% summarise(single=sum(MEHRL==1 | MEHRL==10),count=n()) %>% mutate(multiples=(count-single)/count)

#merge birth mode: new: 2 levels. 0=vaginal, 1=CS
V<-as.numeric(births1_old$ENTBINDUNGSART)
V<-replace(V,V==1 | V==5 | V==6 | V==7,0)
V<-replace(V,V==2| V==3 | V==4 ,1)
births1_old$ENTBINDUNGSART<-as.factor(V)
remove('V')
summary(births1_old)
totalno_CS_spontaneous<-births1_old %>% group_by(ENTBINDUNGSART)%>% summarise(count=n())
write.csv(totalno_CS_spontaneous,"totalno_CS_spontaneous.csv")

#drop the cases where birth mode is not indicated. 
births1<- births1_old %>% filter(ENTBINDUNGSART==1 | ENTBINDUNGSART==0 )
summary(births1)


###############################
#merge education categories: previously: 6 levels. new: 4 levels
remove('V')
V<-as.numeric(births1$BILDM)
V<-replace(V,V==3,2)
V<-replace(V,V==4,3)
V<-replace(V,V==6 | V==5,4)
births1$BILDM=as.factor(V)
remove('V')
# V<-as.numeric(births1$BILDV)
# V<-replace(V,V==3,2)
# V<-replace(V,V==4,3)
# V<-replace(V,V==6 | V==5,4)
# remove('V')
summary(births1)



###################################
#parity 


#calculate average parity per year and relative frequency of firstborn children per year. Goes up over time! 
#check this table for plausibility: should be more firstborns each year than secondborns, etc.
averageparity<- births1 %>% group_by(GEBJAHR) %>% summarise(count=n(),avgparity=mean(GEBURTENFOLGE_LEB),stdparity=sqrt(var(GEBURTENFOLGE_LEB)),relfreqfirstborn=sum(GEBURTENFOLGE_LEB==1)/(length(GEBURTENFOLGE_LEB)),relfreqsecondborn=sum(GEBURTENFOLGE_LEB==2)/(length(GEBURTENFOLGE_LEB)),relfreqthirdborn=sum(GEBURTENFOLGE_LEB==3)/(length(GEBURTENFOLGE_LEB)),relfreqfourthbornorhigher=sum(GEBURTENFOLGE_LEB>=4)/(length(GEBURTENFOLGE_LEB)))
write.csv(averageparity,"averageparity.csv")

#Calculate parity distributions, total births per year, total no of CS per year
totalbirthsperyear<-births1 %>% group_by(GEBJAHR) %>% summarise(totalbirths=n())
write.csv(totalbirthsperyear,"totalbirthsperyear.csv")
distriparity<-births1 %>% group_by(GEBJAHR,GEBURTENFOLGE_LEB) %>% summarise(totalperparity=n()) %>% left_join(totalbirthsperyear,by="GEBJAHR") %>% mutate(relfreq=totalperparity/totalbirths)
write.csv(distriparity,"distriparity.csv")
distriparity_1<-distriparity %>% ungroup %>% mutate(GEBJAHR=as.factor(GEBJAHR))%>% filter(GEBJAHR==1995 | GEBJAHR==2016)

#plot parity distribution in 1995 vs. 2016
ggplot(distriparity_1,aes(GEBURTENFOLGE_LEB,relfreq))+
  geom_line(aes(colour=GEBJAHR),show.legend = FALSE)+
  xlab("parity")+
  ylab("relative frequency")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("grey44", "gray70"))
ggsave("paritydistributions.pdf")
write.csv(distriparity,"distriparity.csv")

#plotting std onto mean for average parity over time 
p2 <- ggplot(averageparity, aes(GEBJAHR,avgparity))
p2 + 
geom_ribbon(aes(ymin=avgparity-stdparity,ymax=avgparity+stdparity), fill = "grey12",alpha=0.15) +
geom_line(aes(y = avgparity))+ 
xlab("year")+
ylab("parity")+
theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))
ggsave("averageparity.pdf")


####################################
#overall CS rate, maternal age

#overall CS rate over time
CSrate_allbirths<-births1 %>% group_by(GEBJAHR) %>% summarise(countCS=sum(ENTBINDUNGSART==1), countspontanous=sum(ENTBINDUNGSART==0), counttotal=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART)))
CSrate_firstbirths<-births1 %>% filter(GEBURTENFOLGE_LEB==1) %>% group_by(GEBJAHR) %>% summarise(count=n(), sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART)))
write.csv(CSrate_allbirths,"CSrate_allbirths.csv")
write.csv(CSrate_firstbirths,"CSrate_firstbirths.csv")

#line plot, overall CS rate over time
ggplot(CSrate_allbirths,aes(GEBJAHR,sectiorate))+geom_line()+
  xlab("year")+
  ylab("CS rate")+
  theme( axis.text = element_text(colour = "black",size = rel(1.5)), axis.title =element_text(colour = "black",size = rel(1.5)))
axis.title = element_text(colour = "black",size = rel(1.5))
ggsave("totalCSrateovertime.pdf")

#line plot, total no of CS and total no of spontaneous births and total births per year
ggplot(CSrate_allbirths)+geom_line(aes(GEBJAHR,countspontanous))+geom_line(aes(GEBJAHR,countCS))+ geom_line(aes(GEBJAHR,counttotal))+
   xlab("year")+
   ylab("CS rate")+
   theme( axis.text = element_text(colour = "black",size = rel(1.5)), axis.title =element_text(colour = "black",size = rel(1.5)))
 axis.title = element_text(colour = "black",size = rel(1.5))+
 ggsave("totalnumbercSbirthsovertime.pdf")

 #line plot, CS rate of first births over time
ggplot(CSrate_firstbirths,aes(GEBJAHR,sectiorate))+geom_line()+
  xlab("year")+
  ylab("CS rate")+
  theme( axis.text = element_text(colour = "black",size = rel(1.5)), axis.title =element_text(colour = "black",size = rel(1.5)))
axis.title = element_text(colour = "black",size = rel(1.5))
ggsave("firstbirthsCSrateovertime.pdf")

#maternal age at birth over time
maternalageatbirth<-births1 %>% group_by(GEBJAHR) %>% summarise(count=n(),avgage=mean(ALTM),stdage=sqrt(var((ALTM))))
#maternal age at first birth over time
maternalageatfirstbirth<-births1 %>% filter(GEBURTENFOLGE_LEB==1) %>% group_by(GEBJAHR) %>% summarise(count=n(),avgage=mean(ALTM),stdage=sqrt(var((ALTM))))
write.csv(maternalageatbirth,"maternalageatbirth.csv")
write.csv(maternalageatfirstbirth,"maternalageatfirstbirth.csv")

#plotting std onto means for maternal age at first birth over time, ribbon plot
p1 <- ggplot(maternalageatfirstbirth, aes(GEBJAHR,avgage))
p1 + 
  geom_ribbon(aes(ymin=avgage-stdage,ymax=avgage+stdage), fill = "grey12",alpha=0.15) +
  geom_line(aes(y = avgage))+ 
  xlab("year")+
  ylab("maternal age")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))
ggsave("maternalageatfirstbirth.pdf")

#maternal age and CS rate for all births
CSformaternalage_allbirths<- births1 %>% group_by(ALTM) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART)))
write.csv(CSformaternalage_allbirths,"CSformaternalage_allbirths.csv")

#bubble plot: maternal age and CS rate for all births together, entire study period
CSformaternalage_allbirths %>% filter(count>=100) %>% ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(size=count),show.legend = FALSE) +
  xlab("maternal age")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))
ggsave("CSformaternalage_allbirths.pdf")


###########################
#new variable:previous birth mode: prevbirthoutcome

#create new variable prevbirthoutcome
births_paar<-births1 %>% group_by(PaarID) %>% mutate(prevbirthoutcome=prevbirthoutcome1(GEBURTENFOLGE_INS,ENTBINDUNGSART))
summary(births_paar)

#pairIDs are missing in many of the more recent years. all firstborns get assigned prevbirthoutcome==3
#3 == first birth
#0 == prev birth was vaginal
#1 == prev birth was Caesarean
#NA == no information about prev birth

#birthspaar_all includes all births, so it has the same entries as birth1, plus prevbirthoutcome where it is available
birthspaar_all=ungroup(births_paar)
for (i in 1:length(birthspaar_all$GEBJAHR))
{	
	if (birthspaar_all$GEBURTENFOLGE_INS[i]==1)
	{birthspaar_all$prevbirthoutcome[i]<-3;
	#print(births1$prevbirthoutcome[i]);
	}
}
birthspaar_all$prevbirthoutcome=as.factor(birthspaar_all$prevbirthoutcome)
summary(birthspaar_all)

#save the current workspace
save.image()


###########################
#different data sets to be used: births 1-4 and percentages in each subset


#births1 and birthpaar_all each have all cases after 1995
nrow(births1)
nrow(births1_old)
nrow(birthspaar_all)

#geb are all cases, including those before 1995 for which birth mode is not available
nrow(geb)

#births2 are the cases where we have the education of the mother
births2<-filter(birthspaar_all,BILDM==1 | BILDM==2 | BILDM==3 | BILDM==4 )

#births3 are all the births where we have education and prevbirthoutcome
births3<-filter(births2,prevbirthoutcome==0 | prevbirthoutcome==3 | prevbirthoutcome==1)

#births4 are all births where we have prevbirthoutcome
births4<-filter(birthspaar_all,prevbirthoutcome==0 | prevbirthoutcome==3 | prevbirthoutcome==1)

#percentage of life births where education is indicated
percentage_births2=nrow(births2)/nrow(birthspaar_all)*100
#percentage of life births where education and prevbirthoutcome is indicated
percentage_births3<-nrow(births3)/nrow(births1)*100
#percentage of life births where prevbirthoutcome is indicated
percentage_births4<-nrow(births4)/nrow(births1)*100
save(percentage_births2,percentage_births3,percentage_births4,file="percentagesdata.RData")


#########################

#maternal age and CS rate, for first births
CSformaternalage_firstborns<- births1 %>% filter(GEBURTENFOLGE_LEB==1) %>% group_by(ALTM) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) 
write.csv(CSformaternalage_firstborns,"CSformaternalage_firstborns.csv")

#bubble plot: maternal age and CS rate for firstborns
CSformaternalage_firstborns %>% filter(count>=100) %>% ggplot(aes(ALTM,sectiorate)) +
geom_point(aes(size=count),show.legend = FALSE) +
xlab("maternal age")+
ylab("CS rate")+
theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))
ggsave("CSformaternalage_firstborns.pdf")

#maternal age and CS rate, for the 3 levels of prevbirthoutcome 
CSformaternalage_prevbirthoutcome<- births4 %>% group_by(ALTM,prevbirthoutcome) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART)))
write.csv(CSformaternalage_prevbirthoutcome,"CSformaternalage_prevbirthoutcome.csv")

#bubble plot: maternal age and CS rate for 3 classes of prevbirthoutcome
CSformaternalage_prevbirthoutcome %>% filter(count>=100) %>% ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(colour=prevbirthoutcome,size=count),show.legend = FALSE) +
  xlab("maternal age")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("darkseagreen", "tan4","gray4"))
ggsave("CSformaternalage_prevbirthoutcome.pdf")

#prevbirthoutcome and CS rate
CS_prevbirthoutcome<- births4 %>% group_by(GEBJAHR,prevbirthoutcome) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) 
write.csv(CS_prevbirthoutcome,"CS_prevbirthoutcome.csv")

#line plot: CS rate over time, for 3 classes of prevbirthoutcome
CS_prevbirthoutcome %>% filter(count>=100) %>%
  ggplot(aes(GEBJAHR,sectiorate)) +
  geom_line(aes(colour=prevbirthoutcome),show.legend = FALSE) +
  xlab("year")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("darkseagreen", "tan4","gray49"))
ggsave("CSformaternalage_prevbirthoutcome.pdf")

#CS rates for women at different age
filter(CSformaternalage_prevbirthoutcome,ALTM==20)
filter(CSformaternalage_prevbirthoutcome,ALTM==30)
filter(CSformaternalage_prevbirthoutcome,ALTM==40)

########################
#EDUCATION 
#use births2 for everything concerning education.

#frequencies of education categories, over time
birth2_BILDMfreq<-births2 %>% group_by(GEBJAHR) %>% summarise(count=n(),BILDM1freq=sum(BILDM==1)/length(BILDM),BILDM2freq=sum(BILDM==2)/length(BILDM),BILDM3freq=sum(BILDM==3)/length(BILDM),BILDM4freq=sum(BILDM==4)/length(BILDM))
write.csv(birth2_BILDMfreq,"birth2_BILDMfreq.csv")

#line plot: freq of education categories
ggplot(birth2_BILDMfreq)+geom_line(aes(GEBJAHR,BILDM1freq))+geom_line(aes(GEBJAHR,BILDM2freq))+geom_line(aes(GEBJAHR,BILDM3freq))+geom_line(aes(GEBJAHR,BILDM4freq))+
  xlab("year")+
  ylab("frequency")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))
ggsave("BILDMovertime.pdf")


CSformaternalage_education<- births2 %>% ungroup %>% group_by(ALTM,BILDM) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART)))
write.csv(CSformaternalage_education,"CSformaternalage_education.csv")

#bubble plot: maternal age and CS rate for education levels 1 and 4, all births
CSformaternalage_education %>% filter(BILDM==1|BILDM==4) %>% filter(count>100) %>% 
  ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(colour=BILDM,size=count),show.legend = FALSE) +
  xlab("maternal age")+
  ylab("CS rate")+
  xlim(14,46)+
  ylim(0.12,0.62)+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("darkorange", "red4"))
ggsave("CSformaternalage_education_allbirths.pdf")

CSformaternalage_education_firstborns<- births2 %>% ungroup %>% filter(GEBURTENFOLGE_LEB==1) %>% group_by(ALTM,BILDM) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART)))
write.csv(CSformaternalage_education_firstborns,"CSformaternalage_education_firstborns.csv")

#bubble plot: maternal age and CS rate for education levels, first births
CSformaternalage_education_firstborns %>% filter(BILDM==1|BILDM==4) %>% filter(count>100) %>% 
  ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(colour=BILDM,size=count),show.legend = FALSE) +
  xlab("maternal age")+
  ylab("CS rate")+
  xlim(14,46)+
  ylim(0.12,0.62)+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("darkorange", "red4"))
ggsave("CSformaternalage_education_firstbirths.pdf")

#############
#CLINIC TYPE

#variable levels: 1 for public, 2 for private and 3 for congregational. merge factor levels. make it 1 for public and congregational, 2 for private. 


#select clinic type, use only cases where location is indicated.
geb_oldvars %>% filter(GEBJAHR>=1995) %>% nrow()
births6<-births1 %>% filter(ENTBINDUNGSORT==1  |ENTBINDUNGSORT==2  | ENTBINDUNGSORT==3 | ENTBINDUNGSORT==4| ENTBINDUNGSORT==5) %>%  select(GEBJAHR,BILDM,ALTM,PaarID,ENTBINDUNGSART,GEBURTENFOLGE_LEB,GEBURTENFOLGE_INS,ENTBINDUNGSORT,KLINIKTYP)
nrow(births6)

#births6 are all cases where location of birth is indicated. 
#percentage of all births that were in a hospital, at home, 
nrow(filter(births6,ENTBINDUNGSORT==1))/nrow(births6)

#merge factor levels. Treat congregational hospital as public. 1 is public, 2 is private.
remove('V')
V<-as.numeric(births6$KLINIKTYP)
V<-replace(V,V==3,1)
births6$KLINIKTYP=as.factor(V)
summary(births6)

totalnoclinictype<-births6 %>% filter(KLINIKTYP==1 | KLINIKTYP==2) %>% group_by(KLINIKTYP) %>% summarise(count=n())
write.csv(totalnoclinictype,"totalnoclinictype.csv")
#C-section rate of firstborns, for hospital categories over time
CS_hospital_firstborns_year <- births6 %>% filter(KLINIKTYP==1 | KLINIKTYP==2,GEBURTENFOLGE_LEB==1) %>% group_by(GEBJAHR,KLINIKTYP) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) 
write.csv(CS_hospital_firstborns_year,"CS_hospital_firstborns_year.csv")

#line plot: C-section rate of firstborns, for hospital categories over time
CS_hospital_firstborns_year %>% filter(count>100) %>%
  ggplot(aes(GEBJAHR,sectiorate))+
  geom_line(aes(colour=KLINIKTYP)) +
  xlab("year")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("navyblue", "darkslategray3"))
ggsave("CS_hospital_firstborns_year.pdf")

#C-section rate of firstborns, for maternal age classes
CS_hospital_firstborns_age <- births6 %>% filter(ENTBINDUNGSORT==1, GEBURTENFOLGE_LEB==1,KLINIKTYP==1 | KLINIKTYP==2) %>% group_by(ALTM,KLINIKTYP) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) 
write.csv(CS_hospital_firstborns_age,"CS_hospital_firstborns_age.csv")

#bubble plot: C-section rate of firstborns, for maternal age classes
CS_hospital_firstborns_age %>% filter(count>100) %>%
  ggplot(aes(ALTM,sectiorate))+
  geom_point(aes(colour=KLINIKTYP,size=count,show.legend = TRUE)) +
  xlab("maternal age")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("navyblue", "darkslategray3"))
ggsave("CS_hospital_firstborns_age.pdf")

#Maternal age at first birth, for hospital classes
maternalage_hospital_firstborns_year <- births6 %>% filter(ENTBINDUNGSORT==1, GEBURTENFOLGE_LEB==1,KLINIKTYP==1 | KLINIKTYP==2) %>% group_by(GEBJAHR,KLINIKTYP) %>% summarise(count=n(),meanage=mean(ALTM),stdage=sqrt(var(ALTM))) 
write.csv(maternalage_hospital_firstborns_year,"maternalage_hospital_firstborns_year.csv")

#Overall differences in maternal age at first birth between public and private hospitals
maternalage_hospital_firstborns<-births6 %>% filter(ENTBINDUNGSORT==1, GEBURTENFOLGE_LEB==1,KLINIKTYP==1 | KLINIKTYP==2)  %>% group_by(KLINIKTYP) %>% summarise(count=n(),meanage=mean(ALTM),stdage=sqrt(var(ALTM))) 
write.csv(maternalage_hospital_firstborns,"maternalage_hospital_firstborns.csv")

#Line plot, maternal age at first birth, for hospital classes
maternalage_hospital_firstborns_year %>% filter(count>100) %>%
  ggplot(aes(GEBJAHR,meanage))+
  geom_line(aes(colour=KLINIKTYP)) +
  xlab("year")+
  ylab("age at first birth")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("navyblue", "darkslategray3"))
ggsave("maternalage_hospital_firstborns_year.pdf")

#CS rate according to birth place
CS_birthplace<-births6  %>%  group_by(KLINIKTYP) %>% summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) %>% filter(count>=100)
write.csv(CS_birthplace,"CS_birthplace.csv")

#percentage of academic mothers 
academicpercentage_clinic<-births6 %>% filter(ENTBINDUNGSORT==1, BILDM==1  | BILDM==2  | BILDM==3  | BILDM==4)  %>%  group_by(KLINIKTYP) %>% summarise(academicfreq=sum(BILDM==4)/length(BILDM),count=n())
write.csv(academicpercentage_clinic,"academicpercentage_clinic.csv")

acadpercentage_overall<-births1 %>% filter(BILDM==1  | BILDM==2  | BILDM==3  | BILDM==4)  %>% summarise(academicfreq=sum(BILDM==4)/length(BILDM),count=n())

##################
#MATERNAL AGE DISTRIBUTIONS; 1995 and 2016 

agecorrectedrates %>% filter(GEBJAHR==1995  | GEBJAHR==2016) %>% 
  ggplot(aes(ALTM,GEBJAHRdistri))+
  geom_line(aes(colour=GEBJAHR),show.legend = FALSE)+
  xlab("maternal age")+
  ylab("relative frequency")+
  xlim(10,50)+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
  scale_color_manual(values=c("grey44", "gray70"))
ggsave("maternalagedistributions.pdf")


