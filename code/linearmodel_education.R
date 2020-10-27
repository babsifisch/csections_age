#Read in data, load R packages
setwd("~/Work/Kaiserschnittprojekt/Neue Analyse/Ergebnisse_2/")
library(patchwork)
library(svglite)
library(dplyr)


CSformaternalage_education_firstborns =read.csv('CSformaternalage_education_firstborns.csv',header=T, sep=',')
CSformaternalage_education_firstborns<-CSformaternalage_education_firstborns %>% mutate(BILDM=as.factor(BILDM))
CSformaternalage_education =read.csv('CSformaternalage_education.csv',header=T, sep=',')
CSformaternalage_education<-CSformaternalage_education %>% mutate(BILDM=as.factor(BILDM))

educationset1=CSformaternalage_education_firstborns %>% filter(BILDM==1|BILDM==4) %>% filter(count>100) 
  m1=lm(sectiorate ~ ALTM+BILDM,data=educationset1)
  m2=lm(sectiorate  ~ poly(ALTM,2)+BILDM,data=educationset1)
  m3a=lm(sectiorate  ~ poly(ALTM,3)+BILDM,data=educationset1)
  m3b=lm(sectiorate  ~ poly(ALTM,3)+BILDM,data=educationset1,weights=educationset1$count)

educationset2<-CSformaternalage_education_firstborns   %>% filter(count>100) 
  m2c=lm(sectiorate  ~ poly(ALTM,2)+BILDM,data=educationset2,weights=educationset2$count)
  m3c=lm(sectiorate  ~ poly(ALTM,3)+BILDM,data=educationset2,weights=educationset2$count)
  m3d=lm(sectiorate  ~ poly(ALTM,3)+BILDM,data=educationset2)


summary(m2)
summary(m3a)
summary(m3b)
summary(m2c)
summary(m3c)
summary(m3d)

newdataBILDM1<-data.frame(ALTM=c(seq(14,46,length=100)),BILDM=c(seq(1,1,length=100)))
newdataBILDM4<-data.frame(ALTM=c(seq(14,46,length=100)),BILDM=c(seq(4,4,length=100)))

newdataBILDM1$BILDM<-as.factor(newdataBILDM1$BILDM)
newdataBILDM4$BILDM<-as.factor(newdataBILDM4$BILDM)

predictvecBILDM1m2c<-predict(m2c,newdataBILDM1,type="response")
predictvecBILDM4m2c<-predict(m2c,newdataBILDM4,type="response")
#predictvecBILDM1<-predict(m2a,newdataBILDM1,type="response")
#predictvecBILDM4<-predict(m2a,newdataBILDM4,type="response")
predictvecBILDM1m3c<-predict(m3c,newdataBILDM1,type="response")
predictvecBILDM4m3c<-predict(m3c,newdataBILDM4,type="response")

agevec<-c(seq(14,46,length=100))
predm2c<-data.frame(agevec,predictvecBILDM1m2c,predictvecBILDM4m2c)
predm2c<-pred %>% mutate(diffedu=predictvecBILDM1m2c-predictvecBILDM4m2c)

predm3c<-data.frame(agevec,predictvecBILDM1m3c,predictvecBILDM4m3c)
predm3c<-pred %>% mutate(diffedu=predictvecBILDM1m3c-predictvecBILDM4m3c)




CSformaternalage_education %>% filter(BILDM==1|BILDM==4) %>% filter(count>100) %>% 
  ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(colour=BILDM,size=count),show.legend = FALSE) +
  xlab("maternal age")+
  ylab("CS frequency")+
  xlim(14,46)+
  ylim(0.12,0.62)+
  theme(axis.text = element_text(colour = "black",size = rel(2)), axis.title = element_text(colour = "black",size = rel(2)))+ scale_color_manual(values=c("darkorange", "red4"))+
  theme(aspect.ratio=700/960)+
  theme(legend.position="none")
ggsave("maternalage_education_allbirths.pdf")
ggsave("maternalage_education_allbirths.svg")


CSformaternalage_education_firstborns %>% filter(BILDM==1|BILDM==4) %>% filter(count>100) %>% 
  ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(colour=BILDM,size=count),show.legend = FALSE) +
  xlab("maternal age")+
  ylab("CS frequency")+
  xlim(14,46)+
  ylim(0.12,0.62)+
  theme(axis.text = element_text(colour = "black",size = rel(2)), axis.title = element_text(colour = "black",size = rel(2)))+ scale_color_manual(values=c("darkorange", "red4"))+
  geom_line(data=predm3c,aes(agevec,predictvecBILDM1m3c))+ 
  geom_line(data=predm3c,aes(agevec,predictvecBILDM4m3c))+
  theme(aspect.ratio=700/960)+
  theme(legend.position="none")
ggsave("maternalage_education_polymodelm3c.pdf")
ggsave("maternalage_education_polymodelm3c.svg")



CS_hospital_firstborns_age =read.csv('CS_hospital_firstborns_age.csv',header=T, sep=',')
CS_hospital_firstborns_age <-CS_hospital_firstborns_age %>% mutate(KLINIKTYP=as.factor(KLINIKTYP))

summary(CS_hospital_firstborns_age)
clinicset1= CS_hospital_firstborns_age %>% filter(count>100)
  mk3=lm(sectiorate  ~ poly(ALTM,3)+KLINIKTYP,data=clinicset1,weights=clinicset1$count)

summary(mk3)

remove('newdataKLINIK1','newdataKLINIK2')
newdataKLINIK1<-data.frame(ALTM=c(seq(14,46,length=100)),KLINIKTYP=c(seq(1,1,length=100)))
newdataKLINIK2<-data.frame(ALTM=c(seq(14,46,length=100)),KLINIKTYP=c(seq(2,2,length=100)))

newdataKLINIK1$KLINIKTYP<-as.factor(newdataKLINIK1$KLINIKTYP)
newdataKLINIK2$KLINIKTYP <-as.factor(newdataKLINIK2$KLINIKTYP)

predictvecKLINIK1<-predict(mk3,newdataKLINIK1,type="response")
predictvecKLINIK2<-predict(mk3,newdataKLINIK2,type="response")
agevec<-c(seq(14,46,length=100))
predKLINIK<-data.frame(agevec,predictvecKLINIK1,predictvecKLINIK2)
 predKLINIK<-predKLINIK %>% mutate(diffklinik=predictvecKLINIK2-predictvecKLINIK1)

CS_hospital_firstborns_age %>% filter(count>100) %>%
  ggplot(aes(ALTM,sectiorate))+
  geom_point(aes(colour=KLINIKTYP,size=count,show.legend = TRUE)) +
  xlab("maternal age")+
  ylab("CS frequency")+
  theme(axis.text = element_text(colour = "black",size = rel(2.5)), axis.title = element_text(colour = "black",size = rel(2.5)))+
  scale_color_manual(values=c("navyblue", "darkslategray3"))+
geom_line(data=predKLINIK,aes(agevec,predictvecKLINIK1))+ 
geom_line(data=predKLINIK,aes(agevec,predictvecKLINIK2))+
theme(aspect.ratio=700/960)+
theme(legend.position="none")
ggsave("CS_hospital_firstborns_age_polymodel.svg")


CS_hospital_firstborns_year =read.csv('CS_hospital_firstborns_year.csv',header=T, sep=',')
CS_hospital_firstborns_year <-CS_hospital_firstborns_year %>% mutate(KLINIKTYP=as.factor(KLINIKTYP))


#line plot: C-section rate of firstborns, for hospital categories over time
CS_hospital_firstborns_year %>% filter(count>100) %>%
  ggplot(aes(GEBJAHR,sectiorate))+
  geom_line(aes(colour=KLINIKTYP)) +
  xlab("year")+
  ylab("CS frequency")+
  theme(axis.text = element_text(colour = "black",size = rel(2.5)), axis.title = element_text(colour = "black",size = rel(2.5)))+
  scale_color_manual(values=c("navyblue", "darkslategray3"))+
  theme(aspect.ratio=700/960)+
  theme(legend.position="none")
  ggsave("CS_hospital_firstborns_year.svg")

maternalage_hospital_firstborns_year =read.csv('maternalage_hospital_firstborns_year.csv',header=T, sep=',')
maternalage_hospital_firstborns_year <-maternalage_hospital_firstborns_year %>% mutate(KLINIKTYP=as.factor(KLINIKTYP))

  
  #Line plot, maternal age at first birth, for hospital classes
maternalage_hospital_firstborns_year %>% filter(count>100) %>%
  ggplot(aes(GEBJAHR,meanage))+
  geom_line(aes(colour=KLINIKTYP)) +
  xlab("year")+
  ylab("age at first birth")+
  theme(axis.text = element_text(colour = "black",size = rel(2.5)), axis.title = element_text(colour = "black",size = rel(2.5)))+
  scale_color_manual(values=c("navyblue", "darkslategray3"))+
  theme(aspect.ratio=700/960)+
  theme(legend.position="none")+
ggsave("maternalage_hospital_firstborns_year.svg")

  


