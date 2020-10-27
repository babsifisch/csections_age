#calculate the age distribution of all first-time-mothers in 1995.
#Then age-adjust the firstborn CS rate of all years according to the age-distribution in 1995
#Define the reference distribution: distribution of maternal ages in 1995. referencedistri is the referencedistribution


sectiodistri<-births1 %>% filter(GEBURTENFOLGE_LEB==1) %>% group_by(GEBJAHR,ALTM) %>%  summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) 
allfirstbirths<-births1 %>% filter(GEBURTENFOLGE_LEB==1) %>% group_by(GEBJAHR) %>% summarise(count=n()) %>% rename(totalfirstbirthsperyear=count)
write.csv(sectiodistri,"sectiodistri.csv")

#complete missing entries with count=0 and sectiorate=0
sectiodistricomplete<-sectiodistri %>% ungroup %>% complete(GEBJAHR,ALTM,fill=list(count=0,sectiorate=0)) %>% left_join(allfirstbirths,by="GEBJAHR") %>% mutate(GEBJAHRdistri=count/totalfirstbirthsperyear)

distri1995<-sectiodistricomplete %>% filter(GEBJAHR==1995) %>% mutate(referencedistri=count/totalfirstbirthsperyear) %>% select(ALTM,referencedistri) 

#sectiorate is the CS rate for all persons of a specific age class. Use values in referencedistri as weights for that each class
agecorrectedrates<-sectiodistricomplete %>% left_join(distri1995,by="ALTM") %>% mutate(agecorrectedsectio=sectiorate*referencedistri)

#treat birthyear GEBJAHR as a factor for plotting
agecorrectedrates<-agecorrectedrates %>% mutate(GEBJAHR=as.factor(GEBJAHR))

#this are the CS rates and age-corrected CS rates for each year, of the firstborns, weighted with the frequency of the ref population for each age

agecorrectedCSrates<-agecorrectedrates %>% group_by(GEBJAHR) %>% summarize(originalsectiorates=sum(sectiorate*GEBJAHRdistri),agecorrectedsectiorates=sum(sectiorate*referencedistri)) 

#This are the non-corrected and the age-corrected CS rates for the firstborns.
write.csv(agecorrectedCSrates,"agecorrectedCSrates.csv")

agecorrectedrates %>% filter(GEBJAHR==1995  | GEBJAHR==2014) %>%
ggplot(aes(ALTM,GEBJAHRdistri))+
geom_line(aes(colour=GEBJAHR),show.legend = FALSE)+
xlab("maternal age")+
ylab("relative frequency")+
theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))+
scale_color_manual(values=c("#CC6666", "#9999CC"))
ggsave("maternalagedistributions.pdf")


