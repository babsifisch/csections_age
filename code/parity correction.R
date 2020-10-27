#calculate age- and parity-corrected CS rates

#new dataset births5: parity groups! 
#merge GEBURTENFOLGE_LEB: new: 4 levels. 1,2,3 and 4 or higher.
V<-as.numeric(births1$GEBURTENFOLGE_LEB)
V<-replace(V,V>=4,4)
births5<-births1
births5$GEBURTENFOLGE_LEB<-as.factor(V)
summary(births5)

#find age-specific CS, separately for each year and parity: sectiodistripar
sectiodistripar<-births5 %>% group_by(GEBJAHR,ALTM,GEBURTENFOLGE_LEB) %>%  summarise(count=n(),sectiorate=sum(ENTBINDUNGSART==1)/(length(ENTBINDUNGSART))) 

#find total no of births for each parity level for each year. 
allbirthspar<-births5 %>% group_by(GEBJAHR,GEBURTENFOLGE_LEB) %>% summarise(count=n()) %>% rename(totalbirthsperyearpar=count)
write.csv(allbirthspar,"allbirthspar.csv")

#total no of births for each year, across parity levels
allbirths<-births5 %>% group_by(GEBJAHR) %>% summarise(count=n()) %>% rename(totalbirthsperyear=count)
write.csv(allbirthsperyear,"allbirthsperyear.csv")

#complete missing entries with count=0 and sectiorate=0, find age distribution for each year GEBJAHRdistri. 
#GEBJAHRdistri sums up to 1 for each year, as it should be! 

sectiodistricompletepar<-sectiodistripar %>% ungroup %>% complete(GEBJAHR,ALTM,GEBURTENFOLGE_LEB,fill=list(count=0,sectiorate=0)) %>% left_join(allbirths,by = c("GEBJAHR")) %>% mutate(GEBJAHRdistri=count/totalbirthsperyear)

#This line is just to check that GEBJAHRdistri sums so 1 in each year. Now weigh this distribution with the referencedistri.
#a<-sectiodistricompletepar %>% group_by(GEBJAHR) %>% summarise(sectiosum=sum(GEBJAHRdistri))

#Find the reference distribution in 1995
distri1995par<-sectiodistricompletepar %>% filter(GEBJAHR==1995) %>% mutate(referencedistripar=count/totalbirthsperyear) %>% select(ALTM,GEBURTENFOLGE_LEB,referencedistripar) 
write.csv(distri1995par,"distri1995par.csv")

#Weigh the age-and-parity specific CS rates in sectiodistricompletepar with the reference distribution to get the corrected rates
agecorrectedratespar<-sectiodistricompletepar %>% left_join(distri1995par,by=c("ALTM","GEBURTENFOLGE_LEB")) %>% mutate(agecorrectedsectiopar=sectiorate*referencedistripar)
write.csv(agecorrectedratespar,"agecorrectedratespar.csv")

#make GEBJAHR into factor for plotting
agecorrectedratespar <-agecorrectedratespar %>% mutate(GEBJAHR=as.factor(GEBJAHR))

#this are the CS rates and age-corrected CS rates for each year and parity level
#age-and-parity-corrected: weighted with the frequency of the ref population for each age and parity level
agecorrectedCSratespar<-agecorrectedratespar %>% group_by(GEBJAHR) %>% summarize(originalsectioratespar=sum(sectiorate*GEBJAHRdistri),agecorrectedsectioratespar=sum(sectiorate*referencedistripar))

#this table includes all the age-corrected and parity-corrected CS rates, with year 1995 used as reference distribution for both age and parity. 

age_paritycorrectedCSrates<-agecorrectedCSratespar_1 %>% ungroup %>% group_by(GEBJAHR) %>% summarise(sectiorate_orig=sum(originalsectios)/allbirths1995,sectiorate_age_parity_corr=sum(agecorrectedsectios)/allbirths1995)
write.csv(age_paritycorrectedCSrates,"age_paritycorrectedCSrates.csv")
