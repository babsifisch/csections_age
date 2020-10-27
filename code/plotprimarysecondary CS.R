#Read in data
setwd("~/Work/Kaiserschnittprojekt/Neue Analyse/Ergebnisse_2")

typeofCSformaternalage_firstborns =read.csv('typeofCSformaternalage_firstborns.csv',header=T, sep=',')
CSformaternalage_firstborns=read.csv('CSformaternalage_firstborns.csv',header=T, sep=',')

CSformaternalage_firstborns %>% filter(count>=100) %>% ggplot(aes(ALTM,sectiorate)) +
  geom_point(aes(size=count),show.legend = FALSE) +
  xlim(13, 48)+ 
  ylim(0.05, 0.5)+
  xlab("maternal age")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))
ggsave("CSformaternalage_firstborns_a.pdf")

typeofCSformaternalage_firstborns %>% filter(count>=30) %>% ggplot(aes()) +
  geom_point(aes(ALTM,sectiorateprimary,size=count,colour = "red"),show.legend = FALSE) +
  geom_point(aes(ALTM,sectioratesecondary,size=count),show.legend = FALSE) +
  xlim(13, 48)+ 
  ylim(0.05, 0.5)+
  xlab("maternal age")+
  ylab("CS rate")+
  theme(axis.text = element_text(colour = "black",size = rel(1.5)), axis.title = element_text(colour = "black",size = rel(1.5)))

ggsave("typeofCSformaternalage_firstborns_a.pdf")