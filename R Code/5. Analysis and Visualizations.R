library(dplyr) #for data manipulation
library(plyr) #for summary stats (ddply)
library(ggh4x)  # for ggplot facet_nested()
library(ggplot2) # for main visualization
library(rstatix) #for anova test

#Analysis
#Summary Stats
ddply(na.omit(DF_Visual_Full), .(sport,sex), summarize,  
      start_age =round(mean(start_age),0), 
      age_success= round(mean(age_success),0), 
      retirement_age = round(mean(retire_age),0), 
      career_length=round(mean(career_length),0))


# Count NAs by group
NA_count <- aggregate(. ~ sport_sub, 
          DF_Visual_Full,
          function(x) { sum(!is.na(x)) },
          na.action = NULL)
NA_count <- NA_count[,c(1,2,5,6,7,8,9,10)]

colnames(NA_count[2]) <- "Count"
NA_count
#Hypothesis 1
#There is an individual age for each sport, where athletes have the most success 
# (the age is lower for sports like ice skating in comparison to golf)

safe_colorblind_palette <- c( "#CC6677","#6699CC","#88CCEE", "#DDCC77", "#117733", 
                              "#661100", "#999933",  "#AA4499",
                              "#44AA99", "#882255", "#332288", "#888888")
safe_colorblond_bold <- c("#882255", "#332288")

mu <- ddply(na.omit(DF_Visual_Full), .(sport, sex), summarise, grp.mean=mean(age_success, na.rm = TRUE))
mu$grp.mean <- round(mu$grp.mean)

#Density Plot (Graph 1)
ggplot(DF_Visual_Full, aes(x=age_success, fill = sex))+
  geom_density(alpha = 0.4, position = "identity", color = "black", size= 0.3)+ 
  scale_y_continuous(limits = c(0,0.125),
                     breaks=seq(0,0.125,0.05))+
  scale_fill_manual(values = safe_colorblind_palette)+
  scale_color_manual(labels=c('Mean Age - Female', 'Mean Age - Male'),values = safe_colorblond_bold)+
  scale_linetype_manual(values = c(rep("dashed", 2)))+
  labs(
    title = "Success Age for Athletes based on Sport and Gender",
    x = "Age",
    y = "Density"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))+
  facet_nested(rows= vars(sport))+
  labs(fill = "Gender", color = "Statistics")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex), alpha = 0.8,
             linetype="dashed", size=0.35)+
  geom_text(data=mu, aes(x = grp.mean-0.1, y = 0.003, label = as.character(grp.mean),
                         color = sex, fontface = "bold"),  size = 3)
###GERMAN

hum_names <- as_labeller(
  c('Iceskating' = "Eislaufen", 'Tennis' = "Tennis",'Handball' = "Handball", 
    'Motorrace' = "Motorsport"))

ggplot(DF_Visual_Full, aes(x=age_success, fill = sex))+
  geom_density(alpha = 0.4, position = "identity", color = "black", size= 0.3)+ 
  scale_y_continuous(limits = c(0,0.125),
                     breaks=seq(0,0.125,0.05))+
  scale_fill_manual(labels=c('Weiblich', 'Männlich'), values = safe_colorblind_palette)+
  scale_color_manual(labels=c('Durchschnittsalter - Weiblich', 'Durchschnittsalter - Männlich'),values = safe_colorblond_bold)+
  scale_linetype_manual(values = c(rep("dashed", 2)))+
  labs(
    title = "Erfolgsalter der Athleten basierend auf Alter und Geschlecht",
    x = "Alter",
    y = "Dichte"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))+
  facet_nested(rows= vars(sport),labeller = hum_names)+
  labs(fill = "Geschlecht", color = "Statistiken")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex), alpha = 0.8,
             linetype="dashed", size=0.35)+
  geom_text(data=mu, aes(x = grp.mean-0.1, y = 0.003, label = as.character(grp.mean),
                         color = sex, fontface = "bold"),  size = 3)
## Comparison of Means (t-test)
DF_Visual_Full %>%
  group_by(sport_sub) %>%
  get_summary_stats(age_success, type = "mean_sd")

# Testing Difference in means of different sports
DF_Visual_Full %>% 
  anova_test(age_success ~ sport)
# Testing difference in means for gender (only tennis and ice skating should be interpreted)
result_ttest <- DF_Visual_Full %>%
  pairwise_t_test(age_success ~ sport_sub, p.adjust.method = "bonferroni")
result_ttest[c(3,15),]

#Hypothesis 2
#The earlier athletes start their career, the earlier they retire

DF_Visual_Full$sport <- factor(DF_Visual_Full$sport,levels=c('Iceskating','Tennis','Handball','Motorrace'))
fit1=lm(retire_age~start_age+sport+sex,data=DF_Visual_Full)
summary(fit1)

DF_Visual_Full <- DF_Visual_Full %>%
  mutate(sex = ifelse(sport_sub %in% c("Tennis-Men", "Iceskating-Men", "Handball-Men", "Motorrace-Men"), "male", "female"))

#Graph 2
## SUCCESS-LENGTH
ggplot(DF_Visual_Full,aes(x=age_success,y=as.numeric(retire_age-start_age),color=factor(sex)))+
  geom_point(shape=1,size=0.3)+
  geom_smooth(method="lm", size=0.5, se = FALSE)+
  facet_wrap(~sport, scale = "free_x")+
  scale_color_manual(labels=c('Female', 'Male'),values = safe_colorblind_palette)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 9, face = "bold"))+
  labs(color = "Gender")+
  labs(
    title = "Age of Success vs Career Length for Athletes",
    subtitle = "based on sport and gender",
    x = "Age of Success",
    y = "Career Length"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))

###GERMAN
ggplot(DF_Visual_Full,aes(x=age_success,y=as.numeric(retire_age-start_age),color=factor(sex)))+
  geom_point(shape=1,size=0.3)+
  geom_smooth(method="lm", size=0.5, se = FALSE)+
  facet_wrap(~sport, scale = "free_x",labeller = hum_names)+
  scale_color_manual(labels=c('Weiblich', 'Männlich'),values = safe_colorblind_palette)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 9, face = "bold"))+
  labs(color = "Geschlecht")+
  labs(
    title = "Erfolgsalter vs. Karrierelänge der Athleten",
    subtitle = "basierend auf Sportart und Geschlecht",
    x = "Erfolgsalter",
    y = "Karrierelänge"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))


## SUCCESS-LENGTH Focused (Success Age > 30)
DF_Visual_Full %>%
  filter(age_success>=30)%>%
ggplot(.,aes(x=age_success,y=as.numeric(retire_age-start_age),color=factor(sex)))+
  geom_point(shape=1,size=0.3)+
  geom_smooth(method="lm", size=0.5, se = FALSE)+
  facet_wrap(~sport, scale = "free_x")+
  scale_color_manual(labels=c('Female', 'Male'),values = safe_colorblind_palette)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 9, face = "bold"))+
  labs(color = "Gender")+
  labs(
    title = "Age of Success vs Career Length for Athletes",
    subtitle = "based on sport and gender (Success Age >= 30)",
    x = "Age of Success",
    y = "Career Length"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))

###GERMAN
DF_Visual_Full %>%
  filter(age_success>=30)%>%
  ggplot(.,aes(x=age_success,y=as.numeric(retire_age-start_age),color=factor(sex)))+
  geom_point(shape=1,size=0.3)+
  geom_smooth(method="lm", size=0.5, se = FALSE)+
  facet_wrap(~sport, scale = "free_x",labeller = hum_names)+
  scale_color_manual(labels=c('Weiblich', 'Männlich'),values = safe_colorblind_palette)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 9, face = "bold"))+
  labs(color = "Geschlecht")+
  labs(
    title = "Erfolgsalter vs. Karrierelänge der Athleten",
    subtitle = "basierend auf Sportart und Geschlecht (Erfolgsalter >= 30)",
    x = "Erfolgsalter",
    y = "Karrierelänge"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))

##Below code for remaining graphs is explained

### Graphik zu Karrierestart, -hoehepunkt und -ende von Eiskunstlaeufer*innen, ###
### Handballern, Tennisspieler*innen und Rennfahrern                           ###

# noetige Pakete laden 
library(openxlsx) # zum Einlesen des Datensatzes 
library(png) # zum Einlesen der Sportsymbolbildchen 


# Arbeitsverzeichnis setzen 
setwd("~/Documents/Dokumente - Annas MacBook Air/Studium/Statistik/Kurse/Sportdatenvisualisierung")

# Datensatz einlesen 
dat <- read.xlsx("DF_Visual_Full_20220912.xlsx", detectDates = TRUE)

# Datensatz "ankleben", damit direkt auf Variablen zugegriffen werden kann 
attach(dat)

# "start_dates" numerisch machen, damit man damit rechnen kann 
str(dat)
dat$start_date <- as.numeric(start_date)
str(dat)

attach(dat)

# pngs einlesen  
handball <- readPNG("handball.png")
ice_skating <- readPNG("iceskating.png")
tennis <- readPNG("tennis.png")
motorrace <- readPNG("motorrace.png")

# Funktion, die fuer eine Sportart das mittlere Alter bei Karrierebeginn berechnet
careerstart <- function(sportart){
  mean(start_date[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE)
}

# Funktionen zum Einfuegen der Sportsymbole an immer den gleichen Stellen 
handballimg <- function(){
  rasterImage(handball, careerstart("Handball-Men") - 3.25, 1.25, 
              careerstart("Handball-Men") - 0.5, 2.25)
}
iceskatingimg <- function(){
  rasterImage(ice_skating, careerstart("Iceskating-Women") - 3.5, 2.75, 
              careerstart("Iceskating-Women") - 1, 3.75)
}
motorraceimg <- function(){
  rasterImage(motorrace, careerstart("Motorrace-Men") - 3.25, 4.25, 
              careerstart("Motorrace-Men") - 0.75, 5.25)
}
tennisimg <- function(){
  rasterImage(tennis, careerstart("Tennis-Women") - 3, 5.75, 
              careerstart("Tennis-Women") - 0.5, 6.75)
}


## 1. Graph, der nur das Alter zum Karrierestart der Disziplinen plottet

# Funktion, die die Punkte zu den mittleren Karrierestart-Altern der Sportarten
# in den bestehenden Graph einfuegt 
linien1 <- function(sportart, h, col, pch){
  attach(dat)
  points(careerstart(sportart), 
         h, col = col, pch = pch, cex = 1.5)
}

#png("visual1.png") # als png abspeichern 
plot(careerstart("Handball-Men"), 
     1.75, "o", col="#6699CC", pch=16, lty=2, xlim = c(15, 40), ylim = c(1,7),
     xlab = "mean age of career start",
     yaxt  = "n", ylab = "", bty = "n", cex = 1.5, lwd = 1.5, cex.lab = 1.35,
     font.lab = 4)

linien1("Iceskating-Men", 3, "#6699CC", 16)
linien1("Iceskating-Women", 3.5, "#CC6677", 17)
linien1("Motorrace-Men", 4.75, "#6699CC", 16)
linien1("Tennis-Men", 6, "#6699CC" , 16)
linien1("Tennis-Women", 6.5, "#CC6677", 17)

handballimg()
iceskatingimg()
motorraceimg()
tennisimg()

legend("topright", c("male", "female"), pch = c(16,17), col = c("#6699CC", "#CC6677"), 
       cex = 1.25, bty = "n", text.col = c("#6699CC", "#CC6677"), text.font = 3)
#dev.off() # Grafikdevice schliessen 


## 2. Graph, der zusaetzlich das Alter zum Karrierehoehepunkt der Disziplinen plottet

# Funktion, die die Linien fuer die restlichen Sportarten zum bestehenden Graph
# hinzufuegt
linien2 <- function(sportart, h, col, pch, lty){
  attach(dat)
  points(c(careerstart(sportart), 
           mean(date_rank[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE)), 
         c(h, h), col = col, pch = pch, cex = c(1.5,2.25))
  lines(c(careerstart(sportart), 
          mean(date_rank[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE)), 
        c(h,h), col = col, lty = lty, lwd = 1.5)
}

#png("visual2.png")
plot(c(careerstart("Handball-Men"), 
       mean(date_rank[sport == "Handball"] - birthdate[sport == "Handball"], na.rm = TRUE)), 
     c(1.75, 1.75), "o", col="#6699CC", pch=16, lty=3, xlim = c(15, 40), ylim = c(1,7),
     xlab = "mean age of career start and success",
     yaxt  = "n", ylab = "", bty = "n", cex = c(1.5,2.25), lwd = 1.5, cex.lab = 1.35,
     font.lab = 4)

linien2("Iceskating-Men", 3, "#6699CC", 16, 3)
linien2("Iceskating-Women", 3.5, "#CC6677", 17, 3)
linien2("Motorrace-Men", 4.75, "#6699CC", 16, 3)
linien2("Tennis-Men", 6, "#6699CC" , 16, 3)
linien2("Tennis-Women", 6.5, "#CC6677", 17, 3)

handballimg()
iceskatingimg()
motorraceimg()
tennisimg()

legend("topright", c("male", "female"), pch = c(16,17), col = c("#6699CC", "#CC6677"), 
       cex = 1.25, bty = "n", text.col = c("#6699CC", "#CC6677"), text.font = 3)

#dev.off()


## 3. Graph, der zusaetzlich das Alter zum Karriereende der Disziplinen plottet

# Funktion, die die Linien fuer die restlichen Sportarten zum bestehenden Graph
# hinzufuegt
linien <- function(sportart, h, col, pch, lty){
  attach(dat)
  points(c(careerstart(sportart), 
           mean(date_rank[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE), 
           mean(retirement_date[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE)), 
         c(h,h,h), col = col, pch = pch, cex = c(1.5,2.25,1.5))
  lines(c(careerstart(sportart), 
          mean(date_rank[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE), 
          mean(retirement_date[sport_sub == sportart] - birthdate[sport_sub == sportart], na.rm = TRUE)), 
        c(h,h,h), col = col, lty = lty, lwd = 1.5)
}

#png("visual3.png")
plot(c(careerstart("Handball-Men"), 
       mean(date_rank[sport == "Handball"] - birthdate[sport == "Handball"], na.rm = TRUE), 
       mean(retirement_date[sport == "Handball"] - birthdate[sport == "Handball"], na.rm = TRUE)), 
     c(1.75,1.75,1.75), "o", col="#6699CC", pch=16, lty=3, xlim = c(15, 40), ylim = c(1,7),
     xlab = "mean age of career start, success and retirement",
     yaxt  = "n", ylab = "", bty = "n", cex = c(1.5,2.25,1.5), lwd = 1.5, cex.lab = 1.35,
     font.lab = 4)


linien("Iceskating-Men", 3, "#6699CC", 16, 3)
linien("Iceskating-Women", 3.5, "#CC6677", 17, 3)
linien("Motorrace-Men", 4.75, "#6699CC", 16, 3)
linien("Tennis-Men", 6, "#6699CC" , 16, 3)
linien("Tennis-Women", 6.5, "#CC6677", 17, 3)

handballimg()
iceskatingimg()
motorraceimg()
tennisimg()

legend("topright", c("male", "female"), pch = c(16,17), col = c("#6699CC", "#CC6677"), 
       cex = 1.25, bty = "n", text.col = c("#6699CC", "#CC6677"), text.font = 3)

#dev.off()

# Datensatz wieder "entkleben"
detach(dat)

