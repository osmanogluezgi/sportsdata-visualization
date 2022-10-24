library(countrycode) #to standardize ISO country codes in "nation" column
library(dplyr) # for pipe operator
#Merging all the data frames from different sports into one standard data frame

##TENNIS
colnames(men_tennis_visuals) <- c("player_name", "nation", "retirement_date", 
                                  "start_date", "age", "birthdate", "best_rank",
                                  "date_rank")
men_tennis_DF <- data.frame(cbind(men_tennis_visuals$player_name,
      men_tennis_visuals$nation,
      rep("Tennis", nrow(men_tennis_visuals)),
      rep("Tennis-Men", nrow(men_tennis_visuals)),
      men_tennis_visuals$age,
      as.character(as.Date(men_tennis_visuals$birthdate,"%d-%m-%Y"),"%Y-%m-%d"),
      men_tennis_visuals$start_date,
      as.character(as.Date(men_tennis_visuals$retirement_date,"%d %B %Y"),"%Y-%m-%d"),
      men_tennis_visuals$best_rank,
      as.character(as.Date(men_tennis_visuals$date_rank,"%d-%m-%Y"),"%Y")))

colnames(men_tennis_DF) <- c("player_name", "nation","sport", "sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

women_tennis_DF <- data.frame(cbind(women_tennis_visuals$player_name,
                       women_tennis_visuals$nation,
                       rep("Tennis", nrow(women_tennis_visuals)) ,
                       rep("Tennis-Women", nrow(women_tennis_visuals)) ,
                       women_tennis_visuals$age,
                       as.character(women_tennis_visuals$birthdate,"%Y-%m-%d"),
                       rep(NA, nrow(women_tennis_visuals)),
                       as.character(as.Date(women_tennis_visuals$retirement_date,"%d %B %Y"),"%Y-%m-%d"),
                       women_tennis_visuals$best_rank,
                       women_tennis_visuals$date_rank))
colnames(women_tennis_DF) <- c("player_name", "nation","sport","sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

##MOTORSPORT
motorrace_DF <- data.frame(cbind(motorrace_visual$name,
                                    countrycode(motorrace_visual$country,origin= "country.name" ,destination="iso3c"),
                                    rep("Motorrace", nrow(motorrace_visual)),
                                    rep("Motorrace-Men", nrow(motorrace_visual)),
                                    motorrace_visual$age,
                                    as.character(motorrace_visual$birthdate.x,"%Y-%m-%d"),
                                    motorrace_visual$first_event,
                                    motorrace_visual$date_last_event,
                                    motorrace_visual$bestpos,
                                    motorrace_visual$bestpos_date))
colnames(motorrace_DF) <- c("player_name", "nation","sport","sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

##HANDBALL
## Removing unnecessary column
handball <-handball_final[,-1] 

handball_DF <- data.frame(cbind(handball$name,
                                countrycode(handball$nation,origin= "country.name" ,destination="iso3c"),
                                rep("Handball", nrow(handball)),
                                rep("Handball-Men", nrow(handball)),
                                floor(as.numeric(difftime(Sys.Date(),handball$birthdate, units = "weeks"))/52.25),
                                as.character(handball$birthdate,"%Y-%m-%d"),
                                as.character(handball$start_date,"%Y-%m-%d"),
                                as.character(handball$end_date,"%Y-%m-%d"),
                                handball_final$best_rank,
                                as.character(handball$date_rank,"%Y-%m-%d")
                          ))
colnames(handball_DF) <- c("player_name", "nation","sport","sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

#EISLAUFEN
women_iceskating_visuals <- iceskating_visuals[iceskating_visuals$sex=="Female",]
men_iceskating_visuals <-   iceskating_visuals[iceskating_visuals$sex=="Male",]

women_iceskating_DF <- data.frame(cbind(women_iceskating_visuals$Name,
                                        women_iceskating_visuals$`Nation(s)`,
                                  rep("Iceskating", nrow(women_iceskating_visuals)) ,
                                  rep("Iceskating-Women", nrow(women_iceskating_visuals)) ,
                                  floor(as.numeric(difftime(Sys.Date(),as.Date(women_iceskating_visuals$birthday,"%d %B %Y"), units = "weeks"))/52.25),
                                  as.character(as.Date(women_iceskating_visuals$birthday,"%d %B %Y"),"%Y-%m-%d"),
                                  women_iceskating_visuals$careerstart,
                                  women_iceskating_visuals$careerend,
                                  women_iceskating_visuals$medalcount,
                                  women_iceskating_visuals$birthyear+women_iceskating_visuals$succ.age
))
colnames(women_iceskating_DF) <- c("player_name", "nation","sport","sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

men_iceskating_DF <- data.frame(cbind(men_iceskating_visuals$Name,
                                        men_iceskating_visuals$`Nation(s)`,
                                        rep("Iceskating", nrow(men_iceskating_visuals)) ,
                                      rep("Iceskating-Men", nrow(men_iceskating_visuals)) ,
                                      floor(as.numeric(difftime(Sys.Date(),as.Date(men_iceskating_visuals$birthday,"%d %B %Y"), units = "weeks"))/52.25),
                                      as.character(as.Date(men_iceskating_visuals$birthday,"%d %B %Y"),"%Y-%m-%d"),
                                      men_iceskating_visuals$careerstart,
                                      men_iceskating_visuals$careerend,
                                      men_iceskating_visuals$medalcount,
                                      men_iceskating_visuals$birthyear+men_iceskating_visuals$succ.age
))
colnames(men_iceskating_DF) <- c("player_name", "nation","sport","sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

men_iceskating_DF <- data.frame(cbind(men_iceskating_visuals$Name,
                                      men_iceskating_visuals$`Nation(s)`,
                                      rep("Iceskating", nrow(men_iceskating_visuals)) ,
                                      rep("Iceskating-Men", nrow(men_iceskating_visuals)) ,
                                      floor(as.numeric(difftime(Sys.Date(),as.Date(men_iceskating_visuals$birthday,"%d %B %Y"), units = "weeks"))/52.25),
                                      as.character(as.Date(men_iceskating_visuals$birthday,"%d %B %Y"),"%Y-%m-%d"),
                                      men_iceskating_visuals$careerstart,
                                      men_iceskating_visuals$careerend,
                                      men_iceskating_visuals$medalcount,
                                      men_iceskating_visuals$birthyear+men_iceskating_visuals$succ.age
))
colnames(men_iceskating_DF) <- c("player_name", "nation","sport","sport_sub", "age", "birthdate", "start_date", "retirement_date", "best_rank", "date_rank")

##Merging all into DF_full dataframe
DF_full <- data.frame(rbind(women_tennis_DF,men_tennis_DF ,women_iceskating_DF, men_iceskating_DF, handball_DF, motorrace_DF))


##Adding start_age, retirement age and sex to the dataset and creating the final
##dataset DF_Visual_Full
DF_Visual_Full <- DF_Full %>%
  mutate(start_age = as.numeric(start_date)-as.numeric(birthdate),
         retire_age = as.numeric(retirement_date)-as.numeric(birthdate),
         sex = ifelse(sport_sub %in% c("Tennis-Men", "Iceskating-Men", "Handball-Men", "Motorrace-Men"), "male", "female"))

###updating formats
DF_Visual_Full$age <- as.numeric(DF_Visual_Full$age)

DF_Visual_Full <- DF_Visual_Full %>%
  mutate(career_length = as.numeric(retirement_date)-as.numeric(start_date))

#Exporting to csv and xlsx files
write.csv(DF_Visual_Full,"SS22/Seminar SportsData/SeminarSports/DF_Visual_Full.csv", row.names = FALSE)
write_xlsx(motorrace_visuals,"SS22/Seminar SportsData/SeminarSports/DF_Visual_Full.xlsx")




