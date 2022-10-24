library(dplyr) #to use the pipes, and data manipulation
library(rvest) #for data scraping
library(RSelenium) #for data scraping
library(writexl) #for exporting to excel
library(strsplit) #text manipulation
library(stringr) #text manipulation
library(pdftools) #pdf manipulation
library (readr) # to read csv for women players' birthdate

# We first get the retired players list, however data is in pdf format thus we downloaded them manually.
# Since the files are in pdf format, we manipulate them and format them to create one data set.

files <- list.files(pattern = "pdf$")

# pdf1 consists of the players who retired before 2015 (both men and women)

pdf1 <-pdf_text("247937.pdf") 

# pdf2 consists of the players who retired after 2015 (both men and women)

pdf2 <-pdf_text("322956.pdf")

# Formatting the text data
pdf1_new <- pdf1 %>%
  str_split("\n")
# Removing unnecessary rows due to the format of scraped text data
pdf1_new[[1]] <- pdf1_new[[1]][-1:-10]

# Men retired until 2015
pdf1_men <- pdf1_new[[1]][1:13]

df_men_bis2015 <- pdf1_men %>%
  str_replace_all(",\\s", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=\\d+)", ",")%>%
  str_replace_all("(?<=\\d) (?=[[:alpha:]])", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=[[:alpha:]])", "")%>%
  str_trim()%>%
  strsplit("\\s+") %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  setNames(c("player_name", "nation", "retirement_date"))%>%
  .[-1,]

df_men_bis2015$player_name <- gsub(","," ",as.character(df_men_bis2015$player_name))
df_men_bis2015$retirement_date <- gsub(","," ",as.character(df_men_bis2015$retirement_date))

# Women retired until 2015
pdf1_women1 <- pdf1_new[[1]][17:36]
pdf1_women2<- pdf1_new[[2]][1:23]
pdf1_women <- append(pdf1_women1, pdf1_women2)

df_women_bis2015 <- pdf1_women %>%
  str_replace_all(",\\s", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=\\d+)", ",")%>%
  str_replace_all("(?<=\\d) (?=[[:alpha:]])", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=[[:alpha:]])", "")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=\\()", "")%>%
  str_trim()%>%
  strsplit("\\s+") %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  setNames(c("player_name", "nation", "retirement_date"))%>%
  .[-1:-2,]

df_women_bis2015$player_name <- gsub(","," ",as.character(df_women_bis2015$player_name))
df_women_bis2015$retirement_date <- gsub(","," ",as.character(df_women_bis2015$retirement_date))

#ab2015
pdf2_new <- pdf2 %>%
  str_split("\n")

pdf2_new[[1]] <- pdf2_new[[1]][-1:-10]

# Men retired after 2015
pdf2_men <- pdf2_new[[1]][3:29]

df_men_ab2015 <- pdf2_men %>%
  str_replace_all(",\\s", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=\\d+)", ",")%>%
  str_replace_all("(?<=\\d) (?=[[:alpha:]])", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=[[:alpha:]])", "")%>%
  str_trim()%>%
  strsplit("\\s+") %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  setNames(c("player_name", "nation", "retirement_date"))%>%
  .[-1,]

df_men_ab2015$player_name <- gsub(","," ",as.character(df_men_ab2015$player_name))
df_men_ab2015$retirement_date <- gsub(","," ",as.character(df_men_ab2015$retirement_date))

# Women retired after 2015
pdf2_women1 <- pdf2_new[[1]][33:40]
pdf2_women2<- pdf2_new[[2]][1:49]
pdf2_women3<- pdf2_new[[3]][1:21]
pdf2_women <- append(pdf2_women1, pdf2_women2)
pdf2_women <- append(pdf2_women, pdf2_women3)

df_women_ab2015 <- pdf2_women %>%
  str_replace_all(",\\s", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=\\d+)", ",")%>%
  str_replace_all("(?<=\\d) (?=[[:alpha:]])", ",")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=[[:alpha:]])", "")%>%
  str_replace_all("(?<=[[:alpha:]]) (?=\\()", "")%>%
  str_trim()%>%
  strsplit("\\s+") %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  setNames(c("player_name", "nation", "retirement_date"))%>%
  .[-1:-2,]

df_women_ab2015$player_name <- gsub(","," ",as.character(df_women_ab2015$player_name))
df_women_ab2015$retirement_date <- gsub(","," ",as.character(df_women_ab2015$retirement_date))

#Exporting the names into one excel file
sheets <- list("Men_bis2015" = df_men_bis2015, "Men_ab2015" = df_men_ab2015, 
               "Women_bis2015"=df_women_bis2015, "Women_ab2015"= df_women_ab2015) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, "Tennis_all.xlsx")

# Men Bio Info and Rankings Scraping 
## We combine the dataset for men
list_men_retirement <- data.frame(rbind(df_men_bis2015, df_men_ab2015))

## We update the names in order to make the search easier.
list_men_retirement[list_men_retirement$player_name=="EndaraRosales Ivan","player_name"]<- "Endara Ivan"
list_men_retirement[list_men_retirement$player_name=="LanzoniNetto Americo","player_name"]<- "Lanzoni Americo"
list_men_retirement[list_men_retirement$player_name=="Podipnik-Castillo Hans","player_name"]<- "Podlipnik-Castillo Hans"

# We use the link below to search the players and get the data for bio and rankings
link_tennis_det <- "https://www.itftennis.com/en/players/" 


driver <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer,browser = c("chrome"), chromever =
                     system2(command = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                             args = "--version",
                             stdout = TRUE,
                             stderr = TRUE) %>%
                     stringr::str_extract(pattern = "(?<=Chrome )\\d+\\.\\d+\\.\\d+\\.") %>%
                     magrittr::extract(!is.na(.)) %>%
                     stringr::str_replace_all(pattern = "\\.",
                                              replacement = "\\\\.") %>%
                     paste0("^",  .) %>%
                     stringr::str_subset(string =
                                           binman::list_versions(appname = "chromedriver") %>%
                                           dplyr::last()) %>%
                     as.numeric_version() %>%
                     max() %>%
                     as.character())
remDr <- driver[["client"]]

remDr$navigate(link_tennis_det)
Sys.sleep(2)

#To close the cookies pop-up
option <- remDr$findElement(using = "css selector", "button[id='onetrust-reject-all-handler']")
option$clickElement()

## We define a data set to store info, and another data set 'exclude' to exclude the following
## names since they don't have data in our source.
data_test <- data.frame()
exclude <- c(which(list_men_retirement$player_name =="Hoeglinger Lucas"),
             which(list_men_retirement$player_name=="Mahlum Colter"),
             which(list_men_retirement$player_name=="Grindley William"),
             which(list_men_retirement$player_name=="Lanzoni Americo"),
             which(list_men_retirement$player_name=="Scott Dylan"))

for(i in (1:nrow(list_men_retirement))[-exclude]){
  #navigating to the source website
  remDr$navigate(link_tennis_det)
  Sys.sleep(2)
  
  #clicking on the search bar
  option <- remDr$findElement(using = "css selector", "input[id='player-search-input']")
  option$clickElement()

  #entering the name
  name <- list_men_retirement[i,1]
  option$sendKeysToElement(list(name, "\uE007"))
  Sys.sleep(5)

  #clicking on the name from dropdown
  option <- remDr$findElement(using = "css selector", "div[class='player-selection__suggested-players'] button")
  option$clickElement()
  Sys.sleep(2)
  
  #scrolling down to load the whole page
  remDr$executeScript("window.scrollTo(0,300);")

  #We choose the tour (Men's) not Juniors since we only use the data from adulthood years
  option <- remDr$findElement(using = "css selector", "div[class=' css-1pu4ewt-control']")
  option$sendKeysToElement(list(key = "end")) 
  option$clickElement()
  Sys.sleep(2)

  #We click on "Mens" from dropdown
  option <- remDr$findElement(using = "css selector", "div[id='react-select-2-option-0")
  option$clickElement()

  #We click on "See Profile" to go to player's page
  option <- remDr$findElement(using = "css selector", "div > a[class='btn player-selection__link ']")
  option$clickElement()
  Sys.sleep(3)

  #We scroll down to rankings table
  remDr$executeScript("window.scrollTo(0,900);")
  Sys.sleep(2)
  
  #We expand the rankings table
  select <- remDr$findElement(using = "css selector","span[class*='button']")
  select$clickElement()
  Sys.sleep(2)

  #We select the Year-end Rankings table
  option <- remDr$findElement(using = "css selector", "table.classic-table")

  #We scrape and unlist the table
  years <- option$getElementText() %>% unlist
  Sys.sleep(3)
  
  #We format the text
  list <- years%>%
    str_replace_all("\n", ",")%>%
    strsplit(",") %>%
    do.call(rbind, .) %>%
    as.vector(.)%>%
    .[-1:-2]

  #We store the rankings by year for the current player
  year <- c()
  ranking <- c()
  for (i in 1:(length(list)/2)){
    year <- append(year,list[2*i-1])
    ranking <- append(ranking,list[2*i])
  }

  new<-data.frame(name,year, ranking)
  data_test <- rbind(data_test,new)

  rm("new")

}

#We store the info for all men players
list_men_rankings <- data_test

remDr$close()
rm("option", "remDr","data_test" )

# Women Bio Info and Rankings Scraping
## We combine the dataset for men
list_women_retirement <- rbind(df_women_bis2015, df_women_ab2015)

## We define a data set to store info, and another data set 'exclude' to exclude the following
## names since they don't have data in our source.

list_women_retirement[list_women_retirement$player_name=="CohenAloro Stephanie","player_name"]<- "Cohen Aloro Stephanie"
list_women_retirement[list_women_retirement$player_name=="Gould(Irvin) Marissa","player_name"]<- "Gould Marissa"
list_women_retirement[list_women_retirement$player_name=="Müller Martina","player_name"]<- "Muller Martina"
list_women_retirement[list_women_retirement$player_name=="Pascual VirginiaRuano","player_name"]<- "Pascual Virginia Ruano"
list_women_retirement[list_women_retirement$player_name=="Salerni MariaEmilia","player_name"]<- "Salerni Maria Emilia"
list_women_retirement[list_women_retirement$player_name=="ChinenyeNdidi Izuogu","player_name"]<- "Chinenye Ndidi Izuogu"
list_women_retirement[list_women_retirement$player_name=="DominguezLino Lourdes","player_name"]<- "Dominguez Lino Lourdes"
list_women_retirement[list_women_retirement$player_name=="JovanovskiPetrovic Bojana","player_name"]<- "Jovanovski Petrovic Bojana"
list_women_retirement[list_women_retirement$player_name=="MedinaGarrigues Anabel","player_name"]<- "Medina Garrigues Anabel"
list_women_retirement[list_women_retirement$player_name=="ParraSantonja Arantxa","player_name"]<- "Parra Santonja Arantxa"
list_women_retirement[list_women_retirement$player_name=="SolerEspinosa Silvia","player_name"]<- "Soler Espinosa Silvia"
list_women_retirement[list_women_retirement$player_name=="SuarezNavarro Carla","player_name"]<- "Suarez Navarro Carla"
list_women_retirement[list_women_retirement$player_name=="Duque-Mariño Mariana","player_name"]<- "Duque-Marino Mariana"
list_women_retirement[list_women_retirement$player_name=="Savchuk¸Olga","player_name"]<- "Savchuk Olga"


exclude_w <- c(which(list_women_retirement$player_name =="Dechy Natalie"),
               which(list_women_retirement$player_name =="Garbin Tatiana"),
               which(list_women_retirement$player_name =="Li Na"),
               which(list_women_retirement$player_name =="O’Brien Katie"),
               which(list_women_retirement$player_name =="Obziler Tzipora"),
               which(list_women_retirement$player_name =="Chinenye Ndidi Izuogu"),
               which(list_women_retirement$player_name =="Glazkova Ekaterina"),
               which(list_women_retirement$player_name =="Halaburda Anastasiya"),
               which(list_women_retirement$player_name =="OsterlohSnape Lilia"),
               which(list_women_retirement$player_name =="Park Soyeon")
               )
             
driver <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer,browser = c("chrome"), chromever =
                     system2(command = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                             args = "--version",
                             stdout = TRUE,
                             stderr = TRUE) %>%
                     stringr::str_extract(pattern = "(?<=Chrome )\\d+\\.\\d+\\.\\d+\\.") %>%
                     magrittr::extract(!is.na(.)) %>%
                     stringr::str_replace_all(pattern = "\\.",
                                              replacement = "\\\\.") %>%
                     paste0("^",  .) %>%
                     stringr::str_subset(string =
                                           binman::list_versions(appname = "chromedriver") %>%
                                           dplyr::last()) %>%
                     as.numeric_version() %>%
                     max() %>%
                     as.character())
remDr <- driver[["client"]]
remDr$navigate(link_tennis_det)
Sys.sleep(2)
#To close the cookies pop-up
option <- remDr$findElement(using = "css selector", "button[id='onetrust-reject-all-handler']")
option$clickElement()

data_test <- data.frame()

for(i in (1:nrow(list_women_retirement))[-exclude_w]){
  #navigating to the source website
  remDr$navigate(link_tennis_det)
  Sys.sleep(2)
  
  #clicking on the search bar
  option <- remDr$findElement(using = "css selector", "input[id='player-search-input']")
  option$clickElement()
  
  #entering the name
  name <- list_women_retirement[i,1]
  option$sendKeysToElement(list(name, "\uE007"))
  Sys.sleep(5)
  
  #clicking on the name from dropdown
  option <- remDr$findElement(using = "css selector", "div[class='player-selection__suggested-players'] button")
  option$clickElement()
  Sys.sleep(2)
  
  #scrolling down to load the whole page
  remDr$executeScript("window.scrollTo(0,300);")
  
  #We choose the tour (Women's) not Juniors since we only use the data from adulthood years
  option <- remDr$findElement(using = "css selector", "div[class=' css-1pu4ewt-control']")
  option$sendKeysToElement(list(key = "end"))
  option$clickElement()
  Sys.sleep(2)
  
  #We click on "Womens" from dropdown
  option <- remDr$findElement(using = "css selector", "div[id='react-select-2-option-0")
  option$clickElement()
  
  #We click on "See Profile" to go to player's page
  option <- remDr$findElement(using = "css selector", "div > a[class='btn player-selection__link ']")
  option$clickElement()
  Sys.sleep(3)
  
  #We scroll down to rankings table
  remDr$executeScript("window.scrollTo(0,900);")
  Sys.sleep(2)
  
  #We expand the rankings table
  select <- remDr$findElement(using = "css selector","span[class*='button']")
  select$clickElement()
  Sys.sleep(2)
  
  #We select the Year-end Rankings table
  option <- remDr$findElement(using = "css selector", "table.classic-table")
  
  #We scrape and unlist the table
  years <- option$getElementText() %>% unlist
  Sys.sleep(3)
  
  #We format the text
  list <- years%>%
    str_replace_all("\n", ",")%>%
    strsplit(",") %>%
    do.call(rbind, .) %>%
    as.vector(.)%>%
    .[-1:-2]
  
  #We store the rankings by year for the current player
  year <- c()
  ranking <- c()
  for (i in 1:(length(list)/2)){
    year <- append(year,list[2*i-1])
    ranking <- append(ranking,list[2*i])
  }
  
  new<-data.frame(name,year, ranking)
  data_test <- rbind(data_test,new)
  
  rm("new")
  
}


#We store the info for all women players

list_women_rankings <- data_test
#backup
list_women_rankings_bkp <- list_women_rankings


# Remove Hunt Louise, Whiley Jordanne, Vergeer Esther since they are from wheelchair tennis
list_women_rankings<-list_women_rankings[-which(list_women_rankings$name=="Hunt Louise"),] #15 rows
list_women_rankings<-list_women_rankings[-which(list_women_rankings$name =="Whiley Jordanne"),] #15 rows
list_women_rankings<-list_women_rankings[-which(list_women_rankings$name =="Vergeer Esther"),]


remDr$close()
rm("option", "remDr","data_test" )

#We define a list of sheets with bio info and rankings info
sheets <- list("Men_bis2015" = df_men_bis2015, "Men_ab2015" = df_men_ab2015, 
               "Women_bis2015"=df_women_bis2015, "Women_ab2015"= df_women_ab2015,
               "Men_rankings"=list_men_rankings, "Women_rankings"=list_women_rankings) #assume sheet1 and sheet2 are data frames
#we export it to excel file.
write_xlsx(sheets, "Tennis/Tennis_all.xlsx")


#We combine all dataset so far into men_tennis and women_tennis to deal with them easier.
men_tennis <- rbind(men_ab2015, Men_bis2015)
women_tennis <- rbind(women_ab2015, women_bis2015)

# ONLY AVAILABLE FOR MEN
# Finally we scrape the remaining information from another website, and get
# the players' birthdate, turned pro, best rank and best rank date
link <- "https://www.ultimatetennisstatistics.com/"

driver <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer,browser = c("chrome"), chromever =
                     system2(command = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                             args = "--version",
                             stdout = TRUE,
                             stderr = TRUE) %>%
                     stringr::str_extract(pattern = "(?<=Chrome )\\d+\\.\\d+\\.\\d+\\.") %>%
                     magrittr::extract(!is.na(.)) %>%
                     stringr::str_replace_all(pattern = "\\.",
                                              replacement = "\\\\.") %>%
                     paste0("^",  .) %>%
                     stringr::str_subset(string =
                                           binman::list_versions(appname = "chromedriver") %>%
                                           dplyr::last()) %>%
                     as.numeric_version() %>%
                     max() %>%
                     as.character())
remDr <- driver[["client"]]
# Before navigating to the link, we create empty vectors for age, best rank, date of best rank, start date
age <- c()
rank <- c() 
start_date <- c()
rank_date <- c()
rows <- c()
#we create a data frame for the additional info that we will scrape now
men_tennis_add <-data.frame(name = character(),
                            name2 = character(),
                            age = character(),
                            start_date= character(),
                            rank = character(),
                            rank_date = character()
) 
# We navigate to the link
remDr$navigate(link)

# Using the list of men players, we scrape one by one by searching their names
for (i in 1:length(men_tennis$player_name)){
  
  ## We click on "Search Player"
  option <- remDr$findElement(using = "css selector", "input[id='player']")
  option$clickElement()
  
  ## We enter the i-th name from the list
  name <- men_tennis$player_name[i]
  option$sendKeysToElement(list(name))
  Sys.sleep(5)
  
  ## We press down and enter to choose the name from the dropdown
  option$sendKeysToElement(list("\ue015","\ue007"))
  Sys.sleep(5)
  
  ## We store the name here in name2 to make sure it's the correct name by comparing to
  ## the original name later
  option <- remDr$findElement(using = "css selector", "h3:nth-of-type(1)")
  name2 <- option$getElementText()
  
  ## We store the age info
  option <- remDr$findElement(using = "css selector", "div[class = 'col-md-4 col-lg-3']:nth-of-type(1) table tr:nth-of-type(1)")
  age <- option$getElementText()
  
  ## We store the best rank info
  option <- remDr$findElements(using = "xpath", "//th[text()='Best Rank']//..//td")
  rank <- ifelse(length(option)!= 0,unlist(lapply(option, function(x) x$getElementText())),NA)
  
  ## We store the date of best rank info
  option <- remDr$findElements(using = "xpath", "//th[text()='Best Rank']//..//td /a")
  rank_date <- ifelse(length(option)!= 0,unlist(lapply(option, function(x) x$getElementText())),NA)
  
  ## We store the date of start (turned pro) info
  option <- remDr$findElements(using = "xpath", "//th[text()='Turned Pro']//..//td")
  start_date <- ifelse(length(option)!= 0,unlist(lapply(option, function(x) x$getElementText())),NA)
  
  ## We update the blanks as NA and bind the vectors to our data frame
  age <- ifelse(length(age) == 0, NA, age)
  name2 <- ifelse(length(name2) == 0, NA, name2)
  rank <- ifelse(length(rank) == 0, NA, rank)
  rank_date <- ifelse(length(rank_date) == 0, NA, rank_date) %>% unlist()
  start_date <- ifelse(length(start_date) == 0, NA, start_date)  %>% unlist()
  rows <- cbind(name, name2, age,start_date, rank,rank_date)
  men_tennis_add <- rbind(men_tennis_add, rows)
}

# We close the connection
remDr$close()
rm("option", "remDr")


# For tennis women birthdates, we use a csv file since we don't have the same website we used for men.
## We define the url of csv file and read & store it.

urlfile="https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_players.csv"
tennis_all<-read_csv(url(urlfile))

## We arrange the format of the names
tennis_all <- tennis_all %>%
  mutate(player_name = paste0(name_last, " ",name_first))
## We merge the biography info of women with the previous dataset women_tennis (with names and retirement dates)
## and we remove the tennis_all df.
women_tennis_bio <- merge(women_tennis, tennis_all, by = "player_name" , all.x = TRUE)
rm(tennis_all)

## We deal with NAs and we format the date of birth ("dob") column.
women_tennis_bio$dob <- ifelse(!is.na(women_tennis_bio$dob), paste0(substr(women_tennis_bio$dob,7,8), "-",
                                                                    substr(women_tennis_bio$dob,5,6),
                                                                    "-",substr(women_tennis_bio$dob,1,4)), NA)
women_tennis_bio$dob <- as.Date(women_tennis_bio$dob, "%d-%m-%Y")

## We create an age column using dob.
women_tennis_bio <- women_tennis_bio %>%
  mutate(age = floor(as.numeric(difftime(Sys.Date(),dob, units = "weeks"))/52.25))

## We get the best rank for each female player from the women_rankings df which was scraped before.
women_best_rank <- women_rankings %>%
  group_by(name) %>%
  slice(which.min(ranking))
## We arrange the data frame, rename the columns and finalize the data for women tennis 
## EXCEPT START DATE
colnames(women_best_rank) <- c("player_name", "date_rank", "best_rank")
women_tennis_bio <- merge(women_tennis_bio,women_best_rank,  by = "player_name" , all.x = TRUE )
names(women_tennis_bio)[names(women_tennis_bio)=="dob"] <- c("birthdate")
women_tennis_visuals <- women_tennis_bio[, c("player_name","nation", "retirement_date", "age", "birthdate", "best_rank", "date_rank")]
write_xlsx(women_tennis_visuals,"SS22/Seminar SportsData/SeminarSports/Tennis/women_tennis_visuals.xlsx")

# We arrange the data for men tennis players
## Reformatting some names that were scraped without space between the names 

men_tennis_add$name2 <- sub("(\\w+)\\s(\\w+)","\\2 \\1", men_tennis_add$name2)
men_tennis_add[men_tennis_add$name=="EndaraRosales Ivan","name2"] <- "EndaraRosales Ivan"
men_tennis_add[men_tennis_add$name=="Felix Jesus","name2"] <- "Felix Jesus"
men_tennis_add[men_tennis_add$name=="LanzoniNetto Americo","name2"] <- "LanzoniNetto Americo"
men_tennis_add[men_tennis_add$name=="Podipnik-Castillo Hans","name2"] <- "Podipnik-Castillo Hans"

## Merging all info we have for men, and renaming columns in the same manner done for women
colnames(men_tennis_add) <- c("org_name", "player_name", "age_org", "start_date_org" ,"best_rank_org", "date_rank")
men_tennis_full <- merge(men_tennis, men_tennis_add, by = "player_name" , all.x = TRUE)

## Extracting correct info from the scraped columns
men_tennis_full$best_rank <- substr(men_tennis_full$best_rank_org,1, nchar(men_tennis_full$best_rank_org)-12)
men_tennis_full$birthdate <- substr(men_tennis_full$age_org,nchar(men_tennis_full$age_org)-10, nchar(men_tennis_full$age_org)-1)
men_tennis_full$age <- substr(men_tennis_full$age_org,5, nchar(men_tennis_full$age_org)-12)

## Dealing with NAs and defining proper NA values
men_tennis_full$birthdate <- ifelse(men_tennis_full$birthdate == "" | men_tennis_full$birthdate == "N" , NA,men_tennis_full$birthdate)
men_tennis_full$best_rank <- ifelse(men_tennis_full$best_rank == "" | men_tennis_full$best_rank == "N" , NA,men_tennis_full$best_rank)
men_tennis_full$date_rank <- ifelse(is.na(men_tennis_full$date_rank)| men_tennis_full$date_rank == "NA", NA,men_tennis_full$date_rank)
men_tennis_full$age <- ifelse(men_tennis_full$age == "" | men_tennis_full$age == "N" , NA,men_tennis_full$age)

men_tennis_full$date_rank <- unlist(men_tennis_full$date_rank)
men_tennis_full$start_date_org <- unlist(men_tennis_full$start_date_org)

## Finalizing the men tennis dataset
men_tennis_visuals <- men_tennis_full[,c(1:3,6,11:8)]

## Exporting data that will be used for visuals
write.csv(men_tennis_visuals,"SS22/Seminar SportsData/SeminarSports/Tennis/men_tennis_visuals", row.names = FALSE)
write_xlsx(men_tennis_visuals,"SS22/Seminar SportsData/SeminarSports/Tennis/men_tennis_visuals.xlsx")

## Start dates for women players come from Anna's code below.
  
### fehlenden career start bei Tennis Frauen scrapen ###

# Paket laden
library(rvest) # kann den html-Code auf Webseiten durchsuchen 
library(tidyverse) # Zeichenketten durchsuchen 

# Arbeitsverzeichnis setzen 
setwd("~/Documents/Dokumente - Annas MacBook Air/Studium/Statistik/Kurse/Sportdatenvisualisierung")

# Datensatz mit Zwischenstand laden 
all_anna <- read.xlsx("Data_full.xlsx")

## ein wenig Vorverarbeitung/Vereinheitlichung

#Datumsangaben auf das Jahr herunterbrechen
all_anna$birthdate <- as.numeric(str_extract(all_anna$birthdate, "[[:digit:]]{4}"))
all_anna$start_date <- as.numeric(str_extract(all_anna$start_date, "[[:digit:]]{4}"))
all_anna$retirement_date <- as.numeric(str_extract(all_anna$retirement_date, "[[:digit:]]{4}"))
all_anna$date_rank[which(str_detect(all_anna$date_rank, "[[:digit:]]{4}\\.[[:digit:]]+"))] <- round(as.numeric(all_anna$date_rank[which(str_detect(all_anna$date_rank, "[[:digit:]]{4}\\.[[:digit:]]+"))]))
all_anna$date_rank <- as.numeric(str_extract(all_anna$date_rank, "[[:digit:]]{4}"))

#Sportart als Faktor
all_anna$sport <- factor(all_anna$sport)

# Iceskating: nur Leute die bis 2019 aktiv waren (da wir bei denen in den Jahren 
# danach nicht sicher sind, ob sie noch aktiv sind)

all_anna[all_anna$sport_sub %in% c("Men - Iceskating", "Women - Iceskating"),] <- all_anna[which(all_anna$sport_sub %in% c("Men - Iceskating", "Women - Iceskating") & all_anna$retirement_date < 2020),] 

str(all_anna)

### fehlenden career start bei Tennis Frauen scrapen 

# Funktion um Webseiten der Athletinnen auf tennis-x aufrufen und Daten herunterziehen
tenniswomen <- function(name){ 
  nam <- strsplit(name, " ")
  nam <- paste(nam[[1]][2], nam[[1]][1], sep = "-")
  link <- paste0("https://www.tennis-x.com/playernews/", nam, ".php")
  start <- read_html(link) %>% html_nodes("td li:nth-child(6)") %>% 
    html_text()
  str_extract(start, "[[:digit:]]{4}")
}

## fehlende Athletinnen ausmachen 
# bug <- function(x,y){
#   for(i in y:length(all_anna[all_anna$sport_sub == "Tennis-Women", 1][-x])){
#     all_anna$start_date[all_anna$sport_sub == "Tennis-Women"][-x][i] <- tenniswomen(all_anna[all_anna$sport_sub == "Tennis-Women", 1][-x][i]) 
#     print(i)
#   }
# }
# 
# bug(x, 1)

# diese wurden entdeckt 
x <- c(3,4,12,14,17,18,21,22,23,25,26,28,30,33,36,38,40,43,46,47,48,49,51,52,55,59,
       73,74,76,77,78,80,82,84,85,89,95,97,100,102,104,105,108,109,110,111,112,114,
       115)

# Career start dates wurden von tennis-x.com mit Funktion fuer diese Athletinnen gefunden:
all_anna[all_anna$sport_sub == "Tennis-Women",1][-x]

# zum Datensatz hinzufuegen, Achtung, dauert ein bisschen 
for(i in 1:length(all_anna[all_anna$sport_sub == "Tennis-Women", 1][-x])){
  all_anna$start_date[all_anna$sport_sub == "Tennis-Women"][-x][i] <- tenniswomen(all_anna[all_anna$sport_sub == "Tennis-Women", 1][-x][i]) 
}

# fuer diese Athletinnen wird haendisch versucht, die richtigen Webseiten zu finden
all_anna[all_anna$sport_sub == "Tennis-Women",1][x]

# haendisch gefunden auf x-Tennis:
all_anna[x[10],7] <- "1989"
all_anna[x[11],7] <- "1994"
all_anna[x[11],1] <- "Dechy Nathalie"
all_anna[x[14],7] <- "2005"
all_anna[x[27],7] <- "1998"
all_anna[x[30],7] <- "1999"
all_anna[x[30],1] <- "Martina Muller"
all_anna[x[34],7] <- "2000"

# "start_date" numerisch machen 
all_anna$start_date <- as.numeric(all_anna$start_date)

# speichern, auch als Excel
save(all_anna, file = "full_anna.RData")
write.xlsx(all_anna, "full_anna.xlsx")



  
  

