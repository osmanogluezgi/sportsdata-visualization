library(dplyr) #to use the pipes, and data manipulation
library(rvest) #for data scraping
library(RSelenium) #for data scraping
library(writexl) #for exporting to excel
library(strsplit) #text manipulation
library(stringr) #text manipulation

# Main page for Motor Racers we use
link_motor <- "https://fiaresultsandstatistics.motorsportstats.com/drivers?filterIds=Event%20Winner&page="

# We define empty vectors to store names, countries, birth dates and last event of the players.
names<- c()
countries <- c()
birthdates <- c()
last_event <- c()

# We navigate through 54 pages and scrape names, countries, birthdates and last events 
# of the all available players
for (i in 1:54){
  names_i <- read_html(paste0(link_motor,i)) %>%
    html_nodes("div._3Jak- a.pxtAj")%>%
    html_text()
  countries_i <- read_html(paste0(link_motor,i)) %>%
    html_nodes("div._1jDUu div._3QDet:nth-child(2)")%>%
    html_text()
  birthdates_i <-  read_html(paste0(link_motor,i)) %>%
    html_nodes("div._1jDUu div._3QDet:nth-child(1)")%>%
    html_text()
  last_event_i <-  read_html(paste0(link_motor,i)) %>%
    html_nodes("div._1jDUu div._3QDet:nth-child(3)")%>%
    html_text()
  names <- append(names, names_i)
  countries <- append(countries, countries_i)
  birthdates <- append(birthdates, birthdates_i)
  last_event <- append(last_event,last_event_i)
}

# Converting names vector to data frame for easy manipualation
data1<-data.frame(names)

# Removing NAs and storing the birthdates in a column in the data frame
birthday_mod <- birthdates
birthday_mod[!grepl("Date of Birth", birthdates, fixed = TRUE)]<-NA
data1$birthdate <- birthday_mod

# Removing NAs and storing the countries in a column in the data frame
# Due to missing values, some countries are stored in birthdates vector while scraping, thus
# we check that vector as well and store it correctly
country_mod_bd <- birthdates
country_mod_bd[!grepl("Country", birthdates, fixed = TRUE)]<-NA
country_mod_c <- countries
country_mod_c[!grepl("Country", countries, fixed = TRUE)]<-NA
data1$country <- do.call(pmax, c(data.frame(country_mod_bd,country_mod_c), list(na.rm=TRUE)))


# Removing NAs and storing the last events in a column in the data frame
# Due to missing values, some countries are stored in countries vector while scraping, thus
# we check that vector as well and store it correctly
lastevent_mod_c <- countries
lastevent_mod_c[!grepl("Last Event", countries, fixed = TRUE)]<-NA
lastevent_mod_l <- last_event 

ind<-which(!(is.na(lastevent_mod_c)))
ind_bd <- which((is.na(lastevent_mod_c)))

for(i in 1:(length(ind))) {
  lastevent_mod_l <- append(lastevent_mod_l, lastevent_mod_c[ind[i]], after=(ind[i]-1))
}

data1$last_event <- lastevent_mod_l

# Cleaning and formatting the text data
data1$birthdate<- gsub("Date of Birth","",as.character(data1$birthdate))
data1$country<- gsub("Country","",as.character(data1$country))
data1$last_event<- gsub("Last Event","",as.character(data1$last_event))
data1$birthdate <- as.Date(data1$birthdate, "%b %d, %Y")
data1 <- data1 %>% extract(last_event, c('place_last_event', 'date_last_event'),"([^,]+), ([^)]+)")

#Exporting the motorrace data to csv and excel files
write.csv(data1,"motorrace.csv", row.names = FALSE)
write_xlsx(data1,"motorrace.xlsx")

#Filtering to retired racers by assumption of last event
retired_motorrace <- motorrace %>%
  filter(date_last_event <=2019 & date_last_event >=2000)

#Renaming the columns
colnames(retired_motorrace) <- c("name","birthdate", "country", "place_last_event", "date_last_event")


#Updating the special characters

names_motorrace <- gsub(" ", "-",tolower(retired_motorrace$name))
names_motorrace <- gsub("'", "-",names_motorrace)
names_motorrace <- gsub("é", "e",names_motorrace)
names_motorrace <- gsub("ä", "a",names_motorrace)
names_motorrace <- gsub("á", "a",names_motorrace)
names_motorrace <- gsub("ã", "a",names_motorrace)
names_motorrace <- gsub("ș", "s",names_motorrace)
names_motorrace <- gsub("ö", "o",names_motorrace)
names_motorrace <- gsub("ï", "i",names_motorrace)
names_motorrace <- gsub("ñ", "n",names_motorrace)
names_motorrace <- gsub("ü", "u",names_motorrace)
names_motorrace <- gsub("í", "i",names_motorrace)
names_motorrace <- gsub("ç", "c",names_motorrace)
names_motorrace <- gsub("ô", "o",names_motorrace)
names_motorrace <- gsub("ú", "u",names_motorrace)
names_motorrace <- gsub("š", "s",names_motorrace)
#to remove jr. sr. to jr sr
names_motorrace <- gsub("\\.", "",names_motorrace)

# Removing racers with no data available
remove <- c("dani-clos", "davide-valsecchi","gary-eastwood", "luiz-razia","tamas-pal-kiss", "sascha-maassen")
indices <- which(names_motorrace %in% remove)

#creating final dataset that includes success information
motorrace_final <- data.frame(name = character(),
                              birthdate = character(),
                              nation = character(),
                              age = character(),
                              champ_name = character(),
                              active_years = character(),
                              best_champ_pos = character()
)
age <- c()
nation <- c()
birthdate <- c()
name <- c()
rank_active_period <- c()
rows<- c()

#Scraping success information for racers
for (i in (1:length(retired_motorrace$name))[-indices]){
  link <- paste0("https://fiaresultsandstatistics.motorsportstats.com/drivers/",names_motorrace[i],"/series")
  name <- retired_motorrace$name[i]
  birthdate <- read_html(link) %>%
    html_nodes("div._1d5IF:nth-of-type(2) div._1ei5x:nth-child(1) ._3wj-5")%>%
    html_text()
  age <- read_html(link) %>%
    html_nodes("div._1d5IF:nth-of-type(2) div._1ei5x:nth-child(2) ._3wj-5")%>%
    html_text()
  nation <- read_html(link) %>%
    html_nodes("div._1d5IF:nth-of-type(3) div._1ei5x:nth-child(2) ._3wj-5")%>%
    html_text()
  rank_active_period <- read_html(link) %>%
    html_node("div._3p983 > :nth-child(3) table") %>%
    html_table()
  age <- ifelse(length(age) == 0, NA, age)
  birthdate <- ifelse(length(birthdate) == 0, NA, birthdate)
  nation <- ifelse(length(nation) == 0, NA, nation)
  rows <- cbind(name, birthdate, nation, age, rank_active_period)
  motorrace_final <- rbind(motorrace_final, rows)
}

# Combining bio and success data
motorrace_full <- merge(retired_motorrace, motorrace_2, by = "name", all.x = TRUE)
motorrace_full <- motorrace_full%>%
  mutate(first_event = substr(motorrace_full$`Active Years`,1,4),
         bestpos = substr(motorrace_full$`Best Champ. Pos.`,1,2),
         bestpos_date = "")

# Remove all the Space 

motorrace_full$bestpos <- gsub('\\s+', '', motorrace_full$bestpos)
motorrace_full$bestpos_date <- substr(motorrace_full$`Best Champ. Pos.`,nchar(motorrace_full$bestpos)+3,nchar(motorrace_full$bestpos)+6)

#Final dataset to use for visuals
motorrace_visuals <- motorrace_full[,c(1:5,8,12:14)]

#Export
write.csv(motorrace_final,"motorrace_2.csv", row.names = FALSE)
write_xlsx(motorrace_final,"motorrace_2.xlsx")


write.csv(motorrace_full,"SS22/Seminar SportsData/SeminarSports/Motorrace/motorrace_full.csv", row.names = FALSE)
write_xlsx(motorrace_full,"SS22/Seminar SportsData/SeminarSports/Motorrace/motorrace_full.xlsx")

write.csv(motorrace_visuals,"SS22/Seminar SportsData/SeminarSports/Motorrace/motorrace_visual.csv", row.names = FALSE)
write_xlsx(motorrace_visuals,"SS22/Seminar SportsData/SeminarSports/Motorrace/motorrace_visual.xlsx")


