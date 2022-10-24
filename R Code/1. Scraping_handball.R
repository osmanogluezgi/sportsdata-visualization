library(dplyr) #to use the pipes, and data manipulation
library(rvest) #for data scraping
library(RSelenium) #for data scraping
library(writexl) #for exporting to excel  
library(strsplit) #text manipulation
library(stringr) #text manipulation

# Handball
# First we get the list of names of the hockey players who are retired.
link_handball_retired <- "https://www.handball-base.com/players?name=&club=&nationality=&league=&position=&sort=&status=retired&p="

names_handball <- c()
#We get all the names from 3 pages, thus for loop from 1 to 3.  
for (i in 1:3){ 
  names_handball_i <- read_html(paste0(link_handball_retired,i)) %>%
  html_nodes("div.textWrapper p.title")%>%
  html_text()
  names_handball <- append(names_handball, names_handball_i)
  }

# We start the RSelenium (this was the only way I could run the code without error, i guess
# it's related to the Mac Chrome version, that's why the input for rsDRiver is this long).
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

# We close the cookies pop-up
option <- remDr$findElement(using = "css selector", "a.yes")
option$clickElement()

# We create a new dataset to store the info
data_test_hb <- c()

#We go one by one over the names list and update the url for the specific player.
for(i in (1:3)){
  link_handball_details <-  paste0(link_handball_retired,i)
  remDr$navigate(link_handball_details)
  Sys.sleep(3)
  #In each page, there are 40 players, so we click one by one on the players.
  for(j in (1:40)){
    option <- remDr$findElement(using = "css selector", paste0("div.playersWrapper > a:nth-of-type(", j , ")"))
    option$clickElement()
    Sys.sleep(3)
    # We store the name of the current player we're scraping data for.
    name <- names_handball[j]

    #We get the first table that includes biograpical info.
    option <- remDr$findElement(using = "css selector", ".clubLeftWrapper > .clubDataWrapper:nth-of-type(1)")
    Sys.sleep(3)

    # We get info of nation, birthdate, weight, height and position in bio dataset.
    bio <- option$getElementText() %>% unlist
    Sys.sleep(3)

    #Formatting the scraped data
    list_bio <- bio%>%
      str_replace_all("\n", ",")%>%
      strsplit(",") %>%
      do.call(rbind, .) %>%
      as.vector(.)%>%
      .[-1]

    Sys.sleep(3)

    #We get the second table that includes career info.
    option <- remDr$findElement(using = "css selector", ".clubRightWrapper > .clubDataWrapper:nth-of-type(2)")
    past <- option$getElementText() %>% unlist
    Sys.sleep(3)

    #Formatting the scraped data
    list_past <- past%>%
      str_replace_all("\n", ",")%>%
      strsplit(",") %>%
      do.call(rbind, .) %>%
      as.vector(.)%>%
      .[-1]
  
    Sys.sleep(3)
    #We get info of first season and last season in list_past data set.
    
    nation <- list_bio[2]
    birthdate <- list_bio[4]
    height <- list_bio[6]
    weight <- list_bio[8]
    position <- list_bio[10]
    last_season <- list_past[2]
    first_season <- tail(list_past, n=1)
    
    Sys.sleep(3)
    #We store the info of the current player before moving to the next one
    new_hb<-data.frame(name,nation, birthdate, height, weight, position, first_season, last_season)
    data_test_hb <- rbind(data_test_hb,new_hb)
    Sys.sleep(3)

    rm("name","nation", "birthdate", "height", "weight", "position", "first_season", "last_season")
    }

  remDr$close()
  rm("option", "remDr" )
  }
# Cleaning and finalizing the data set of retired players bio
rownames(data_test_hb) <- 1:nrow(data_test_hb)
data_test_hb$birthdate <- as.Date(data_test_hb$birthdate, "%d.%m.%Y.")

data_test_hb[!grepl("kg", data_test_hb$weight, fixed = TRUE),6] <- 
  data_test_hb[!grepl("kg", data_test_hb$weight, fixed = TRUE),5]

data_test_hb[!grepl("kg", data_test_hb$weight, fixed = TRUE),6] <- 
  data_test_hb[!grepl("kg", data_test_hb$weight, fixed = TRUE),5]

data_test_hb[!grepl("cm", data_test_hb$height, fixed = TRUE),6] <- 
data_test_hb[!grepl("cm", data_test_hb$height, fixed = TRUE),4]


data_test_hb[!grepl("kg", data_test_hb$weight, fixed = TRUE),5] <- NA
data_test_hb[!grepl("cm", data_test_hb$height, fixed = TRUE),4] <- NA

# We rename the data set cleaner, to indicate it stores biography info of players.
handball_bio <- data_test_hb

# Now we scrape information for players' success statistics.
#Erfolge

#Cleaning up the names to get standardized urls later
names_handball <- gsub("Ø", "o",names_handball)
names_handball <- gsub("ø", "o",names_handball)
names_handball <- gsub("ð", "d",names_handball)
names_handball <- gsub("ć", "c",names_handball)
names_handball <- gsub("Ł", "L",names_handball)
names_handball <- gsub("ã", "a",names_handball)
names_handball <- gsub("č", "c",names_handball)
names_handball <- gsub("ň", "n",names_handball)
names_handball <- gsub("ý", "y",names_handball)
names_handball <- gsub("ł", "l",names_handball)
names_handball <- gsub("ń", "n",names_handball)

# We use new website for success statistics, and we go through the names list (except
# few that don't exist in the website which are excluded in the for loop)

link_success <- "https://www.playmakerstats.com/"
remDr$navigate(link_success)

for (i in (1:length(names_handball))[-c(1,9,16,33,57)]){

# We click on the search bar  
  option <- remDr$findElement(using = "css selector", "div.zz-header-search input")
  option$clickElement()
  Sys.sleep(3)

# We feed the name to the search bar  
  name <- names_handball[i]
  option$sendKeysToElement(list(name))
  Sys.sleep(3)

# We click to search  
  option<-remDr$findElement(using = "css selector", " div[id='searchresults'] a")
  option$clickElement()
  Sys.sleep(3)

# We store the urls with success information and the name of the corresponding player
  url <- remDr$getCurrentUrl()
  rows <- cbind(name,url)
  success_urls <- rbind(success_urls, rows)
}
  
# Final trivial modifications
rows <- cbind(name,erfolge)
success_handball <- rbind(success_handball, rows)
Sys.sleep(3)

remDr$close()
rm("option", "remDr" )

# We go over the urls one by one and get the necessary information and store it
# handball success data set.
success_urls_result <- gsub("jogador", "player_seasons",success_urls[,2])
handball_succes <- data.frame()

# We start from 12 due to unimportant info in the first 11 rows (due to scraping format)
for (i in 12:length(success_urls_result)){
handball_success_i <- success_urls_result[i] %>%
  read_html() %>%
  html_node("div[id='team_games']:nth-of-type(1) table") %>%
  html_table() 

name <- data.frame(unlist(rep(success_urls[i,1], nrow(handball_success_i))))
colnames(name) <- "name"
handball_success_i <- data.frame(name, handball_success_i)
handball_succes <- rbind(handball_succes, handball_success_i)

}

Handball_success_final <- data.frame(rbind(Handball_success_1, Handball_success_2))
Handball_success_final <- Handball_success_final[,-c(2,5,8)]

# We filter out the rows with NULL values to have a cleaner data set.
handball_stats <- Handball_success_final %>%
  filter(G!="-"& GS != "-") %>%
  filter(Season != "" & Club != "")

# Renaming columns
colnames(Handball_stats) <- c("name", "season", "club" ,"games", "goalsscored")

#Final Dataset, removing special characters for standardization
handball_bio[,1] <- gsub("Ø", "o",handball_bio[,1])
handball_bio[,1] <- gsub("ø", "o",handball_bio[,1])
handball_bio[,1] <- gsub("ð", "d",handball_bio[,1])
handball_bio[,1] <- gsub("ć", "c",handball_bio[,1])
handball_bio[,1] <- gsub("Ł", "L",handball_bio[,1])
handball_bio[,1] <- gsub("ã", "a",handball_bio[,1])
handball_bio[,1] <- gsub("č", "c",handball_bio[,1])
handball_bio[,1] <- gsub("ň", "n",handball_bio[,1])
handball_bio[,1] <- gsub("ý", "y",handball_bio[,1])
handball_bio[,1] <- gsub("ł", "l",handball_bio[,1])
handball_bio[,1] <- gsub("ń", "n",handball_bio[,1])
handball_bio[,1] <- gsub("ú", "ú",handball_bio[,1])
handball_bio[,1] <- gsub("í", "i",handball_bio[,1])
handball_bio[,1] <- gsub("á", "a",handball_bio[,1])
handball_bio[,1] <- gsub("ó", "a",handball_bio[,1])
handball_bio[,1] <- gsub("ó", "a",handball_bio[,1])
handball_bio[,1] <- gsub("  ", " ",handball_bio[,1])


#Exporting the bio and success data
write.csv(names_handball, file= "Handball/names_handball.csv")
write_xlsx(handball_bio, "Handball/handball_bio.xlsx")
write_xlsx(Handball_stats, "Handball/handball_stats.xlsx")

#We format the columns 
handball$start_date <- as.Date(substr(handball$first_season,8,11), format="%Y")
handball$end_date <- as.Date(substr(handball$last_season,13,16), format="%Y")
handball$birthdate <- as.Date(handball$birthdate, format = "%Y-%m-%d")
handball$age_start <- as.numeric(difftime(handball$start_date,handball$birthdate, unit = "weeks"))/52.25
handball$age_end <- as.numeric(difftime(handball$end_date,handball$birthdate, unit = "weeks"))/52.25
handball$length_career <- handball$age_end-handball$age_start

#We manually update some columns to get rid of special characters
handball_stats[,1] <- replace(handball_stats[,1], handball_stats[,1]=="Gudjón Valur Sigurdsson", "Gudjan Valur Sigurdsson")
handball_stats[,1] <- replace(handball_stats[,1], handball_stats[,1]=="Raúl Entrerríos", "Raúl Entrerrios")
handball_stats[,1] <- replace(handball_stats[,1], handball_stats[,1]=="Víctor Tomás", "Victor Tomas")

#We add a success info "goalpergame" to the data frame
handball_stats <- handball_stats%>%
  mutate(goalpergame = as.numeric(GS)/as.numeric(G))

#We get the date for best success for each player (date where the goalpergame is maximum)
handball_best_date <- handball_stats %>%
  group_by(name) %>%
  slice(which.max(goalpergame))
#We store the necessary info to be merged with bio info later and rename the columns
handball_best_date <- handball_best_date[,c(1,2,6)]
colnames(handball_best_date) <- c("name", "date_rank", "best_rank")

#We merge the success and bio info, by joining on name
handball_final <- merge(handball,handball_best_date,  by = "name" , all.x = TRUE )
#We format the date column and finalize the data frame 
handball_final$date_rank <- as.Date(substr(handball_final$date_rank,1,4), format="%Y")

#We export the data to be merged in final set
write.csv(handball_final, file= "Handball/handball_final.csv")



