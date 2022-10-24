# Sports Data Scraping and Visualization
A data scraping and visualization project on sports data  

## Table of contents
* [Introduction](#introduction)
* [Scraping](#scraping)
* [Dataset](#dataset)
* [Key Findings and Visuals](#key-findings-and-visuals)

## Introduction
This project is prepared for the class "Sports Data Visualization" at TU Dortmund, with the collaboration of students of Data Science and Journalism. It aims to find a relationship between the length of an athlete's career and the success gained throughout one's career until retirement while searching for a changing pattern of a specific age range where the success is attained based on each sport. The analysis is on the data of the retired athletes of *handball, tennis, motorrace and ice skating* and looks for evidence for the hypotheses below;

1. The later the age of success, the longer the career of an athlete
2. The earlier athletes start their career, the earlier they retire
3. There is an individual age for each sport, where athletes have the most success (the age is lower for sports like ice skating in comparison to motorrace)

## Scraping
* Handball: The data set is created by scraping the websites https://www.handball-base.com and "https://www.playmakerstats.com/. Using the names of the athletes, the data was scraped using RSelenium, The first source is used to get the players' **name**, **nation**, **birthdate**, **first and last season** they played. The second source is used to get their success history, which are the data fields such as **number of goals scored**, **number of games played**,and **the respective season** which are used to create the **best rank** and **date_rank** variables.
  - **best rank**: The maximum average goals per game value calculated as **goals scored**/**games in season**) in seniors club games
* Motorrace: The data set is created by scraping the website https://fiaresultsandstatistics.motorsportstats.com, and since no information is available if the players are active or retired, the scraped data is filtered using the last event date they joined. The racers with a last event between *2000 and 2019* are considered retired.
  - **best rank**: Best position in championships (FIA European Championship, FIA Sportscar Championship etc.)
* Tennis: The list of retired tennis players (both male and female) are downloaded from https://antidoping.itftennis.com/antidoping/news/retired-players.aspx , however *the website is not accessible* anymore. Importing the pdfs as a data set, a list of names of the retired players are created, and the information about the players' bio and successes are scraped from https://www.itftennis.com/en/players/ (both men and women) and https://www.ultimatetennisstatistics.com/ (available only for men). 
  - For women, the website https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_players.csv (https://github.com/JeffSackmann/tennis_wta) is used to to get birthdate information. Finally, from https://www.tennis-x.com/playernews/ is used to get women players' career start dates. 
## Dataset
The final dataset SportsData_Dataset.csv is as below;
Column |Description|
--- | --- | 
player_name |name of the player |
nation | nation of the player (ISO alpha-3 abbreviations) |
sport | sport type (Iceskating, Tennis, Motorrace or Handball)|
sport_sub | sport and gender information (e.g. Tennis- Men, Handball-Men etc.) |
age | age of the player |
birthdate | birth year |
nation | nation of the player (ISO alpha-3 abbreviations) |
start_date | year of career start |
retirement_date | year of career end |
best_rank | best rank/success of the player |
date_rank | year of best rank achieved |
age_success | date_rank-birthdate |
start_age | start_date - birthdate |
retire_age | retirement_date - birthdate |
sex | male/female |

## Key Findings and Visuals
* As expected, the age where the athletes are the most successful changes between sports. 
  - For ice skating, the average age of success is lower than other sports, especially for females. 
  - For motorrace, the average age of success is the highest and it should be noted that the retirement age is also the highest which partly explains higher success ages.
* Comparison of mean age of success between sports shows that there is a unique age of success for each sport. We could confirm this by ANOVA test, which checks whether there is a difference in the means of the groups for success age.
* We were able to confirm our hypotheses "The later the age of success, the longer the career of an athlete." for all sports except motorrace. 
  - To check the relationship better, we focused on the athletes who achieves their peak after 30 (age_success >=30) and there we saw an inverse significant relationship for motorracers, where the later comes the success, the shorter the career length.
<p align="middle">
  <img src="https://user-images.githubusercontent.com/77609842/197524797-55a7f3a0-3bef-40ba-b6f1-d59c07b5e27d.png" width="70%" />
  <img src="https://user-images.githubusercontent.com/77609842/197524724-6b5facc7-f8c6-4c14-9f28-9113e39b2840.png" width="70%" />
</p>
