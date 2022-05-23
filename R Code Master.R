####### Code for Covid-19 and Trust
#### Install
install.packages("interactions") 
install.packages("moments") 
install.packages("psych") 
install.packages("readtext") 
install.packages("hrbrthemes")
install.packages("sjPlot")

###### Library  
library(tidyverse) 
library(plm) 
library(lfe) 
library(lmtest) 
library(car) 
library(geepack) 
library(hrbrthemes) 
library(dplyr) 
library(lubridate) 
library(stargazer) 
library(jtools) 
library(readr) 
library(readxl) 
library(interactions) 
library(moments) 
library(psych) 
library(MASS) 
library(httr) 
library(jsonlite) 
library(readtext) 

library(sjPlot)
library(sjmisc)

###### Datasets


##### Trust
Trust_Regions <- read_excel("C:/Users/bjarteos/OneDrive - Universitetet i Oslo/Trust Regions.xlsx") 
names(Trust_Regions) 
## Note all Norwegian letters are replaced to prevent issues with R competabillety


#### Out World In Data
owid_covid_data_2_ <- read_csv("C:/Users/bjarteos/OneDrive - Universitetet i Oslo/owid-covid-data (2).csv") 


## Datasett with Norway, Sweden, Denmark and Unteid States 
DataN <- filter(owid_covid_data_2_, location == "Norway") 
DataS <- filter(owid_covid_data_2_, location == "Sweden") 
DataD <- filter(owid_covid_data_2_, location == "Denmark") 
DataU <- filter(owid_covid_data_2_, location == "United States") 


# Selectin varibles 
DataN2 = dplyr::select(DataN, date, weekly_hosp_admissions, stringency_index, total_vaccinations_per_hundred, new_cases) 




#### Health Data from HD
#curl -v -X GET "https://api.helsedirektoratet.no/ProduktCovid19/Covid19statistikk/helseforetak" -H "Ocp-Apim-Subscription-Key: 069641eec337417ba71b19a245ec0030" --data-ascii "{body}" -o "output.html"  

Helse <- readtext("C:/Users/bjarteos/OneDrive - Universitetet i Oslo/output.html") 
Helse$text 
Helse2 <- fromJSON(Helse$text) 

names(Helse2) 
## Untangeling
Helse3 <- Helse2 %>% 
  unnest(covidRegistreringer) 
summary(Helse3$helseforetakNavn) 

## Combining into regions
Helse3$Dummys <- ifelse(Helse3$helseforetakNavn == 'Helse MÃ¸re og Romsdal',5, 0)+ 
  ifelse(Helse3$helseforetakNavn == 'Helse Nord-TrÃ¸ndelag', 5, 0)+ 
  ifelse(Helse3$helseforetakNavn == "St. Olavs hospital ", 5, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Finnmarkssykehuset HF", 6, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Helgelandssykehuset HF", 6, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Nordlandssykehuset HF", 6, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Universitetssykehuset Nord-Norge HF", 6, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Helse Bergen HF + Privat med avtale med Helse Vest i Bergen", 4, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Helse Fonna HF", 4, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Helse FÃ¸rde HF", 4, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Helse Stavanger HF", 4, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Sykehuset Innlandet HF", 2, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Sykehuset i Vestfold HF", 3, 0)+ 
  ifelse(Helse3$helseforetakNavn == "SÃ¸rlandet sykehus HF", 3, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Sykehuset Telemark HF", 3, 0)+   
  ifelse(Helse3$helseforetakNavn == "Oslo universitetssykehus HF", 1, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Private med avtale med Helse SÃ¸r-Ã˜st i Oslo", 1, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Akershus universitetssykehus HF", 1, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Sunnaas Sykehus HF", 1, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Sykehuset Ã˜stfold HF", 1, 0)+ 
  ifelse(Helse3$helseforetakNavn == "Vestre Viken HF", 1, 0) 


#### Combining Datasets
Trust_Regions$date <- make_datetime(year = Trust_Regions$Year) + weeks(Trust_Regions$Week) 


## Giving week and year values 
DataN2$Week <- week(DataN2$date) 
DataN2$Year <- year(DataN2$date) 

Sweek <- DataN2 %>% 
  group_by(Week, Year) %>% 
  summarise( 
    String = mean (stringency_index, na.rm = TRUE), 
    Hosp = mean (weekly_hosp_admissions, na.rm = TRUE), 
    Vac = mean (total_vaccinations_per_hundred, na.rm = TRUE), 
    New = mean (new_cases, na.rm = TRUE)) 



## Joining datasets 
NewNorway <- left_join(Trust_Regions, Sweek, by = c("Year","Week")) 

### Dummy for regions 
NewNorway$Dummys = ifelse(NewNorway$Region == 'Oslo og Viken', 1, 0)+ 
  ifelse(NewNorway$Region == 'Innlandet', 2, 0)+ 
  ifelse(NewNorway$Region == 'Agder og Sor-ostlandet', 3, 0)+ 
  ifelse(NewNorway$Region == 'Vestlandet', 4, 0)+ 
  ifelse(NewNorway$Region == 'Trondelag', 5, 0)+ 
  ifelse(NewNorway$Region == 'Nord Norge', 6, 0) 



## Adding in HD data
Helse3$Week <- week(Helse3$dato) 
Helse3$Year <- year(Helse3$dato) 

Sweek3 <- Helse3 %>% 
  group_by(Week, Year, Dummys) %>% 
  summarise( 
    Hosp2 = mean(antInnlagte, na.rm = TRUE)) 

NewNorway <- left_join(NewNorway, Sweek3, by = c("Year","Week","Dummys")) 



# Dealing with missing
NewNorway = NewNorway %>% fill(Hosp2, .direction = 'up') 
NewNorway = NewNorway %>% fill(String, .direction = 'up')
NewNorway = NewNorway %>% fill(String, .direction = 'down') 
NewNorway = NewNorway %>% fill(Hosp, .direction = 'up')
NewNorway = NewNorway %>% fill(Vac, .direction = 'up') 
DataN2 = DataN2 %>% fill(total_vaccinations_per_hundred, .direction = 'up') 


### Making a data sett with averge values each week

Comb = NewNorway %>% 
  group_by(Week, Year) %>% 
  summarise( 
    `1` = mean (`1`, na.rm = TRUE), 
    `2` = mean (`2`, na.rm = TRUE), 
    `3` = mean (`3`, na.rm = TRUE), 
    `4` = mean (`4`, na.rm = TRUE), 
    `5` = mean (`5`, na.rm = TRUE)) 

Comb$date <- make_datetime(year = Comb$Year) + weeks(Comb$Week) 
Comb$High <- Comb$`4`+ Comb$`5` 
Comb$Low <- Comb$`1`+ Comb$`2` 

Comb <- left_join(Comb, Sweek, by = c("Year","Week")) 
Comb <- Comb %>% arrange(date) 



#### Transforming Varibles

## Giving dummy to Phases 
NewNorway$Phases = ifelse(NewNorway$date > "2020-03-18", 1, 0)+ 
  ifelse(NewNorway$date > "2020-04-22", 1, 0)+ 
  ifelse(NewNorway$date > "2020-10-21", 1, 0)+ 
  ifelse(NewNorway$date > "2021-02-26", 1, 0)+ 
  ifelse(NewNorway$date > "2021-04-16", 1, 0)+ 
  ifelse(NewNorway$date > "2021-09-24", 1, 0)+ 
  ifelse(NewNorway$date > "2021-12-03", 1, 0)+ 
  ifelse(NewNorway$date > "2022-01-29", 1, 0) 



### Rally and Pandemic Fatigue 
NewNorway$RP = ifelse(NewNorway$date > "2020-03-18", 1, 0)+ 
  ifelse(NewNorway$date > "2020-04-22", 1, 0) +
  ifelse(NewNorway$date > "2022-01-29", 1, 0)
  

### Index of National and Regional Hosp numbers
NewNorway$Population <- ifelse(NewNorway$Region == 'Oslo og Viken', 1934659, 0)+ 
  ifelse(NewNorway$Region == 'Innlandet',371385, 0)+ 
  ifelse(NewNorway$Region == 'Agder og Sor-ostlandet',726627, 0)+ 
  ifelse(NewNorway$Region == 'Vestlandet',1322218, 0)+ 
  ifelse(NewNorway$Region == 'Trondelag', 438241, 0)+ 
  ifelse(NewNorway$Region == 'Nord Norge',475507, 0) 


NewNorway$Hindex <- NewNorway$Hosp + NewNorway$Hosp2 
NewNorway$Hindex2<- NewNorway$Hosp2/NewNorway$Population*1000000 
NewNorway$Hindex3<- NewNorway$Hosp/5425270*1000000 
NewNorway$Hindex4<- NewNorway$Hindex3+NewNorway$Hindex2 





## Creatin High political trust variable 
NewNorway$High <- (NewNorway$'4' + NewNorway$'5')*100 



#### Controll Variables 

### Population Density 
NewNorway$PopulationDens <- ifelse(NewNorway$Region == 'Oslo og Viken', 1934659/25046.7, 0)+ 
  ifelse(NewNorway$Region == 'Innlandet',371385/24592.59, 0)+ 
  ifelse(NewNorway$Region == 'Agder og Sor-ostlandet',726627/33900.04, 0)+ 
  ifelse(NewNorway$Region == 'Vestlandet',1322218/33870.99, 0)+ 
  ifelse(NewNorway$Region == 'Trondelag', 438241/42201.59, 0)+ 
  ifelse(NewNorway$Region == 'Nord Norge',475507/112984.42, 0) 



### Age 
NewNorway$Age <- ifelse(NewNorway$Region == 'Oslo og Viken', 39.7, 0)+ 
  ifelse(NewNorway$Region == 'Innlandet', 44.2, 0)+ 
  ifelse(NewNorway$Region == 'Agder og Sor-ostlandet', 41.85, 0)+ 
  ifelse(NewNorway$Region == 'Vestlandet', 40.6, 0)+ 
  ifelse(NewNorway$Region == 'Trondelag', 40.9, 0)+ 
  ifelse(NewNorway$Region == 'Nord Norge', 42.45, 0) 



### Gender 
NewNorway$Gender  <- ifelse(NewNorway$Region == 'Oslo og Viken',(638814+349108)/(630416+350719), 0)+ 
  ifelse(NewNorway$Region == 'Innlandet', 186293/184960, 0)+ 
  ifelse(NewNorway$Region == 'Agder og Sor-ostlandet',(212921+156655)/(211911+154479), 0)+ 
  ifelse(NewNorway$Region == 'Vestlandet', (325104+246841+135421)/(316188+238956+130427), 0)+ 
  ifelse(NewNorway$Region == 'Trondelag', 24812/23319, 0)+ 
  ifelse(NewNorway$Region == 'Nord Norge', (122024+123339)/(118166+118397), 0) 


### Education
NewNorway$Edu  <- ifelse(NewNorway$Region == 'Oslo og Viken',(10.1+22.0)/2, 0)+ 
  ifelse(NewNorway$Region == 'Innlandet', 5.9, 0)+ 
  ifelse(NewNorway$Region == 'Agder og Sor-ostlandet',(7.0+7.6)/(2), 0)+ 
  ifelse(NewNorway$Region == 'Vestlandet', (10.4+10.1)/(2), 0)+ 
  ifelse(NewNorway$Region == 'Trondelag', (10.5+6.2)/(2), 0)+ 
  ifelse(NewNorway$Region == 'Nord Norge', (6.2+9.2)/(2), 0)



### Nummer of days
startdate <- as.Date("2020-02-12") 
NewNorway$date2 <-  as.Date(NewNorway$date,"%d/%m/%Y") 
NewNorway$NumDays  <- difftime(NewNorway$date, startdate ,units="days") 
NewNorway$NumDays <- as.numeric(NewNorway$NumDays) 



#### Creating Datasets with data before 2022
NewNorwayEnd <-  filter(NewNorway, date <= "2022-01-29") 

#### Creating Datasets with Only data after 2022
NewNorwayOnly <- filter(NewNorway, date >= "2022-01-29") 


#### Creating Datasets without phase 0 and 8
NewNorwayFinal <- filter(NewNorwayEnd, date >= "2020-03-18") 


#### Creating Datasets with 
NewNorwayNoB <-  filter(NewNorway, date >= "2020-03-18") 


##### Tabulation
stargazer(as.data.frame(NewNorway[c("High","String","Hosp","Vac")]), type = "html", out="DS.htm")
stargazer(as.data.frame(NewNorwayEnd[c("High","String","Hosp","Vac")]), type = "html", out="DS2.htm")


##### Visualizing Data

### Showcasing political trust variable 
## Trust for all regions. 
NewNorway %>% 
  ggplot( aes(x=date, y=High, color=Region)) + 
  geom_line(size = 1) + 
  geom_point() + 
  theme_ipsum() + 
  theme(legend.position="bottom")+ 
  ggtitle("Level of High Trust During the Pandemic")+
  labs(x = "Dates", y = "High Political Trust")+
  ylim(0, 100)



## Cases All Countries 
ggplot() + 
  geom_line(data=DataN, aes(x=date, y=new_cases_smoothed_per_million, colour = "Norway"), size=1) + 
  geom_line(data=DataS, aes(x=date, y=new_cases_smoothed_per_million, colour = "Sweden"), size=1)+ 
  geom_line(data=DataD, aes(x=date, y=new_cases_smoothed_per_million, colour = "Denmark"), size=1)+ 
  geom_line(data=DataU, aes(x=date, y=new_cases_smoothed_per_million, colour = "United States"), size=1)+ 
  theme_ipsum()+ 
  labs(x = "Dates", y = "Number of Cases per Million")+ 
  theme(legend.position="bottom")+ 
  ggtitle("Number of Cases per Million in Norway, Sweden, Denmark and United States")




## Creating graph highlightning Oslo 

NewNorwayO <-filter(NewNorway, Region == "Oslo og Viken") 

ggplot() + 
  geom_line(data=NewNorway, aes(x=date, y=High, group= Region,colour="gray"), size=1) + 
  geom_point(data=NewNorwayO, aes(x=date, y=High, colour="darkred")) +  
  geom_line(data=NewNorwayO, aes(x=date, y=High, colour="darkred"), size=1)+ 
  theme_ipsum()+ 
  ylim(0, 100)+
  theme(legend.position="bottom")+ 
  ggtitle("High Level of Trust in Oslo and Viken")+ 
  labs(x = "Dates", y = "High Political Trust")+ 
  scale_color_manual(name="Legend",  
                     labels = c("Other Regions",  
                                "Oslo and Viken"), 
                     values = c("gray"="gray",  
                                "darkred"="darkred")) 



## Creating graph highlightning Nordland and Oslo
NewNorwayN <-filter(NewNorway, Region == "Nord Norge") 

ggplot() + 
  geom_line(data=NewNorway, aes(x=date, y=High, group= Region, 
                                colour="gray"), size=1) + 
  geom_point(data=NewNorwayO, aes(x=date, y=High, colour="darkred")) +  
  geom_line(data=NewNorwayO, aes(x=date, y=High,colour="darkred"), size=1)+ 
  geom_point(data=NewNorwayN, aes(x=date, y=High, colour="darkblue")) +  
  geom_line(data=NewNorwayN, aes(x=date, y=High, colour="darkblue"), size=1)+ 
  theme_ipsum()+
  ylim(0, 100)+
  theme(legend.position="bottom")+ 
  ggtitle("High Level of Trust in Northern Norway and Oslo and Viken")+ 
  labs(x = "Dates", y = "High Political Trust")+ 
  scale_color_manual(name="Legend",  
                     labels = c("Other Regions",  
                                "Oslo and Viken", 
                                "Northern Norway"), 
                     values = c("gray"="gray",  
                                "darkred"="darkred", 
                                "darkblue"="darkblue")) 


#### Showing Covid-19 data 
ggplot(data = NewNorway) + 
  geom_line(mapping = aes(x = date, y = Hosp),  colour='red', size=1)+ 
  geom_line(mapping = aes(x = date, y = New/10), colour='orange', size=1)+ 
  theme_ipsum()+ 
  labs(x = "Dates")+ 
  ggtitle("New Cases and Hospitalizations")+ 
  scale_y_continuous(name = "Number of Hospitalizations",  
                     sec.axis = sec_axis(trans=~.*10, name="Number of New Cases"))


#### Showing Trust and Hospitalizations 
ggplot(data = Comb) + 
  geom_line(mapping = aes(x = date, y = High*100, color='High'), size=1)+ 
  geom_line(mapping = aes(x = date, y = Hosp/5 , color='Hosp'), size=1)+ 
  theme_ipsum()+ 
  labs(x = "Dates", y = "High Political Trust")+ 
  ggtitle("High level of Trust and Number of Hospitalizations")+ 
  scale_color_manual(name="Legend",  
                     labels = c("Level of Trust",  
                                "Hospitalizations"), 
                     values = c("High"="blue", 
                                "Hosp"= "red")) 

#### Showing Trust, Stringency and Hospitalizations 
ggplot(data = Comb) + 
  geom_line(mapping = aes(x = date, y = String, color='String'), size=1)+ 
  geom_line(mapping = aes(x = date, y = High*100, color='High'), size=1)+ 
  geom_line(mapping = aes(x = date, y = Hosp/5, color='Hosp'), size=1)+ 
  theme_ipsum()+ 
  scale_y_continuous(name = "Trust and Stringency Scale from 0 to 100",  
                     sec.axis = sec_axis(trans=~.*5, name="Number of Hospitalizations"))+ 
  theme_ipsum()+ 
  theme(legend.position="bottom")+ 
  labs(x = "Dates")+ 
  ggtitle("Level of Trust, Stringency and Number of Hospitalizations")+ 
  scale_color_manual(name="Legend",  
                     labels = c("Level of Trust",  
                                "Hospitalizations", 
                                "Stringency"), 
                     values = c("High"="blue", 
                                "Hosp"= "red", 
                                "String"= "green")) 
#### Showing all levels of Trust 
ggplot(data = Comb) + 
  geom_line(mapping = aes(x = date, y = `1`*100, color = "1"), size=1)+ 
  geom_line(mapping = aes(x = date, y = `2`*100, color = "2"), size=1)+ 
  geom_line(mapping = aes(x = date, y = `3`*100, color = "3"), size=1)+ 
  geom_line(mapping = aes(x = date, y = `4`*100, color = "4"), size=1)+ 
  geom_line(mapping = aes(x = date, y = `5`*100, color = "5"), size=1)+ 
  theme_ipsum()+
  ylim(0, 100)+
  theme(legend.position="bottom")+ 
  labs(x = "Dates", y = "Percentage of Responses")+ 
  ggtitle("All Levels of Trust")+ 
  scale_color_manual(name="Scale:",  
                     labels = c("1 = To a very small extent",  
                                "2", 
                                "3", 
                                "4", 
                                "5 = To a very large extent"), 
                     values = c("1"="red",  
                                "2"="darkred", 
                                "3"="black", 
                                "4"="darkblue", 
                                "5"="blue")) 

#### Hospitalizations regions

NewNorway %>% 
  ggplot( aes(x=date, y=Hosp2, color=Region)) + 
  geom_line(size = 1) + 
  geom_point() + 
  theme_ipsum() + 
  theme(legend.position="bottom")+ 
  ggtitle("Number of Hospitalizations in Each Region")+
  labs(x = "Dates", y = "Number of Hospitalizations")


NewNorway %>% 
  ggplot( aes(x=date, y=Hosp2/Population*1000000, color=Region)) + 
  geom_line(size = 1) + 
  geom_point() + 
  theme_ipsum() + 
  theme(legend.position="bottom")+ 
  ggtitle("Number of Hospitalizations in Each Region per Million")+
  labs(x = "Dates", y = "Number of Hospitalizations per Million")




#### Only Strigency  

ggplot(data = Comb) + 
  geom_line(mapping = aes(x = date, y = String), color='green', size=1)+ 
  theme_ipsum()+ 
  labs(x = "Dates", y = "Strigency Index from 0 to 100")+ 
  ggtitle("Norwegian Level of Strigency of Goverement Respons")+
  ylim(0,100)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-03-18"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-04-22"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-10-21"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-02-26"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-04-16"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-09-24"))), linetype=4)+  
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-12-03"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2022-01-29"))), linetype=4)



#### Distribution of variables
ggplot(NewNorway, aes(High, fill = Region)) + 
  geom_histogram()+ 
  theme_ipsum()+ 
  labs(x = "Political Trust", y = "Count")+ 
  ggtitle("Distribution of Trust Varible") 


ggplot(NewNorway, aes(Hosp2, fill = Region)) + 
  geom_histogram()+ 
  theme_ipsum()+ 
  labs(x = "Hospitalizations", y = "Count")+ 
  ggtitle("Distribution of Hospitalization Region Based") 


ggplot(NewNorway, aes(String)) + 
  geom_histogram()+ 
  theme_ipsum()+ 
  labs(x = "Stringency", y = "Count")+ 
  ggtitle("Distribution of Stringency Varible") 

ggplot(NewNorway, aes(Hosp)) + 
  geom_histogram()+ 
  theme_ipsum()+ 
  labs(x = "Hospitalizations", y = "Count")+ 
  ggtitle("Distribution of Hospitalizations Varible") 

ggplot(NewNorway, aes(Vac))+ 
  geom_histogram()+ 
  theme_ipsum()+ 
  labs(x = "Vaccinations", y = "Count")+ 
  ggtitle("Distribution of Vaccination Varible") 


#### Interaction between variables

ggplot(data = NewNorwayFinal) + 
  geom_point(mapping = aes(y = High, x = String),  colour='green', size=1)+
  theme_ipsum()+ 
  labs(y = "Percentage High Trust", x= "Stringency")+
  ggtitle("Relationship Between High Trust and Stringency")+
  ylim(0,100)+
  geom_point(data=NewNorwayOnly, mapping = aes(y = High, x = String),  colour='black', size=1)+
  geom_point(data=NewNorwayPhase0, mapping = aes(y = High, x = String),  colour='gray', size=1)+
  geom_smooth(data=NewNorwayFinal, mapping = aes(y = High, x = String), method = lm)



ggplot(data = NewNorway) + 
  geom_point(mapping = aes(y = High, x = Hosp),  colour='red', size=1)+
  theme_ipsum()+
  labs(y = "Percentage High Trust", x= "Hospitalizations")+
  ggtitle("Relationship Between High Trust and Hospitalizations")+
  ylim(0,100)+
  geom_point(data=NewNorwayFinal, mapping = aes(y = High, x = Hosp),  colour='red', size=1)+
  geom_point(data=NewNorwayOnly, mapping = aes(y = High, x = Hosp),  colour='black', size=1)+
  geom_point(data=NewNorwayPhase0, mapping = aes(y = High, x = Hosp),  colour='gray', size=1)+
  geom_smooth(data=NewNorwayFinal, mapping = aes(y = High, x = Hosp), method = lm)


ggplot(data = NewNorway) + 
  geom_point(mapping = aes(y = High, x = Vac),  colour='darkgreen', size=1)+
  theme_ipsum()+
  labs(y = "Percentage High Trust", x= "Vaccinations")+
  ggtitle("Relationship Between High Trust and Vaccinations")+
  ylim(0,100)+
  xlim(1, 220)+
  geom_line(mapping = aes(y = High, x = date),  colour='blue', size=1)


##### Modeling

#### Panel Model without Phase 1 or 8

### Hospitalizations and Stringency 
Model1 <- plm(High ~ String + Hosp
                 , data = NewNorwayFinal, 
                 index = c("Dummys", "date"),  
                 effect = "individual",  
                 model = "random") 
summary(Model1) 


# Distribution 
hist(RestleddModel1) 
RestleddModel1<-residuals(Model1) 
describe(RestleddModel1) 
plot(RestleddModel1) 



## With Interaction 
Model2 <- plm(High ~ String * Hosp, 
                 data = NewNorwayFinal, 
                 index = c("Dummys", "date"),  
                 effect = "individual",  
                 model = "random") 

summary(Model2) 

# Distribution 
RestleddModel2<-residuals(Model2) 
hist(RestleddModel2) 
describe(RestleddModel2) 
plot(RestleddModel2) 

names(NewNorway)


#### Fixed Effects
Model4<- plm(High ~ String * Hosp + PopulationDens + Age + Gender + Edu,  
             data = NewNorwayFinal, 
             index = c("Dummys", "date"),  
             effect = "individual",  
             model = "within") 
summary(Model4) 


### With Robust standard errors 
robust_seModel1 <- as.vector(summary(Model1,robust = T)$coefficients[,"Std. Error"])
robust_seModel2 <- as.vector(summary(Model2,robust = T)$coefficients[,"Std. Error"])
robust_seModel4 <- as.vector(summary(Model4,robust = T)$coefficients[,"Std. Error"])

stargazer(Model1, Model2, title="Results", align=TRUE, type = 'html', out = "Model12.htm"
          ,se = list(robust_seModel1,robust_seModel2)) 



#### With Phase 1

Model1b <- plm(High ~ String + Hosp
              , data = NewNorwayEnd, 
              index = c("Dummys", "date"),  
              effect = "individual",  
              model = "random") 
summary(Model1b) 



# Distribution 
RestleddModel1b<-residuals(Model1b) 
hist(RestleddModel1b) 
describe(RestleddModel1b) 
plot(RestleddModelb1) 



## With Interaction 
Model2b <- plm(High ~ String * Hosp, 
              data = NewNorwayEnd, 
              index = c("Dummys", "date"),  
              effect = "individual",  
              model = "random") 

summary(Model2b) 


# Distribution 
RestleddModel2b<-residuals(Model2b) 
hist(RestleddModel2b) 
describe(RestleddModel2b) 
plot(RestleddModel2b) 

names(NewNorway)


#### Fixed Effects

Model4b<- plm(High ~ String * Hosp,  
             data = NewNorwayEnd, 
             index = c("Dummys", "date"),  
             effect = "individual",  
             model = "within") 
summary(Model4b) 

### With Robust standard errors 
robust_seModel1b <- as.vector(summary(Model1b,robust = T)$coefficients[,"Std. Error"])
robust_seModel2b <- as.vector(summary(Model2b,robust = T)$coefficients[,"Std. Error"])
robust_seModel3b <- as.vector(summary(Model3b,robust = T)$coefficients[,"Std. Error"])
robust_seModel4b <- as.vector(summary(Model4b,robust = T)$coefficients[,"Std. Error"])

stargazer(Model1b, Model2b, Model3b, title="Results", align=TRUE, type = 'html', out = "Model123b.htm"
          ,se = list(robust_seModel1b,robust_seModel2b,robust_seModel3b,robust_seModel4b)) 



#### With Regions
ModelR1 <- plm(High ~ String + Hosp2
              , data = NewNorwayFinal, 
              index = c("Dummys", "date"),  
              effect = "individual",  
              model = "random") 
summary(ModelR1) 


# Distribution 
RestleddModel1b<-residuals(Model1b) 
hist(RestleddModel1b) 
describe(RestleddModel1b) 
plot(RestleddModel1b) 


## With Interaction 
ModelR2 <- plm(High ~ String * Hosp2, 
              data = NewNorwayFinal, 
              index = c("Dummys", "date"),  
              effect = "individual",  
              model = "random") 

summary(ModelR2) 

# Distribution 
RestleddModel2b<-residuals(Model2b) 
hist(RestleddModel2b) 
describe(RestleddModel2b) 
plot(RestleddModel2b) 


### Stargazer
robust_seModelR1 <- as.vector(summary(ModelR1,robust = T)$coefficients[,"Std. Error"])
robust_seModelR2 <- as.vector(summary(ModelR2,robust = T)$coefficients[,"Std. Error"])

stargazer(ModelR1, ModelR2, title="Results", align=TRUE, type = 'html', out = "ModelR12.htm"
          ,se = list(robust_seModelR1,robust_seModelR2)) 


### With Robust standard errors 
robust_seModel1b <- as.vector(summary(Model1b,robust = T)$coefficients[,"Std. Error"])
robust_seModel2b <- as.vector(summary(Model2b,robust = T)$coefficients[,"Std. Error"])
robust_seModel3b <- as.vector(summary(Model3b,robust = T)$coefficients[,"Std. Error"])
robust_seModel4b <- as.vector(summary(Model4b,robust = T)$coefficients[,"Std. Error"])


stargazer(Model1b, Model2b, Model3b, Model4b, title="Results", align=TRUE, type = 'html', out = "Model123b.htm"
          ,se = list(robust_seModel1b,robust_seModel2b,robust_seModel4b)) 



###### Models with phase 8
Model1c <- plm(High ~ String + Hosp
               , data = NewNorwayNoB, 
               index = c("Dummys", "date"),  
               effect = "individual",  
               model = "random") 
summary(Model1c) 



# Distribution 
RestleddModel1c<-residuals(Model1c) 
hist(RestleddModel1c) 
describe(RestleddModel1c) 
plot(RestleddModel1c) 


## With Interaction 
Model2c <- plm(High ~ String * Hosp, 
               data = NewNorwayNoB, 
               index = c("Dummys", "date"),  
               effect = "individual",  
               model = "random") 

summary(Model2c) 


# Distribution 
RestleddModel2c<-residuals(Model2c) 
hist(RestleddModel2c) 
describe(RestleddModel2c) 
plot(RestleddModel2c) 


#### With Fixed effects
Model4c<- plm(High ~ String * Hosp,  
              data = NewNorwayNoB, 
              index = c("Dummys", "date"),  
              effect = "individual",  
              model = "within") 
summary(Model4c) 


### Only phase 8

## With Interaction 
Model2d <- plm(High ~ String * Hosp, 
               data = NewNorwayOnly, 
               index = c("Dummys", "date"),  
               effect = "individual",  
               model = "random") 

summary(Model2d) 



RestleddModel2d <-residuals(Model2d) 
hist(RestleddModel2d) 

### With Robust standard errors 
robust_seModel1c <- as.vector(summary(Model1c,robust = T)$coefficients[,"Std. Error"])
robust_seModel2c <- as.vector(summary(Model2c,robust = T)$coefficients[,"Std. Error"])
robust_seModel4c <- as.vector(summary(Model4c,robust = T)$coefficients[,"Std. Error"])
robust_seModel2d <- as.vector(summary(Model2d,robust = T)$coefficients[,"Std. Error"])

stargazer(Model1c, Model2c, Model2d, title="Results", align=TRUE, type = 'html', out = "Model12cd.htm"
          ,se = list(robust_seModel1c,robust_seModel2c,robust_seModel2d)) 



###### Stargazer of all fixed effects models

stargazer(Model4, Model4b, Model4c, title="Results", align=TRUE, type = 'html', out = "Model4bc.htm"
          ,se = list(robust_seModel4,robust_seModel4b,robust_seModel4c,robust_seModel4b)) 




#### Visualization of Models

### johnson_neyman Plot Model 2
johnson_neyman(model = Model2, pred = String, modx = Hosp)

### Plot of interaction effect
plot_model(Model2, type = "int")+theme_ipsum() 


### Predicted Values Model 2
NewNorwayEnd$predModel2 <- plm:::predict.plm(Model2, NewNorwayEnd) 
ggplot() +  
  geom_point(data=NewNorwayEnd, aes(x=date, y=High))+
  geom_line(data=Comb, aes(x=date, y=High*100), colour="blue", size = 1)+
  geom_line(data=NewNorwayEnd, aes(x=date, y=predModel2), colour="red", size= 1)+ 
  theme_ipsum() +
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-03-18"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-04-22"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-10-21"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-02-26"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-04-16"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-09-24"))), linetype=4)+  
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-12-03"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2022-01-29"))), linetype=4)+
  ylim(0,100)






### johnson_neyman Plot Model 2b
johnson_neyman(model = Model3, pred = Hosp, modx = String)


### Predicted Values Model 2b
NewNorwayFinal$predModel2 <- plm:::predict.plm(Model2, NewNorwayFinal) 

ggplot() +  
  geom_point(data=NewNorway, aes(x=date, y=High))+
  geom_line(data=Comb, aes(x=date, y=High*100), colour="blue", size = 1)+
  geom_line(data=NewNorwayFinal, aes(x=date, y=predModel2), colour="red", size= 1)+ 
  theme_ipsum() +
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-03-18"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-04-22"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-10-21"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-02-26"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-04-16"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-09-24"))), linetype=4)+  
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-12-03"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2022-01-29"))), linetype=4)+
  ylim(0,100)





#### Showing only beginning

#### Making dataset for each Phase
NewNorwayPhase0 <-  filter(NewNorway, Phases == "0")
NewNorwayPhase1 <-  filter(NewNorway, Phases == "1")
NewNorwayPhase2 <-  filter(NewNorway, Phases == "2")
NewNorwayPhase3 <-  filter(NewNorway, Phases == "3")
NewNorwayPhase4 <-  filter(NewNorway, Phases == "4")
NewNorwayPhase5 <-  filter(NewNorway, Phases == "5")
NewNorwayPhase6 <-  filter(NewNorway, Phases == "6")
NewNorwayPhase7 <-  filter(NewNorway, Phases == "7")
NewNorwayPhase8 <-  filter(NewNorway, Phases == "8")

NewNorwayEndPhase0 <-  filter(NewNorwayEnd, Phases == "0")
NewNorwayEndPhase1 <-  filter(NewNorwayEnd, Phases == "1")
NewNorwayEndPhase2 <-  filter(NewNorwayEnd, Phases == "2")
NewNorwayEndPhase3 <-  filter(NewNorwayEnd, Phases == "3")
NewNorwayEndPhase4 <-  filter(NewNorwayEnd, Phases == "4")
NewNorwayEndPhase5 <-  filter(NewNorwayEnd, Phases == "5")
NewNorwayEndPhase6 <-  filter(NewNorwayEnd, Phases == "6")
NewNorwayEndPhase7 <-  filter(NewNorwayEnd, Phases == "7")




### johnson_neyman Plot Model 2c
johnson_neyman(model = Model2c, pred = String, modx = Hosp) 

### Plot of interaction effect
plot_model(Model2c, type = "int")+theme_ipsum() 

### Predicted Values Model 2c
NewNorwayOnly$predModel2d <- plm:::predict.plm(Model2d, NewNorwayOnly) 
NewNorwayNoB$predModel3c <- plm:::predict.plm(Model3c, NewNorwayNoB) 
NewNorwayNoB$predModel2c <- plm:::predict.plm(Model2c, NewNorwayNoB) 

ggplot() +  
  geom_point(data=NewNorway, aes(x=date, y=High))+
  geom_line(data=Comb, aes(x=date, y=High*100), colour="blue", size = 1)+
  geom_line(data=NewNorwayNoB, aes(x=date, y=predModel2c), colour="red", size= 1)+
  theme_ipsum() +
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-03-18"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-04-22"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2020-10-21"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-02-26"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-04-16"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-09-24"))), linetype=4)+  
  geom_vline(xintercept = as.POSIXct(as.Date(c("2021-12-03"))), linetype=4)+
  geom_vline(xintercept = as.POSIXct(as.Date(c("2022-01-29"))), linetype=4)+
  ylim(0,100)






##### With varibles

ggplot(data = Comb) + 
  geom_line(mapping = aes(x = date, y = String, color='String'), size=1)+ 
  geom_line(mapping = aes(x = date, y = High*100, color='High'), size=1)+ 
  geom_line(mapping = aes(x = date, y = Hosp/5, color='Hosp'), size=1)+ 
  theme_ipsum()+ 
  scale_y_continuous(name = "Trust and Stringency Scale from 0 to 100",  
                     sec.axis = sec_axis(trans=~.*5, name="Number of Hospitalizations"))+ 
  theme_ipsum()+ 
  theme(legend.position="bottom")+ 
  labs(x = "Dates")+ 
  ggtitle("Model 2 Predictions With Varibles")+ 
  scale_color_manual(name="Legend",  
                     labels = c("Level of Trust",  
                                "Hospitalizations", 
                                "Stringency",
                                "Predicted Values"), 
                     values = c("High"="blue", 
                                "Hosp"= "red", 
                                "String"= "green",
                                "predModel2"="black"))+
  geom_line(data=NewNorwayFinal, aes(x=date, y=predModel2), size= 1)
  




#### Testing

#### Testing the Results 

### FE or RE 
phtest(Model2, Model4) 
phtest(Model3b, Model4b) 
phtest(Model3c, Model4c) 


## Multicolinearity 
vif(Model1) 
vif(Model2)
vif(Model1b) 
vif(Model2b)
vif(Model1c)
vif(Model2c) 
vif(Model2d) 



## Heteroscedasticity   
plm::pcdtest(Model1) 
plm::pcdtest(Model2) 
plm::pcdtest(Model4)
plm::pcdtest(Model1b) 
plm::pcdtest(Model2b) 
plm::pcdtest(Model4b)
plm::pcdtest(Model1c)
plm::pcdtest(Model2c)
plm::pcdtest(Model4c)
plm::pcdtest(Model3d)



## Serial Correlation
pdwtest(Model1) 
pdwtest(Model2)
pdwtest(Model4) 
pdwtest(Model1b) 
pdwtest(Model2b)
pdwtest(Model4b) 
pdwtest(Model1c)
pdwtest(Model2c)
pdwtest(Model4c) 
pdwtest(Model2d)


bgtest(Model1) 
bgtest(Model2) 
bgtest(Model3)
bgtest(Model1b) 
bgtest(Model2b) 
bgtest(Model3b)
bgtest(Model1c)
bgtest(Model2c)
bgtest(Model3c)
bgtest(Model3d)


#### Testing for Outliers
install.packages("outliers")
library(outliers)

chisq.out.test(NewNorway$High)
chisq.out.test(NewNorway$String)
chisq.out.test(NewNorway$Hosp)
chisq.out.test(NewNorway$High, opposite = TRUE)
chisq.out.test(NewNorway$String, opposite = TRUE)
chisq.out.test(NewNorway$Hosp, opposite = TRUE)

chisq.out.test(NewNorwayFinal$High)
chisq.out.test(NewNorwayFinal$String)
chisq.out.test(NewNorwayFinal$Hosp)
chisq.out.test(NewNorwayFinal$High, opposite = TRUE)
chisq.out.test(NewNorwayFinal$String, opposite = TRUE)
chisq.out.test(NewNorwayFinal$Hosp, opposite = TRUE)


#### Tabluation of datasets
names(NewNorway)
summary(NewNorway$`No. of cases`)

stargazer(as.data.frame(NewNorway[c("High","String","Hosp")]), type = "html", out="DS.htm")

stargazer(as.data.frame(NewNorwayEnd[c("High","String","Hosp")]), type = "html", out="DS2.htm")

stargazer(as.data.frame(NewNorwayFinal[c("High","String","Hosp")]), type = "html", out="DS3.htm")











