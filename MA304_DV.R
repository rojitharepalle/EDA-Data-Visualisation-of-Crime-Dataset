## R code

library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyverse)
install.packages('plotly')
library(plotly)
library(gridExtra)
library(ggrepel)
library(naniar)
library(lubridate)
install.packages('rgdal')
library(rgdal)

# Reading dataset
incidents<-read.csv('C:/Users/HP/Downloads/37-00049_UOF-P_2016_prepped.csv')

view(incidents) #viewing the dataset
dim(incidents)  #to view number of rows and columns
duplicated(incidents) %>% sum() #checking for any duplicates
#removing second row from the dataset as it doesn't make much difference
incidents = incidents[-1,]
head(incidents)
dim(incidents)
sum(is.na(incidents)) #checking for number of missing values



#convert null values to NA
incidents[incidents == " "] = NA
incidents[incidents == "NULL"] = NA

#calculating sum of NAs
sapply(incidents, function(x) sum(is.na(x)))

#new dataframe race is created 
race <- names(which(sapply(incidents,is.character)))
view(race)


#plotting graph to view the count of officers race w.r.t. subject's race
ggplot(incidents, aes(x = OFFICER_RACE,fill = SUBJECT_RACE)) + 
  geom_bar(position = "dodge") + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

#plotting graph to view the number of years the officers were on force
ggplot(incidents, aes(x = as.numeric(OFFICER_YEARS_ON_FORCE))) +
  geom_histogram(fill = "light blue", color = "black", bins = 20) +
  labs(x = "Officer years on force", y = "Count")


#plot to view the injury of subject w.r.t officer_race
p1 =  ggplot(incidents,aes(x = OFFICER_RACE,fill = SUBJECT_INJURY)) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject injury"))
p2 = ggplot(incidents,aes(x = OFFICER_RACE,fill = SUBJECT_INJURY)) + 
  geom_bar(position = "dodge") + 
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject injury"))
grid.arrange(p1,p2,ncol = 2)

#Reason for arrest and subject race
p3 = ggplot(incidents,aes(x = SUBJECT_DESCRIPTION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject description") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

p4 = ggplot(incidents,aes(x = SUBJECT_DESCRIPTION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject description") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

grid.arrange(p3,p4,ncol = 2)

#which division more blacks got arrested?
p5 = ggplot(incidents,aes(x = DIVISION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Division") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))

p6 = ggplot(incidents,aes(x = DIVISION,fill = SUBJECT_RACE)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Division") + ylab("Count") + 
  guides(fill=guide_legend(title="Subject race"))
grid.arrange(p5,p6,ncol = 2)

#reason foe incident w.r.t. officers race
p7 = ggplot(incidents,aes(x = OFFICER_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))

p8 = ggplot(incidents,aes(x = OFFICER_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1))  + xlab("Officer race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))
grid.arrange(p7,p8,ncol = 2)

p9 = ggplot(incidents,aes(x = SUBJECT_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))

p10 = ggplot(incidents,aes(x = SUBJECT_RACE,fill = INCIDENT_REASON)) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(size  = 8,
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 1)) + xlab("Subject race") + ylab("Count") + 
  guides(fill=guide_legend(title="Incident reason"))
grid.arrange(p9,p10,ncol = 2)


#Including plots

incidents$INCIDENT_DATE <- as.Date(incidents$INCIDENT_DATE, format = "%m/%d/%Y" )
incidents$INCIDENT_DATE <- gsub("00","20",incidents$INCIDENT_DATE)
incidents$INCIDENT_DATE <- as.Date(incidents$INCIDENT_DATE, format = "%Y-%m-%d")

incidents$INCIDENT_TIME <- format(strptime(incidents$INCIDENT_TIME, "%I:%M:%S %p"), "%H:%M:%S")
incidents$INCIDENT_MONTH <- months(as.Date(incidents$INCIDENT_DATE))
incidents$INC_MONTH <-format(incidents$INCIDENT_DATE,"%m")
incidents$INCIDENT_HOUR <- as.numeric(substr(incidents$INCIDENT_TIME, 0, 2))
incidents$INCIDENT_DAY <- wday(incidents$INCIDENT_DATE, label=TRUE)
incidents$INC_HOUR <- substr(incidents$INCIDENT_TIME, 0, 2)
incidents$INC_DATE <- substr(incidents$INCIDENT_DATE, 9, 10)




incidents_year <-  incidents %>%
  group_by(INCIDENT_DATE,INCIDENT_MONTH,INCIDENT_DAY) %>%
  summarize(count = n())

incidents_month <-  incidents %>%
  group_by(INC_MONTH) %>%
  summarize(count = n())

incidents_day <-  incidents %>%
  group_by(INCIDENT_DAY,INCIDENT_HOUR) %>%
  summarize(count = n())

incidents$INC_HOUR <- substr(incidents$INCIDENT_TIME, 0, 2)

incidents_time <-incidents
incidents_time <- incidents_time%>% dplyr::filter(!is.na(INCIDENT_TIME))
incidents_time$INC_HOUR <- substr(incidents_time$INCIDENT_TIME, 0, 2)
incidents_time   %>% group_by(INC_HOUR) %>%
  summarize(avg =n()) -> incidents_hour_n


c1 <- ggplot(data = incidents_year, aes(INCIDENT_DATE, count)) +   geom_line(size=0.25, col="gray") +
  geom_smooth(method = "loess", color = "red", span = 1/10) + theme_bw() + labs(x="Months ", y= "INCIDENT COUNTS", title="1.a Year vs Incidents")


r1 <- ggplot(incidents_month, aes(x=INC_MONTH, y =count, group=1)) + geom_line()  + geom_line( size = 1,colour ="steelblue") + labs(x="MONTHS OF 2016", y= "INCIDENT COUNTS", title="1.b Months vs  Incident Rates")  + theme_bw()



#checking NA
r2 <- ggplot(incidents_hour_n, aes(x = INC_HOUR, y = avg, group = "count")) + geom_line( size = 1, colour = "orange") + labs(x="HOURS IN A DAY", y= "INCIDENT COUNTS", title="1.c Hours vs  Incident Rates")+ theme_bw() +
  theme(axis.text.x=element_text(angle=-90, vjust=0.5)) +
  
  labs(x = "Hour of the day", y = "count") + theme_bw()

r3 <- ggplot(incidents_year, aes(count)) +
  geom_density(alpha = 0.5, colour = "black", fill ="blue")+ labs(x="Incident counts", y= "Density", title="1.d Distribuion of incident rates") + theme_bw()

ggplotly(c1)
ggplotly(r1)
ggplotly(r2)
ggplotly(r3)

#pie graph
incidents_SubRace <-  incidents %>%
  group_by(SUBJECT_RACE) %>%
  summarize(count = n())

incidents_SubRace <- incidents_SubRace %>%
  arrange(desc(SUBJECT_RACE)) %>%
  mutate(lab.ypos = cumsum(count) - 0.5*count)



p1 <- ggplot(incidents_SubRace, aes(x = "", y = count, fill = SUBJECT_RACE)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = count), color = "black")+
  scale_fill_brewer(palette = "YlGnBu", direction=-1) +
  ggtitle("Share of each race in the total  number of crimes")+
  theme_void()

p1

#co-relation graph

incidents %>%
  group_by(SUBJECT_RACE) %>%
  filter(!is.na(SUBJECT_RACE)) %>%
  summarise(Count = n()) %>%
  mutate(TotalCount = nrow(incidents)) %>%
  mutate(Percentage = (Count/TotalCount) * 100) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(SUBJECT_RACE = reorder(SUBJECT_RACE,Count)) %>%
  
  p2=ggplot(aes(x = SUBJECT_RACE,y = Percentage)) +
  geom_bar(stat='identity',colour="white", fill = #FFA07A) +
             geom_text(aes(x = SUBJECT_RACE, y = 1, label = paste0("(",round(Percentage,2)," % )",sep="")),
                       hjust=0, vjust=.5, size = 4, colour = 'black',
                       fontface = 'bold')) +
             labs(x = ' Subject Race', 
                  y = 'Percentage', 
                  title = 'Race Percentage in Crimes') +
             coord_flip() + 
             theme_bw()
           
           racemap <- ggplot(data = incidents, aes(LOCATION_LONGITUDE,LOCATION_LATITUDE)) +
             geom_polygon(aes(group = group), fill="lightblue") +
             coord_equal() 
           
           race_crime = incidents %>%
             dplyr::group_by(LOCATION_LONGITUDE,LOCATION_LATITUDE,SUBJECT_RACE,LOCATION_DISTRICT) %>% 
             count() %>% arrange(desc(n)) %>% filter(n > 1 )
           
           
           racemap + geom_point(aes(x = LOCATION_LONGITUDE, y = LOCATION_LONGITUDE, size = n, alpha = 0.8, 
                                    color = Race), data = race_crime)
           
           l1=racemap + geom_point(aes(x = LOCATION_LONGITUDE, y = LOCATION_LONGITUDE, size = n, alpha = 0.8, color = SUBJECT_RACE), data = race_crime)
           
#Race percentage of crimes
           incidents%>%
             group_by(SUBJECT_RACE) %>%
             filter(!is.na(SUBJECT_RACE)) %>%
             summarise(Count = n()) %>%
             mutate(TotalCount = nrow(incidents)) %>%
             mutate(Percentage = (Count/TotalCount) * 100) %>%
             arrange(desc(Count)) %>%
             ungroup() %>%
             mutate(SUBJECT_RACE = reorder(SUBJECT_RACE,Count)) %>%
             
             ggplot(aes(x = SUBJECT_RACE,y = Percentage)) +
             geom_bar(stat='identity',colour="white") +
             geom_text(aes(x = SUBJECT_RACE, y = 1, label = paste0("(",round(Percentage,2)," % )",sep="")),
                       hjust=0, vjust=.5, size = 4, colour = 'red',
                       fontface = 'bold') +
             labs(x = 'Race', 
                  y = 'Percentage', 
                  title = 'Race Percentage in Crimes') +
             coord_flip() + 
             theme_bw()
           attach(incidents)
           min(INCIDENT_DATE)
           max(INCIDENT_DATE)

