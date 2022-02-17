#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("ggplot2")
library('jsonlite')
library('ggplot2')
setwd("/home/computer/국내논문")
df1=fromJSON("국내_논문_전문_텍스트_데이터셋_1.json")

library(dplyr) 

data=df1[,4:5]
data = data %>% filter(year>=2000)
data$year=ifelse(data$year>=2000 & data$year<=2002 ,1,data$year)
data$year=ifelse(data$year>=2003 & data$year<=2005 ,2,data$year)
data$year=ifelse(data$year>=2006 & data$year<=2008 ,3,data$year)
data$year=ifelse(data$year>=2009 & data$year<=2011 ,4,data$year)
data$year=ifelse(data$year>=2012 & data$year<=2014 ,5,data$year)
data$year=ifelse(data$year>=2015 & data$year<=2017 ,6,data$year)
data$year=ifelse(data$year<=2018,7,data$year)

journal_data<-data %>% 
  group_by(year) %>%
  summarize(subject = journal$ko)

# count subject
journal_data_2 <- journal_data %>%
  group_by(subject) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)                         # sort by counts

journal_data<-journal_data %>%filter(subject==c("대한전기학회:학술대회논문집","전기학회논문지","한국철도학회:학술대회논문집","아세아태평양축산학회지","한국컴퓨터정보학회논문지"))
journal_data<-table(journal_data)

journal_data<-as.data.frame(journal_data)
journal_data

graph<- journal_data %>% ggplot(aes(year,Freq,color=subject,group=subject))+geom_line()
graph

