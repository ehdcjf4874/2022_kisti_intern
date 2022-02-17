#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("ggplot2")
library('jsonlite')
library('ggplot2')
setwd("/home/computer/국내논문")
df1=fromJSON("국내_논문_전문_텍스트_데이터셋_1.json",rownames<-NULL)
df2=fromJSON("국내_논문_전문_텍스트_데이터셋_2.json",rownames<-NULL)
df3=fromJSON("국내_논문_전문_텍스트_데이터셋_3.json",rownaems<-NULL)
df4=fromJSON("국내_논문_전문_텍스트_데이터셋_4.json",rownames<-NULL)
df5=fromJSON("국내_논문_전문_텍스트_데이터셋_5.json",rownames<-NULL)

journal_names1 = unlist(lapply(df1, function(x){x$journal$ko}))
journal_year1 =  unlist(lapply(df1, function(x){x$year}))
journal_names1
journal_year1
data.frame(journal_names1,journal_year1)
journal_names1=unlist(lapply(df1, function(x) {if(is.null(x$journal$ko)){'NULL'} else {x$journal$ko}}))
data1<-data.frame(journal_names1,journal_year1)
names(data1)<-c("journal_names","journal_year")

journal_names2=unlist(lapply(df2, function(x) {if(is.null(x$journal$ko)){'NULL'} else {x$journal$ko}}))
journal_year2 = unlist(lapply(df2, function(x){x$year}))
journal_names2
journal_year2
data2<-data.frame(journal_names2,journal_year2)
names(data2)<-c("journal_names","journal_year")

journal_names3=unlist(lapply(df3, function(x) {if(is.null(x$journal$ko)){'NULL'} else {x$journal$ko}}))
journal_year3 = unlist(lapply(df3, function(x){x$year}))
journal_names3
journal_year3
data3<-data.frame(journal_names3,journal_year3)
names(data3)<-c("journal_names","journal_year")

journal_names4=unlist(lapply(df4, function(x) {if(is.null(x$journal$ko)){'NULL'} else {x$journal$ko}}))
journal_year4 = unlist(lapply(df4, function(x){x$year}))
journal_names4
journal_year4
data4<-data.frame(journal_names4,journal_year4)
names(data4)<-c("journal_names","journal_year")

journal_names5=unlist(lapply(df5, function(x) {if(is.null(x$journal$ko)){'NULL'} else {x$journal$ko}}))
journal_year5 = unlist(lapply(df5, function(x){x$year}))
journal_names5
journal_year5
data5<-data.frame(journal_names5,journal_year5)
names(data5)<-c("journal_names","journal_year")


data<-rbind(data1,data2,data3,data4,data5)
library(dplyr) 
data = data %>% filter(journal_year>=2000)
data$journal_year=ifelse(data$journal_year>=2000 & data$journal_year<=2002 ,2000,data$journal_year)
data$journal_year=ifelse(data$journal_year>=2003 & data$journal_year<=2005 ,2005,data$journal_year)
data$journal_year=ifelse(data$journal_year>=2006 & data$journal_year<=2008 ,2008,data$journal_year)
data$journal_year=ifelse(data$journal_year>=2009 & data$journal_year<=2011 ,2011,data$journal_year)
data$journal_year=ifelse(data$journal_year>=2012 & data$journal_year<=2014 ,2014,data$journal_year)
data$journal_year=ifelse(data$journal_year>=2015 & data$journal_year<=2017 ,2017,data$journal_year)
data$journal_year=ifelse(data$journal_year>=2018 & data$journal_year<=2020 ,2020,data$journal_year)
table(data$journal_year)

#연도에따라 과목을 나눔
journal_data<-data %>% 
  group_by(journal_year) %>%
  summarize(subject = journal_names)

# count subject
journal_data_2 <- journal_data %>%
  group_by(subject) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)                         # sort by counts

journal_data<-journal_data %>%filter(subject==c("한국산학기술학회논문지","한국정보처리학회:학술대회논문집",
"한국콘텐츠학회논문지","한국정밀공학회:학술대회논문집","디지털융복합연구"))


journal_data<-table(journal_data)

journal_data<-as.data.frame(journal_data)
journal_data

graph<- journal_data %>% ggplot(aes(journal_year,Freq,color=subject,group=subject))+geom_line()
graph + ggtitle("연도에 따른 주제별 논문수 추이") +
  xlab("DATE") + ylab("PAPER")



