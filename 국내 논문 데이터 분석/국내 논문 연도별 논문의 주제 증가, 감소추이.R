library('jsonlite')
library('ggplot2')
setwd("C:/Users/ehdcj/Desktop/국내논문")
data=fromJSON("국내_논문_전문_텍스트_데이터셋샘플.json")

#필요없는 데이터 제거
data=data[,4:5]

#결측치 제거
library(dplyr) # dplyr 패키지 로드
journal_data= data %>% filter(!is.na(journal$ko))  # journal$ko NA 제거
journal_data= journal_data %>% filter(journal$ko != '')  # journal$ko 공백 제거

#연도별로 주제를 나누기
df<-journal_data %>% 
  group_by(year) %>%
  summarize(subject = journal$ko)

#table로 만들어서 주제의 연도별 빈도수 만들기
df<-table(df)

#table을 dataframe화 시키기
df<-as.data.frame(df)

#table을 dataframe-matrix화 시키기
#t<-as.data.frame.matrix(t) 

#Freq가 0인 값 제거
df<- df %>%
  filter(Freq!="0")

#그래프 그리기
graph<-ggplot(df, aes(year,Freq,color=subject,group=1))+geom_line()

