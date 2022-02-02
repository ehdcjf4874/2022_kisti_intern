library('jsonlite')
library('ggplot2')
setwd("C:/Users/ehdcj/Desktop/국내논문")
df=fromJSON("국내_논문_전문_텍스트_데이터셋샘플.json")

#필요없는 데이터 제거
data=df[,1:6]

#결측치 제거
library(dplyr) # dplyr 패키지 로드
journal_data= data %>% filter(!is.na(journal$ko))  # journal$ko NA 제거
journal_data= journal_data %>% filter(journal$ko != '')  # journal$ko 공백 제거

#연도별 논문 수 표, 막대 그래프
table(data$year)
ggplot(data=data, aes(x=year))+geom_bar()

#학회지별 논문 수 보기
table(data$journal$ko)

#로봇 학회지 논문 데이터로 정제
journal_data_robot<-journal_data %>% filter(journal$ko =='로봇학회논문지')

#대기 학회지 논문 데이터로 정제
journal_data_air<-journal_data %>% filter(journal$ko =='대기')

#의류 학회지 논문 데이터로 정제
journal_data_clothes<-journal_data %>% filter(journal$ko =='한국의류산업학회지')

#의류 학회지별 논문 수 데이터 정제
journal_data_clothes <- journal_data_clothes %>%
  group_by(year) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)

#로봇 학회지별 논문 수 데이터 정제
journal_data_robot <- journal_data_robot %>%
  group_by(year) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)

#대기 학회지별 논문 수 데이터 정제
journal_data_air <- journal_data_air %>%
  group_by(year) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)

#학회지별 논문 수 데이터 정제
#해야할것: 행을 연도로 열을 주제별로 논문의 수를 나타내는 데이터프레임 만들기
journal_data <- journal_data %>%
  group_by(year) %>%
  group_by(journal$ko) %>% 
  summarize(counts = n()) %>%
  arrange(-counts)

#학회지의 연도별 논문 수 표
air<-c()
robot<-c()
dff<-data.frame(air,robot)
table(journal_data_air$year)
table(journal_data_robot$year)
table(journal_data_clothes$year)

#학회지의 연도별 논문 수 꺾은선 그래프
p<-ggplot(data = journal_data_air, aes(x = year,y=counts,group=1))
p<-p+geom_line(color="red")+geom_line(data=journal_data_clothes, color="blue",group=1)
p+geom_point(data=journal_data_robot, color="green",group=1)+geom_line(show.legned=T)

