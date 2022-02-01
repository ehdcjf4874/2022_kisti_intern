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
journal_data2<- journal_data%>%
  group_by(journal) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)

#대기 학회지의 연도별 논문 수
table(journal_data_air$year)
table(journal_data_robot$year)
library(patchwork)
ggplot(data = journal_data, aes(x = year,y=counts,group=journal$ko,colour=journal$ko))+geom_line()
p<-ggplot(data = journal_data_air, aes(x = year,y=counts,group=1))
p+geom_line(color="red")+geom_line(data=journal_data_clothes, color="blue",group=1)

#학회지별 논문 수 데이터 정제
journal_data_2 <- journal_data %>%
  group_by(journal) %>%                    # calculate the counts
  summarize(counts = n()) %>%
  arrange(-counts)                         # sort by counts

#학회지별 논문 수 표, 막대 그래프
sort(table(journal_data$journal$ko))
p<-ggplot(journal_data_2, aes(x =reorder(journal$ko,counts),y=counts))+geom_bar(stat="identity")+ coord_flip()

#x,y축,제목달기
p<-p+labs(x='학회지',y='논문수',title="학회지별 논문수")

#강조하기 journal_data$journal$ko, y = journal_data$year, prop.t=FALSE, expected=TRUE, chisq =TRUE)
p<-p+ geom_bar(data=journal_data_2[journal_data_2$journal$ko=='한국광학회:학술대회논문집', ], aes(x=journal$ko, y=counts), fill='#5CBED2', stat='identity') 

#레이블 달기
p+ geom_text(aes(label=counts), size=3, hjust=1.25, color='#FFFFFF')

#연도와 학회지의 연관관계
library(gmodels)

#
CrossTable(x = journal_data$journal$ko, y = journal_data$year, prop.t=FALSE, expected=TRUE, chisq =TRUE)
# 일반횟수
# 카이 제곱 ( 기대치 비율 )
# 행을 기준으로 비율 값 ( 가로로 읽는다. )
# 컬럼을 기준으로 비율 값 ( 세로로 읽는다. )
# 전체를 기준으로 비율 값

# p값이 0.05보다 크다면 가설이 맞다고 하기 힘듬.
