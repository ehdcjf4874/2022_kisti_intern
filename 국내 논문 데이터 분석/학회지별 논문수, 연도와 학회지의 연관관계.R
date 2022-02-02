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
CrossTable(x = journal_data$journal$ko, y = journal_data$year, prop.t=FALSE, expected=TRUE, chisq =TRUE)
# 일반횟수
# 카이 제곱 ( 기대치 비율 )
# 행을 기준으로 비율 값 ( 가로로 읽는다. )
# 컬럼을 기준으로 비율 값 ( 세로로 읽는다. )
# 전체를 기준으로 비율 값

# p값이 0.05보다 크다면 가설이 맞다고 하기 힘듬.
