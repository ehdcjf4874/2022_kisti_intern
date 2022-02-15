install.packages("remotes") 
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
useNIADic()
library(stringr)


extractNoun("오늘 먹은 된장찌개는 세상에서 가장 맛있었다.")

food <- paste(SimplePos09("오늘먹은 된장찌개는 세상에서 가장 맛있었다."))
food
NP <- str_match(food,'([가-힣]+)/N') #동사, 형용사만 추출
NP

str_match(food, '[가-힣]') #한글자씩 추출
str_match(food,'[가-힣]+') #한글뿐만아니라 끝나는거 모두 추출
str_match(food,'[가-힣]+/N') #명사만 추출
keyword <- NP[,2]
keyword
keyword[!is.na(keyword)]

#다량의 문장 추출
setwd("C:/Users/master/Documents/GitHub/2022_kisti_intern/R_study/텍스트데이터 분석")
data1<- readLines("우도맛집추천.txt")
data1=as.character(data1)
longpos<-paste(SimplePos09(data1))
longpos
NP <-str_match_all(longpos,'([가-힣]+)/[Np]')
NP

length(NP)
foo <- c()
alldata = c()
for(k in 1:length(NP)){
  foo <- NP[[k]][,2]
  alldata = c(alldata,foo)
  print(foo)
}
data
3 = Filter(function(x){nchar(x)>=2},alldata)
data3
