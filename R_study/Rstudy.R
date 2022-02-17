#csv파일 불러오기
all_data=read.csv("C:/Users/ehdcj/Downloads/public-master/public-master/ONLINE/R_BASIC/kflow_result.csv", sep=",", header=TRUE)
head(all_data)

# 비어있는 필드 0으로 채우는 함수 만들기
na.zero<- function (x) {
  x[is.na(x)] <-0
  return(x)
}

# 비어있는 필드 0으로 채우기
all_data <- na.zero(all_data)

set.seed(213213132)

all_data_shuffled = all_data[sample(nrow(all_data)), ]
head(all_data_shuffled)
sample(5)

train_data = all_data_shuffled[1:6800, ]
test_data = all_data_shuffled[6801:7210, ]

nrow(train_data)
nrow(test_data)

model = lm(formula = Cl ~thickness + Umach + AOA+ RE, data =train_data)
model

library(DAAG)
cv.lm(data = train_data, form.lm=model, m=5, printit = FALSE)
summary(model)

predicted = predict(model, newdata= test_data)
head(predicted)
head(test_data)


model2 = lm(formula = Cdt ~thickness + Umach + AOA+ RE, data =train_data)

predicted2 = predict(model2, newdata= test_data)
head(predicted2)
head(test_data)

vs <- cbind(as.data.frame(predicted), as.data.frame(test_data$Cl))
head(vs)
plot(vs)
abline(a=0,b=1)

library(car)
leveragePlots(model)
summary(model)

library(Metrics)
pref=mae(vs[,2],vs[,1])
pref
mae
pref_mape=mape(vs[,2],vs[,1])
pref_mape
pref_rmse = rmse(vs[,2],vs[,1])
pref_rmse

library(rsq)
pref_r2 = rsq(model)
pref_r2
