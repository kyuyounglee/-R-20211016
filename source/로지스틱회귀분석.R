# 데이터
getwd()
setwd("C:/Rwork-2nd/Part-IV")
weather = read.csv("weather.csv",header = TRUE)
head(weather)
colnames(weather)

#데이터 정재
# 날자(1), 문자(6,8),raintoday(14) 제거

weather_df =  weather[c(-1,-6,-8,-14)]
str(weather_df)

# 분류의 대상이되는 RainTomorrow 의 문자를 숫자로 변경한다.
weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] = 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] = 0
weather_df$RainTomorrow = as.numeric(weather_df$RainTomorrow)
str(weather_df)

# 데이터를 잘 섞고 학습용과(70%) 테스트용(30%) 분리한다.

index=  sample(1:nrow(weather_df), 0.7*nrow(weather_df))
train = weather_df[index,]
test =  weather_df[-index,]

# 모델--> 로지스틱 회귀
weather_model =  glm(RainTomorrow ~ .,data = train,family='binomial')
weather_model

summary(weather_model)

# 예측
pred = predict(weather_model,newdata = test, type = 'response')
pred[1]
test[1,]

# 0~1사이의 값을 0.5이상이면 1 그렇지 않으면 0
result_pred =  ifelse(pred>=0.5,1,0)

table(result_pred,test$RainTomorrow)
# accuracy
(85+10) / (85+6+7+10)
# recall
85 / (85+7)

# ROC 커브를 이용한 평가
install.packages("ROCR")
library(ROCR)
pr =prediction(pred,test$RainTomorrow)
# 결측치 에러발생 제거

summary(pred)
summary(test$RainTomorrow)


idx = as.data.frame(which(is.na(pred)))[,1]
idx

pred[-idx]
test$RainTomorrow[-idx]

pr =  prediction(pred[-idx],test$RainTomorrow[-idx])
prf = performance(pr,measure = 'tpr', x.measure = 'fpr')
plot(prf)

# 예측치와 실제 테스트 값의 상관계수
cor(as.data.frame( result_pred[-idx])[,1],test$RainTomorrow[-idx])


