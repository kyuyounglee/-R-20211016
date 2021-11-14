.libPaths('C:/Rlib')
setwd('C:/Rlib/Rwork-2nd/Part-III')
# 통계페키지 중에 하나인 spss와 연동가능한 함수들
install.packages("memisc")
library(memisc)
getwd()
data.spss =  as.data.set( 
  spss.system.file('drinking_water.sav') 
  )
str(data.spss)
dw =  data.spss[1:11]
# type casting 형변환
dw.df =  as.data.frame(dw)
head(dw.df)
str(dw.df)
# 요인분석을 통해 부 적절한 feature를 제거 q4
result = factanal(dw.df, factors = 3,rotation = 'varimax')

dw_df =   head(dw.df[-4])

# 부류별로 데이터를 분리 data frame

c= data.frame(dw_df$Q1,dw_df$Q2,dw_df$Q3)
p= data.frame(dw_df$Q5,dw_df$Q6,dw_df$Q7)
s= data.frame(dw_df$Q8,dw_df$Q9,dw_df$Q10,dw_df$Q11)

# 요인별(컬럼별) 산술평균
satisfaction =  round(  (s$dw_df.Q8+s$dw_df.Q9+s$dw_df.Q10+s$dw_df.Q11) / ncol(s), 2 )
closeness =  round(  (c$dw_df.Q1+c$dw_df.Q2+c$dw_df.Q3) / ncol(c), 2 )
pertinence =  round(  (p$dw_df.Q5+p$dw_df.Q6+p$dw_df.Q7) / ncol(p), 2 )





drink_water_factor_df =  data.frame("제품만족도"=satisfaction,
           "제품친밀도"=closeness,
           "제품적절성"=pertinence
)

cor(drink_water_factor_df)

#
getwd()
product =  read.csv('product.csv', header = TRUE)
head(product)
summary(product)
boxplot(product)
cor(product)


install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(product,histogram = ,pch = '+')



# bodycheck.csv  데이터 상관관계 분석하기
# 어느 요인끼리 관계가 있는지 -- 해보기..

# 15장 기계학습

setwd('C:/Rlib/Rwork-2nd/Part-IV')
product = read.csv('product.csv',header = TRUE)
str(product)

#상관계수
cor(product)
plot(product$제품_적절성,product$제품_만족도)


x = product$제품_적절성
y = product$제품_만족도
# 데이터 준비
df = data.frame(x,y)
str(df)
#알고리즘 선택 및 학습
result.lm =  lm(formula = y~x,data=df)
result.lm

head(df,1)

str(result.lm[1][1])

result.lm$coefficients[1] # 절편   b
result.lm$coefficients[2] # 기울기   a
df[1:2,]
# y = ax + b
result.lm$coefficients[2] * 4 + result.lm$coefficients[1]

3-3.735963   # 3
2-2.996687    #2

#모델의 오차(잔차)
residuals(result.lm)[1]
3.735963 + -0.735963
#####################################
#1. 데이터 준비
product

#2 데이터의 각 컬럼(피처)들간의 상관관계(인과관계)를 
#분석--> 상관지수  -1~1 
cor(product)
#               제품_친밀도 제품_적절성 제품_만족도
#제품_친밀도   1.0000000   0.4992086   0.4671450
#제품_적절성   0.4992086   1.0000000   0.7668527
#제품_만족도   0.4671450   0.7668527   1.0000000

# 독립변수(x축)와 종속변수(y축)를 정의

# 독립변수와 종속변수를 가지고 dataframe을 생성
# 여기까지가 데이터준비

# 적절한 모델을 선택 -- 단순 선형 회귀 lm()
# lm()을 통한 학습
# 학습한 결과 - 모델

# 모델이 생성된것은 방정식을 풀었다. 즉 해를 구했다.

# 독립변수에 해당하는 x의 값을(제품 적절성) 주면... y 제품만족도를
# 예측 할 수 있다

#검증(테스트)
# 실제 x을 모델에 넣었을때 예측한 y값과  실제 값의 차이가. 오차...
# y(실제값) - y'(예측값) = o(오차)
# o + y'(예측값) 

# 시각화
plot(y~x, data = product,xlab = '제품의적절성', 
     ylab = '제품의 만족도',
     main = "제품의 적절성 대비 만족도")
result.lm =  lm(y~x, data = df)
abline(result.lm, col = 'red')

# 회귀분석의 결과
summary(result.lm)


# 다중회귀분석 ---- 독립변수가 여러개 일때 
# 1.데이터 -> 2.적절한 알고리즘(기계학습의 종류를 선택)
# -> 3.훈련 ->4.모델
# ->5.평가... 

#1. 데이터
y = product$제품_만족도
x1 = product$제품_친밀도
x2 = product$제품_적절성
df = data.frame(x1,x2,y)

# 2. lm()  3.   4
result.lm =  lm(y ~x1+x2, data = df)


# 다중공선성 -> 독립변수들간의 관계를 확인하고 제거할 목적
# vif(분산팽창요인)
install.packages("car")
library(car)
vif(result.lm)

# 독립변수들 간의 문제가 없음을 확인
summary(result.lm)

############################  기계학습을 했고.. 사용된 독립변수들간의 적절성을 평가 #############

# 모델을 평가 & 다중공선성 문제
data("iris")
head(iris)

model =  lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,data = iris)

# 다중공선성 문제 확인 10보다 크면 문제가 있을수 있음
vif(model)
#Sepal.Width Petal.Length  Petal.Width 
#1.270815    15.097572    14.234335 
head(iris[-5])
cor(iris[-5])


# 다중 공선성 문제로 인해 마지막 학습데이터를 제거한다(학습)
#Petal.Width
#이왕이면.. 원래데이터에서 순서대로 7:3 이게아니라.
# 잘 섞은후에 나누는게 더 좋다.
x = sample(1:nrow(iris),nrow(iris)*0.7) # 인덱스 번호

train = iris[x,]
test = iris[-x,]
# 다중공선성 해결, 훈련데이터를 섞어서 70% 사용
model =  lm(Sepal.Length ~ Sepal.Width + Petal.Length,
            data = train)

summary(model)

#예측 predict
pred =  predict(model, test)
head(test)


head(test[1:3],3)
pred[1:3]



