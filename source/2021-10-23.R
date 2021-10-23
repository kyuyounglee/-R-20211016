# 나머지 연산자
# 물건값이 1200원이고 길동이가 2000원을 냈으면 잔돈은 얼마?
money = 2000
price = 1200
money %% price

# 짝수 홀수
# 주어진 수가 짝수인지 홀수인지 판단하는 방법?
value = 9584574553 # 짝수인지 홀수인지 판단 cat("짝수")
# 1. 조건문을 이용   if()
# 2. 조건문안에 짝수 홀수를 판단하는 조건 작성  value %% 2 == 0
# 3. 상황에 맞는 출력문을 작성
if(value %% 2 == 0){
  cat("짝수")
}else{
  cat("홀수")
}

# 3의 배수  x %% 3 == 0

# 제곱연산자
value = 2
# value변수를 3제곱
value*value*value
value^3
value**3

a = TRUE
!a

!(value == 2)  # value는 2가 아니다
value != 2

value > 2  #value는 2초과
!(value > 2) #value는 2이하
value <= 2


!(value >10 & value < 50 )
value <= 10 | value >= 50

name = c('kim','lee','choi')
which(name=='lee') # 데이터를 탐색할때..... 인덱스 번호.. 위치번호

key = 'kim'

# 해당하는 case의 값을 반환
switch (key,
  'lee' = '서울',
  'kim' = '대전',
  'choi' = '대구'
)

# for
# 1부터 10까지의 합계를 구해보세요

# 1 ~ 10 까지의 합
list_a = c(1:10)
sum = 0
for(i in list_a){
  sum = sum + i
}

list_a
sum(list_a)

myf =  function(){
  list_a = c(1:10)
  sum = 0
  for(i in list_a){
    sum = sum + i
  }
  cat(sum)
}

myf()

# 현재위치 getwd()
# 1 현재위치에 원하는 파일을 복사
# 2 해당파일의 위치를 기본경로로 설정  -> 사용
# 3 해당파일의 전체경로를 표시
getwd()
# "D:\주말 빅데이터_이규영\Rwork-2nd-20211016T001801Z-001\Rwork-2nd\Part-I" -> 문자열로 사용 못합
# "\n" -> new line
# "\t" -> tab 만큼 띄운다

# "D:\\주말 빅데이터_이규영\\Rwork-2nd-20211016T001801Z-001\\Rwork-2nd\\Part-I"
# "D:/주말 빅데이터_이규영/Rwork-2nd-20211016T001801Z-001/Rwork-2nd/Part-I"
setwd("D:\\주말 빅데이터_이규영\\Rwork-2nd-20211016T001801Z-001\\Rwork-2nd\\Part-I")
getwd()

df = read.csv("test.csv", header = T)
head(df)  # 처음부터 6개
tail(df)  # 마지막부터 6개

df[c(1:6),]
summary(df)

v = c(1:11)
v
summary(v)
boxplot(v)

# 데이터의 분포 퍼짐정도
# 결측치  NA
summary(df)
boxplot(df)

min(df['A'])

table(df['E'])

# 분산 과 표준편차
# 분산
# 편차   값 - 평균   양수 음수
# 편차를 제곱 -- 양수로 만들기 위해서
# 편차의 제곱을 변량의 개수로 나눈다
# 분산 -> 편차의 제곱의 평균
# sum(편차^2) / 개수
v = c(1:10)
length(v)
#분산 variance
sum((v - mean(v))**2) /(length(v)-1)
var(v)
# 분산 --> 제곱을 사용했기때문에 데이터가 너무 거졌다
# 표준편차  Standard deviation  
sd(v)

# 평균->편차(x- mean) ->분산( 편차^2/개수-1) ->편차(분산의 제곱근)
# var(x)  분산
# sd(x)  표준편차

var(df$A)
sd(df$E)


v = c(sample(1:100, 3),NA,sample(1:100, 3),NA)
v  

#수치함수를 사용할때 예를들어 sum min max mean var sd
# NA(결측치)가 포함되어 있으면 계산이 안됨
# 옵션으로 na.rm=T 주면 NA를 제외하고 계산함
mean(v,na.rm=T)

# 그러면.. 원래 데이터에서 NA를 제거하고(제외하고) 
# 계산 가능한 데이터를 추출해 보자
!is.na(v)
v[!is.na(v)]

# 결측치 처리하는 방법
# 1. 제거하는방법
v[!is.na(v)]
# 2. 값을 대처하는 방법 - 평균으로 대처, 0으로 대처하는 방법
ifelse(is.na(v),0,v)
# 특정한 값 --> 평균
ifelse(is.na(v),mean(v, na.rm = T),v)

v.notna =  v[!is.na(v)]
range(v.notna)
sort(v.notna)
sort(v.notna,decreasing = T)


v.notna
sort(v.notna)

#order를 이용하면 정렬된 인덱스를 알기때문에 정렬이가능
order(v.notna)
v.notna[order(v.notna)]

rank(v.notna)

#정규분포
temp = rnorm(100,mean=0,sd = 1)
hist(temp)

#균등분포
temp = runif(100,min=0,max=100)
hist(temp)

x = matrix(1:9, nrow=3,byrow = T)
y = matrix(1:3)

x
y = cbind(x,10:12)
y = rbind(y,sample(1:100, 4))
y
apply(y,1,sum)
apply(y,2,sum)

y
apply(y,1,sum)
y = cbind(y,apply(y,1,sum))
y
y = rbind(y,apply(y,2,sum))
y

# 집합함수

temp = y[5,]
temp

c(36,2,128) %in% temp


# 시각화
# 범주형    :  비숫자, 범주(category)
#              남녀, 혈액형
# 비연속성  :  숫자, 비연속성(정해진 몇개의 값만)
#              자녀수, 자동차 판매대수

y =  sample(100:1000, 8)

names(y) = c("2020-1st","2020-2st","2020-3st","2020-4st",
             "2021-1st","2021-2st","2021-3st","2021-4st")
max(y)
# 2020년도는 green
# 2021년도는 red
colv = c('green','green','green','green','red','red','red','red')
barplot(y,ylim = c(0,1000),xlab = "년도별 분기현황",
        ylab = '매출액액',
        main = "2020 ~ 2021년도 분기별 매출액",
        col= colv)

# 매출액중에 550 이상인 분기는 red
# 나머지는 gray

colv =ifelse(y >= 550, 'red','gray')
barplot(y,xlab = "년도별 분기현황",
        horiz = T,
        ylab = '매출액액',
        main = "2020 ~ 2021년도 분기별 매출액",
        col= colv)

par(mfrow = c(1,1))
barplot(y,ylim = c(0,1000),xlab = "년도별 분기현황",
        ylab = '매출액액',
        main = "2020 ~ 2021년도 분기별 매출액",
        col= colv)
barplot(y,xlab = "년도별 분기현황",
        horiz = T,
        ylab = '매출액액',
        main = "2020 ~ 2021년도 분기별 매출액",
        col= colv)

# 점차트
dotchart(y)


# 기존데이터 불러오기
data("VADeaths")
VADeaths
par(mfrow = c(1,2))
barplot(VADeaths,col = rainbow(5))
legend(3.8,200,c('50-54','55-59','60-64','65-69','70-74')
       ,cex=0.8,fill = rainbow(5))

barplot(VADeaths,beside = T, col = rainbow(5))
legend(20,70,c('50-54','55-59','60-64','65-69','70-74')
       ,cex=0.8,fill = rainbow(5))

par(mfrow = c(1,1))
dotchart(VADeaths,color = rainbow(20))

pie(VADeaths)

summary(VADeaths)
boxplot(VADeaths,range = 0)
abline(h=37,lty=3,col='red')

# iris 데이터 셋

data("iris")
head(iris)
str(iris)
# 데이터분석의 순서 수치상으로 분석
# str  데이터의 종류(숫자 문자..  총 row col수를 알수)
# summary 4분위수, na가 있다면 몇개 있는지
summary(iris)

# 시각화 hist  히스토그램 --> x축은 계급  Y 계급에대한 분포수

v = sample(1:50,10)
hist(v)

iris$Sepal.Length
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)

