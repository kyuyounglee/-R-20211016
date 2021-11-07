#  히스토그램
#히스토그램(histogram)은 외관상 막대그래프와 비슷한 그래프로, 그룹이 명시적으로 존재하지 않는 수치형 자료의 분포를 시각화할 때 사용함.
head(cars)
cars_dist = cars[,2]
cars_dist
result =  hist(cars_dist,
     main = 'Histogram for 제동거리',
     xlab = '제동거리',
     ylab ='빈도수',
     border ='blue',
     col = rainbow(12),
     las = 2,
     breaks=5)

result     


# Stat2Data 패키지 Diamonds 데이터셋  다이아몬드 시세
.libPaths('C:/Rlib')
install.packages("Stat2Data")
library(Stat2Data)
data(Diamonds)
ds = Diamonds$PricePerCt
head(ds)
str(ds)

hist(ds, main = '캐럿당 가격 분포',breaks = 9,las = 1 )
color = rep('green',9)
color[3] = 'red'
hist(ds, main = '캐럿당 가격 분포',breaks = 9,
     las = 1,col=color )

# 다중그래프  
par(mfrow=c(2,2), mar=c(3,3,4,2))
hist(iris$Sepal.Length,col = 'orange')
barplot(table(mtcars$cyl),col = c('red','green','blue'))
barplot(table(mtcars$gear),col = rainbow(3),horiz = TRUE)
pie(table(mtcars$cyl), col=topo.colors(3),radius = 2)
par(mfrow=c(1,1))

#구체적으로 응답자의 지역, 성, 교육 수준, 연령대, 수입, 정책의 
#지지도에 따른 분포 그래프를 2행 3열의 레이아웃으로 요청하
#였습니다. 의뢰받은 그래프를 작성해봅시다.

install.packages("carData")
library(carData)
ds = Chile
head(ds)
# 화면 분할을 한다.
par(mfrow = c(2,3))
# 막대 그래프를   barplot
#ds$region          지역별 분포
#ds$sex              성별 분포
#ds$education          교육수준별 분포

barplot(table(ds$region), main = '지역별 분포')
barplot(table(ds$sex), main = '성별 분포')
barplot(table(ds$education), main = '교육수준별 분포')

# 두번째 행에는 연령 수입 정책 지지도에 대한 히스토그램
#ds$age    '연령'
#ds$income   '수입'
#ds$statusquo '정책 지지도'

hist(ds$age , main = '연령')
hist(ds$income , main = '수입')
hist(ds$statusquo , main = '정책 지지도')
par(mfrow = c(1,1))

#reshape2 패키지 안에 들어있는 tips 데이터셋은 어떤 종업원이 #기록한 팁 정보입니다. 그래프를 통해 데이터를 분석해봅시다

install.packages("reshape2")
library(reshape2)
str(tips)

# 빈도... sex , day(요일) time(식사시간대) size(일행의 규모)
#화면 분활
par(mfrow = c(2,2))
barplot(table(tips$sex), main = '성별', 
        col = setRedColor((tips$sex)))
barplot(table(tips$day), main = '요일',
        col = setRedColor((tips$day)))
barplot(table(tips$time), main = '식사시간대',
        col = setRedColor((tips$time)))
barplot(table(tips$size), main = '일행의 규모',
        col = setRedColor((tips$size)))

# 함수로 만들어서 반복사용하기
# 사용방법 : 빈도수를 구한 데이터를 table_data의 위치에 넣는다
setRedColor = function(table_data){
  # 빈도수데이터중에 가장 큰 값을 가지는 인덱스를 찾기
  index = which(table(table_data) == max(table(table_data)))
  
  # 빈도수 데이터크기만큼 기본 컬러셋팅하기
  length(table(table_data))
  color = rep("gray",length(table(table_data)))
  # 기본컬러중에 가장큰 값을 가지는 컬러를 red로 셋팅하기
  color[index] = 'red'
  return(color )
}

# 함수란   코드들의 집합
# 그 결과를 돌려줄수도 있고 아닐수도 있다
# 함수를 실행하기 위한 데이터를 받아서 또는 안받고 처리할수 있다
# 원래부터 있던 함수 - > 시스템 함수
# 우리가 만든 함수 -> 사용자 정의 함수


# 1인당 주문금액과 팁을 주는것과의 관계가 있는지 
head(tips)
tips$perBill =  tips$total_bill / tips$size

# 1인당 주문 금액 전체, 런치, 디너 별로 비교할수 있도록 3개
# 히스토
par(mfrow = c(1,3))
hist(tips$perBill, main = '1인당 주문 금액',breaks=5, col=color
     )
hist(tips[tips$time == 'Lunch','perBill'], breaks=4,
     main = '1인당 주문 금액(런치)',col = color
     )
hist(tips[tips$time == 'Dinner','perBill'], breaks=4,
     main = '1인당 주문 금액(저녁)', col =color
     )
color = rep("gray",5)
color[2] = 'red'

# 비율로 확인하기  주문금액대비 받은 비율
# 팁 / 총금액 * 100
# 점심과 저녁의 비율 시각화
tips$percent = tips$tip / tips$total_bill * 100
head(tips)


# 화면분활 1 x 2 형태로 분활
# 런치데이터중의 percent만 추려서 히스토그램
# 디너데이터중의 percent만 추려서 히스토그램

par(mfrow = c(1,2))
res.lunch =  hist(tips[tips$time == 'Lunch','percent'], 
     main = '주문 금액 대비 팁의 비율(점시)' ,breaks = 5 )
res.dinner = hist(tips[tips$time == 'Dinner','percent'], 
     main = '주문 금액 대비 팁의 비율(저녁)',breaks = 4  )

res.lunch$counts[1:4]
res.dinner$counts

res = rbind(lunch = res.lunch$counts[1:4], dinner = res.dinner$counts)
res
colnames(res) = res.dinner$breaks[2:5]
res

barplot(res, main='식사 시간에 따른 팁 비율 분포',
        legend.text = c('점심','저녁'), col = rainbow(2))




# 과학과목에 대한 선호도 점수별 
par(mfrow = c(1,1))
install.packages("DAAG")
library(DAAG)

head(science)
# 선호 점수  like 
# like 컬럼에 대한 빈도
table(science$like)
hist(science$like)
barplot(table(science$like), 
        ylim = c( 0, max(table(science$like)+200 )
                  )
        )
pie(table(science$like), radius = 1)


# 3차원 그래프를 이용한 시각화
install.packages("plotrix")
library(plotrix)
ds = table(science$like)
pie3D(ds, labels = names(ds), labelcex = 1.0,explode = 0.1,
      radius = 1.5,col = c('brown','green'))

#선그래프

year = 1875:1972
ds = data.frame(year, LakeHuron)

plot(ds$year,ds$LakeHuron,main = '수위변화', type = 'b',lty=1,
     col='blue')

# boxplot
# 단일변수 자료를 수치화 할때

dist =  cars[,2]
boxplot(dist, main='자동차 제동거리')
boxplot.stats(dist)

# 특이값
test = c(sample(20:30,5),sample(20:30,5),sample(30:20,5),sample(0:10,5))
boxplot(test)
boxplot.stats(test)





# 그룹이 있는 데이터의 상자그림
head(iris)
table(iris$Species)

boxplot(Sepal.Length~Species,data=iris,col=c('green','yellow','blue'))


# 자동차 데이터 분석  상자그림
boxplot(mtcars$mpg)

boxplot(mtcars$mpg ~ mtcars$gear)

# 0 V자형 엔진  1 일렬형태의 엔진
boxplot(mtcars$mpg ~ mtcars$vs)

#변속기의 종류
boxplot(mtcars$mpg~mtcars$am)

# 기어 4 수동 일반엔진

mtcars$wt
mean(mtcars$wt)  #평균
mtcars$grp[mtcars$wt >= mean(mtcars$wt)] = 'hight'
mtcars$grp[mtcars$wt < mean(mtcars$wt)] = 'low'

boxplot(mtcars$mpg~mtcars$grp)

boxplot(mtcars$hp~mtcars$grp)


#산점도 - 두 변수의 데이터분포, 두변수의 관계를 파악

plot(mtcars$wt, mtcars$mpg, xlab = '중량' ,ylab = '연비',  pch = 20,
     col='red')


target = mtcars[,names(mtcars)[1:4]]
plot(target)

names(iris)
iris.2 = iris[,3:4]
levels(iris$Species)
group =  as.numeric(iris$Species)
color = c('red','green','blue')

plot(iris.2, col = color[group], pch = group,)
legend(x = 'bottomright',legend = levels(iris$Species)
       , col = color,pch = c(1:3))