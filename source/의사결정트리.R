install.packages("party")
library(party)
library(datasets)
str(airquality)
fm = Temp ~Solar.R+ Wind + Ozone# + Month + Day  
#model = lm(formula =fm, data =airquality  )
#sqrt(vif(model)) >= 2
air_ctree = ctree(fm,data = airquality)
plot(air_ctree)

# iris -- >ctree (의사결정나무)
# 학습데이터 & 검증데이터  7:3
idx = sample(1:nrow(iris), 0.7*nrow(iris))
train = iris[idx,]
test = iris[-idx,]

# 의사결정 트리에서는 독립변수들강의 강한 상관관계를 체크 하지 않는다

names(iris)
fm = Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
iris_ctree =  ctree(fm,data=train)
plot(iris_ctree)

# 의사결정트리 평가
pred =  predict(iris_ctree,test)
pred[20]
test$Species[20]

# 혼돈메트릭스 confusion matrix
table(pred, test$Species)

accuracy = (17+13+13) / nrow(test)
accuracy

#평가지수의 객관성을 높이기 위해서... k겹 검정
library(cvTools)
cross = cvFolds(nrow(iris), K=3, R=2)
str(cross)
cross

R = 1:2
K = 1:3
CNT = 0
ACC = numeric()

View(cross$subsets)
cross$subsets[cross$which == 1,1]
cross$which == 1

idx1 = cross$subsets[cross$which == 1,1]
idx2 = cross$subsets[cross$which == 2,1]
idx3 = cross$subsets[cross$which == 3,1]

for(r in R){
  cat('\n R=',r,'\n')
  for(k in K){
    datas_idx = cross$subsets[cross$which == k,r]
    test = iris[datas_idx,]
    cat('test : ',nrow(test),'\n')
    
    train = iris[-datas_idx,]
    cat('train : ',nrow(train),'\n')
    
    fm = Species ~ .
    model= ctree(fm,data=train)
    pred = predict(model,test)
    t = table(pred, test$Species)
    print(t)
    CNT = CNT + 1
    ACC[CNT] =  (t[1,1]+t[2,2]+t[3,3]) / sum(t)
  }
}
ACC
result_acc =  mean(ACC, na.rm = T)


# 실습....................
# 데이터
install.packages("ggplot2")
library(ggplot2)
data("mpg")

# 결측치 확인
summary(mpg)
mpg[is.na(mpg)]

idx = sample(1:nrow(mpg), 0.7*nrow(mpg))
train = mpg[idx,]
test = mpg[-idx,]
names(test)
formula = hwy ~ displ+cyl+drv    

test$drv =  factor(test$drv)

test_ctree =  ctree(formula,data=test)
plot(test_ctree)


# AdultUCI
install.packages("arules")
library(arules)
data("AdultUCI")
str(AdultUCI)
str(AdultUCI$income)

choice = sample(1:nrow(AdultUCI), 10000)
adult.df = AdultUCI[choice,]
str(adult.df)

adult.df = adult.df[c('age','fnlwgt','education-num','capital-gain','capital-loss','hours-per-week')]

head(adult.df)
names(adult.df) = c('age','fnlwgt','education','capital','capitalloss','hoursperweek')
head(adult.df)

str(adult.df)
formula = capital~age+fnlwgt+education+capitalloss+hoursperweek
model = lm(formula,data=adult.df)
sqrt(vif(model))>2

adult_ctree =  ctree(formula,data=adult.df)
plot(adult_ctree)



#rpart 사용해 보기  ctree 비해서 단순한 형태를 제공
install.packages("rpart")
library(rpart)


