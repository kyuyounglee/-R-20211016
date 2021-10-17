#연산자
num1 = 100
num2 = 200
num1 == num2
num1 != num2
num1 > num2
num1 >= num2
num1 < num2
num1 <= num2
num1 & num2
num1 | num2
num1 > 10 & num2 < 200

kor<-80
eng<-90
math<-50
# 3과목의 평균이 60이상이고 각 과목의 점수가 40이상이면 
#합격을 처리하기위한 조건을 만드시오
mean(c(kor,eng,math))/3 >= 60 &
kor >= 40 & eng >= 40 & math >= 40


mean(c(kor,eng,math))
(kor+eng+math)/3

# 데이터 만들고 평균 구하기
no<- c(1:5)
name<-c("홍길동","이순신","강감찬","유관순","김유신")
score <- c(85,78,89,90,74)

exam<-cbind(no,name,score)
exam<-data.frame(no,name,score)
# 평균이상인 데이터
index = which(exam$score>=mean(exam$score))
exam[index,]
index = which(exam$score<mean(exam$score))
exam[index,]

#exam 평균 이상과 미만인 데이터의 수
index = which(exam$score>=mean(exam$score))
exam[index,]
nrow(exam[index,])

index = which(exam$score<mean(exam$score))
exam[index,]
nrow(exam[index,])

score<-scan()
# 학점계산기
# 100 ~ 90 : A
# 89 ~ 80 : B
# 79 ~ 70 : C
# 69 ~    : F


# 잘못된 표현방법
score = 90
if(score >=90 & score<=100){cat('A')}
if(score >=80 & score<=89){ cat('B')}


# 성인인증 방법
# 나이가 19살 이상이면 성인
age<-15
result<-0
if(age>=19){ 
  result<-'성인' 
}else{
    result<-'미성년' 
}
result


result<-ifelse(age>=19,'성인','미성년')
result

color<-"gray"
#  color의 변수가 red면 붉은색, green이면 파랑  gray 회색
#이라고 표현해 보자
switch (color,
  'red' = '붉은색'
  ,'green' = '파란색'
  ,'gray' = '회색'
)

cat("2 x",1,'=', 2*1 )
cat("2 x",2,'=', 2*2 )
cat("2 x",3,'=', 2*3 )

# 반복문
for (n in c(1:9) ) {
  cat("2 x",n,'=', 2*n,'\n' )
}

# 완전한 구구단
for(n in c(2:9)){
  for(m in c(1:9)){
    cat(n,"x",m,'=', m*m,'\n' )
  }
  cat("\n")  # 단수사이 한줄 띄우기
}

# 완전한 구구단
for(m in c(1:9)){
  for(n in c(2:9)){
    cat(n,"x",m,'=', n*m,'\t' )
  }
  cat("\n")  # 단수사이 한줄 띄우기
}

#제어문  - if  if~else    if~elseif   ifelse
      #  - switch,  (which)
# 순환문  for   벡터를 이용해서 그 벡터의 수 만큼 반복
# 벡터의 요소를 순환할때마다 가져온다다
