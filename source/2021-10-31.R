# 교차검증 k-fold  225page
Name = c('a', 'b', 'c', 'd', 'e', 'f')
Score = c(90,85,99,75,65,88)
df = data.frame(Name,Score)
df

install.packages("cvTools")
library(cvTools)

cross =  cvFolds(n=6,K=3,R=1,type = 'random')
cross
str(cross)

cross$which == 1
cross$subsets[cross$which == 1,1]
cross$subsets[cross$which == 2,1]
cross$subsets[cross$which == 3,1]

for(k in 1:3){
  idx =  cross$subsets[cross$which == k,1]
  print('------------------------n')
  cat(k,' : 검증세트')
  print(df[idx,])
  cat('훈련세트')
  print(df[-idx,])
  
}


# 학습용과 훈련용 데이터 구분
# 고전적인 방법  7:3 으로 셈플링
# 진전된 방법이 k fold  

# 7장 연습문제 6번 확인
df1 = read.csv('D:\\data\\Part-II\\user_data.csv',header = T)
df2 = read.csv('D:\\data\\Part-II\\return_data.csv',header = T)
View(df2)
table(df2$return_code)
df2$return_code2[df2$return_code == 1] = 'return_code1'
df2$return_code2[df2$return_code == 2] = 'return_code2'
df2$return_code2[df2$return_code == 3] = 'return_code3'
df2$return_code2[df2$return_code == 4] = 'return_code4'
head(df2)

df2 = dcast(df2,user_id ~ return_code2,length)
head(df1)

left_join(df1,df2,by='user_id')


install.packages("lattice")
library(lattice)

install.packages("mlmRev")
library(mlmRev)
data("Chem97")
View(Chem97)


par(mfrow = c(1,1))
hist(Chem97$gcsescore)
histogram(~gcsescore|factor(score),data= Chem97)
table(Chem97$score)

hist(table(Chem97$score, Chem97$gcsescore))

densityplot(~gcsescore|factor(score),data=Chem97,
            group = gender,
            plot.points=T, auto.key=T)


barchart(lea ~ school, data= Chem97)

# 비정형 데이터 분석 토픽분석  구름 클라우드 
install.packages("koNLP")

R.version


# KONLP  단어 추출하는 library
# https://cran.r-project.org/bin/windows/Rtools/ 
# R에서 자주 사용하는 페키지모음
install.packages("multilinguer")
library(multilinguer)
# konlp를 다운로드할때 또는 사용할때 필요한 java 이며
# 이 함수는 multilinguer 에 포함되어 있다
install_jdk()

# git이라는 저장소가 있는데.. 인터넷에서 소스를 다운로드하기위해
# 사용하는 명령어 또는 저장소를 git이라고 하고
# git을 통해 다운로드하게 하는 페키지 remote
install.packages("remotes")
# 인터넷 저장공간에서 소스를다운로드 하는 방법
remotes::install_github('haven-jeon/KoNLP', upgrade = "never",
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
install.packages('tm')
library(tm)
install.packages("wordcloud")
library(wordcloud)

facebook =  file("D:\\data\\Part-II\\facebook_bigdata.txt", encoding = 'utf-8')
facebook_data =  readLines(facebook)
facebook_data[1]
extractNoun(facebook_data[1])
paste(extractNoun(facebook_data[1]), collapse = " ")
# 함수로 적성...
exNouns = function(x)
{
  paste(extractNoun(x), collapse = " ")
}

facebook_nouns =  sapply(facebook_data,exNouns)

facebook_nouns[1]

myCorpus =  Corpus(VectorSource(facebook_nouns))

myCorpusPrepro =  tm_map(myCorpus,removePunctuation)
myCorpusPrepro =  tm_map(myCorpusPrepro,removeNumbers)
myCorpusPrepro =  tm_map(myCorpusPrepro,tolower)
myCorpusPrepro =  tm_map(myCorpusPrepro,removeWords,stopwords('english'))
inspect(myCorpusPrepro)

term =  TermDocumentMatrix(myCorpusPrepro, 
                   control = list(wordLenghts=c(4,16)))

term

myTerm_df = as.data.frame(as.matrix(term))
wordResult = sort(rowSums(myTerm_df), decreasing = TRUE)

names(wordResult)
word_df = data.frame(word = names(wordResult), freq = wordResult)
pal = brewer.pal(12,'Paired')
wordcloud(word_df$word,word_df$freq,scale = c(5,1),
          min.freq = 3,random.order = F,
          rot.per = .1, colors = pal,family = 'malgun')







# 구름모양 시각화 wordcloud / wordcloud2
install.packages("wordcloud2")
library(wordcloud2)

#  KoNLP가 정상적으로 설치되었는지 검증
#textData<-"동해물과 백두산이 마르고 닳도록 하느님이 보우하사  다방커피"
#extractNoun(textData)

# 검증이 끝나면...
# 필요한 단어추가를 위해서 기본 사전을 로드
user_dic<- data.frame(term=c("다방커피"),tag='ncn')
buildDictionary(ext_dic = "sejong",user_dic = user_dic)

# 시각화 할 텍스트 데이터 
# 작업경로 설정
setwd("E:/빅데이터_공유폴더/Part-II")
# 설정된 작업경로에 있는 파일 선택 ,  utf-8로 읽어오기위해서 파일을 access한것음
facebook<- file("marketing.txt", encoding = "UTF-8")
# 해당파일에대한 접근권한을 얻어온거기 때문에.. 이제는 파일의 데이터를 읽자
data.class()<- readLines(facebook)
# 읽어온 데이터 확인
head(data)
