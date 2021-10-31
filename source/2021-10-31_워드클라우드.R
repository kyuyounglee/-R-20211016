# konlp 가져오는 방법
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

# 한글 데이터 로드
facebook =  file("D:\\data\\Part-II\\facebook_bigdata.txt", encoding = 'utf-8')
facebook_data = readLines(facebook)


library(stringr)
# 한글만 찾아주는 함수
hangle_choice = function(list_x){
  result_list = c()
  for(index in 1:length(df$data)){
    temp = str_extract(list_x[index], "[ㄱ-ㅣ가-힣]+")
    result_list = append(result_list, temp)  
  }  
  result_list
}
# konlp를 이용한 한글 명사 추출
cloudExtract = function(string_data){
  # 단어선별
  data =  extractNoun(string_data)
  df = as.data.frame(data)
  
  df = subset(df,str_length(df$data) > 1)
  length(df$data)
  
  convert_data = hangle_choice(df$data)
  # data fram 형태로 변환
  convert_data = as.data.frame(convert_data)
  convert_data[!is.na(convert_data$convert_data),]
}

#데이터 수집을 위한 벡터변수 선언
result_v = c()
#리스트 형태로 되어 있는 한글을 우리가 만든 함수적용해서 데이터 합치기
for(index in 1:length(facebook_data)){
  result_v = append(result_v,cloudExtract(facebook_data[index]))
}

# 워드 클라우드 로딩
install.packages("wordcloud")
library(wordcloud)
pal = brewer.pal(12,'Paired')
wordcloud(names(table(result_v)), table(result_v),
          min.freq = 3,random.order = F,
          rot.per = .1, colors = pal,family = 'malgun')
