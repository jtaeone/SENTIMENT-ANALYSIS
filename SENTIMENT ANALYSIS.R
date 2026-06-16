#댓글 크롤링
library(rvest)
library(RSelenium)
#cd C:\Rselenium
#java -Dwebdriver.gecko.driver=”geckodriver.exe” -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
remD <- remoteDriver(remoteServerAddr = 'localhost', port = 4445L, browserName = 'chrome')
remD$open()
remD$navigate("https://youtu.be/vpc_gnrRh8M?si=KRB7ymx3wCYU5FNr") #golden
remD$navigate("https://youtu.be/4cbTQhFIagg?si=Rv1jTdaGIzdNzqjm") #희재
remD$navigate("https://youtube.com/shorts/SHbYMsHitsU?si=st7oZOV65rUieBY1") #사결시
remD$navigate("https://youtu.be/IRlXSNRAebM?si=5foAgFY0lZCy5qNC") #어떻게 사랑이 그래요
remD$navigate('https://youtu.be/VkGRr9S4jlE?si=I-68SQ3WzZXwjwxD') #가슴아 가슴아
remD$navigate('https://youtu.be/O2DR7zbMRys?si=CvnzoRlLwOdxj_If') #미운오리새끼
remD$navigate('https://youtu.be/3REcll87Cls?si=S6tt_RqpVhcFhtq1') #사랑했지만
remD$navigate('https://youtu.be/f4cVKpK2YIY?si=Yh831rvbmm516sT7') #꿈에
remD$navigate('https://youtube.com/shorts/Mj9f1w3o5fI?si=x_wXXTKPtAjj0ebS') #말리꽃
remD$navigate('https://youtu.be/K9o1z-DvfEA?si=ZxfaZYx6uepeczsn') #Ready


#홈페이지 스크롤
prev_height <- remD$executeScript('return document.documentElement.scrollHeight')

while (TRUE) {
  remD$executeScript('window.scrollTo(0, document.documentElement.scrollHeight);')
  Sys.sleep(2)
  
  curr_height = remD$executeScript('return document.documentElement.scrollHeight')
  if (unlist(curr_height) == unlist(prev_height)) {
    break
  }
  prev_height = curr_height
}

html<-remD$getPageSource()[[1]]
html<-read_html(html) #페이지 소스 읽어오기

youtube_comments <- html %>% html_nodes("#content-text") %>%
  html_text() #선택된 노드를 텍스트화
youtube_comments <- youtube_comments[1:300]
head(youtube_comments)
youtube_comments

#텍스트 전처리
library(dplyr)
library(stringr)
library(tidytext)
library(readr)
youtube_comments <- gsub("\n", "", youtube_comments) #특정 문자를 원하는 형태(공백)로 변경
youtube_comments <- trimws(youtube_comments) #공백 제거
youtube_comments

youtube_comments <- youtube_comments %>% str_replace_all('[^가-힣]', ' ') %>% #한글이 아닌 모든 글자를 공백 처리 
  str_squish() %>% #연속된 공백 제거(공백 최대 1개)
  as_tibble()
youtube_comments

#원본 댓글 파일 저장 - 댓글 데이터 소실 방지
write.table(youtube_comments,
            file="son2.txt",
            sep=",",
            row.names=FALSE,
            quote=FALSE)
write.csv(youtube_comments,
          file="son10.csv")

youtube_comments <- youtube_comments %>%
  mutate(comment_id = row_number()) %>%
  unnest_tokens(input = value, output = word, token = "words", drop = F) #문장을 단어 기준으로 쪼개기
youtube_comments

#감성 사전 구축
dic <- read_delim('SentiWord_Dict.txt',
                  delim = '\t',
                  col_names = c('word', 'score'))
dic

#감성 사전 보정 및 감성 라벨링 - 부정확한 맥락 파악으로 인한 감정 오분류를 막기 위함
dic <- dic %>%
  filter(!word %in% c("미친", "슬픈", "소름", "일부러", "눈물", "슬픔"))

validated_comments <- youtube_comments %>%
  left_join(dic, by = "word") %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  group_by(comment_id, value) %>% 
  summarise(total_score = sum(score)) %>%
  ungroup() %>%
  mutate(
    has_real_negative = ifelse(
      str_detect(value, "망했|못하|싫|최악|불편|나대|실망|짜증|소음|꽥꽥|시끄|삑사리|테러|개판|어휴|에휴|시발|과잉|지겨|지겹"), 
      TRUE, 
      FALSE
    ),
    is_positive_context = ifelse(
      str_detect(value, "레전드|대박|천재|소름|잘하|넘사|월클"),
      TRUE,
      FALSE
    )
  ) %>%
  mutate(
    sentiment = ifelse(
      (total_score < 0 & !is_positive_context) | has_real_negative, 
      1,
      0  
    )
  )
  
#영상별 부정 댓글 개수
validated_comments %>%
  group_by(sentiment) %>%
  count()

#부정 문장 확인
validated_comments %>%
  filter(sentiment == 1) %>%
  select(comment_id, total_score, value) %>%
  arrange(total_score)

#마스터 데이터셋 구축
youtube_data <- data.frame(
  video_id   = c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10"),
  is_cover   = c(1, 1, 1, 1, 0, 0, 1, 1, 1, 0),
  is_shorts  = c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  comp_low = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1),
  comp_mid = c(0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
  comp_high = c(0, 1, 0, 0, 0, 0, 0, 1, 0, 0),
  n_negative = c(7, 22, 19, 13, 4, 9, 17, 27, 25, 9),
  n_total = c(282, 294, 295, 288, 85, 64, 260, 274, 181, 261)
)

#데이터 확인(EDA)
head(youtube_data)
str(youtube_data)
summary(youtube_data)

#로지스틱 회귀분석
logit_model <- glm(
  cbind(n_negative, n_total - n_negative) ~ is_cover + is_shorts + comp_low + comp_mid + comp_high,
  family = binomial,
  data = youtube_data)
summary(logit_model)
