library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)

welfare = raw_welfare
head(welfare)
tail(welfare)
dim(welfare)
str(welfare)
summary(welfare)
welfare <- rename(welfare, sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10, 
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

welfare$sex = ifelse(welfare$sex == 1, "male", "female")

#성별 변수 검토 및 전처리
#변수 검토 (이상치 확인)
class(welfare$sex)
table(welfare$sex) #이상치확인

#이상치 결측 처리
welfare$sex = ifelse(welfare$sex == 9, NA, welfare)
#결측치 확인
table(is.na(welfare$sex))

#월급 변수 검토 및 전처리
class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
welfare$income = ifelse(welfare$income %in% c(0,9999), NA, welfare$income) # 이상치 결측처리
table(is.na(welfare$income))

#성별 따른 월급 차이 분석
#1. 성별 월급 평균표
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

#2 성별 월급 평균 그래프 
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

#나이와 월급 관계 (몇살에 월급을 가장 많이 받는지)
#1. 변수 검토
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#2. 전처리
#이상치 확인
summary(welfare$birth)
#결측치 확인
table(is.na(welfare$birth))
#이상치 결측 처리
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
#결측치 확인
table(is.na(welfare$birth))

#3. 파생변수만들기 (나이)
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)

qplot(welfare$age)

#4. 나이와 월급의 관계 분석하기
#1 나이에 따른 월급 평균표
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))

head(age_income)
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#09-4 연령대 따른 월급 차 (어떤 연령대의 월급이 가장 많은지)
#연령대 변수 검토 및 전처리
#파생변수 만들기 - 연령대
welfare = welfare %>% 
  mutate(age1 = ifelse(age < 30, "age_20",
                       ifelse(age < 40, "age_30",
                              ifelse(age < 50, "age_40",
                                     ifelse(age < 60, "age_50", "age_60")))))
               
table(welfare$age1)
qplot(welfare$age1)

#연령대 따른 월급 차이 분석
#1 연령대별 월급 평균표
age1_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age1) %>% 
  summarise(mean_income = mean(income))
age1_income

qplot(data = age1_income, aes(x = age1, y = mean_income)) + geom_col()

#파생변수 만들기 - 나이
welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age < 59, "middle", "old")))
table(welfare$ageg)                 

ageg_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

qplot(welfare$ageg)
#그래프 만들기
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()

#막대정렬 : 초년, 중년, 노년 나이순
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#09-5 연령대 및 성별월급 차이
#성별 월급 차이는 연령대별로
#(1.변수 검토전처리(연령, 성별, 월급) 2.분석 )
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarize(mean_income = mean(income))
sex_income

#2 그래프 만들기
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#성별 막대 분리
ggplot(data = sex_income, aes(x = age, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))

#나이 및 성별 월급 차이 분석
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))

head(sex_age)

#나이 및 성별 월급차 그래프
ggplot(data = sex_age, aes(x = age, y = mean_income, l = sex)) + geom_line()

#09-6 직업별 월급차이
#변수 검토 및 전처리
class(welfare$code_job)
table(welfare$code_job)

#전처리 (직업분류코드 목록부르기)
library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)

#welfare에 직업명 결합
welfare = left_join(welfare, list_job, id = "code_job")
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job)