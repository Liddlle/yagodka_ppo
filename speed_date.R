library(readr)
library(dplyr)

Speed_Dating_Data <- read_csv("~/yagodka_ppo/Speed Dating Data.csv")

Speed_Dating_Data$race = factor(Speed_Dating_Data$race)
Speed_Dating_Data$race = plyr::revalue(Speed_Dating_Data$race, c("1"="Black", 
                                                                 "2"="European", 
                                                                 "3"="Latino", 
                                                                 "4"="Asian", 
                                                                 "6"="Other"))
Speed_Dating_Data$goal = factor(Speed_Dating_Data$goal)
Speed_Dating_Data$goal = plyr::revalue(Speed_Dating_Data$goal, c("1"="Seemed like a fun night out", 
                                                                 "2"="To meet new people", 
                                                                 "3"="To get a date", 
                                                                 "4"="Looking for a serious relationship", 
                                                                 "5"="To say I did it", 
                                                                 "6"="Other"))
#тоже самое надо для fields of study
uniq = Speed_Dating_Data %>% group_by(iid,age,gender, field_cd,race,imprelig,goal,imprace,exphappy) %>% tally()


library(plotly)
library(ggplot2)
#возраст
ggplot(data=na.omit(select(uniq, age, gender)), aes(x=factor(age), fill = factor(gender))) +
  geom_bar()
ggplotly()

#раса 
ggplot(data=na.omit(select(uniq, gender,race)), aes(x=race, fill = factor(gender))) +
  geom_bar()
ggplotly()

#field of study
ggplot(data=na.omit(select(uniq, field_cd)), aes(x=(field_cd))) +
  geom_bar()
ggplotly()

#как с возрастом меняется цель свиданий
ggplot(data=na.omit(select(uniq, age,goal)), aes(age, fill = goal, colour = goal)) +
  geom_density(alpha = 0.1)
ggplotly()


plot_ly(data = Speed_Dating_Data, x = ~age, y = ~factor(imprelig))

ggplot() + geom_bar(data = uniq, aes(x = age, color = factor(imprelig))) #, position = 'dodge')
ggplot() + geom_point(data = Speed_Dating_Data, aes(x = age, y = age_o, color = factor(gender)))



#Match by Number of date




ggplot(data=na.omit(select(uniq, age, gender,exphappy)), aes(y = (exphappy),x = factor(gender))) +
  geom_boxplot() +ggplotly()
t.test(exphappy~factor(gender), data = na.omit(select(uniq, age, gender,exphappy)))




ggplot(data=na.omit(select(uniq, age,goal)), aes(age, fill = goal, colour = goal)) +
  geom_density(alpha = 0.1)


ggplot(data=na.omit(select(uniq, age, gender,exphappy,goal)), aes(x=age, y = (exphappy),color = factor(gender))) +
  geom_point()+ facet_wrap(~ goal) 



num_match = Speed_Dating_Data %>% group_by(iid,age,gender, field_cd,race,imprelig,goal,imprace,exphappy) %>% 
  dplyr::summarise(n_dec = sum(dec), n_deco = sum(dec_o), n_match = sum(match))
num_match = num_match %>% filter(as.character(goal) != 'Other')

ggplot(data=na.omit(select(num_match, age, gender,n_match,n_dec))) +
         geom_point(aes(x=age, y = n_dec, color = factor(gender),size= n_match))+ facet_wrap(~ goal) 

                                                                                                                                                                            