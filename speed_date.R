Speed_Dating_Data <- read_csv("~/krasotochki/Speed Dating Data.csv")

library(dplyr)
uniq = Speed_Dating_Data %>% group_by(iid,age,gender) %>% tally()


library(ggplot2)
ggplot(data = uniq, aes(x = factor(age), fill=factor(gender)))+geom_bar()


