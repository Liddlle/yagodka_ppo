library(readr)
library(dplyr)

Speed_Dating_Data <- read_csv("~/yagodka_ppo/Speed Dating Data.csv")

uniq = Speed_Dating_Data %>% group_by(iid,age,gender) %>% tally()


library(plotly)
plot_ly(uniq, x = ~factor(age), color = ~factor(gender), type = 'histogram', name = 'Age and gender') %>%
 layout(yaxis = list(title = 'Count'), barmode = 'stack')

