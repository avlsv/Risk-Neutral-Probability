

setwd("~/Documents/Risk-Neutral-Probability/code")
data<-read_csv("data/Sep_t_Jan.csv")

data_selected <- data|> filter(str_detect(name, 'Adjusted'), term=="2024 Sep")

data_selected$term|> unique()

plot(data_selected$value)
