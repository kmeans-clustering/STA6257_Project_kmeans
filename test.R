if(!require('readxl')) {
  install.packages('readxl')
  library('readxl')
}

filename <- "online_retail_II.xlsx"
df = read_excel(filename) 
head(df)
summary(df)
#library(xtable)
tab1 <- summarize(df, type = "numeric")
#print(xtable(summary(df)), comment=FALSE)
library("knitr")
#install.packages('papeR')
library('papeR') 
summarize(df)
library('tidyverse')
kable(summarize(df))
kable(summarize(df, type='numeric'))
library(kableExtra)

summarize(df) %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

#install.packages("kableExtra")
