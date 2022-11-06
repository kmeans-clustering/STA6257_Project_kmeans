if(!require('readxl')) {
  install.packages('readxl')
  library('readxl')
}

filename <- "online_retail_II.xlsx"
df = read_excel(filename) 
str(df)
head(df, n=5L)
summary(df)
df_trimmed = subset(df, select = -c(Description) )
#scaled_df<- scale(df)
#library(xtable)
#tab1 <- summarize(df, type = "numeric")
#print(xtable(summary(df)), comment=FALSE)
library("knitr")
#install.packages('papeR')
library('papeR') 
summarize(df)
library('tidyverse')
kable(summarize(df))
kable(summarize(df, type='numeric'))
library(kableExtra)
row.names(df)
summarize(df) %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

#install.packages("kableExtra")
d <- structure(list(Hostname = structure(c(8L, 8L, 9L, 5L, 6L, 7L, 
                                           1L, 2L, 3L, 4L), .Label = c("db01", "db02", "farm01", "farm02", 
                                                                       "tom01", "tom02", "tom03", "web01", "web03"), class = "factor"), 
                    Date = structure(c(6L, 10L, 5L, 3L, 2L, 1L, 8L, 9L, 7L, 4L
                    ), .Label = c("10/5/2015 1:15", "10/5/2015 1:30", "10/5/2015 2:15", 
                                  "10/5/2015 4:30", "10/5/2015 8:30", "10/5/2015 8:45", "10/6/2015 8:15", 
                                  "10/6/2015 8:30", "9/11/2015 5:00", "9/11/2015 6:00"), class = "factor"), 
                    Cpubusy = c(31L, 20L, 30L, 20L, 18L, 20L, 41L, 21L, 29L, 
                                24L), UsedPercentMemory = c(99L, 98L, 95L, 99L, 99L, 99L, 
                                                            99L, 98L, 63L, 99L)), .Names = c("Hostname", "Date", "Cpubusy", 
                                                                                             "UsedPercentMemory"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                       -10L))
kable(d)
library('qwraps2')
our_summary1 <-
  list("Miles Per Gallon" =
         list("min"       = ~ min(mpg),
              "max"       = ~ max(mpg),
              "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
         list("min"       = ~ min(disp),
              "median"    = ~ median(disp),
              "max"       = ~ max(disp),
              "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
         list("min"       = ~ min(wt),
              "max"       = ~ max(wt),
              "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
         list("Three" = ~ qwraps2::n_perc0(gear == 3),
              "Four"  = ~ qwraps2::n_perc0(gear == 4),
              "Five"  = ~ qwraps2::n_perc0(gear == 5))
  )
summary_table(mtcars2, our_summary1)
