#Analysis of Sims

library(ggplot2)
library(dplyr)

good <- read.csv("/Users/ashleymullan/Documents/Grad School/Wake Forest/M.S. Coursework/Fall 2023/STA712/Final Project/sta712_f23_demos/good_results.csv")


#notes: all 10K runs generated p-vals that round to 0 with 50 decimal places

bad1 <- read.csv("/Users/ashleymullan/Documents/Grad School/Wake Forest/M.S. Coursework/Fall 2023/STA712/Final Project/sta712_f23_demos/bad_results1.csv")

bad1 |> 
  pull(p_t) |>
  round(10) |>
  table()

bad1 |> ggplot(aes(x = slope)) + geom_histogram()

bad2 <- read.csv("/Users/ashleymullan/Documents/Grad School/Wake Forest/M.S. Coursework/Fall 2023/STA712/Final Project/sta712_f23_demos/bad_results2.csv")

bad2 |> 
  pull(p_t) |>
  round(10) |>
  table()

bad2 |> ggplot(aes(x = slope)) + geom_histogram()

bad2 |> 
  pull(p_perm) |>
  table()
