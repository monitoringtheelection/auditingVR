library(tidyverse)

number_rows <- report %>%
  bind_rows(
    .[1, ] %>%
      mutate(
        date_origin = "042618",
        totalB = totalA
      ), .
  ) %>%
  select(date_origin, totalB) %>%
  Kmisc::dedup() %>%
  mutate(
    date = as.Date(lubridate::mdy(date_origin))
  )

options(digits = 4, scipen = 999)

p <- ggplot(number_rows, aes(x = date, y = totalB)) + 
  geom_line() + 
  xlab("Date") + 
  ylab("Number of Registered Voters (Million)") + 
  scale_y_continuous(
    labels = function(x) format(x / 1e6, nsmall = 2)
  ) + 
  scale_x_date(
    breaks = c(seq(
      from = as.Date("2018-05-01"),
      to = as.Date("2019-06-01"), 
      by = "3 months"
    )), 
    date_labels = "%b %Y"
  )

pdf("CAVR-OC/Ch4_nrow.pdf", width = 4.5, height = 3)
Kmisc::pdf_default(p)
dev.off()



  
  
