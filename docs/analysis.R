incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

library(dplyr)
library(ggplot2)

df <- incarceration %>%
  select(yfips, year, state, total_pop, urbanicity, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, white_jail_pop) 

#part I Summary
#1 What is the proportion of the jail population for each race?
sum_total_jail_pop <- round(sum(df$total_jail_pop, na.rm = TRUE))

total_aapi <- round(sum(df$aapi_jail_pop, na.rm = TRUE))
prop_aapi <- round(total_aapi/ sum_total_jail_pop, 5)

total_black <- round(sum(df$black_jail_pop, na.rm =TRUE))
prop_black <- round(total_black/sum_total_jail_pop, 4)

total_latinx <- round(sum(df$latinx_jail_pop, na.rm =TRUE))
prop_latinx <- round(total_latinx/sum_total_jail_pop, 3)

total_white <-round(sum(df$white_jail_pop, na.rm =TRUE))
prop_white <- round(total_white/sum_total_jail_pop, 3)


#2 What is the average of the jail population for each race?
avg_aapi <- round(mean(df$aapi_jail_pop,na.rm = TRUE),2)
avg_black <- round(mean(df$black_jail_pop, na.rm = TRUE),2)
avg_latinx <- round(mean(df$latinx_jail_pop, na.rm = TRUE), 2)
avg_white <- round(mean(df$white_jail_pop, na.rm = TRUE),2)

#3 What is the highest jail population for each race?
highest_aapi <- df %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm =TRUE)) %>%
  pull (aapi_jail_pop)
highest_black <- df %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm =TRUE)) %>%
  pull(black_jail_pop)
highest_latinx <- df %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm =TRUE)) %>%
  pull (latinx_jail_pop)
highest_white <- df %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm =TRUE)) %>%
  pull (white_jail_pop)

#4 Which state had the highest number of the jail population in each race?
highest_aapi_state <- df %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm =TRUE)) %>%
  pull (state)
highest_black_state <- df %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm =TRUE)) %>%
  pull(state)
highest_latinx_state <- df %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm =TRUE)) %>%
  pull (state)
highest_white_state <- df %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm =TRUE)) %>%
  pull (state)

#5 How much has my variable change over the last 30 years?
aapi_pop_1988 <- df %>%
  group_by(year) %>%
  filter(year == 1988) %>%
  summarize(aapi_total_pop_1988 = round(sum(aapi_jail_pop, na.rm = TRUE))) %>%
  pull(aapi_total_pop_1988)
aapi_pop_2018 <- df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(aapi_total_pop_2018 = round(sum(aapi_jail_pop, na.rm = TRUE))) %>%
  pull(aapi_total_pop_2018)
change_aapi_jail_pop_10yr <- (aapi_pop_2018 - aapi_pop_1988)

black_pop_1988 <- df %>%
  group_by(year) %>%
  filter(year == 1988) %>%
  summarize(black_total_pop_1988 = round(sum(black_jail_pop, na.rm = TRUE))) %>%
  pull(black_total_pop_1988)
black_pop_2018 <- df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(black_total_pop_2018 = round(sum(black_jail_pop, na.rm = TRUE))) %>%
  pull(black_total_pop_2018)
change_black_jail_pop_10yr <- (black_pop_2018 - black_pop_1988)

latinx_pop_1988 <- df %>%
  group_by(year) %>%
  filter(year == 1988) %>%
  summarize(latinx_total_pop_1988 = round(sum(latinx_jail_pop, na.rm = TRUE))) %>%
  pull(latinx_total_pop_1988)
latinx_pop_2018 <- df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(latinx_total_pop_2018 = round(sum(latinx_jail_pop, na.rm = TRUE))) %>%
  pull(latinx_total_pop_2018)
change_latinx_jail_pop_10yr <- (latinx_pop_2018 - latinx_pop_1988)

white_pop_1988 <- df %>%
  group_by(year) %>%
  filter(year == 1988) %>%
  summarize(white_total_pop_1988 = round(sum(white_jail_pop, na.rm = TRUE))) %>%
  pull(white_total_pop_1988)
white_pop_2018 <- df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(white_total_pop_2018 = round(sum(white_jail_pop, na.rm = TRUE))) %>%
  pull(white_total_pop_2018)
change_white_jail_pop_10yr <- (white_pop_2018 - white_pop_1988)


# Part II jail population for each race from 1987-2018
df2 <- incarceration %>%
  filter(year >=1987) %>%
  select(year,total_jail_pop, aapi_jail_pop,black_jail_pop, white_jail_pop,latinx_jail_pop) %>%
  group_by(year) %>%
  summarise(
    black_jail_sum =sum(black_jail_pop, na.rm =TRUE),
    white_jail_sum =sum(white_jail_pop, na.rm =TRUE),
    aapi_jail_sum =sum(aapi_jail_pop, na.rm =TRUE),
    latinx_jail_sum= sum(latinx_jail_pop, na.rm =TRUE)
  )


ggplot(df2, aes(x=year)) + 
  geom_line(aes(y = aapi_jail_sum), color = "red") + 
  geom_line(aes(y = white_jail_sum), color="blue") +
  geom_line(aes(y= black_jail_sum), color="yellow") +
  geom_line(aes(y= latinx_jail_sum), color="green")+
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Population for each race from 1987-2018")+
  xlab("Year") +
  ylab("Race") 

# part III the relationship between the black jail population and the white jail population 
df <- df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  mutate(white_trend_2018 = white_jail_pop)
df <- df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  mutate(black_trend_2018 = black_jail_pop)


ggplot(df, aes(x=white_trend_2018, y=black_trend_2018)) +
  geom_point () +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) +
  labs(title = "White pop vs Black pop in 2018",
       x = "White jail population in 2018",
       y ="Black jail population  in 2018")

# part 4 map
# White jail population map since 1990

#install.packages("mapproj")
#install.packages("usdata")
#install.packages("patchwork")
library("tidyverse")
library("maps")
library("mapproj")
library("usdata")
library("patchwork")

incarceration <- na.omit(incarceration)  

white_jail <-incarceration %>%
  filter(year >= "1990", na.rm = TRUE) %>%
  select(white_jail_pop, fips)


county_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",",na.rm = TRUE) %>%
  left_join(county.fips, by="polyname", na.rm=TRUE)



map_data <- county_shape %>%
  left_join(white_jail, by="fips", na.rm = TRUE) 


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

ggplot(map_data) +
  geom_polygon(aes(x=long, y= lat, group= group, fill = white_jail_pop),
               color = "black",
               size = 0.3
  ) +
  scale_fill_continuous(limits = c(0, max(map_data$white_jail_pop)), na.value = "gray", low = "yellow", high ="red") +
  coord_map() +
  labs(fill = "population") +
  blank_theme +
  ggtitle("Black Jail Population since 1990")
