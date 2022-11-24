install.packages("usmap")
library(tidyverse)
library(usmap)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}
View(df)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Variables to answer questions

# This calculates a dataframe that groups the sum of total_jail_pop for every year
year_most_inc <- df %>% group_by(year)%>% 
  summarise(total_jail_pop = max(sum(na.omit(
  total_jail_pop)))) 

# This pulls the year with the most people incarcerated
most_year <- year_most_inc %>% filter(total_jail_pop == (max(na.omit(total_jail_pop)))) %>%
pull(year)
print(most_year)

# This calculates the most urbanicity of the county with the most people incarcerated
num_most_inc <- df %>%
  filter(total_jail_pop == (max(na.omit(total_jail_pop))))%>%
  pull(urbanicity)
print(num_most_inc)

# This pulls the actual number from the county with the most incarcerated
num_most_inc <- df %>%
  filter(total_jail_pop == (max(na.omit(total_jail_pop))))%>%
  pull(county_name)
print(num_most_inc)

# This pulls the state where the most people are incarcerated because of ICE
ice_arrests <- df %>%
  filter(total_jail_from_ice == (max(na.omit(total_jail_from_ice))))%>%
  pull(state)
print(ice_arrests)

# This pulls the county where the most people are incarcerated because of ICE
ice_arrests_county <- df %>%
  filter(total_jail_from_ice == (max(na.omit(total_jail_from_ice))))%>%
  pull(county_name)
print(ice_arrests_county)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function groups the dataset by year first, removing na values.
# It then calculates the sum of the total in jail in each year. 

get_year_jail_pop <- function() {
 
  total_pop <- df %>% group_by(year) %>% 
  filter(total_jail_pop != "NA") %>%
  summarise(total_jail_pop = sum(total_jail_pop))
    return(total_pop)   
}

# This function takes the function that calculates the total in jail every year.
# It plots a bar graph to show the distribution of those incarcerated from 1970-2018.

plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
       x = "Year",
       y = "Total Jail Population",
       subtitle = "Shows the jail population distribution ranging from 1970 to 2018.
       There is a steady increase.") +
    scale_y_continuous(labels = scales::comma) 
} 
plot_jail_pop_for_us()

# Summary Paragraph:
# In this graph, it answers the question "what does the jail population each year in the US look like?
# The population of those in jail in the US are displayed.
# One of the key observations is that it is increasing, with numerous factors.
# There is an increase in population that will get more people arrested, but there
# are also instances of violence and hate that could come from racism that could result in arrests
# In addition, the wealth gap begins to increase, resulting in more poverty and an
# increase of crimes in those communities. This poverty can also be related to 
# inequalities because those communities could contain underrepresented groups of people.

## Section 4  ---- 
#----------------------------------------------------------------------------#
# This function groups and separates by state, removing NA
# It then calculates the sum of each state for total jail population per year

get_year_jail_pop_states <- function(states){
 total_pop_state <- df %>% 
    filter(str_detect(state, paste(states, collapse = "|")) == TRUE) %>%
  group_by(year, state) %>%
    filter(total_jail_pop != "NA")%>%
    summarise(total_jail_pop = sum(total_jail_pop))
    return(total_pop_state)
}

# This uses the function that separates states and sum, and uses it to create a graph
# It uses a line chart, and takes in WA, OR, and CA as arguments to show
# The jail population of each state from 1970-2018

plot_jail_pop_by_states <- function(states){
  ggplot(get_year_jail_pop_states(states), aes(x = year, y = total_jail_pop)) +
  geom_line(aes(color = state, linetype = state)) +
    labs(title = "Jail Population in States (1970-2018)",
         x = "Years",
         y = "Jail Population",
         caption = "Line chart that shows three different states, and their 
         corresponding jail population throughout the years.")
}
plot_jail_pop_by_states(c("WA", "OR", "CA"))

## Summary Paragraph
# This graph answers the question "what is the jail population throughout each year
# in specific states?" Of the three, the key observation is that California has a 
# much higher jail population compared to Oregon and Washington. This is because it has
# a much higher general population as well. From 1978 to 1990, there is a big increase in the
# jail population in California. This may because they started getting stricter such as
# cracking down on drugs, and other crimes. Another key observation is that while Oregon 
# and Washington are gradual increases in jail population alongside their general population, 
# California fluctuates a lot in increasing and decreasing its jail population. 
# With a bigger population, the distribution can get a lot more extreme.

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# This function creates a column calculating the total jail admittance divided by 
# the total county population to calculate percent
# It then separates by year, urbanicity, and percent

proportion <- df %>% mutate(percent = (total_jail_adm / total_pop) * 10)

get_urbanicity <- function(country_type){
  total_type <- proportion %>% 
    filter(str_detect(urbanicity, paste(country_type, collapse = "|")) == TRUE, year > 1980) %>%
    group_by(percent, urbanicity, year) %>%
    select('percent', 'urbanicity', 'year')
  return(total_type)
}

# Uses the above function to create a vertical line chart to show the percentage
# of people in jail in different county urbanicities. 

plot_rural_urban <- function(country_type)  {
  ggplot(get_urbanicity(country_type), aes(x = year, y=percent)) +
    geom_line(aes(color = urbanicity, linetype = urbanicity)) +
    labs(title = "Average percentage in Jail (Rural vs Suburban)",
         x = "Year",
         y = "Percent", 
         caption = "The graph shows that between rural and suburban
         there is an overall higher percent of incarceration in rural areas.
         The percentages can get relatively high in terms of jail admittance.")
         
  }

# Provides two of the types of counties compared, taken as arguments for the function.
plot_rural_urban(c("suburban","rural"))

## Summary Paragraph
# This graph answers the question "How does poverty in the county affect the percentage of
# people incarcerated? Comparing rural areas to suburban areas, the overwhelming majority
# shows that there is a huge gap in the percentages with rural having highs above 10% after 
# 2004. This graph also corresponds with the increase in jail populations graph as there is 
# an increase in percentages throughout the years up until 2018. In 2008, we see a 
# big spike in jail percentage in suburban counties. This could be because of the financial
# crisis that also affecte counties like the suburban areas, and causes panic.
# Overall, it shows that there may be inequality through the wealth gap as poverty 
# can lead to a more riskier quality of life, and crimes can be commited all around in necessity.

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Function that takes the total jailed from ice, separating by state name, and regions
# Chose specifically 2018 for most recent data

ice_map <- function() {
ice_data <- df %>%
    filter(year == 2018) %>%
 select(state, total_jail_from_ice) %>%
    group_by(state)%>%
    summarise(total_jail_from_ice = sum(total_jail_from_ice, na.rm = TRUE))
  state_abb <- data.frame(state.abb, state.name) %>%
    rename(state = state.abb) 
  ice_data <- left_join(ice_data, state_abb, by = "state")
  ice_data$state.name <- tolower(ice_data$state.name)
  state_data <- map_data("state")
  state_map <- left_join(state_data, ice_data, by = c('region' = 'state.name'))
  return(ice_data)   
}

ice_map_data <- ice_map()

# Creates a map using the total jail from ice in each state function above
# Added in colors and legend

map_ice <- function(){
ice_arrest_plot <- plot_usmap(
  data = ice_map_data, values = "total_jail_from_ice", lines = "purple"
  ) + 
  scale_fill_continuous(
    low = "white", high = "purple", name = "Number of Ice Arrests", label = scales::comma
  ) + 
  labs(title = "ICE Arrests Throughout US States", 
  caption = "There are several states near the border that have a lot of arrests, and raises concerns.") + 
  theme(legend.position = "right")
return(ice_arrest_plot)
}

map_ice()

## Summary Paragraph
# This map answers the question "What are the most popular states with high ICE arrests in America?"
# California, Texas, and Florida are all near the Mexican Border and have the most ICE arrests
# in the country. This shows that there is more of a inequality in Southern states for 
# people arrested from ICE as a result. There is an inequality in these states on how immigrants 
# are treated, and there is a lot of xenophobia as a result too. In recent years, these problems
# have become even more relevant.

