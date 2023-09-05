###
# Fake read in data ##
# setwd('~/BIOL570L/data)
# sqdf <- read.csv('squirrel_time.csv')
#

# set up environment
rm(list = ls())
library(ggplot2)
library(dplyr)

# Real read in data
sqdf <- read.csv('./data/example_data.csv')


# Take a look at the data
View(sqdf)


# summarize by category
sq_summary <- sqdf |> 
  group_by(category) |> 
  summarize(total_time = sum(total_mins))

# create graph
ggplot(data = sq_summary) +
  geom_bar(aes(x = category, y = total_time, fill = category),
           stat = 'identity') +
  scale_fill_discrete()+
  labs(x = "Behavior Category", y = "Total Time Spent [mins]")+
  theme_classic()+
  theme(legend.position = 'none')


###
# Chi-squared analysis
####

chisq.test(sq_summary$total_time)


# advanced analysis

# let's say you are interested in comparing 
# for unique categories
possible_categories <- unique(sqdf$category) # pull out individual category names

total_observation_time <- sum(sqdf$total_mins) #total up all time observed

# create a vector of expected time spent if all time was spent equally
expected_time_spent <- rep(total_observation_time/length(possible_categories), 
                           length(possible_categories))

# add to the sq_summary dataframe expected values
# this is a more advanced dplyr statement which reflects a common sql statement
sq_summary <- sq_summary |> # reassign a value to sq_summary by first taking sq_summary 
  left_join( #send sq_summary to left_join
    data.frame( # join a data frame which I create inside the statement
      category = possible_categories,
      expected_time = expected_time_spent 
    )
  )


chisq.test(x = sq_summary$total_time, p = sq_summary$expected_time,
           rescale.p = T)


# now we can change the time expected for a hypothetical squirrel
# in this case assume a squirrel spends more time foraging and resting but less time in conflict
expected_proportions <- c(0.1, 0.3,.3,.3)
expected_model <- total_observation_time * expected_proportions
chisq.test(x = sq_summary$total_time, p = expected_model,
           rescale.p = T)


##
# More detailed figure ##
##


# create a custom color scale for your data
# note this must be customized for your defined actions

colors = c(
  `chasing` = '#D81B60',
  `running_squirrel` = '#C75780',
  `taunting` = "#BF7993",
  `running_other` = "#1E88E5",
  `running_predator` = "#66A8E2",
  `searching` = "#FFC107",
  `collecting_food` = "#FDD458",
  `storing_food` = "#FFE597",
  `jumping` = "#FFEFBF",
  `eating` = '#004D40',
  `resting` = '#4D7F77'
)

ggplot(sqdf) + 
  geom_bar(aes(x = category, y = total_mins,
               fill = action),
           stat = 'identity', position = 'stack') +
  scale_fill_manual(values = colors)


ggplot(sqdf) + 
  geom_bar(aes(x = category, y = num_events,
               fill = action),
           stat = 'identity', position = 'stack') +
  scale_fill_manual(values = colors)
