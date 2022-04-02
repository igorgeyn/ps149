
### Igor Geyn, TA PS149
### Spring 20222

### This is the first of several scripts I'll be sharing with you all for PS149.
### The idea is to give you a taste of some of the things you'll see in lecture,
### reinforce key aspects of the tidyverse, and so on.

## this is how you install packages.
## just 'un-comment' the lines below (i.e. remove the pound/hashtag sign at the front)
## to comment you can also hit cmd + Shift + C on a Mac (PC command is analagous).

# install.packages('tidyverse')
# install.packages('lubridate')
# install.packages('vaporwave')
# install.packages('wesanderson')

## this is how you load packages. 
library(tidyverse)
library(lubridate)

## you can also load packages this way.
library(wesanderson); require(magrittr) #; library(vaporwave)

## this is a dataset on car velocity and distance-to-braking.
## it is used in a bunch of different R-learning situations.
data(cars)

# examine our data a bit
glimpse(cars)
head(cars)
tail(cars)

## create three variables/columns that we add on to our original df, `cars`,
## and then save this modified dataframe as `cars_out`.
## you should see this new 'object' in your environment (usually to the right
## of this script that you're looking at).
cars_out =
  cars %>% 
  mutate(
    speed_squared = speed^2,
    half_distance = dist/2,
    speed_times_distance = speed*dist
    )
## take a look at this new object
glimpse(cars_out)


## take the average of each of the two variables in the original `cars` dataset
## and spit out them as mean_speed and mean_dist.
## the output will appear in the console (also usually to the right).
cars %>% 
  summarise(
    mean_speed = mean(speed),
    mean_dist = mean(dist)
  )

## take the original `cars` dataset, group by each distinct value of speed,
## and then create an average braking distance **specific to that particular value of speed**
## the output will appear in the console (also usually to the right).
cars %>% 
  group_by(speed) %>% 
  summarise(
    mean_dist = mean(dist)
  )

## save three objects (cars_speed_quart_1, and so on) that capture three different values:
## the first, second, and third quartiles of the `speed` variable in the cars `dataset.`
## another way of thinking about quartiles is in terms of percentiles--
## the first quartile is the 25th percentile, the second is the 50th percentile (median),
## and so on.
cars_speed_quart_1 = quantile(cars$speed, 0.25)
cars_speed_quart_2 = quantile(cars$speed, 0.50)
cars_speed_quart_3 = quantile(cars$speed, 0.75)
# cars_speed_99_tile = quantile(cars$speed, 0.99)

### create a new dataframe, `cars_mod`, that:
### (1) aggregates (groups) to the level of the speed quartiles that we just created above.
### (2) spits out an average braking distance for each of the quartiles/groups.
### we'll use this to create some plots below.
cars_mod = cars %>% 
  mutate(
    # note diff between less than sign and the greater than *or equal to* sign
    speed_bucket = case_when(speed < cars_speed_quart_1~1,
                             speed >= cars_speed_quart_1 & speed < cars_speed_quart_2~2,
                             speed >= cars_speed_quart_2 & speed < cars_speed_quart_3~3,
                             speed >= cars_speed_quart_3~4)
    ) %>% 
  group_by(speed_bucket) %>% 
  summarise(
    avg_dist_bucket = mean(dist),
    sd_dist_bucket = sqrt(var(dist))
    )
### spit out the new dataframe we just created
cars_mod


### let's make some plots!

## histogram
cars %>% 
  ## ggplot() is a function/verb that creates plots and figures.
  ## in some examples that you see, you will see stuff populated in the parens;
  ## i tend to leave it blank at this stage and then populate the 'aesthetics'
  ## in later parts of the code.
  ggplot() + ## note plus sign rather than pipe
  ## this gives a histogram over the braking distance variable.
  ## alpha just makes it a little more transparent (try playing with this if you want).
  geom_histogram(aes(x = dist), alpha = 0.25, bins = 10) +
  theme_bw()

## density plot
cars %>% 
  ## ggplot() is a function/verb that creates plots and figures.
  ## in some examples that you see, you will see stuff populated in the parens;
  ## i tend to leave it blank at this stage and then populate the 'aesthetics'
  ## in later parts of the code.
  ggplot() + ## note plus sign rather than pipe
  ## this gives a density plot (like a smooth histogram) over braking distance.
  geom_density(aes(x = dist)) +
  theme_bw()

### this is similar to the dataframe we made above,
### but rather than grouping we're just going to 'mutate' (i.e. create)
### a variable `speed_bucket` that is equal to each observations quartile for speed
### i.e. if it's in the first quartile, `speed_bucket`==1; second quartile --> `speed_bucket`==2,
### and so on.
cars_mod_simple = cars %>% 
  mutate(
    # note diff between less than sign and the greater than *or equal to* sign
    speed_bucket = case_when(speed < cars_speed_quart_1~1,
                             speed >= cars_speed_quart_1 & speed < cars_speed_quart_2~2,
                             speed >= cars_speed_quart_2 & speed < cars_speed_quart_3~3,
                             speed >= cars_speed_quart_3~4)
  )

### plot the simple relationship between car speed and braking distance.
### ask yourself: is this a positive relationship? negative? how strong is it?
cars_mod_simple %>% 
  ggplot() +
  geom_point(aes(x = speed, y = dist)) + 
  theme_bw()

### create that same plot, but color the points according to where they fall in 
### the quartiles for speed (i.e. depending on their value of `speed_bucket`.)
cars_mod_simple %>% 
  ggplot() +
  geom_point(aes(x = speed, y = dist, color = as.factor(speed_bucket))) + 
  theme_bw()


### and that's pretty much it! come to OH / reach out via email if you have questions.
### you'll also learn a lot more in lecture / from Prof Thompson.

