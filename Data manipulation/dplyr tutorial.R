library(dplyr)

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)



# column name	Description
# name	common name
# genus	taxonomic rank
# vore	carnivore, omnivore or herbivore?
#   order	taxonomic rank
# conservation	the conservation status of the mammal
# sleep_total	total amount of sleep, in hours
# sleep_rem	rem sleep, in hours
# sleep_cycle	length of sleep cycle, in hours
# awake	amount of time spent awake, in hours
# brainwt	brain weight in kilograms
# bodywt	body weight in kilograms

################################################
#select columns
sleepData <- select(msleep, name, sleep_total)
head(sleepData)

#pipe operatore to combine many functions
msleep %>% 
  select(name, sleep_total) %>% 
  head

#To select all the columns except a specific column
head(select(msleep, -name))

#select a range of columns
head(select(msleep, name:order))

#select all columns that start with a character string 
head(select(msleep, starts_with("sl")))

# Some additional options to select columns based on a specific criteria include
# ends_with() = Select columns that end with a character string
# contains() = Select columns that contain a character string
# matches() = Select columns that match a regular expression
# one_of() = Select columns names that are from a group of names

####################################################
#filter rows
#boolean operators (e.g. >, <, >=, <=, !=, %in%) to create the logical tests.

#one filter
filter(msleep, sleep_total >= 16)

#many numeric filters
filter(msleep, sleep_total >= 16, bodywt >= 1)

#categorical filters
filter(msleep, order %in% c("Perissodactyla", "Primates"))

####################################################
#arrange (re-order) rows by a column
msleep %>% arrange(order) %>% head

#arrange rows by a column and then by another one
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head

#use desc(column_name) for descending order
msleep %>% 
  arrange(order, desc(sleep_total))%>% 
  head

####################################################
#sample
#select random rows from a table
msleep %>% 
sample_n(size = 10)

msleep %>% 
  sample_frac(size = 0.10)




####################################################
#mutate
#add calculated column to the dataframe 
msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

#add many new calculated columns
msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, 
         bodywt_grams = bodywt * 1000) %>%
  head

###################################################
#summarise will create summary statistics for a given column

msleep %>% 
  summarise(avg_sleep = mean(sleep_total, na.rm=T))

#na.rm = TRUE will remove all NA values
msleep %>% 
  group_by(vore)%>% 
  summarise(mean(sleep_total),sd(sleep_total))


#There are many other summary statistics you could consider 
#such sd(), min(), max(), median(), sum(), n() (returns the length of vector), 
#first() (returns first value in vector), last() (returns last value in vector) 
#n_distinct() (number of distinct values in vector).

#count gives total rows by levels of one or many columns

msleep %>% 
  count(vore)
msleep %>% 
count(vore,conservation)


###################################################
#group by
#it's related to concept of "split-apply-combine". 
#1.split the data frame by some variable (e.g. a column)
#apply a function to the individual data frames
#combine the output with original dataframe.

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())


