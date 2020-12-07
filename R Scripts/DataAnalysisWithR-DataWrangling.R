library(tidyverse)
library(scales)
library(repr)

#################################################################
#################################################################
#################################################################
# R Subsetting columns

# Acquire the necessary data that will be used in the examples
population_data <- read_csv("population_rawdata.csv")


# Subsetting columns:  picking the County, State, and all columns
# that ends with "E"
popdata_estimates <- 
  select(population_data, County, State, B02001_002E, 
         B02001_003E, B02001_004E, B02001_005E, B02001_006E, 
         B02001_007E, B02001_008E)
head(popdata_estimates)


# Doing the same task as above but with the "ends_with()"
# function instead
popdata_estimates <- select(population_data, County, State, ends_with("E"))
head(popdata_estimates)


# Renaming certain columns in the data frame. Note the rename() verb 
# keeps columns that were not renamed. If you use the select() verb
# then only the columsn specified are kept.
popdata_rename <-
  rename(
    popdata_estimates, 
    `White` = B02001_002E,
    `Black` = B02001_003E,
    `American Indian` = B02001_004E,
    `Asian` = B02001_005E,
    `Pacific Islander` = B02001_006E,
    `Other Race` = B02001_007E,
    `Two or More Races` = B02001_008E
  )
head(popdata_rename)

#################################################################
#################################################################
#################################################################
# Adding calculated column
popdata_rowtotals <-
  mutate(popdata_rename, 
         All = White + Black + `American Indian` + Asian + 
           `Pacific Islander` + `Other Race` + `Two or More Races`
  )
head(popdata_rowtotals)


# Same as above with less code
popdata_rowtotals <-
  mutate(popdata_rename, All = rowSums(popdata_rename[3:9]))
head(popdata_rowtotals)


# Calculating percent of total - Long way
popdata_percents <-
  mutate(popdata_rename, 
         `Percent White` = 
           White / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`),
         `Percent Black` = 
           Black / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`),
         `Percent American Indian` = 
           `American Indian` / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`),
         `Percent Asian` = 
           Asian / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`),
         `Percent Pacific Islander` = 
           `Pacific Islander` / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`),
         `Percent Other Race` = 
           `Other Race` / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`),
         `Percent Two or More Races` = 
           `Two or More Races` / 
           (White + Black + `American Indian` + Asian + 
              `Pacific Islander` + `Other Race` + `Two or More Races`)
  )
head(popdata_percents)


# Calculating percent of total - Short way
popdata_percents <-
  mutate(popdata_rename, 
         `Percent White` = 
           White / 
           rowSums(popdata_rename[3:9]),
         `Percent Black` = 
           Black / 
           rowSums(popdata_rename[3:9]),
         `Percent American Indian` = 
           `American Indian` / 
           rowSums(popdata_rename[3:9]),
         `Percent Asian` = 
           Asian / 
           rowSums(popdata_rename[3:9]),
         `Percent Pacific Islander` = 
           `Pacific Islander` / 
           rowSums(popdata_rename[3:9]),
         `Percent Other Race` = 
           `Other Race` / 
           rowSums(popdata_rename[3:9]),
         `Percent Two or More Races` = 
           `Two or More Races` / rowSums(popdata_rename[3:9])
  )
popdata_percents


# Formatting percent calculations and adding calcuated columns with 
# the transmute() verb
popdata_percents_transumte <-
  transmute(popdata_rename,
            County,
            State,
            `Percent White` = 
              percent(White / rowSums(popdata_rename[3:9])),
            `Percent Black` = 
              percent(Black / rowSums(popdata_rename[3:9])),
            `Percent American Indian` = 
              percent(`American Indian` / rowSums(popdata_rename[3:9])),
            `Percent Asian` = 
              percent(Asian / rowSums(popdata_rename[3:9])),
            `Percent Pacific Islander` = 
              percent(`Pacific Islander` / rowSums(popdata_rename[3:9])),
            `Percent Other Race` = 
              percent(`Other Race` / rowSums(popdata_rename[3:9])),
            `Percent Two or More Races` = 
              percent(`Two or More Races` / rowSums(popdata_rename[3:9]))
  )
popdata_percents_transumte


# Picking two columns based on position using rowSums()
popdata_bw <-
  transmute(popdata_rename, 
            County,
            State,
            White,
            Black,
            `Black and White` = rowSums(popdata_rename[,c(3,4)])
  )
head(popdata_bw,10)


# Picking two columns based on name using rowSums()
popdata_bw <-
  transmute(popdata_rename, 
            County,
            State,
            White,
            Black,
            `Black and White` = rowSums(popdata_rename[,c("White","Black")])
  )
head(popdata_bw,10)


#################################################################
#################################################################
#################################################################
# Aggregating and pivoting data

# grouping data using group_by() and summarize(). Note that you 
# need to use the na.rm parameter to TRUE to prevent NAs from
# effecting your calculations.
popdata_group <-
  popdata_rename %>%
  group_by(State) %>%
  summarize(
    White = sum(White, na.rm=TRUE), 
    Black = sum(Black, na.rm=TRUE), 
    `American Indian`= sum(`American Indian`, na.rm=TRUE), 
    Asian = sum(Asian, na.rm=TRUE), 
    `Pacific Islander`= sum(`Pacific Islander`, na.rm=TRUE), 
    `Other Race` = sum(`Other Race`, na.rm=TRUE), 
    `Two or More Races` = sum(`Two or More Races`, na.rm=TRUE),
    `Number of Counties`=n(),
    .groups = 'drop'
  )
head(popdata_group,10)


# Same as above but with less code via the summarize_if() verb
popdata_group <-
  popdata_rename %>%
  group_by(State) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)
head(popdata_group,10)


# Performing data wrangling and data visualization in the same
# workflow
options(repr.plot.width=15, repr.plot.height=8)
plot <-
  popdata_unpivot %>%
  filter(State == "Texas") %>%
  transmute(Race=fct_reorder(Race, Population), Population) %>%
  ggplot(aes(x=Race, y=Population, label=comma(Population))) +
  geom_bar(stat="Identity", fill="red") + 
  geom_label(size=6) +
  coord_flip() +
  scale_y_continuous(labels=NULL) +
  theme_minimal() +
  labs(
    title = "Main Title"
    ,subtitle = "Sub Title"
    ,caption = "My Caption"
  ) + 
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15),
        plot.title = element_text(size=25),
        plot.subtitle = element_text(size=20),
        plot.caption = element_text(size=8))

plot
