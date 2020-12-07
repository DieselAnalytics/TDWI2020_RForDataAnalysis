# Required packages needed for the script
library("tidyverse")
library("lubridate")
library("scales")
library("ggthemes")

###############################################################
###############################################################
###############################################################
# Scatter plots in R with ggplot2

# Create data set needed for example visual
data("diamonds")
plot.data <-
  diamonds %>%
  filter(color == "D") %>%
  sample_n(200)
head(plot.data)

# Initiate ggplot() function
ggplot(data = plot.data, mapping = aes(x = carat, y = price))


# The code below create a scatter plot using the geom_point() geom. 
# Geoms are the geometric object you are plotting. Examples of common 
# geoms are geom_bar() for bar charts and geom_line() for line charts.
ggplot(data = plot.data, mapping = aes(x = carat, y = price)) +
  geom_point()

# Execute the code below to get the shapes you can use with the geom_point()
# geom
shapes <- data.frame(
  shape = c(0:24),
  x = 0:24 %/% 5,
  y = -(0:24 %% 5)
)
ggplot(shapes, aes(x, y)) + 
  geom_point(aes(shape = shape), size = 5, fill = "red") +
  geom_text(aes(label = shape), hjust = 0, nudge_x = 0.15) +
  scale_shape_identity() +
  expand_limits(x = 4.1) +
  theme_void()

# Using a diamond shape instead of solid cirles with geom_point()
ggplot(data = plot.data, mapping = aes(x = carat, y = price)) +
  geom_point(shape=18, size = 5)


# Add titles to the chart
ggplot(data = plot.data, mapping = aes(x = carat, y = price)) +
  geom_point(shape=18, size = 5) +
  labs(
    title = "Chart Title",
    subtitle = "Chart Subtitle",
    caption = "Chart Caption"
  )


# Add themes
ggplot(data = plot.data, mapping = aes(x = carat, y = price)) +
  geom_point(shape=18, size = 5) +
  labs(
    title = "Chart Title",
    subtitle = "Chart Subtitle",
    caption = "Chart Caption"
  ) +
  theme_minimal()


# List of available **geoms** and **themes**
#
# Link to available geoms:  https://ggplot2.tidyverse.org/reference/  
# List of available themes:  https://ggplot2.tidyverse.org/reference/ggtheme.html  
# List of themes in "ggthemes":  https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/ 

###############################################################
###############################################################
###############################################################
# Creating line plots in R with ggplot2

# Get data needed for the chart. The "lakers" data set comes from the
# lubridate package
players <- c("Kobe Bryant", "Pau Gasol", "Lamar Odom")
plot.data <- lakers %>%
  filter(result == "made" & team == "LAL" & player %in% players) %>%
  mutate(date = ymd(date)) %>%
  group_by(player, date) %>%
  summarize(points = sum(points))
head(plot.data,10)
  
# Create line chart that compares total points scored by kobe Byrant, 
# Lamar Odom, and Pau Gasol during the 2008 season using geom_line():
ggplot(data=plot.data, mapping=aes(x=date,y=points,color=player)) +
  geom_line() +
  labs(title = "geom_line") +
  theme_minimal()


# What happens when you don't specify a color argument for the players 
# in the geom_line()
ggplot(data=plot.data, mapping=aes(x=date,y=points)) +
  geom_line() +
  labs(title = "geom_line") +
  theme_minimal()


# The code below splits the chart by player using the facet_wrap() 
# function. The guides() and theme() function were also used to change
# some of the non-data portions of the chart.
ggplot(data=plot.data, mapping=aes(x=date,y=points,color=player)) +
  geom_line() +
  labs(title = "geom_line") +
  theme_minimal() +
  facet_wrap(~player) +
  guides(color="none") +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15),
        strip.text = element_text(size=20))


###############################################################
###############################################################
###############################################################
# Creating bar plots in R with ggplot2


# Getting the data that will be used in the visualization
data("Titanic")
plot.data <-
  data.frame(Titanic) %>%
  group_by(Class) %>%
  summarize(`Total Freq` = sum(Freq))
plot.data


# Create a vertical column chart to show the fatalities experienced 
# on the Titantic
ggplot(data = plot.data, mapping = aes(x = Class, y = `Total Freq`)) +
  geom_col() +
  labs(title = "geom_bar") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))


# Convert the above vertical bar chart to horizontal bar chart
ggplot(plot.data, aes(x = Class, y = `Total Freq`, fill = Class)) +
  geom_col() +
  coord_flip() +
  labs(title = "geom_bar") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))


# Order the bars in the above chart in descending order to show rank and
# color the bars blue
ggplot(plot.data, aes(x = fct_reorder(Class,`Total Freq`) , y = `Total Freq`)) +
  geom_col(fill = "blue") +
  xlab(label = "Class") +
  coord_flip() +
  labs(title = "geom_bar") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))


###############################################################
###############################################################
###############################################################
# Creating histogram plots in R with ggplot2


# Acquire the data needed for the visual
plot.data <- as_tibble(airquality)
head(plot.data,10)


# Create a histogram of the temparatures using a bin width of 10
ggplot(plot.data, aes(Temp)) +
  geom_histogram(binwidth = 10) +
  labs(title = "geom_histogram") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))


# Change the bin width from 10 to 5
ggplot(plot.data, aes(Temp)) +
  geom_histogram(binwidth = 5) +
  labs(title = "geom_histogram") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))


# Add borders around the bins to show separation
ggplot(plot.data, aes(Temp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(title = "geom_histogram") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))


# Change colors of the bins to pink
ggplot(plot.data, aes(Temp)) +
  geom_histogram(binwidth = 5, color = "white", fill = "pink") +
  labs(title = "geom_histogram") +
  theme_minimal() +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=15))



