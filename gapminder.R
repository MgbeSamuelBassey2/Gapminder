# Hi everyone
# today we are going to explore the gapminder dataset exploring the life expectancy, Population
# and GDP per capita of countries and continents from 1950 - 2007

# First lets load the dats

install.packages('gapminder')
library(gapminder)
View(gapminder)
library(tidyverse)
# lets explore the data in gapminder
gapminder %>%
  aggregate(lifeExp ~ year, mean) %>%
library(tidyverse)
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  labs(
    title = 'Average life expectancy (1950 - 2000)'
  )
# we can see that life expectancy shot up from 50years to 65years  

# lets take a look at the life expectancy for Nigeria over the years

gapminder %>%
  filter(country == 'Nigeria') %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  labs(
    title = 'Nigerias life expectancy from 1950 to 2000',
    x = 'Year',
    y = 'Life Expectancy'
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold'),
  axis.title.x = element_text(face = "bold"),
  axis.title.y = element_text(face = "bold"))

# we can see that that Nigeria's life expectancy increased a lot from 1950.
# lets see which county has the highest life expectancy

gapminder %>%
  aggregate(lifeExp ~ continent, mean) %>%
  arrange(desc(lifeExp)) %>%
  ggplot(aes(continent, lifeExp)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(
    title = 'Life expectancy of Continents',
    y = 'Life Expectancy'
  )
# Oceania has the highest life expectancy
# Next lets explore Population

gapminder %>%
  aggregate(pop ~ year, sum) %>%
  ggplot(aes(year, pop)) +
  geom_line() +
  labs(
    title = 'Population increase from 1950 - 2000',
    y = 'Population'
  ) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma)
# Population increased from under twenty 3billion to over 6 billion people in 2000

# lets take a look at countries with the highest popultion at 2007
gapminder %>%
  filter(year == 2007) %>%
  aggregate(pop ~ country, sum) %>%
  arrange(desc(pop)) %>%
  head(5) %>%
  ggplot(aes(country, pop)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = 'Top five most populated countries in 2007',
    y = 'population'
  )

# china and india are the pass all other countries by a wide margin
# lets take a look at chinas GDPpercapita

gapminder %>%
  filter(country == 'China', year ==  2007)

# but what is the correlation between GDP percapita and population
# lets plot a scatter plot to find out

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(pop, gdpPercap)) +
  geom_point() +
  labs(
    title = 'Corelation between Population and GDP per Capita',
    x = 'Population',
    y = 'GDP per capita'
  ) +
  scale_x_continuous(labels = scales::comma)

# countries with  higher population seem to hve a lower GDP per capita while countries with lower population seem to have a higher GDP per capita

# lets look at the correlation between GDP per capita and life expectancy
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(lifeExp, gdpPercap)) +
  geom_smooth(method = 'lm') +
  geom_point() +
  labs(
    title = 'Corelation between GDP per capita and Life Expectancy',
    x = 'Life Expctancy',
    y = 'GDP per capita'
  )

# Countries with a higher GDP per capita have a higher life expectancy
# lets look at the correlation between population and life expetancy

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(lifeExp, pop)) +
  geom_point() +
  labs(
    title = 'Correlation between Population and Life Expectancy',
    x = 'Life Expctancy',
    y = 'Population'
  ) +
  scale_y_continuous(labels = scales::comma)

# lets take a look at the countries with the highest GDP per Capita

gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap)) %>%
  head() %>%
  ggplot(aes(country, gdpPercap)) +
  geom_bar(stat = 'identity')

gapminder %>%
  group_by(country) %>%
  arrange(year)


#let us get a summary for each continent which includes average life expectancy, Average population
# and average gdp_per_capita
summary_data <- gapminder %>%
  group_by(continent) %>%
  summarize(
    Average_GDPpercapita = mean(gdpPercap, na.rm = TRUE),
    avg_life_expectancy = mean(lifeExp, na.rm = TRUE),
    Average_population = mean(pop, na.rm = TRUE)
  )
summary_data



# Next lets get the summary for each country
country_summary <- gapminder %>%
  group_by(country) %>%
  summarize(
    Average_GDPpercapita = mean(gdpPercap, na.rm = TRUE),
    avg_life_expectancy = mean(lifeExp, na.rm = TRUE),
    Average_population = mean(pop, na.rm = TRUE)
  )
View(country_summary)


# Now lets summarize by year
year_summary <- gapminder %>%
  group_by(year) %>%
  summarize(
    Average_GDPpercapita = mean(gdpPercap, na.rm = TRUE),
    avg_life_expectancy = mean(lifeExp, na.rm = TRUE),
    Average_population = mean(pop, na.rm = TRUE)
  )
year_summary


# now lets summarize the total population for each continent each year
total_population <- gapminder %>%
  group_by(continent) %>%
  group_by(year) %>%
  summarize(
    total_population = sum(pop, na.rm = TRUE)
  )
total_population

#lets look at the country with the highest GDP per capita as at 2007
gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap)) %>%
  head(5)

gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap)) %>%
  head(5) %>%
  ggplot(aes(country, gdpPercap)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(title = 'Top 5 Countries with the highest life expectancy as at 2007') +
  theme_minimal()
  
#Countries with the highest Gdp per Capita seem to come from europe

# Now lets look at the countries with the lowest GDP per capita
gapminder %>%
  filter(year == 2007) %>%
  arrange(gdpPercap) %>%
  head(5)

 
gapminder %>%
  filter(year == 2007) %>%
  arrange(gdpPercap) %>%
  head(5) %>%
  ggplot(aes(country, gdpPercap)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(title = 'Countries with the lowst Gdp per capita') +
  theme_minimal()

#Countries with the Lowest Gdp per Capita seem to come from Africa
# lets look at the countries with the lowest  life expectancy
gapminder %>%
  filter(year == 2007) %>%
  arrange(lifeExp) %>%
  head(5)
# Unfortunately, countries with the lowest life expectancy also come from Africa

gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(lifeExp)) %>%
  head(5)
# Country with the highest life expectancy come mostly from Europe and Asia

'''
Conclusion:
The analysis of the Gapminder dataset underscores the remarkable global progress in key development indicators such as life expectancy and income levels over the past several decades.
While significant advancements have been made, particularly in Asia and Latin America, substantial disparities remain between regions, especially in Sub-Saharan Africa and parts of South Asia.
The strong correlation between income and life expectancy highlights the ongoing challenge of addressing inequality and ensuring that improvements in health and economic well-being reach all populations.
Moving forward, it is crucial for policymakers and global organizations to focus on strategies that promote sustainable development, improve healthcare, and reduce inequality, while also considering the impact of population growth on resources and infrastructure.
The dataset serves as a valuable tool for understanding the dynamics of global development and guiding future efforts to improve quality of life worldwide.
'''


