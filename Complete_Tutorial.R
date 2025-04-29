getwd()
print('This is R through and through')
data()
View(iris)
library(tidyverse)
df = iris
starwars
df2 = starwars
View(df2)
df2 %>%
  filter(height>100 & mass < 250) %>% 
  mutate(height_metres = height/100, mass_tonnes = mass/1000) %>% 
  select(height_metres,mass_tonnes) %>% 
  View() 
  #plot()
View(msleep)
glimpse(msleep)
glimpse(df2)
head(df)
head(msleep)
length(df2)
length(msleep$name)
names(msleep)
names(df2)
unique(df2$sex)
missing <- !complete.cases(msleep)
msleep[missing,]
mean(starwars$height)
mean(starwars$height, na.rm=TRUE)
df2 = df2 %>% 
  mutate(tallness = if_else(height < 100,
                            'short',
                            'tall'))
df2$tallness
library(gapminder)
gapminder
View(gapminder)
df3 = gapminder %>% 
  select(country,year,lifeExp)
df3
wide <- df3 %>% 
  pivot_wider(id_cols = country, 
              names_from = year,
              values_from = lifeExp)
View(wide)
long = wide %>% 
  pivot_longer(cols = (2:13),
               names_to = 'Year',
               values_to = 'lifeExp')
View(long)

#Describe
summary(df3$year)
IQR(df3$year)

#Visualize

plot(pressure)
plot(df3[2:3])

#Barplots
ggplot(data = starwars,
       mapping = aes(x=gender))+
  geom_bar()
ggplot(data = df3,
       mapping = aes(x=year))+
  geom_bar()
ggplot(data = df3,
       mapping = aes(x=year,y=lifeExp))+
  geom_col()
starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(height))+
  geom_histogram()

#Hypothesis Testing
gapminder %>% 
  filter(continent %in% c('Africa','Europe')) %>% 
  t.test(lifeExp~continent,data=.)
gapminder %>% 
  aov(lifeExp~continent,data=.) %>% 
  summary()
gapminder %>% 
  filter(year > 2000) %>% 
  aov(lifeExp~continent, data=.) %>% 
  TukeyHSD()
#model
cars %>% 
  lm(dist~speed, data = .) %>% 
  summary()
