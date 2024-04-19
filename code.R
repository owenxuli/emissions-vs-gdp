## ============
## Packages
## ============

library(tidyverse)
library(dplyr)
library(countrycode)
library(gridExtra)
library(ggfortify)
library(pubtheme)

## ============
## Data
## ============

emissions <- read.csv('data/emissions.csv')

gdp <- read.csv('data/gdp.csv')

# temp data frames to merge

x <- gdp %>% 
  rename(country_name = Country.Name, country_code = Country.Code)

y <- emissions

# to merge the data frames by country code, name, and year:
complete <- merge(x, y, by = intersect(names(x), names(y))) %>%
  mutate(status = if_else(GDP_per_capita_USD > 15000, 'Developed', 'Developing'),
         lgemissions = log(value),
         lggdp = log(GDP_USD)) %>%
  filter(!is.na(GDP_per_capita_USD),
         !is.na(GDP_USD),
         !is.na(value),
         is.finite(lgemissions),
         is.finite(lggdp)) %>%
  select(country_code,
         country_name,
         year,
         lggdp,
         GDP_per_capita_USD,
         lgemissions,
         status)

# to add the region to which each country belongs to:
complete <- complete %>%
  mutate(wregion = countrycode(country_name, 
                               origin = 'country.name', 
                               destination = 'region')) %>%
  filter(!is.na(wregion)) %>%
  select(country_name,
         country_code,
         wregion,
         year,
         lggdp,
         GDP_per_capita_USD,
         lgemissions,
         status)

complete <- complete %>% mutate(bins = cut(lggdp, breaks = 10))

# data frame with the variance per subset of GDP

variance <- complete %>%
  group_by(bins) %>%
  summarize(variance = var(lgemissions),
            mean = mean(lgemissions))

## ============
## Plots
## ============

# Scatterplot

completegg <- ggplot(complete, aes(x = lggdp, y = lgemissions)) +
  geom_point(alpha = 0.3, color = pubblue) +
  geom_smooth(method = 'lm', se = FALSE, color = pubred) +
  labs(x = 'Log GDP in US Dollars',
       y = 'Log Emissions in Kilotons',
       title = 'Histogram of GDP in US Dollars',
       col = NULL)

completegg %>% pub(type = "scatter")

# Residuals

lm <- lm(lgemissions ~ lggdp, data = complete)
resid <- data.frame(x = residuals(lm))
residgg <- ggplot(resid, aes(x = x)) +
  geom_histogram(binwidth = 0.2, fill = pubblue, color = "black") +
  labs(x = 'Residuals',
       y = 'Frequency',
       title = 'Histogram of Residuals',
       col = NULL)

residgg %>% pub(type = "hist")

# Autoplot

autoplot(lm, col = pubdarkgray, alpha = 0.3) + theme_pub()

# Scatterplot, faceted by development status, and colored by world region

complete %>%
  ggplot(aes(
    x = lggdp,
    y = lgemissions,
    col = wregion
  )) +
  geom_point(alpha = 0.3) +
  facet_wrap(facets = vars(status)) +
  labs(x = 'Log GDP (USD)',
       y = 'Log CO2 emissions (kt)',
       title = 'Historical GDP against CO2 Emissions',
       col = 'World Region') +
  theme_pub() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = c(.95, .05),
        legend.justification = c('right', 'bottom'),
        legend.margin = margin(0,0,0,0))

# Boxplot

complete %>% 
  ggplot(aes(
    x = wregion,
    y = lgemissions,
    col = status
  )) +
  geom_boxplot(outlier.size = -1) +
  labs(
    x = NULL,
    y = 'Log CO2 Emissions (kt)',
    title = 'Boxplot of CO2 Emissions per Region in the World, 1960-2019',
    col = NULL) +
  theme_pub() +
  coord_flip()

# Emissions, faceted by development status

complete %>%
  ggplot(
    aes(x = lggdp, 
        y = lgemissions,
        col = status)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(
    x = 'Log GDP in US Dollars',
    y = 'Log CO2 Emissions in Kilotons',
    title = 'GDP versus CO2 Emissions per Country',
    col = NULL
  ) +
  facet_wrap(facets = vars(status)) +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) +
  theme_pub()

## ============
## Linear Regression Models
## ============

# Model using logGDP, status, and region

modeld <- lm(lgemissions ~ lggdp + wregion + status, data = complete)

summary(modeld)

# Model using logGDP and region

modele <- lm(lgemissions ~ lggdp + wregion, data = complete)

summary(modele)

# Model using logGDP and status

modelf <- lm(lgemissions ~ lggdp + status, data = complete)

summary(modelf)

# Model using logGDP

modelg <- lm(lgemissions ~ lggdp, data = complete)

summary(modelg)

# Model using status and world region

modelh <- lm(lgemissions ~ status + wregion, data = complete)

summary(modelh)

## ============
## Hypothesis test
## ============

developed <- complete %>% filter(status == 'Developed')
developing <- complete %>% filter(status == 'Developing')

t.test(developed$lgemissions, developing$lgemissions, var.equal = T)

