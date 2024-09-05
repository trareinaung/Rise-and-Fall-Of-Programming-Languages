# Rise and Fall of Programming Languages

This project explores the rise and fall of programming languages using R and includes trend analysis, peak popularity, and more insights.

## View Full HTML Report
You can view the full HTML report [here](path/to/your/R_Project.html).

```{r}
library(readr)
library(dplyr)
library(ggplot2)
```

## Load and Inspecting the data

```{r}
programming_data <- read.csv("programming_data.csv")
head(programming_data)
```
| year | tag           | number | year_total |
|------|---------------|--------|------------|
| 2008 | .htaccess     | 54     | 58390      |
| 2008 | .net          | 5910   | 58390      |
| 2008 | .net-2.0      | 289    | 58390      |
| 2008 | .net-3.5      | 319    | 58390      |
| 2008 | .net-4.0      | 6      | 58390      |
| 2008 | .net-assembly | 3      | 58390      |


## 1.Trend Analysis: Percentage share of programming languages over the years

```{r}
all_programming_data <- programming_data %>%
  mutate(percentage = round((number / year_total) * 100, 2))

# Visualizing trends for popular programming languages
popular_languages <- c("c#", "java", "javascript", "python", "r")
selected_data <- all_programming_data %>%
  filter(tag %in% popular_languages)

ggplot(selected_data, aes(x = year, y = percentage, color = tag)) +
  geom_line() +
  labs(title = "Rise and Fall of Programming Languages",
       x = "Year",
       y = "Percentage of Total",
       color = "Programming Languages") +
  theme_minimal()
```
![](https://github.com/trareinaung/Rise-and-Fall-Of-Programming-Languages/blob/main/Popular%20Programming%20Languages.png?raw=true)<!-- -->
## 2. Most Popular Programming Languages Each Year

```{r}
most_popular <- all_programming_data %>%
  group_by(year) %>%
  top_n(1, number)

cat("Most Popular Language Each Year:\n")
print(most_popular)
```
| year | tag        | number | year_total | percentage |
|------|------------|--------|------------|------------|
| 2008 | c#         | 7473   | 58390      | 12.8       |
| 2009 | c#         | 46044  | 343868     | 13.4       |
| 2010 | c#         | 75501  | 694391     | 10.9       |
| 2011 | c#         | 113408 | 1200551    | 9.45       |
| 2012 | java       | 145640 | 1645404    | 8.85       |
| 2013 | javascript | 197101 | 2060473    | 9.57       |
| 2014 | javascript | 237415 | 2164701    | 11.0       |
| 2015 | javascript | 257006 | 2219527    | 11.6       |
| 2016 | javascript | 265896 | 2226072    | 11.9       |
| 2017 | javascript | 266762 | 2305207    | 11.6       |
| 2018 | javascript | 115726 | 1085170    | 10.7       |

## 3. Has R been growing or shrinking over time?

```{r}
r_over_time <- all_programming_data %>%
  filter(tag == "r")

ggplot(r_over_time, aes(x = year, y = percentage)) +
  geom_line(color = "red") +
  labs(title = "Growth of R Over Time",
       x = "Year",
       y = "Percentage of Total")

```
![](https://github.com/trareinaung/Rise-and-Fall-Of-Programming-Languages/blob/main/R%20Trends.png?raw=true)<!-- -->
## 4. Emerging and Declining Languages

```{r}
# Calculate the percentage change in tag occurrences year-over-year
emerging_declining_languages <- all_programming_data %>%
  group_by(tag) %>%
  arrange(year) %>%
  mutate(perc_change = round((number - lag(number)) / lag(number) * 100, 2))

# Filter languages with significant positive or negative changes
emerging_languages <- emerging_declining_languages %>%
  filter(perc_change > 50)

declining_languages <- emerging_declining_languages %>%
  filter(perc_change < -50)

cat("Emerging Languages (More than 50% growth in any year):\n")
print(emerging_languages)
```
| year | tag            | number | year_total | percentage | perc_change |
|------|----------------|--------|------------|------------|-------------|
| 2009 | .htaccess      | 828    | 343868     | 0.24       | 1433        |
| 2009 | .net           | 23076  | 343868     | 6.71       | 290         |
| 2009 | .net-2.0       | 593    | 343868     | 0.17       | 105         |
| 2009 | .net-3.5       | 1087   | 343868     | 0.32       | 241         |
| 2009 | .net-4.0       | 129    | 343868     | 0.04       | 2050        |
| 2009 | .net-assembly  | 13     | 343868     | 0.00       | 333         |
| 2009 | 2d             | 143    | 343868     | 0.04       | 240         |
| 2009 | 32-bit         | 99     | 343868     | 0.03       | 421         |
| 2009 | 32bit-64bit    | 63     | 343868     | 0.02       | 1475        |
| 2009 | 3d             | 414    | 343868     | 0.12       | 467         |
```{r}
cat("Declining Languages (More than 50% decline in any year):\n")
print(declining_languages)
```

| year | tag                       | number | year_total | percentage | perc_change |
|------|---------------------------|--------|------------|------------|-------------|
| 2009 | template-meta-programming  | 1      | 343868     | 0          | -75         |
| 2010 | asp.net-web-api            | 3      | 694391     | 0          | -57.1       |
| 2010 | child-process              | 2      | 694391     | 0          | -75         |
| 2010 | ssrs-2008-r2               | 1      | 694391     | 0          | -80         |
| 2010 | subsonic                   | 304    | 694391     | 0.04       | -55.8       |
| 2010 | windows-vista              | 312    | 694391     | 0.04       | -52.9       |
| 2011 | agile                      | 92     | 1200551    | 0.01       | -51.6       |
| 2011 | google-sheets-api          | 1      | 1200551    | 0          | -66.7       |
| 2011 | latex                      | 405    | 1200551    | 0.03       | -61.0       |
| 2011 | maven-2                    | 954    | 1200551    | 0.08       | -50.6       |


## 5. Peak Popularity: Finding the year when each language was most popular

```{r}
peak_popularity <- all_programming_data %>%
  group_by(tag) %>%
  filter(number == max(number))

cat("Peak Popularity of Each Language:\n")
print(peak_popularity)

```
| year | tag                | number | year_total | percentage |
|------|--------------------|--------|------------|------------|
| 2009 | .net-2.0           | 593    | 343868     | 0.17       |
| 2009 | agile              | 202    | 343868     | 0.06       |
| 2009 | build-process      | 469    | 343868     | 0.14       |
| 2009 | compact-framework  | 657    | 343868     | 0.19       |
| 2009 | nant               | 220    | 343868     | 0.06       |
| 2009 | project-management | 401    | 343868     | 0.12       |
| 2009 | remoting           | 220    | 343868     | 0.06       |
| 2009 | subsonic           | 687    | 343868     | 0.20       |
| 2009 | visual-studio-2005 | 801    | 343868     | 0.23       |
| 2009 | windows-mobile     | 805    | 343868     | 0.23       |

## Author

[Thu Rein Aung](https://github.com/trareinaung)
