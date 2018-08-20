# This code comes from this pages:
# https://www.sharpsightlabs.com/blog/data-analysis-example-r-supercars-part1/
# https://www.sharpsightlabs.com/blog/data-analysis-example-r-supercars-part2/

library(dplyr)
library(ggplot2)

# This does not work on windows. Why?
#df.car_torque <- read.csv("./data/auto-snout_torque.txt")
#df.car_0_60_times  <- read.csv("./data/auto-snout_0-60-times.txt")
#df.car_engine_size <- read.csv("./data/auto-snout_engine-size.txt")
#df.car_horsepower  <- read.csv("./data/auto-snout_horsepower.txt")
#df.car_top_speed   <- read.csv("./data/auto-snout_top-speed.txt")
#df.car_power_to_weight <- read.csv("./data/auto-snout_power-to-weight.txt")

df.car_torque <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"))
df.car_0_60_times  <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
df.car_engine_size <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"))
df.car_horsepower  <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"))
df.car_top_speed   <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"))
df.car_power_to_weight <- read.csv(url("https://vrzkj25a871bpq7t1ugcgmn9-wpengine.netdna-ssl.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"))

  
# finding duplicates:
df.car_torque %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_horsepower %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_top_speed %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)

#deduping (we found duplicates):
df.car_torque <- distinct(df.car_torque, car_full_nm, .keep_all = TRUE)
df.car_0_60_times <- distinct(df.car_0_60_times, car_full_nm, .keep_all = TRUE)
df.car_engine_size <- distinct(df.car_engine_size, car_full_nm, .keep_all = TRUE)
df.car_horsepower <- distinct(df.car_horsepower, car_full_nm, .keep_all = TRUE)
df.car_top_speed <- distinct(df.car_top_speed, car_full_nm, .keep_all = TRUE)
df.car_power_to_weight <- distinct(df.car_power_to_weight, car_full_nm, .keep_all = TRUE)

# 'left join':
df.car_spec_data = left_join(df.car_horsepower, df.car_torque, by="car_full_nm") %>%
                  left_join(df.car_0_60_times, by="car_full_nm") %>%
                  left_join(df.car_engine_size, by="car_full_nm") %>%
                  left_join(df.car_top_speed, by="car_full_nm") %>%
                  left_join(df.car_power_to_weight, by="car_full_nm")

# testing everything's OK (no duplicate entries):
df.car_spec_data %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)

head(df.car_spec_data)

# new variables:
df.car_spec_data <- mutate(df.car_spec_data, year=sub(".*\\[([0-9]{4})\\]","\\1",car_full_nm))

df.car_spec_data <- mutate(df.car_spec_data, 
                           decade = as.factor(
                            ifelse(substring(df.car_spec_data$year,1,3)=='193','1930s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='194','1940s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='195','1950s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='196','1960s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='197','1970s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='198','1980s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='199','1990s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='200','2000s',
                            ifelse(substring(df.car_spec_data$year,1,3)=='201','2010s',"ERROR")))))))))))

df.car_spec_data <- mutate(df.car_spec_data, make_nm = gsub(" .*$","", df.car_spec_data$car_full_nm))

df.car_spec_data <- mutate(df.car_spec_data, car_weight_tons = horsepower_bhp / horsepower_per_ton_bhp)

df.car_spec_data <- mutate(df.car_spec_data, torque_per_ton = torque_lb_ft / car_weight_tons)

head(df.car_spec_data)

# Checking the decade column:
df.car_spec_data %>%  
  group_by(decade) %>%
  summarize(count=n())

# Checking the make_nm column:
df.car_spec_data %>%
  group_by(make_nm) %>%
  summarise(make_count = length(make_nm)) %>%
  arrange(desc(make_count))

# Themes for the charts:
theme.car_chart <-
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 26, family = "Trebuchet MS", face = "bold", hjust = 0, color = "#666666")) +
  theme(axis.title = element_text(size = 26, family = "Trebuchet MS", face = "bold", color = "#666666")) +
  theme(axis.title.y = element_text(angle = 0))

# Scatterplot theme
theme.car_chart_SCATTER <- theme.car_chart +
  theme(axis.title.x = element_text(hjust = 0, vjust = -.5))

# Histogram theme
theme.car_chart_HIST <- theme.car_chart +
  theme(axis.title.x = element_text(hjust = 0, vjust = -.5))

# Small multiples theme
theme.car_chart_SMALLM <- theme.car_chart +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size = 16, family = "Trebuchet MS", face = "bold", hjust = 0, color = "#666666"))

##############################
# First graph: HP vs top speed
ggplot(data = df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.4, size=4, color="#880011")+
  ggtitle("Horsepower vs. Top Speed") +
  labs(x="Horsepower", y="Top Speed,\n mph") +
  theme.car_chart_SCATTER

#######################
#Histogram of top speed
ggplot(data = df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill="#880011") +
  ggtitle("Histogram of top speed") +
  labs(x="Top Speed, mph", y="Count\nof Records") +
  theme.car_chart_HIST

# Now we know from the previous plots that there's a group of cars that
# have a limit to their max speed; let's see where is that limit:
df.car_spec_data %>%
  filter(top_speed_mph > 149 & top_speed_mph < 159) %>%
  ggplot(aes(x=as.factor(top_speed_mph))) +
  geom_bar(fill="#880011") +
  labs(x="Top speed, mph") +
  theme.car_chart

#it's at 155 mph then :)

# Histogram of top speed by decade:
ggplot(data = df.car_spec_data, aes(x=top_speed_mph)) +
  geom_histogram(fill = "#880011") +
  ggtitle("Histogram of Top Speed\nby decade") +
  labs(x="Top speed, mph", y="Count\nof records") +
  facet_wrap(~decade) +
  theme.car_chart_SMALLM

df.car_spec_data$year <- as.character(df.car_spec_data$year)

# Manufacturesrs at max speed = 155 mph
df.car_spec_data %>%
  filter(top_speed_mph == 155 & year >= 1990) %>%
  group_by(make_nm) %>% summarize(count_speed_controlled=n()) %>%
  arrange(desc(count_speed_controlled))

# Horsepower vs speed
ggplot(data = df.car_spec_data, aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=0.6, color = "#880011") +
  facet_wrap(~decade) +
  ggtitle("Horsepower vs Top Speed\nby decade") +
  labs(x="Horsepower, bhp", y="Top speed, mph") +
  theme.car_chart_SMALLM