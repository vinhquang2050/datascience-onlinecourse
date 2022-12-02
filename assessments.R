library(tidyverse)
library(dslabs)
library(ggplot2)
library(gtools)
library(NHANES)
library(dplyr)
library(ggrepel)
library(gggenes)
data(murders)

murders %>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()


ind <- c("Alabama", "Alaska", "Arizona", "H")
!ind %in% murders$state
match(ind, murders$state)

us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

a <-  2
b <- -1
c <- -4


(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)

log(1024,4)

data(movielens)
str(movielens)
class(movielens$title)
class(movielens$genre)
levels(movielens$genre)
nlevels(movielens$genre)


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)


time <- time/60
speed <- distance/time

result <- data.frame(name = name, dist = distance, time = time, speed = speed)
result
result$name[which.max(result$speed)]


data(NHANES)
str(NHANES)
head(NHANES)
tab <- NHANES %>% filter(AgeDecade == " 20-29", Gender == "female")
head(tab)
levels(NHANES$AgeDecade)

data(heights)
options(digits = 3)
str(heights)
ind <- filter(heights, height > mean(height))
nrow(ind)
ind <- filter(heights, height > mean(height) & sex == "Female")
nrow(ind)
female_proportion <- nrow(filter(heights, sex == "Female"))/nrow(heights)
female_proportion
min_height <- min(heights$height)
id_min_height <- match(min_height, heights$height)
heights$sex[id_min_height]
max_height <- max(heights$height)
max_height
x<-50:82
ind <- x %in% heights$height
sum(!ind)
heights2 <- heights %>% mutate(ht_cm = height*2.54)
mean(heights2$ht_cm)
females <- filter(heights2, sex == "Female")
mean(females$ht_cm)
data(olive)
str(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(olive$palmitic~olive$region)

gender_vector <- ifelse(heights$sex == "Female", 1, 2)
sum(gender_vector)
height_vector <- ifelse(heights$height > 72, heights$height, 0)
mean(height_vector)
inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)
less_5feet <- sapply(heights$height, inches_to_ft) < 5
sum(less_5feet)

test <- vector(length = 5)
test
for (i in 1:5){
  test[i] <- i^2
}
test

# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n



B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 10){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates


# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins <- replicate(B, {
  a <- simulated_games
  any(a == "win")
})

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
sum(celtic_wins)

players <- 8
jamaica_players <- 3
medals <- 3
possibilities <- permutations(players, medals, 1:players, repeats.allowed = FALSE)
jamaica_possibilities <- permutations(jamaica_players, medals, 1:players, repeats.allowed = FALSE)
Pr_Jamaica <- nrow(jamaica_possibilities)/nrow(possibilities)
Pr_Jamaica

B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
simulation <- replicate(B,{
  winners <- sample(runners, 3, replace = FALSE)
  all(winners == "Jamaica")
})
mean(simulation)


entre <- c(1:6)
entre <- paste("entre", entre)

side <- c(1:6)
side <- paste("side", side)

drink <- c(1:3)
drink <- paste("drink", drink)

side_possibilies <- combinations(length(side), 3, side, repeats.allowed = FALSE)
length(entre)*nrow(side_possibilies)*length(drink)


meal <- function(n){
  entre <- c(1:n)
  entre <- paste("entre", entre)
  
  side <- c(1:6)
  side <- paste("side", side)
  
  drink <- c(1:3)
  drink <- paste("drink", drink)
  
  side_possibilies <- combinations(length(side), 2, side, repeats.allowed = FALSE)
  length(entre)*nrow(side_possibilies)*length(drink)
}

sapply(1:12, meal)



meal <- function(n){
  entre <- c(1:6)
  entre <- paste("entre", entre)
  
  side <- c(1:n)
  side <- paste("side", side)
  
  drink <- c(1:3)
  drink <- paste("drink", drink)
  
  side_possibilies <- combinations(length(side), 2, side, repeats.allowed = FALSE)
  length(entre)*nrow(side_possibilies)*length(drink)
}

sapply(2:12, meal)

data(esoph)
head(esoph)
str(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
levels(esoph$alcgp)
highest_alc <- filter(esoph, alcgp == "120+")
sum(highest_alc$ncases)/(sum(highest_alc$ncases)+sum(highest_alc$ncontrols))

lowest_alc <- filter(esoph, alcgp == "0-39g/day")
sum(lowest_alc$ncases)/(sum(lowest_alc$ncases)+sum(lowest_alc$ncontrols))

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)


levels(esoph$tobgp)
esoph %>%
  filter(tobgp != "0-9g/day") %>%
  summarize(ncases = sum(ncases)) %>%
  mutate(p_case = ncases / all_cases) %>%
  pull(p_case)

esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()/all_controls

highest_alc <- filter(esoph, alcgp == "120+")
sum(highest_alc$ncases)/all_cases

esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()/all_cases

esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncases) %>%
  sum()/all_cases

esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncases) %>%
  sum()/all_cases

esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()/all_controls

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / ncontrols) %>%
  pull(p_case)


highest_alc <- filter(esoph, alcgp == "120+")
p_highest_alc_case <- sum(highest_alc$ncases)/all_cases
p_highest_alc_control <- sum(highest_alc$ncontrols)/all_controls
p_highest_alc_case/p_highest_alc_control


esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()/all_controls

esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()/all_controls

esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()/all_controls



p_high_both_case <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncases) %>%
  sum()/all_cases

p_high_both_control <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()/all_controls

p_high_both_case/p_high_both_control

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
sum(act_scores > 30)/length(act_scores)
sum(act_scores <= 10)/length(act_scores)
x <- 1:36
f_x = dnorm(x, 20.9, 5.7)
plot(x, f_x)
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
sum(z_scores > 2)/length(act_scores)
20.9+5.7*2
qnorm(0.975, mean(act_scores), sd(act_scores))
act_cdf <- function(x){
  sum(act_scores <= x)/length(act_scores)
}
p_act_scores <- sapply(1:36, act_cdf)
min(which(p_act_scores > 0.95))

qnorm(0.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles = quantile(act_scores, p)
min(which(sample_quantiles > 26)) - 1
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
theoretical_quantiles
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

n_questions <- 44
p_correct = 1/5
p_incorrect = 1 - p_correct
E_1 = 1*p_correct + (-0.25)*p_incorrect
E_44 = n_questions * E_1
SE_44 = sqrt(n_questions)*abs(-0.25-1)*sqrt(p_correct*p_incorrect)
P_8 = 1 - pnorm(8, E_44, SE_44)
set.seed(21, sample.kind = "Rounding")
B <- 10000
X <- replicate(B, {
  sum(sample(c(1, -0.25), n_questions, replace = TRUE, prob = c(p_correct, p_incorrect)))
})
mean(X >= 8)

n_questions <- 44
p_correct = 1/4
p_incorrect = 1 - p_correct
E_1 = 1*p_correct + (0)*p_incorrect
E_44 = n_questions * E_1
SE_44 = sqrt(n_questions)*abs(0-1)*sqrt(p_correct*p_incorrect)
P_8 = 1 - pnorm(8, E_44, SE_44)
set.seed(21, sample.kind = "Rounding")
B <- 10000
X <- replicate(B, {
  sum(sample(c(1, 0), n_questions, replace = TRUE, prob = c(p_correct, p_incorrect)))
})
mean(X >= 8)

p <- seq(0.25, 0.95, 0.05)
n_questions <- 44
p_correct = 0.95
p_incorrect = 1 - p_correct
E = n_questions*(1*p_correct + 0*(p_incorrect))
SE = sqrt(n_questions)*abs(0-1)*sqrt(p_correct*p_incorrect)
P_8 = 1 - pnorm(8, E, SE)
f <- function(p){
  E = n_questions*(1*p + 0*(1-p))
  SE = sqrt(n_questions)*abs(0-1)*sqrt(p*(1-p))
  1 - pnorm(35, E, SE)
}

f(p)

p_win = 5/38
p_lose = 1 - p_win
n_bet <- 500
E_1 = 6*p_win + (-1)*p_lose
SE_1 = abs(-1-6)*sqrt(p_win*p_lose)
E_avg_500 = E_1
SE_avg_500 = SE_1/sqrt(n_bet)
E_sum_500 = n_bet * E_1
SE_sum_500 = sqrt(n_bet) * SE_1
pnorm(0, E_sum_500, SE_sum_500)


data(death_prob)
head(death_prob)
str(death_prob)
p <- death_prob %>% filter(age == 50, sex == "Female") %>% pull(prob)
loss <- -150000
gain <- 1150
E_1 <- p*loss + (1-p)*1150
SE_1 <- abs(gain - loss)*sqrt(p*(1-p))
E_1000 <- 1000*(p*loss + (1-p)*1150)
SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p*(1-p))
pnorm(0, E_1000, SE_1000)

p <- death_prob %>% filter(age == 50, sex == "Male") %>% pull(prob)
loss <- -150000
gain <- 1150
E_1 <- p*loss + (1-p)*1150
SE_1 <- abs(gain - loss)*sqrt(p*(1-p))
E_1000 <- 1000*(p*loss + (1-p)*1150)
SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p*(1-p))
pnorm(0, E_1000, SE_1000)

premium <- (700000/1000 - p*loss)/(1-p)
p <- death_prob %>% filter(age == 50, sex == "Male") %>% pull(prob)
loss <- -150000
gain <- premium
E_1 <- p*loss + (1-p)*gain
SE_1 <- abs(gain - loss)*sqrt(p*(1-p))
E_1000 <- 1000*(p*loss + (1-p)*gain)
SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p*(1-p))
pnorm(0, E_1000, SE_1000)


p <- 0.015
loss <- -150000
gain <- 1150
E_1 <- p*loss + (1-p)*gain
SE_1 <- abs(gain - loss)*sqrt(p*(1-p))
E_1000 <- 1000*(p*loss + (1-p)*gain)
SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p*(1-p))
pnorm(0, E_1000, SE_1000)
pnorm(-1000000, E_1000, SE_1000)


p <- seq(.01, .03, .001)
cal_p_lose <- function(p){
  loss <- -150000
  gain <- 1150
  E_1000 <- 1000*(p*loss + (1-p)*gain)
  SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p*(1-p))
  pnorm(0, E_1000, SE_1000)
}
p_lose <- sapply(p, cal_p_lose)
p[min(which(p_lose > 0.9))]

p <- seq(.01, .03, .0025)
cal_p_lose_1M <- function(p){
  loss <- -150000
  gain <- 1150
  E_1000 <- 1000*(p*loss + (1-p)*gain)
  SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p*(1-p))
  pnorm(-1000000, E_1000, SE_1000)
}
p_lose <- sapply(p, cal_p_lose_1M)
p[min(which(p_lose > 0.9))]

set.seed(25, sample.kind = "Rounding")
N_loans <- 1000
p_loss <- 0.015
p_gain <- 1 - p_loss
loss <- -150000
gain <- 1150
X <- sample(c(loss, gain), N_loans, replace = TRUE, prob = c(p_loss, p_gain))
sum(X)

set.seed(27, sample.kind = "Rounding")
B <- 10000
N_loans <- 1000
p_loss <- 0.015
p_gain <- 1 - p_loss
loss <- -150000
gain <- 1150
X <- replicate(B, {
  sum(sample(c(loss, gain), N_loans, replace = TRUE, prob = c(p_loss, p_gain)))
})
sum(X <= -1000000)/B


N_loans <- 1000
p_loss <- 0.015
p_gain <- 1 - p_loss
loss <- -150000
p_lose_money <- 0.05
z <- qnorm(p_lose_money)
gain <- (loss*z*sqrt(N_loans*p_loss*p_gain) - loss*p_loss*N_loans)/
  (z*sqrt(N_loans*p_loss*p_gain)+p_gain*N_loans)
E_1 <- p_loss*loss + p_gain*gain
SE_1 <- abs(gain - loss)*sqrt(p_loss*p_gain)
E_1000 <- 1000*(p_loss*loss + p_gain*gain)
SE_1000 <- sqrt(1000)*abs(gain - loss)*sqrt(p_loss*p_gain)


set.seed(28, sample.kind = "Rounding")
B <- 10000
N_loans <- 1000
p_loss <- 0.015
p_gain <- 1 - p_loss
loss <- -150000
gain <- 3268
X <- replicate(B, {
  sum(sample(c(loss, gain), N_loans, replace = TRUE, prob = c(p_loss, p_gain)))
})
sum(X < 0)/B

set.seed(29, sample.kind = "Rounding")
B <- 10000
p_loss <- 0.015
N_loans <- 1000
loss <- -150000
gain <- 3268
X <- replicate(B, {
  p_loss <- p_loss + sample(seq(-0.01, 0.01, length = 100), 1)
  p_gain <- 1 - p_loss
  sum(sample(c(loss, gain), N_loans, replace = TRUE, prob = c(p_loss, p_gain)))
})
mean(X)
sum(X < 0)/B
sum(X < -1000000)/B

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  ggplot(aes(dollars_per_day, ..count..), group = year) + 
  geom_density() +
  scale_x_continuous(trans = "log2")+
  facet_grid(year~.)

options(digits = 3) 
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

str(titanic)

titanic %>% 
  group_by(Sex) %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex~.)

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack") 

titanic %>% 
  group_by(Sex) %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)

male_number <-  nrow(filter(titanic, Sex == "male"))
female_number <-  nrow(filter(titanic, Sex == "female"))

male_number_40 <-  nrow(filter(titanic, Sex == "male" & Age == 40))
female_number_40 <-  nrow(filter(titanic, Sex == "female" & Age == 40))

male_number_1835 <-  nrow(filter(titanic, Sex == "male" & Age %in% 18:35))
female_number_1835 <-  nrow(filter(titanic, Sex == "female" & Age %in% 18:35))

male_number_1835/male_number
female_number_1835/female_number

male_number_u17 <-  nrow(filter(titanic, Sex == "male" & Age < 17))
female_number_u17 <-  nrow(filter(titanic, Sex == "female" & Age < 17))

male_number_u17/male_number
female_number_u17/female_number

titanic$Sex[which.max(titanic$Age)]

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

titanic %>% 
  ggplot(aes(Survived, y = ..count.., fill = Survived)) +
  geom_bar(position = position_dodge())

titanic %>% 
  group_by(Sex) %>% 
  ggplot(aes(Survived, y = ..count.., fill = Sex)) +
  geom_bar(position = position_dodge())


titanic %>% filter(!is.na(Age)) %>%
  group_by(Survived) %>% 
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)

titanic %>% filter(Fare > 0) %>%
  group_by(Survived) %>% 
  ggplot(aes(Survived, Fare, fill = Survived)) +
  scale_y_continuous(trans = "log2") +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2)


titanic %>%
  group_by(Pclass) %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>%
  group_by(Pclass) %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>%
  group_by(Survived) %>% 
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>% filter(!is.na(Age)) %>%
  group_by(Survived) %>% 
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, position = "stack") +
  facet_grid(Sex~Pclass)


data(stars)
options(digits = 3)

str(stars)
mean(stars$magnitude)
sd(stars$magnitude)

stars %>% 
  ggplot(aes(magnitude)) +
  geom_density()

stars %>% 
  ggplot(aes(temp)) +
  geom_density()

stars %>% 
  ggplot(aes(temp, magnitude)) +
  geom_point()

stars %>%
  ggplot(aes(x=log10(temp), magnitude)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_point()

stars %>%
  ggplot(aes(x=log10(temp), magnitude, label = star)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_text_repel() +
  geom_point()


stars %>%
  ggplot(aes(x=log10(temp), magnitude, color = type)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_point() + 
  scale_colour_manual(values = c("#000000", "#AAAAAA", "#0022BB", "#22BB00", "#CCCCCC", "#CC00CC", "#CCCC00", "#00CCCC", "#CC0000", "#888888"))


data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  summarize(year_minmax = c(max(year),min(year)), temp_anomaly_minmax = c(temp_anomaly[which.max(year)], temp_anomaly[which.min(year)])) %>%
  pull(year_minmax, temp_anomaly_minmax)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  summarize(carbon_emissions[which.max(year)]/carbon_emissions[which.min(year)]) %>%
  pull()

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + geom_line(aes(year, ocean_anomaly), col = "red") +
  geom_line(aes(year, land_anomaly), col = "green")


greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept = 1850), col = "red")

co2_time <- historic_co2 %>%
  filter(!is.na(co2)) %>%
  ggplot(aes(year, co2, color = source)) +
  geom_line()

historic_co2 %>%
  filter(!is.na(co2)) %>%
  ggplot(aes(year, co2, color = source)) +
  xlim(-3000, 2018) +
  geom_line()


# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500
sqrt(p*(1-p)*1500)
X_hat <- p
X_hat_se <- sqrt(X_hat*(1-X_hat)/N)
d <- -0.038
d_se <- 2*X_hat_se

head(brexit_polls)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

spread_mean <- mean(brexit_polls$spread)
spread_sd <- sd(brexit_polls$spread)
x_hat_mean <- mean(brexit_polls$x_hat)
x_hat_sd <- sd(brexit_polls$x_hat)

brexit_polls[1,]
x_hat <- 0.52
N <- 4772
se_hat <- sqrt(x_hat*(1-x_hat)/N)

x_hat_ci <- c(x_hat - qnorm(0.975)*se_hat, x_hat + qnorm(0.975)*se_hat)

june_polls <- brexit_polls %>% filter(enddate > "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), 
         se_spread = 2*se_x_hat, 
         lower = spread - qnorm(0.975)*se_spread, 
         upper = spread + qnorm(0.975)*se_spread, 
         hit = lower < d & upper > d,
         zero = lower < 0 & upper > 0,
         remain = lower > 0)

mean(june_polls$zero)
mean(june_polls$hit)
mean(june_polls$remain)

june_polls %>% group_by(pollster) %>%
  summarise(p_hit = mean(hit), n = n()) %>%
  arrange(p_hit)

june_polls %>% group_by(poll_type) %>%
  ggplot(aes(poll_type, spread)) +
  geom_boxplot()


combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>%
  mutate(se_p_hat = sqrt(p_hat*(1-p_hat)/N),
         se_spread = 2*se_p_hat,
         lower = spread - qnorm(0.975)*se_spread, 
         upper = spread + qnorm(0.975)*se_spread)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value

brexit_chisq
p_hit_online <- brexit_chisq[[3]]/(brexit_chisq[[1]]+brexit_chisq[[3]])
p_hit_tel <- brexit_chisq[[4]]/(brexit_chisq[[2]]+brexit_chisq[[4]])
p_hit_online
p_hit_tel

ratio_online <- p_hit_online / (brexit_chisq[[1]]/(brexit_chisq[[1]]+brexit_chisq[[3]]))
ratio_tel <- p_hit_tel / (brexit_chisq[[2]]/(brexit_chisq[[2]]+brexit_chisq[[4]]))
ratio_online
ratio_tel
ratio_online/ratio_tel

brexit_polls %>% ggplot(aes(enddate, spread, col = poll_type)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.4) +
  geom_hline(aes(yintercept = -0.038))

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate, proportion, color = vote)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3)