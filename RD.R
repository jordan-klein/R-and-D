# Install packages

library(rvest)
library(dplyr)

# Scrape data

R1 <- read_html("https://en.wikipedia.org/wiki/Research_I_university")
Unis <- R1 %>% 
  html_nodes(".wikitable") %>%
  html_table(header = TRUE)
Unis <- Unis[[1]]

wpop <- read_html("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population")
pop <- wpop %>% 
  html_nodes("table.wikitable:nth-child(11)") %>%
  html_table(header = TRUE)
pop <- pop[[1]]

wgdp <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP")
gdp <- wgdp %>% 
  html_nodes("table.wikitable:nth-child(11)") %>% 
  html_table(header = TRUE)
gdp <- gdp[[1]]

nsf <- read_html("https://ncsesdata.nsf.gov/sgrd/2016/html/SGRD2016_DST_04.html")
rnd <- nsf %>%
  html_nodes("#data-table") %>%
  html_table(header = TRUE, fill = TRUE)
rnd <- rnd[[1]]

# Clean data

names(rnd) <- rnd[1, ]

rnd <- rnd[-c(1:2), ]
rnd$states <- state.abb[match(rnd$State, state.name)]
rnd$states[rnd$State == "District of Columbia"] <- "DC"

rnd <- select(rnd, states, `All R&D expenditures`)

States_U <- as.data.frame(table(Unis$State, useNA = "ifany"))
names(States_U) <- c("states", "universities")

pop$states <- state.abb[match(pop$`State or territory`, state.name)]
pop$states[pop$`State or territory` == "District of Columbia"] <- "DC"
pop$population <- pop$`Population estimate, July 1, 2017[4]`

pop <- select(pop, states, population)
pop <- filter(pop, !is.na(states))

gdp$states <- state.abb[match(gdp$`State or territory`, state.name)]
gdp$states[gdp$`State or territory` == "District of Columbia"] <- "DC"
gdp$GDP <- gdp$`2017`

gdp <- gdp %>% 
  select(states, GDP) %>%
  filter(!is.na(states))

RND <- read.csv("~/Downloads/rnd.csv")
RND <- RND %>%
  select(State, All.R.D) %>%
  filter(State != "")

RND$states <- state.abb[match(RND$State, state.name)]
RND$states[RND$State == "District of Columbia"] <- "DC"
RND <- filter(RND, !is.na(states)) %>%
  select(states, All.R.D)

Edu <- left_join(pop, gdp) %>%
  left_join(rnd) %>%
  left_join(RND) %>%
  left_join(States_U)
Edu$universities[is.na(Edu$universities)] <- 0

Edu[, c(2:5)] <- lapply(Edu[, c(2:5)], function(x) {
  gsub(",", "", x) %>%
    as.numeric(x)
})

Edu$U_per_cap <- Edu$universities/Edu$population
Edu$U_per_gdp <- Edu$universities/Edu$GDP

Edu$R_per_cap <- Edu$All.R.D/Edu$population
Edu$R_per_gdp <- Edu$All.R.D/Edu$GDP

Edu$U_rank <- rank(-Edu$universities, ties.method = c("min"))
Edu$U_PC_rank <- rank(-Edu$U_per_cap, ties.method = c("min"))
Edu$U_PG_rank <- rank(-Edu$U_per_gdp, ties.method = c("min"))

Edu$Rnd_rank <- rank(-Edu$All.R.D, ties.method = c("min"))
Edu$R_PC_rank <- rank(-Edu$R_per_cap, ties.method = c("min"))
Edu$R_PG_rank <- rank(-Edu$R_per_gdp, ties.method = c("min"))

Edu$Rscore <- Edu$U_PG_rank + Edu$R_PG_rank
