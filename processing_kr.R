library(tidyverse)
library(ggmap)

#testing geocode
d <- readxl::read_excel(path = "Nevada Data Example.xlsx", sheet = 1, col_names = T)
names(d) <- tolower(names(d))

t <- d[1,4]
s <- d[1,c("state")]

l <- regmatches(t, gregexpr("\\b[A-Z]\\w+", t))

#testing if geocode function returns lat and lon for "Franklinton, Louisiana"
x <- paste(l$locallity[3],s, sep=", ")

g <- geocode(location = x, source = "google",output = "all")

lat <- g$results[[1]]$geometry$bounds$southwest$lat
lon <- g$results[[1]]$geometry$viewport$southwest$lng

#what happens when we input "Vacant, Louisiana" into the geocode function?
x <- paste(l$locallity[1],s, sep=", ")
g <- geocode(location = x, source = "google", output = "all")

lat <- g$results[[1]]$geometry$bounds$southwest$lat
lon <- g$results[[1]]$geometry$viewport$southwest$lng

#what happens when we input "I10, Louisiana" into the geocode function?
x <- paste(l$locallity[2],s, sep=", ")
g <- geocode(location = x, source = "google", output = "all")

lat <- g$results[[1]]$geometry$bounds$southwest$lat
lon <- g$results[[1]]$geometry$viewport$southwest$lng


#set up iteration to test if input character is an actual place that meets our criteria

d <- readxl::read_excel(path = "Nevada Data Example.xlsx", sheet = 1, col_names = T)
names(d) <- tolower(names(d))

t <- d[1,4]
s <- d[1,c("state")]

l <- regmatches(t, gregexpr("\\b[A-Z]\\w+", t))

#step 1: make an empty data frame to hold results
df <- data.frame(locale=character(), lat=numeric(), lon=numeric())

for(i in 1:length(l$locallity)){

  h.name <- l$locallity[i]

  h.name.state <- paste(h.name, s, sep=", ") #test with l$locallity[1], then l$locallity[2], then l$locallity[3]

  g <- geocode(location = h.name.state, source = "google",output = "all")

  google.long.name <- g$results[[1]]$address_components[[1]]$long_name

  if(h.name == google.long.name){

    lat <- g$results[[1]]$geometry$bounds$southwest$lat
    lon <- g$results[[1]]$geometry$viewport$southwest$lng

  } else {

    lat <- NA
    lon <- NA
  }

  new.row <- c(google.long.name, lat, lon)
  df <- rbind(df, new.row)

}


  #xget the highway numbers out - nevada to test

t <- d[2,4]

#l <- regmatches(t, gregexpr(t, "^([A-Za-z]{1,2})"))
p <- "L" %R% ANY_CHAR
h <- str_extract(string = t, pattern = p)

install.packages("RVerbalExpressions")
library(RVerbalExpressions)
#strings = c('La 110','la 62','La 690',  'I10')

strings = t

expr =  rx_alpha()
stringr::str_extract_all(strings,expr)

expr =  rx_digit()
stringr::str_extract_all(strings,expr)


expr =  rx_alpha()  %>%  rx_word() %>% rx_alpha()
stringr::str_extract_all(strings,expr)

expr

"[A-z]\\d"
