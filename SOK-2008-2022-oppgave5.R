# Utfordring 5
# Oppgave 5.1.1

# Fjerner alt fra Global Envirement så ingenting kan klusse til koden senere.
rm(list = ls())

# Laster inn nødvendige pakker.
library(sf)
library(plyr)
library(httr) 
library(gdata)
library(dplyr)
library(readr)
library(tibble)
library(rjstat)
library(ggpubr)
library(rstatix)
library(cowplot)
library(janitor)
library(ggplot2)
library(tidyverse)
library(ggtextures)


# Setter lokalet til no-No for å få norsk språk (for å få øæå).
Sys.setlocale(locale="no_NO")

options(scipen=999)

# Setter arbeidsplassen.
setwd("~/")

#lagrer url
data <- "https://data.ssb.no/api/v0/no/table/05185/"

#lagrer etterspørringen til dataene i json stil.
ettersp <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "agg:Verdensdel2",
        "values": [
          "b0",
          "b11",
          "b12",
          "b13",
          "b14",
          "b2",
          "b3",
          "b4",
          "b5",
          "b6",
          "b8",
          "b9"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

# Lagre disse i en ny variabel.
innvandring <- data %>%
  POST(body = ettersp, encode = "json")

# Lagre i tibble format.
innvandring <-  innvandring %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()

# Fjerner brukte "values".
rm(data, ettersp)

# Summerer kjønn, mann og kvinne ilag.
innvandring2 <- innvandring %>%
  group_by(landbakgrunn, statistikkvariabel, år) %>% 
  transmute(value=sum(value))

# Fjerner alt som er oppført 2 ganger.
innvandring2 <- unique(innvandring2)

# Deler på 1000 for å få store verdier mindre.
innvandring2 <- innvandring2 %>% 
  mutate(Antall_i_1000 = value/1000)

# Plotter plottet.
innvandring2 %>% 
  group_by(landbakgrunn, år, Antall_i_1000) %>% 
  filter(år > 2004) %>% 
  ggplot(aes(x=år,y=Antall_i_1000,color=landbakgrunn, group=Antall_i_1000)) +
  geom_line(aes(group=landbakgrunn), size=1) +
  geom_point(size=2.5)+
  labs(x = "År" , y ="Arbeidsledige i prosent %") +
  labs(x = "År",
       y = "Antall innvandrere i 1000",
       title = "Innvandring til Norge etter landbakgrunn for tidsperioden 2005-2022",
       caption = "Source: https://www.ssb.no/statbank/table/05185/")

# Innvandringen til Norge har økt mest fra EU- land i Øst-Europa.



# Oppgave 5.1.2

data <- "https://data.ssb.no/api/v0/no/table/13215/"

ettersp <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "00",
          "194",
          "015a",
          "100c",
          "694c",
          "400",
          "200b",
          "794a"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "agg:NACE260InnvGrupp2",
        "values": [
          "SNI-01-03",
          "SNI-05-09",
          "SNI-10-33",
          "SNI-35-39",
          "SNI-41-43",
          "SNI-45-47",
          "SNI-49-53",
          "SNI-49.3",
          "SNI-55",
          "SNI-56",
          "SNI-58-63",
          "SNI-64-66",
          "SNI-68-75",
          "SNI-77-82",
          "SNI-78.2",
          "SNI-81.2",
          "SNI-84",
          "SNI-85",
          "SNI-86-88",
          "SNI-90-99",
          "SNI-00"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2021"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

# Lagre disse i en ny variabel.
jobber <- data %>%
  POST(body = ettersp, encode = "json")

# Lagre i tibble format.
jobber <-  jobber %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()

# Deler på tusen for å unngå høye tall.
jobber <- jobber %>% 
  mutate(Antall_i_1000 = value/1000)

jobber <- jobber %>% 
  filter(landbakgrunn == "EU-land i Øst-Europa")

# Plotter plott 2.
ggplot(jobber, aes(x = reorder(`næring (SN2007)`, Antall_i_1000) ,
                   y = Antall_i_1000, fill = `næring (SN2007)`)) +
  geom_bar(stat="identity", 
           width=.5, 
           position = "dodge") +
  rotate() +
  labs(x = "Ulike sektorer" , y ="Antall i 1000") +
  labs(x = "Ulike sektorer",
       y = "Antall innvandrere i 1000",
       title = "Næringsfordeling blant sysselsatte i ulike innvandringskategorier 
både mann og kvinner etter landbakgrunn i alderen 15-74 år for år 2021.",
       caption = "Source: https://www.ssb.no/statbank/table/13215")

# Flest med denne landbakgrunnen jobbet med bygg og anleggsvirksomhet.
