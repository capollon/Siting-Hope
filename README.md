---
title: "Predicting Mesa Opioid Overdoses"
author: "Marlana Zink and Celine Apollon"
date: "12/17/2021"
output: 
  bookdown::html_document2:
    code_folding: hide
    fig_caption: yes
    toc: yes
editor_options: 
  markdown:
    wrap: sentence
    chunk_output_type: console
---

```{css, echo=FALSE, warning = FALSE, message = FALSE, results = FALSE }
pre {
  max-height: 300px;
  overflow-y: auto;
}

pre[class] {
  max-height: 300px;
}
```

```{css, echo=FALSE, warning = FALSE, message = FALSE, results = FALSE }
.scroll-300 {
  max-height: 300px;
  overflow-y: auto;
  background-color: inherit;
}
```

```{r setup_1, include=FALSE, warning = FALSE, message = FALSE, results = FALSE }
knitr::opts_chunk$set(echo = TRUE)
```

# Video Overview of Siting Hope 

Link: <https://www.youtube.com/watch?v=D9gscHyHEzM>

# Introduction & Use Case

## Introduction 

No one wants to receive a call that their loved one has overdosed---let alone watch their loved ones suffer from a city's inability to efficiently and equitably allocate health and social service resources.
For decades, city health departments, scientists, and health specialists have been trying to analyze the most effective strategy for helping people suffering from opioid use disorders, as overdose fatalities reached an all-time high of over 100,000 in 2020. 
Recently, cities including New York City, have pivoted to a "harm reduction" strategy, focusing on ways to work *with* people with opioid addictions rather than over police, restrict resources, and outprice these users. 
Since this shift, overdoses have decreased, however there is missing "ground game" component from cities' approach to combatting the opioid epidemic. 
 

As of November 30, 2021, New York City was the first city in the United States to agree to open two Overdose Protection Site (OPS), also known as safe injection facilities (SIF), in the neighborhoods of Washington Heights and East Harlem.
The NYC Health Department conducted a feasibility study using similar implementation strategies to those of Vancouver, British Colombia whose OPS have reduced overdoses by 30%.
Their research and results confirmed the large cost savings that OPS can create, due to decreased money spent on ambulances, hospital visits, and in-patient rehabilitation care.
[1, 2] NYC predicts that one OPS can save 19 lives from overdoses. 
Mayor de Blasio states these sites provide "less punitive and more accessible solution" to the epidemic problem. 
 

Overdose health experts are onsite at all times to provide clean needles, administered naloxone to reverse overdoses, and options for addiction treatment. 
There are currently 100 OPS operating in 67 cities and in over 10 countries, and as of 2018, no fatal overdose has been documented on any OPS in the world.
OPS is widely supported by health professionals due to the scientific evidence that shows how OPS destigmatizes drug use, allowing drug users to feel more comfortable speaking with medical mental health providers and social services.
[3] The data overtime also refutes the claim that OPS will increase drug usage or crime and reveals that user have gained more affordable access to routine primary care and reduced the length of users' hospital stays. 
Findings have also shown that OPS spaces curate healthy ways for users suffering from drug abuse to share community and build mutually trusted relationships with their neighbors, further destigmatizing drug users. 
 Many call them "safe havens" for marginalized or disconnected populations of users (particularly women and houseless individuals).
[4, 5] Most importantly, OPS reduces overdose fatalities and health consequences. 

## Environmental Factors

The Health Foundation in the UK conducted research on the impact of a person's environment on their mental health.
They found that positive health stems from safe and communal facilities, areas to socialize and play, and abundant and accessible public infrastructure such as benches or streetlights. 
Other studies by the National Counseling Society show that whether addicts relapse is highly determined by their environments.
[6] Further, building designers have started to consider environmental factors when designing rehabilitation centers such as the new Ohana rehabilitation center shown below designed by HGA.
Despite this shift in rehab center design from prisons to havens embedded into the environment, these centers are unaffordable for the typical addict.
[7] Additionally, critics question if rehab centers in their entirety are missing the mark for helping addicts heal, recover, and adapt.
Unfortunately, rehab centers do not fix the factors of a person's life---jobs, housing, relationships---that originally induced him/her/them to develop an addiction.
In fact, it's questionable whether isolating individuals during rehab helps a person walk down the path to recovery, as isolation can exacerbate these factors upon return into normal life. 
So maybe the solution is buying, using, and recovery all in the same place? 


![Ohana Rehab Center ](C:\Users\Celine\Desktop\Fall 2021\Public Policy Analytics\FINAL\ohana mental health facility rendering.png)

## Use Case

Mesa's current use case for its current dashboard for all of Maricopa County called MaricopaRx, is geared towards connecting medical providers, educators, and other stakeholders engaged in the fight to end the opioid epidemic in Maricopa County.
However, the dashboard excludes a direct use for opioid users themselves.
Thus, to fill this gap, Siting Hope's primary use case will be for opioid users to locate nearest OPS site in addition to the following:  

-   Educating bystanders or witnesses of what to do in a situation when someone is experiencing an overdose. 
     

-   Customizable hotline call feature that connects users to social services allowing users to input racial, gender, or language preferences to eliminate concern of police and ensure users receive the most comforting and helpful service. 
     

-   Maps showing the nearest places to go purchase Narcan/ naloxone including specific pharmacies not requiring health insurance 

-   Dashboard version for public health officials of Maricopa County to monitor the status of each OPS including construction costs, \# of social services, and \# of prevented overdoses.
    The dashboard can also be used to help public officials determine where to invest in curating environments and providing social service resources for users including establishing overdose-specific EMS services in hotspot neighborhoods. 
     

# Siting Hope Data

Both location and demographic data were used for our analysis, with the final datasets being: 

[Dependent Variable]{.ul}

-   2017 overdose events -- Source: City of Mesa Open Data Portal 

[Risk Factors]{.ul}

-   2017 code violations, filtered for public nuisance violations -- Source: City of Mesa Open Data Portal  

-   2017 counts of unsheltered people -- Source: City of Mesa Open Data Portal  

-   2017 drug crime incidents, filtered to exclude incidents involving marijuana -- Source: City of Mesa Open Data Portal 

```{=html}
<!-- -->
```
-   2017 census tracts with more than 11% of the population reporting poor mental health -- Source: CDC 

-   2017 census tracts with more than 15% of the population lacking insurance -- Source: CDC 

-   Street light locations -- City of Mesa Open Data Portal  

Because opioid abuse is a complex issue, it was important that our data reflected the environmental, demographic, and social factors that may play contribute to the likelihood of an overdose event.
We recognize that there are many other factors that can influence an overdose and, while many other datasets were explored, each dataset came with its own limitations which led to the decision of whether or not it should or even could be included.
These limitations are described in more detail in the limitations section. 
 

```{r setup_2, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE }

setwd("C:/Users/Celine/Desktop/Fall 2021/Public Policy Analytics/FINAL")

library(RSocrata)
library(dplyr)
library(tidyverse)
library(sf)
library(tidycensus)
library(ggplot2)
library(mapview)
library(tmap)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(readxl)
install.packages("osmdata")
library(osmdata)
library(tigris)
install.packages("plotly")
library(plotly)
install.packages("ggmap")
library(ggmap)


root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
 
crossValidate1 <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset$cvID)
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, cvID != thisFold) %>% as.data.frame() %>% 
      dplyr::select(cvID, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, cvID == thisFold) %>% as.data.frame() %>% 
      dplyr::select(cvID, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countOverdose2017 ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -cvID))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}


crossValidate2 <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  districts_list <- unique(dataset$districts)
  
  for (i in districts_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, districts != thisFold) %>% as.data.frame() %>% 
      dplyr::select(districts, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, districts == thisFold) %>% as.data.frame() %>% 
      dplyr::select(districts, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countOverdose2017 ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -districts))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}


```

```{r load_data, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE }

### ---- Geographic Data ----

## Mesa Boundaries 
census_api_key("e0f5bf7187090237f6ebb3e0289be4cc832b2112", overwrite = TRUE)
v18 <- load_variables(2018, "acs5", cache = TRUE)

MesaCityBoundary <- st_read("City Boundary2.geojson") %>%
  st_transform('EPSG:26912')


CouncilDistricts <- st_read("Council District.geojson") %>% 
  st_transform('EPSG:26912')

MesaTracts <- 
  get_acs(geography = "tract", variables = c("B01003_001E", # Total population
                                             "B01001_011E", # Male 25-29
                                             "B01001_012E", # Male 30-34
                                             "B01001_013E", # Male 35-39
                                             "B01001_014E"), # Male 40-44
          year=2018, state=04, county=013, geometry=T) %>% 
  st_transform('EPSG:26912')

MesaTracts <- 
  MesaTracts %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B01003_001, 
         Male25_29 = B01001_011, 
         Male30_34 = B01001_012,
         Male35_39 = B01001_013, 
         Male40_44 = B01001_014) 

MesaTracts <- MesaTracts %>%
  mutate(male25_44 = (Male25_29 + Male30_34 + Male35_39 + Male40_44),
         pctMale25_44 = ((male25_44)/(TotalPop) * 100))

### --- Dependent Variable ---

MesaOverdoses <- read.socrata("https://data.mesaaz.gov/resource/qufy-tzv6.json")

MesaOverdoses <- MesaOverdoses[!(is.na(MesaOverdoses$latitude)), ]

MesaOverdoses <- st_as_sf(x = MesaOverdoses,                         
            coords = c("longitude", "latitude"),
            crs = "EPSG:4326", agr = "constant", remove = FALSE)

MesaOverdoses <- MesaOverdoses %>% 
  st_transform('EPSG:26912')

MesaOverdoses2017 <- MesaOverdoses %>%
  dplyr::filter(year == "2017")

### --- Risk Factors ---

## CDC Places Data 

CDC.Places2017 <- read.socrata("https://chronicdata.cdc.gov/resource/kucs-wizg.json") %>%
  dplyr::select(placename, placefips, tractfips, access2_crudeprev, mhlth_crudeprev, sleep_crudeprev, teethlost_crudeprev) %>% 
  dplyr::filter(placefips == "0446000")

## Joing CDC and tracts data 

MesaTracts <- MesaTracts %>%
  left_join(CDC.Places2017, by = c("GEOID" = "tractfips"))

DemographicCentroids <- st_centroid(MesaTracts)

## Lack insurance

Insurance <- DemographicCentroids %>%
  dplyr::select(access2_crudeprev, geometry) %>%
  dplyr::filter(as.numeric(access2_crudeprev) > 15.0)

Insurance <- Insurance[!(is.na(Insurance$access2_crudeprev)), ]

Insurance.sf <- Insurance %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(legend = "Lack_Insurance")

mapview(MesaCityBoundary) + mapview(Insurance.sf)


## Poor mental health

MentalHealth <- DemographicCentroids %>%
  dplyr::select(mhlth_crudeprev, geometry) %>%
  dplyr::filter(as.numeric(mhlth_crudeprev) > 11.0)

MentalHealth <- MentalHealth[!(is.na(MentalHealth$mhlth_crudeprev)), ]

MentalHealth.sf <- MentalHealth %>%
  dplyr::select(geometry) %>%
  dplyr::mutate(legend = "Mental_Health")

mapview(MesaCityBoundary) + mapview(MentalHealth.sf)

## Unsheltered People 

unshelteredPeople <- read.socrata("https://data.mesaaz.gov/resource/jagk-fkkw.json") %>%
  dplyr::filter(city == "Mesa")

unshelteredPeople2017 <- read.socrata("https://data.mesaaz.gov/resource/jagk-fkkw.json") %>%
  dplyr::filter(city == "Mesa" & reporting_year == "2017")

unshelteredPeople2017.sf <- st_as_sf(x = unshelteredPeople2017,                         
                          coords = c("longitude", "latitude"),
                          crs = 4326) %>% 
  st_transform('EPSG:26912') %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Unsheltered_People")

## Code Violations

codeViolations <- read.socrata("https://data.mesaaz.gov/resource/ears-rpf9.json") %>%
  dplyr::filter(ordinance_in_violation == "8-6-3(Z)" | ordinance_in_violation == "8-6-3(A)" |
                  ordinance_in_violation == "8-6-3(B)" | ordinance_in_violation == "8-6-3(C)" |
                  ordinance_in_violation == "8-6-3(D)")

codeViolations.sf <- st_as_sf(x = codeViolations,                         
                                     coords = c("longitude", "latitude"),
                                     crs = 4326)

codeViolations2017.sf <- codeViolations.sf %>%
  dplyr::filter(year_opened == "2017") %>% 
  st_transform('EPSG:26912')%>% 
  dplyr::select(geometry) %>%
  mutate(legend = "Code_Violations")

## Street Light Fixtures 

streetLights <- st_read("Streetlight Fixtures.geojson") %>% 
  dplyr::filter(fixture_usage == "Pedestrian" | fixture_usage == "Novelty") %>% 
  st_transform('EPSG:26912') %>%
  dplyr::select(geometry) %>%
  mutate(legend = "Street_Lights")

## Crime Incidents

policeIncidents <- read.socrata("https://data.mesaaz.gov/resource/39rt-2rfj.json")

policeIncidents <- policeIncidents[!(is.na(policeIncidents$latitude)), ]

policeIncidents2017 <- policeIncidents %>%
  dplyr::filter(report_year == "2017")

policeIncidents2017 <- policeIncidents2017 %>%
  dplyr::filter(national_incident_based_crime_reporting_description == "DRUG EQUIPMENT VIOLATION" | 
                  national_incident_based_crime_reporting_description == "DRUG/NARCOTIC VIOLATION" &
                  crime_type != "MARIJUANA-POSSESS-USE") 


policeInxidents2017.sf <- st_as_sf(x = policeIncidents2017,                         
                                     coords = c("longitude", "latitude"),
                                     crs = 4326) %>% 
                          st_transform('EPSG:26912') %>%
                          dplyr::select(geometry) %>%
                          mutate(legend = "Police_Incidents")

```

# Exploratory Analysis

## Initial Exploration

To begin our exploratory analysis, we first created a fishnet, or a grid of 500 square meter cells, for the City of Mesa.
The grid allows us to see how many overdose and risk factors events happen within each cell, giving us a standardized way to compare the concentration of these otherwise scattered events across space.
The maps below shows which areas of the city have the highest concentration of overdoses and of each risk factor. 
 

```{r exploratory_analysis_1, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }

### ---- Creating fishnet

fishnet <- MesaCityBoundary %>%
  st_make_grid(cellsize = 500) %>%   
  st_intersection(MesaCityBoundary) %>%
  st_sf() %>%
  mutate(id = row_number()) %>% 
  st_transform(4326) %>%
  st_transform("EPSG:26912")

### ---- Joining 2017 overdoses to fishnet  

overdoses2017_net <- MesaOverdoses2017 %>% 
  mutate(countOverdose2017 = 1) %>% 
  dplyr::select(countOverdose2017) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countOverdose2017 = replace_na(countOverdose2017, 0),
         id = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = overdoses2017_net, aes(fill=countOverdose2017), colour=NA) +
  scale_fill_viridis_c(option = "C") +
  labs(title= "2017 Overdoses by Fishnet") +
  mapTheme()

```

```{r exploratory_analysis_2, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }

### --- 2017 Risk Factors by Fishnet 

vars_net <- 
  rbind(codeViolations2017.sf, unshelteredPeople2017.sf, streetLights, 
        policeInxidents2017.sf, MentalHealth.sf, Insurance.sf) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(id, legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -id)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis_c(option = "C") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol=3, top="Overdose Risk Factors by Fishnet"))

```

Since we are working with spatial data, another way to explore it is to calculate the average nearest neighbor distance for each risk factor, shown below.
The nearest neighbor calculation helps us see how far risk factor events are happening from one another.
While the fishnet allows us to see the concentration of events within the 500 square meter cells, the nearest neighbor calculation helps us being to explore, on average, how far each given drug crime is on from the three drug crime nearest to it.
Drug crimes with very close neighboring drug crimes tells a very different story than drug crimes with very far neighboring drug crimes. 
As the map below shows, many risk factors, like code violations, homelessness, and high concentrations of people without insurance, are happening near other similar events.
We can see this depicted in purple on the maps.
On the other hand, we see less purple in a risk factor like street lights because those tend to be more evenly distributed across a city by design.

```{r exploratory_analysis_3, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }

### --- Nearest Neighborhood of Risk Factors 

st_c <- st_coordinates
st_coid <- st_centroid

vars_net <-
  vars_net %>%
  dplyr::mutate(
    Code_Violations.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(codeViolations2017.sf),3),
    Police_Incidents.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(policeInxidents2017.sf),3),
    Street_Lights.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(streetLights),3),
    Unsheltered_People.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(unshelteredPeople2017.sf),3),
   MentalHealth.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(MentalHealth.sf),3),
    Insurance.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(Insurance.sf),3))


vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

vars <- unique(vars_net.long.nn$Variable)

mapList <- list()
for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), 
            aes(fill=value),
            colour=NA) +
    scale_fill_viridis_c(option = "C") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol = 3, top = "Nearest Neighbor Risk Factors by Fishnet"))

```

## Exploring Spatial Process

The spatial process, or observed relationship within space, that we are seeing in our exploratory maps of overdoses and risk factor events is clustering.
This spatial process is important to highlight because it suggests that there may be relationships between overdose events that happen, as well as relationships between overdose events and our selected risk factors.
With that said, how do we decipher where there are relationships between events as opposed to what might just be clustered by chance? 
 

To understand where clusters of overdoses are statistically significant in Mesa, we calculated and mapped the Local Moran's I values and p-values for overdoses across space.
Local Moran's I is a statistical calculation used to identify local clusters of events.
We used this statistic to map where statistically significant overdose hotspots are located in the City of Mesa.
We can see that statistically significant clusters of overdoses are largely found on the west side of the City of Mesa. 
 

```{r exploratory_analysis_5, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }

### --- Exploring Statistical Significance  

fishnet$id <- as.character(fishnet$id)
vars_net$id <- as.character(vars_net$id)

final_net <-
  left_join(overdoses2017_net, st_drop_geometry(vars_net), by="id") 

final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(CouncilDistricts, districts)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, id)) %>%
  st_sf() %>%
  na.omit()

final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(final_net$countOverdose2017, final_net.weights, zero.policy = TRUE)),
    as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Overdose_Count = countOverdose2017, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.05, 1, 0)) %>%
  gather(Variable, Value, -geometry)

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis_c(option = "C") +
    labs(title=i) +
    mapTheme() + theme(legend.position="bottom")}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Overdoses"))

```

To understand the relationship between locations of overdoses and the locations of each risk factor, created correlation plots.
The closer r is to 1, the more strongly correlated the risk factor is to overdose events.
In Mesa, with the data used for this analysis, we can see that drug crimes are the most related to overdoses relative to the other risk factors. 
 

```{r exploratory_analysis_4, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 15, fig.width= 20}

final_net <-
  final_net %>% 
  mutate(overdose.isSig = 
           ifelse(localmoran(final_net$countOverdose2017, 
                             final_net.weights, zero.policy = TRUE)[,5] <= 0.0000001, 1, 0)) %>%
  mutate(overdose.isSig.dist = 
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(
                         filter(final_net, overdose.isSig == 1))), 1))


### --- Correlation Plots 

## Make table of variables and correlation and/or consider interactive plots 

correlation.long <-
  st_drop_geometry(final_net) %>%
  dplyr::select(-id, -cvID, -districts) %>%
  gather(Variable, Value, -countOverdose2017)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, countOverdose2017, use = "complete.obs"))

ggplot(correlation.long, aes(Value, countOverdose2017)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Overdose count as a Function of risk factors") +
  plotTheme()

```

# Predicting Future Overdoses

## Building a Model 

Because we are dealing with counts of overdose events as opposed to continuous or binary data, we are using a Poisson regression to make a model of predicted overdoses.
Our model uses the risk factors shown above to predict where future overdose events may be likely to occur. 
 

```{r model, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE}

### ---- Model variables

reg.vars <- c("Code_Violations.nn", "Police_Incidents.nn", "Street_Lights.nn", 
              "Insurance.nn", "MentalHealth.nn", "Unsheltered_People.nn")

reg.ss.vars <- c("Code_Violations.nn", "Police_Incidents.nn", "Street_Lights.nn", 
                 "Insurance.nn", "MentalHealth.nn", "Unsheltered_People.nn", 
                 "overdose.isSig", "overdose.isSig.dist")

```

## Cross Validation

It is crucial that our model is able to make accurate predictions and remain accurate when exposed to new data.
To test this, we used two methods to cross-validate our model: k-fold cross-validation and leave-one-group-out cross-validation (LOGO-CV).
K-fold cross validation involves splitting the data into k groups, training the model on k-1 groups (or folds), and testing how well the model predicts on the fold that was left out.
This process is repeated so that each fold is used as a test set.
The LOGO-CV works similarly, however the groups are not random but are instead defined by spatial boundaries.
We used city council district boundaries for our analysis.
The results of our cross-validations are below. 

```{r cross_validation, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }

### --- Cross Validation  

reg.cv <- crossValidate1(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countOverdose2017",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = cvID, countOverdose2017, Prediction, geometry)

reg.ss.cv <- crossValidate1(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countOverdose2017",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = cvID, countOverdose2017, Prediction, geometry)

reg.spatialCV <- crossValidate2(
  dataset = final_net,
  id = "districts",
  dependentVariable = "countOverdose2017",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = districts, countOverdose2017, Prediction, geometry)

reg.ss.spatialCV <- crossValidate2(
  dataset = final_net,
  id = "districts",
  dependentVariable = "countOverdose2017",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = districts, countOverdose2017, Prediction, geometry)


## Accuracy & Generalizability 

reg.summary <- 
  rbind(
    mutate(reg.cv,           Error = Prediction - countOverdose2017,
           Regression = "Random k-fold CV: Just Risk Factors"),
    mutate(reg.ss.cv,        Error = Prediction - countOverdose2017,
           Regression = "Random k-fold CV: Spatial Process"),
     mutate(reg.spatialCV,    Error = Prediction - countOverdose2017,
           Regression = "Spatial LOGO-CV: Just Risk Factors"),
    mutate(reg.ss.spatialCV, Error = Prediction - countOverdose2017,
           Regression = "Spatial LOGO-CV: Spatial Process")) %>%
  st_sf() 


error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countOverdose2017, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()

error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
  geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
  facet_wrap(~Regression) +  
  geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Distribution of MAE", subtitle = "k-fold cross validation vs. LOGO-CV",
       x="Mean Absolute Error", y="Count") +
  plotTheme()


st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
  summarize(Mean_MAE = round(mean(MAE), 2),
            SD_MAE = round(sd(MAE), 2)) %>%
  kable() %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(2, color = "black", background = "#FDE725FF") %>%
  row_spec(4, color = "black", background = "#FDE725FF") 


error_by_reg_and_fold %>%
  filter(str_detect(Regression, "LOGO")) %>%
  ggplot() +
  geom_sf(aes(fill = MAE)) +
  facet_wrap(~Regression) +
  scale_fill_viridis() +
  labs(title = "Overdose errors by LOGO-CV Regression") +
  mapTheme() + theme(legend.position="bottom")


neighborhood.weights <-
  filter(error_by_reg_and_fold, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  group_by(cvID) %>%
  poly2nb(as_Spatial(.), queen=TRUE) %>%
  nb2listw(., style="W", zero.policy=TRUE)

filter(error_by_reg_and_fold, str_detect(Regression, "LOGO"))  %>% 
  st_drop_geometry() %>%
  group_by(Regression) %>%
  summarize(Morans_I = moran.mc(abs(Mean_Error), neighborhood.weights, 
                                nsim = 99, zero.policy = TRUE, 
                                na.action=na.omit)[[1]],
            p_value = moran.mc(abs(Mean_Error), neighborhood.weights, 
                               nsim = 99, zero.policy = TRUE, 
                               na.action=na.omit)[[3]])

```

## Predictive Power

Our model of predicted 2018 overdoses was compared to actual 2018 overdoses to get a sense of how well it is able to help the City of Mesa proactively determine future overdose events.
The results are shown below.
As we can see, the spatial comparison of our model to actual overdose events captures the highest-risk city hotspots.
This is further validated through the bar chart which shows that, although our model is not perfect due to the imperfect availability of relevant data, it does capture almost just as great of a share of overdose events as the kernel density map in the highest risk category.

```{r additional_analysis1, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }

overdose_ppp <- as.ppp(st_coordinates(MesaOverdoses2017), W = st_bbox(final_net))
overdose_KD <- density.ppp(overdose_ppp, 1000)

as.data.frame(overdose_KD) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  ggplot() +
  geom_sf(aes(fill=value)) +
  geom_sf(data = sample_n(MesaOverdoses2017, 1500, replace=TRUE), size = .5) +
  scale_fill_viridis(name = "Density") +
  labs(title = "Kernel Density of 2017 Overdoses") +
  mapTheme()

MesaOverdoses2018 <- MesaOverdoses %>%
  dplyr::filter(year == "2018") %>%
  .[fishnet,]


overdose_KDE_sf <- as.data.frame(overdose_KD) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  mutate(label = "Kernel Density",
         Risk_Category = ntile(value, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(MesaOverdoses2018) %>% mutate(overdoseCount2018 = 1), ., sum)%>%
      mutate(overdoseCount2018 = replace_na(overdoseCount2018, 0))) %>%
  dplyr::select(label, Risk_Category, overdoseCount2018)


overdose_risk_sf <-
  filter(reg.summary, Regression == "Spatial LOGO-CV: Spatial Process") %>%
  mutate(label = "Risk Predictions",
         Risk_Category = ntile(Prediction, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(MesaOverdoses2018) %>% mutate(overdoseCount2018 = 1), ., sum) %>%
      mutate(overdoseCount2018 = replace_na(overdoseCount2018, 0))) %>%
  dplyr::select(label,Risk_Category, overdoseCount2018)

```

```{r additional_analysis2, class.output="scroll-100", warning = FALSE, message = FALSE, results = FALSE, fig.height= 6, fig.width= 12 }


rbind(overdose_KDE_sf, overdose_risk_sf) %>%
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill = Risk_Category), colour = NA) +
  geom_sf(data = sample_n(MesaOverdoses2018, 3000, replace=TRUE), size = .5, colour = "black") +
  facet_wrap(~label, ) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Comparison of Overdose Kernel Density and Risk Predictions",
       subtitle="2018 overdoses compared to 2017 risk predictions") +
  mapTheme()



rbind(overdose_KDE_sf, overdose_risk_sf) %>%
  st_set_geometry(NULL) %>% na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category) %>%
  group_by(label, Risk_Category) %>%
  summarize(MesaOverdoses2018 = sum(Value)) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(Rate_of_test_set_crimes = MesaOverdoses2018 / sum(MesaOverdoses2018)) %>%
  ggplot(aes(Risk_Category,Rate_of_test_set_crimes)) +
  geom_bar(aes(fill=label), position="dodge", stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Risk prediction vs. Kernel density, 2018 overdoses") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


```

# Siting Hope with the Greatest Impact

Overdose Protection Sites (OPS) are an under-utilized strategy of combatting the opioid epidemic.
There are currently 100 OPS sites worldwide operating in 10 countries and 67 cities.
As of 2018, no fatal overdose has been documented in any OPS in the world.
Their outcomes have proven to improve community health, public safety, and reduce health consequences from injection drug use.
These facilities offer medically supervised injection use and hygienic spaces where people can access sterile equipment.
Medically trained staff are on site at all times, ready to respond to any overdose situation, but these staff do not intervene with injections.
OPS sites also offer on-site health, mental health, substance use, and social services supplementing the current harm reduction strategies, curating increased opportunities for engagement, education, and treatment.
The years of scientific evidence through evaluations of existing facilities prove the safety and effectiveness of OPS.

## Site Selection

The map below provides an example of what City of Mesa public health officials would be able to view on the Siting Hope dashboard.
Public health officials looking to site new resources, with a particular focus on OPS sites, by proactively analyzing the number of people who may be impacted by a particular site selection within different radii.

```{r  additional_analysis3, class.output="scroll-100", warning = FALSE, message = FALSE, fig.height= 6, fig.width= 12 }

sample_site <- data.frame(lat= c(-111.83920989923757), lon= c(33.41080887865899)) %>%
  st_as_sf(coords = c("lat", "lon"), crs=4326) %>%
  st_transform("EPSG:26912")

sample_buffer1 <- st_buffer(sample_site, 500)
sample_buffer2 <- st_buffer(sample_site, 1000)
sample_buffer3 <- st_buffer(sample_site, 1500)

Impact_Buffer1 <- overdose_risk_sf %>% 
  st_centroid() %>%
  dplyr::select(overdoseCount2018) %>%
  aggregate(., sample_buffer1, sum)

Impact_Buffer2 <- overdose_risk_sf %>% 
  st_centroid() %>%
  dplyr::select(overdoseCount2018) %>%
  aggregate(., sample_buffer2, sum)

Impact_Buffer3 <- overdose_risk_sf %>% 
  st_centroid() %>%
  dplyr::select(overdoseCount2018) %>%
  aggregate(., sample_buffer3, sum)

tmap_mode("view")

  tm_shape(Impact_Buffer3)+
    tm_fill(col = NA, alpha = 0) +
    tm_polygons("overdoseCount2018", col = NA, border.col = "red") +
    tm_fill(col = NA) +
  tm_shape(Impact_Buffer2)+
    tm_fill(col = NA, alpha = 0) +
    tm_polygons("overdoseCount2018", col = NA, border.col = "red") +
    tm_fill(col = NA) +
  tm_shape(Impact_Buffer1)+
    tm_fill(col = NA, alpha = 0) +
    tm_polygons("overdoseCount2018", col = NA, border.col = "red") +
    tm_fill(col = NA)
    


```

## Cost/Benefit Analysis

Finally, to help public health officials make decisions that not only impact a large share of the population that is at risk of overdosing but also makes financial sense for the city, Siting Hope also provides a cost/benefit analysis based on the latest SIF/OPS research and the number of people a certain site is projected to help.
In the example below, we acknowledge that there are certainly much broader costs to factor into the cost benefit analysis, such as construction costs, maintenance, and social costs (lost productivity, loss of quality of life, criminal justice, etc.) For the sake of simplicity we focused on the main overdose costs the Maricopa Health Department is pays including ambulances, ED Visits, and Inpatient Discharge.

```{r CBA, class.output="scroll-100", warning = FALSE, message = FALSE, fig.height= 6, fig.width= 12 }

read.csv("CBA.csv") %>%
  kable() %>%
  kable_styling()

```

## Limitations and Conclusion

Because we used a fishnet in our analysis, we needed to work with risk factor data that was granular enough to show differences between the different grid cells.
This limited the data we could use, especially given that we are dealing with data that is related to peoples' health.
Health data is often protected because it can be personally identifiable or protected by HIPPA.
If it is available, it is often heavily aggregated to meet confidentiality standards.
Many of the interesting datasets we felt could be strongly associated with overdoses were not granular enough for use, were not public, or did not have location data at all.
Despite these limitations, our analysis does meet several needs for the City of Mesa: a method for proactively predicting overdoses based on risk factors that are correlated with overdosing, a tool to integrate predictive analysis of overdoses into the site selection process when planning safe injection sites, and a tool to calculate the costs and benefits of their final intervention strategy.
Finally, this data will also be used to help connect bystanders and those at risk of overdosing with compassionate care and the locations of future safe injection sites and other resources.
By providing the capabilities to the City of Mesa, we are able to fulfill Siting Hope's mission to understand, contextualize, and site places for hope in solidarity with those fighting to end the opioid epidemic. 
 
