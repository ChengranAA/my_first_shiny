# <u>**Exploration**</u>

<<<<<<< HEAD
The  *covid_full_by_country_pop* was mapped using the `joinCountryData2Map()` function from the `library(rworldmap)` by ISO3 code. However, due to the limitation of the map data, 7 countries did not mapped to the data. The following code was used to examined the countries did not mapped to the world map. The population is displayed as millions of. The infected and death percentage is displayed as percenatge of (%). 
=======
The  *covid_full_by_country_pop* was mapped using the `joinCountryData2Map()` function from the `library(rworldmap)` by ISO3 code. However, due to the limitation of the map data, 7 countries did not mapped to the data. The following code was used to examined the countries did not mapped to the world map. The population is displayed ad millions of. The infected and death percentage is displayed as percenatge of (%). 
>>>>>>> 3337f3c31a22b160db6c9f19bcc0912cb7eb02e1

```{R}
setdiff(covid_full_by_country_pop$COUNTRY_ALPHA_3_CODE, joinData@data[["ISO3"]])
```

The seven countries are "BES", "BUR", "GIB", "GLP", "MTQ", "MYT", "REU". 

