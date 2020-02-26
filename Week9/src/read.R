#'  Create TidyTuesday (Week 9) Visualization
#'
#'  This program takes the WSJ school based measles data, summarizes it at the
#'  county level, and then creates a U.S. cloropleth map.  We create 4 seperate
#'  maps and then combine them into one png
#'  
#'  Measles data from https://github.com/WSJ/measles-data
#'  Shapefile from https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip
#'  State FIPS from https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
#'  

library(tidyverse)
library(sf)
library(stringr)




# Get Data and shapefile --------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/all-measles-rates.csv",
               col_types=cols(district = col_character(),
                              xrel=col_double()))

#download, unzip, and read shape file
dir.create("Week9/data/cb_2018_us_county_20m", showWarnings = F )
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip", "Week9/data/cb_2018_us_county_20m/shape.zip" )
unzip("Week9/data/cb_2018_us_county_20m/shape.zip", exdir="Week9/data/cb_2018_us_county_20m")
shp <- read_sf("Week9/data/cb_2018_us_county_20m/")

stateFIPS <- read_csv("Week9/data/stateFIPS.csv")
# Transform data into County format ---------------------------------------

# clean overall and mmr which have -1 values
removeNeg1 <- function(x) {
  ifelse(x==-1,NA,x)
}
df <- df %>%
  mutate_at(vars(overall, mmr),removeNeg1)

# Change percents to counts;
df <- df %>%
  mutate(mmr_c=mmr/100*enroll,
         overall_c=overall/100*enroll,
         xrel_c=xrel/100*enroll,
         xmed_c=xmed/100*enroll,
         xper_c=xper/100*enroll)
#This gives some cooky numbers.  Something is wrong with some of the percents or enrollment figures

zeroToNA <- function(x){
  ifelse(x==0,NA,x)
}
df.county <- df %>%
  group_by(state, county) %>%
  summarize(enroll=sum(enroll, na.rm=T),
            overall_c=sum(overall_c, na.rm=T),
            mmr_c=sum(mmr_c, na.rm=T),
            xrel_c=sum(xrel_c, na.rm=T),
            xmed_c=sum(xmed_c, na.rm=T),
            xper_c=sum(xper_c, na.rm=T)) %>%
  mutate_at(vars(overall_c,mmr_c,xrel_c,xmed_c,xper_c), zeroToNA) %>%
  mutate(overall=overall_c/enroll*100,
         mmr=mmr_c/enroll*100,
         xrel=xrel_c/enroll*100,
         xmed=xmed_c/enroll*100,
         xper=xper_c/enroll*100,
  )

#clean and match States

df.county <- inner_join(df.county, stateFIPS, by=c("state" = "Name" )) %>%
  mutate(STATEFP=str_pad(Numeric_code, 2, pad="0"))


#clean our missing County names
df.county <- df.county[!is.na(df.county$county),]

#make manual corrections. 
df.county[df.county$state=="Illinois" & df.county$county=="Dupage","county"] <-"DuPage"
df.county[df.county$state=="Illinois" & df.county$county=="Dekalb","county"] <-"DeKalb"
df.county[df.county$state=="Illinois" & df.county$county=="Dewitt","county"] <-"De Witt"
df.county[df.county$state=="Illinois" & df.county$county=="La Salle","county"] <-"LaSalle"
df.county[df.county$state=="Illinois" & df.county$county=="Saint Clair","county"] <-"St. Clair"

df.c2 <- right_join(df.county, shp,by=c("STATEFP" = "STATEFP",
                                        "county" = "NAME"))


# Create Illinois dataset -------------------------------------------------


df.illinois <- filter(df.c2, STATEFP=="17")
dftest <- df.illinois[df.illinois$mmr>95.1,]
hist(dftest$mmr)
df.illinois$mmrclass <- cut(df.illinois$mmr, c(0,94,96, 98,100))
df.county
table(df.illinois$mmrclass)
hist(df.illinois$mmr)

ggplot(df.illinois)+
  geom_sf(aes(geometry=geometry,fill=mmrclass)) +
  coord_sf(crs=st_crs(3857))+
  ggthemes::theme_map()+
labs(caption = "data: https://github.com/WSJ/measles-data\ncode: Pauloo, et al. 2017)")
ggsave("Week9/output/tidy_tues_week9.png")
