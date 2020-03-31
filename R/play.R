#### Setup ####

rm(list=ls())


#### Packages ####

list.of.packages <- c(
  "readr"
  ,"lubridate"
  ,"stringr"
  ,"dplyr"
)

# identify required packages that are not already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install required packages that are not already installed
if(length(new.packages)) install.packages(new.packages,type="binary")
# load the packages
lapply(list.of.packages, require, character.only = TRUE)


# #### Data ####
# 
# # download data from JHU Github site
# urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-28-2020.csv"
# tmp <- read_csv(url(urlfile))
# 
# ohio <- tmp[which(tmp$Province_State=="Ohio"),]
# View(ohio)



#### All DATA ####

# discover all the files in COVID-19 daily reports folder on Github - source local clone 
files.raw <- Sys.glob("../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/*.csv")
# create a list to hold the data
files <- list()
# populate the list with and element for each imported daily update as tibble 
for (i in 1:length(files.raw)) {
  file.date <- strsplit(basename(files.raw[i]),"\\.")[[1]][1]
  files[[file.date]] = read_csv(files.raw[i])
}
# have a look at the data
# files # uncomment
# have a look at the fields in each daily update
lapply(files,colnames)
## IMPORTANT
# there are 3 different file structures
# 1. 01/22 -> 02/29: Province/State, Country/Region, Last Update, Confirmed, Deaths, Recovered 
# 2. 03/01 -> 03/21: Province/State, Country/Region, Last Update, Confirmed, Deaths, Recovered, Latitude, Longitude
# 3. 03/22 ->      : FIPS, Admin2, Province_State, Country_Region, Last_Update, Lat, Long_, Confirmed, Deaths, Recovered, Active, Combined_Key 
# have a look at an example of each
head(files$`01-22-2020`) # type 1
head(files$`03-01-2020`) # type 2
head(files$`03-22-2020`) # type 3
# define the list element number when each new structure starts
start.str1 <- which(names(files)=='01-22-2020')
start.str2 <- which(names(files)=='03-01-2020')
start.str3 <- which(names(files)=='03-22-2020')
## IMPORTANT
# have a look at first row of each tibble to see what the date formats are
for (i in 1:length(files)) {
  print(head(files[[i]],1))
}
# there are, inexplicably, two date formats
# 1. -m/-d/yyyy hh:mm:       01/22 -> 02/01
# 2. yyyy/mm/dd hh:mm:ss:    02/02 -> present
# define the list element number when each new date format starts
start.date1 <- which(names(files)=='01-22-2020')
start.date2 <- which(names(files)=='02-02-2020')
# create an actual date for the first batch
files.tmp1 <- lapply(files[(start.date1:(start.date2-1))],function(x) {
  std.date <- mdy_hm(x$`Last Update`)
  cbind(x,std.date)
})
# create an actual date for the second batch
# now for the second batch, the field name for "Last Update" is either 
#   "Last Update" or "Last_Update", starting with the last structure
# process the "Last Update" batch
files.tmp2 <- lapply(files[(start.date2:(start.str3-1))],function(x) {
  std.date <- ymd_hms(x$`Last Update`)
  cbind(x,std.date)
})
# process the "Last_Update" batch
files.tmp3 <- lapply(files[(start.str3:length(files))],function(x) {
  std.date <- ymd_hms(x$Last_Update)
  cbind(x,std.date)
})
# put them back together into a temporary list
files.tmp <- c(files.tmp1,files.tmp2,files.tmp3)
# and guess what, the first update in type 3 structure goes back to a non standard data format, just this one!
files.tmp[[start.str3]]$std.date <- mdy_hm(files.tmp[[start.str3]]$Last_Update)

# rename the columns we want to extract using standard names
head(files.tmp$`01-22-2020`) # type 1 structure
files.tmp1.1 <- lapply(files.tmp[(start.str1:(start.str2-1))],setNames, c('admin1','country','raw.date','cases','deaths','recovered','std.date'))
# convert to standard types
files.tmp1.1 <- lapply(files.tmp1.1,function(x) {
  transform(x, admin1 = as.character(admin1),
            country = as.character(country),
            raw.date = as.character(raw.date),
            cases = as.numeric(cases),
            deaths = as.numeric(deaths),
            recovered = as.numeric(recovered))
})

head(files.tmp$`03-01-2020`) # type 2 structure
files.tmp2.1 <- lapply(files.tmp[(start.str2:(start.str3-1))],setNames, c('admin1','country','raw.date','cases','deaths','recovered','lat','lng','std.date'))
# convert to standard types
files.tmp2.1 <- lapply(files.tmp2.1,function(x) {
  transform(x, admin1 = as.character(admin1),
            country = as.character(country),
            raw.date = as.character(raw.date),
            cases = as.numeric(cases),
            deaths = as.numeric(deaths),
            recovered = as.numeric(recovered),
            lat = as.numeric(lat),
            lng = as.numeric(lng))
})

head(files.tmp$`03-22-2020`) # type 3 structure
files.tmp3.1 <- lapply(files.tmp[(start.str3:length(files.tmp))],setNames, c('fips','admin2','admin1','country','raw.date','lat','lng','cases','deaths','recovered','active','key','std.date'))
# convert to standard types
files.tmp3.1 <- lapply(files.tmp3.1,function(x) {
  transform(x, fips = as.numeric(fips),
            admin2 = as.character(admin2),
            admin1 = as.character(admin1),
            country = as.character(country),
            raw.date = as.character(raw.date),
            lat = as.numeric(lat),
            lng = as.numeric(lng),
            cases = as.numeric(cases),
            deaths = as.numeric(deaths),
            recovered = as.numeric(recovered),
            active = as.numeric(active),
            key = as.character(key))
})

# put them back together into a temporary list
files.tmp.1 <- c(files.tmp1.1,files.tmp2.1,files.tmp3.1)
length(files.tmp.1)

# union all of them into a nice long data frame with the same fields and a STANDARD date column
data.final <- files.tmp.1[[length(files.tmp.1)]]
for (i in (length(files.tmp.1)-1):1) {
  data.final <- bind_rows(data.final,files.tmp.1[[i]])
}

str(data.final)
# order by country
data.final.sorted <- data.final[order(data.final$country,data.final$admin1,data.final$std.date),]
View(data.final.sorted) # ta-da!
# look for duplicates - there are many
# duplicated(data.final.sorted) # uncomment
# see how many rows there are in the final dataset
rows.before <- dim(data.final.sorted)[1]
# take unique values across all fields
data <- unique(data.final.sorted)
# see how many rows there are now
rows.after <- dim(data)[1]
print(paste((rows.before-rows.after),'rows were deleted as duplicates',sep=" "))
# View(data)

# identify duplicates except for lat/lng
dup.latlng <- which(duplicated(data[,c('fips','admin2','admin1','country','raw.date','cases','deaths','recovered','active','key','std.date')])==TRUE)
# the preceeding record for the duplicates, i.e. the other member of the duplicated pair
dup.latlng.pair <- dup.latlng-1
# an index of the duplicated pairs (still sorted)
dup.latlng.all <- sort(c(dup.latlng,dup.latlng.pair))
# have a look at the duplicates
View(cbind(dup.latlng.all,data[dup.latlng.all,]))
# after reviewing manually, safe to delete the second row in each pair of duplicates
rows.before <- dim(data)[1]
data.clean.0 <- data[!(seq(1,dim(data)[1],1) %in% dup.latlng),]
rows.after <- dim(data.clean.0)[1]
print(paste((rows.before-rows.after),'rows were deleted as duplicates',sep=" "))
View(data.clean.0)

# now there are duplicates within the same day: take the latest one
rows.before <- dim(data.clean.0)[1]
data.clean.1 <- 
  data.clean.0 %>% 
  group_by(country,admin1,admin2,date=date(std.date)) %>% 
  arrange(std.date) %>%  
  slice(n())
View(data.clean.1)
rows.after <- dim(data.clean.1)[1]
print(paste((rows.before-rows.after),'rows were deleted as duplicates',sep=" "))
# check that there are no more duplicates for each unique country, admin1, admin2, date
tmp <- 
  data.clean.1 %>% 
    mutate(date = date(std.date)) %>%
    group_by(country,admin1,admin2,date) %>% 
    tally()
nrow(tmp[which(tmp$n>1),])
View(data.clean.1)

# now tally up states totals for the US
tmp <- data.clean.1 %>%
  filter(country=="US") %>%
  group_by(admin1,date) %>% 
  summarize(cases.tot = sum(cases),
            deaths.tot = sum(deaths))
View(tmp)
