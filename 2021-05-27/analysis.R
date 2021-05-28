library(data.table)


# Get current time for later use
currenttime <- paste("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " (Pacific Time)", sep="")

# Read data from https://data.acgov.org/datasets/
# Also save a copy

byplace <- fread("../2020-08-05/f6195a2de0b0440c99ceb9290fa95316_0.csv")

download.file("https://opendata.arcgis.com/datasets/5d6bf4760af64db48b6d053e7569a47b_6.csv", "COVID-19_Statistics.csv")
cumulative_byplace <- fread("COVID-19_Statistics.csv", na.strings=c(""))

# download.file("https://github.com/datadesk/california-coronavirus-data/raw/master/latimes-place-totals.csv", "latimes-place-totals.csv")

latimes_byplace <- fread("https://github.com/datadesk/california-coronavirus-data/raw/master/latimes-place-totals.csv", na.strings=c(""))







############################
# Data wrangling / summary #
############################


### Alameda county side ###

# Change "DtCreate" to a date vector
cumulative_byplace[, record_date := as.Date(dtcreate)]

# Sorry - only interested in Trivalley for now
# Subset dates and Trivalley data
# Sort by date
trivalley <- cumulative_byplace[, list(record_date, Dublin, Pleasanton, Livermore)]
setorder(trivalley, record_date)

# remove extra rows, with only NAs
trivalley <- trivalley[!(is.na(Dublin) & is.na(Pleasanton) & is.na(Livermore))]



# Data has both "0" cases and "<10" cases
# For ease of plotting, change them to NAs

remove_tens <- function(x){
	gsub("^0|<10", NA, x)
}

trivalley <- trivalley[, lapply(.SD, remove_tens)]


# Rename columns for clarity, fix classes
names(trivalley) <- c("record_date", "cumul_dublin", "cumul_pleasanton", "cumul_livermore")
trivalley[, record_date := as.Date(record_date)]
trivalley[, cumul_dublin := as.integer(cumul_dublin)]
trivalley[, cumul_pleasanton := as.integer(cumul_pleasanton)]
trivalley[, cumul_livermore := as.integer(cumul_livermore)]





### Contra Costa county side ###

# subset
trivalley2 <- latimes_byplace[name %in% c("San Ramon", "Danville", "Alamo", "Diablo")]

# dcast, clean up
trivalley2 <- dcast(trivalley2, date~name, value.var="confirmed_cases")

# rename columns for clarity
names(trivalley2) <- c("record_date", "cumul_alamo", "cumul_danville", "cumul_diablo", "cumul_sanramon")

# clean up Diablo, as it seems suspicious to have 0 jumping to 13 in a day
trivalley2$cumul_diablo <- gsub(0, NA, trivalley2$cumul_diablo)

# fix class
trivalley2[, record_date := as.Date(record_date)]
trivalley2[, cumul_alamo := as.integer(cumul_alamo)]
trivalley2[, cumul_danville := as.integer(cumul_danville)]
trivalley2[, cumul_diablo := as.integer(cumul_diablo)]
trivalley2[, cumul_sanramon := as.integer(cumul_sanramon)]




### Combine Alameda and Contra Costa County ###
trivalley <- merge(x=trivalley, y=trivalley2, by="record_date", all=T)


# get the earliest and latest dates in dataset
dayEarliest <- min(trivalley$record_date, na.rm=T)
dayLatest <- max(trivalley$record_date, na.rm=T)







# Calculate day change
trivalley[, day_dublin := c(NA, diff(cumul_dublin))]
trivalley[, day_pleasanton := c(NA, diff(cumul_pleasanton))]
trivalley[, day_livermore := c(NA, diff(cumul_livermore))]
trivalley[, day_alamo := c(NA, diff(cumul_alamo))]
trivalley[, day_danville := c(NA, diff(cumul_danville))]
trivalley[, day_diablo := c(NA, diff(cumul_diablo))]
trivalley[, day_sanramon := c(NA, diff(cumul_sanramon))]


# Calculate (smoothed) 7 day average for the day change
# feed in a vector, returns a vector
seven_day_mean <- function(x){

	# initialize
	placeholder <- NULL
	
	# loop through each row
	vectorlength <- length(x)
	for (i in 1:vectorlength){

		# for first 6 rows, calculate the mean from row 1 to current row
		if (i %in% 1:6){
			targetmean <- mean(x[1:i], na.rm=T)
		} 
		
		# otherwise, calculate mean from current-6th row to current row
		else {
			targetmean <- mean(x[(i-6):i], na.rm=T)
		} 
		
		placeholder <- c(placeholder, round(targetmean, 2))

	}

	# return the resulted vector	
	return(placeholder)
}



trivalley$day_7mean_dublin <- seven_day_mean(trivalley$day_dublin)
trivalley$day_7mean_pleasanton <- seven_day_mean(trivalley$day_pleasanton)
trivalley$day_7mean_livermore <- seven_day_mean(trivalley$day_livermore)
trivalley$day_7mean_alamo <- seven_day_mean(trivalley$day_alamo)
trivalley$day_7mean_danville <- seven_day_mean(trivalley$day_danville)
trivalley$day_7mean_diablo <- seven_day_mean(trivalley$day_diablo)
trivalley$day_7mean_sanramon <- seven_day_mean(trivalley$day_sanramon)




# Get population numbers as well
pop_dublin <- byplace[Geography=="Dublin"]$Population
pop_pleasanton <- byplace[Geography=="Pleasanton"]$Population
pop_livermore <- byplace[Geography=="Livermore"]$Population
pop_alamo <- unique(latimes_byplace[name=="Alamo"]$population)
pop_diablo <- unique(latimes_byplace[name=="Diablo"]$population)
pop_danville <- unique(latimes_byplace[name=="Danville"]$population)
pop_sanramon <- unique(latimes_byplace[name=="San Ramon"]$population)



# Get cumulative per 100k rate
trivalley[, cumul_per100k_dublin := round(cumul_dublin / pop_dublin * 100000, 2)]
trivalley[, cumul_per100k_pleasanton := round(cumul_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, cumul_per100k_livermore := round(cumul_livermore / pop_livermore * 100000, 2)]
trivalley[, cumul_per100k_alamo := round(cumul_alamo / pop_alamo * 100000, 2)]
trivalley[, cumul_per100k_diablo := round(cumul_diablo / pop_diablo * 100000, 2)]
trivalley[, cumul_per100k_danville := round(cumul_danville / pop_danville * 100000, 2)]
trivalley[, cumul_per100k_sanramon := round(cumul_sanramon / pop_sanramon * 100000, 2)]



# Calculate day changes per 100k rates
trivalley[, day_per100k_dublin := round(day_dublin / pop_dublin * 100000, 2)]
trivalley[, day_per100k_pleasanton := round(day_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, day_per100k_livermore := round(day_livermore / pop_livermore * 100000, 2)]
trivalley[, day_per100k_alamo := round(day_alamo / pop_alamo * 100000, 2)]
trivalley[, day_per100k_diablo := round(day_diablo / pop_diablo * 100000, 2)]
trivalley[, day_per100k_danville := round(day_danville / pop_danville * 100000, 2)]
trivalley[, day_per100k_sanramon := round(day_sanramon / pop_sanramon * 100000, 2)]


trivalley[, day_7mean_per100k_dublin := round(day_7mean_dublin / pop_dublin * 100000, 2)]
trivalley[, day_7mean_per100k_pleasanton := round(day_7mean_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, day_7mean_per100k_livermore := round(day_7mean_livermore / pop_livermore * 100000, 2)]
trivalley[, day_7mean_per100k_alamo := round(day_7mean_alamo / pop_alamo * 100000, 2)]
trivalley[, day_7mean_per100k_diablo := round(day_7mean_diablo / pop_diablo * 100000, 2)]
trivalley[, day_7mean_per100k_danville := round(day_7mean_danville / pop_danville * 100000, 2)]
trivalley[, day_7mean_per100k_sanramon := round(day_7mean_sanramon / pop_sanramon * 100000, 2)]





##############
# Make plots #
##############

library(plotly)

# cumulative
fig_cumul <- plot_ly() %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_dublin, name="Dublin", line=list(color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_pleasanton, name="Pleasanton", line=list(color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_livermore, name="Livermore", line=list(color="blue")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_alamo, name="Alamo", line=list(color="#CC79A7")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_danville, name="Danville", line=list(color="black")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_diablo, name="Diablo", line=list(color="#F0E442")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_sanramon, name="San Ramon", line=list(color="#56B4E9")) %>%
	layout(title="Cumulative # of COVID-19 cases",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))

# cumulative per 100K
fig_cumul_100K <- plot_ly(mode="lines") %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_dublin, name="Dublin", line=list(color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_pleasanton, name="Pleasanton", line=list(color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_livermore, name="Livermore", line=list(color="blue")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_alamo, name="Alamo", line=list(color="#CC79A7")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_danville, name="Danville", line=list(color="black")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_diablo, name="Diablo", line=list(color="#F0E442")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_sanramon, name="San Ramon", line=list(color="#56B4E9")) %>%
	layout(title="Cumulative # of COVID-19 cases, per 100K population",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))


# Day changes
fig_day <- plot_ly() %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_dublin, name="Dublin (raw)", marker=list(opacity=0.2, color="green")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_pleasanton, name="Pleasanton (raw)", marker=list(opacity=0.2, color="orange")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_livermore, name="Livermore (raw)", marker=list(opacity=0.2, color="blue")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_alamo, name="Alamo (raw)", marker=list(opacity=0.2, color="#CC79A7")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_danville, name="Danville (raw)", marker=list(opacity=0.2, color="black")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_diablo, name="Diablo (raw)", marker=list(opacity=0.2, color="#F0E442")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_sanramon, name="San Ramon (raw)", marker=list(opacity=0.2, color="#56B4E9")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_dublin, name="Dublin (7 day ave.)", line=list(shape="spline", color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_pleasanton, name="Pleasanton (7 day ave.)", line=list(shape="spline", color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_livermore, name="Livermore (7 day ave.)", line=list(shape="spline", color="blue")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_alamo, name="Alamo (7 day ave.)", line=list(shape="spline", color="#CC79A7")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_danville, name="Danville (7 day ave.)", line=list(shape="spline", color="black")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_diablo, name="Diablo (7 day ave.)", line=list(shape="spline", color="#F0E442")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_sanramon, name="San Ramon (7 day ave.)", line=list(shape="spline", color="#56B4E9")) %>%
	layout(title="# of new COVID-19 cases by day\nDots = raw numbers, lines = 7 day average",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))

# Day changes, per 100K
fig_day_100K <- plot_ly() %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_dublin, name="Dublin (raw)", marker=list(opacity=0.2, color="green")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_pleasanton, name="Pleasanton (raw)", marker=list(opacity=0.2, color="orange")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_livermore, name="Livermore (raw)", marker=list(opacity=0.2, color="blue")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_alamo, name="Alamo (raw)", marker=list(opacity=0.2, color="#CC79A7")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_danville, name="Danville (raw)", marker=list(opacity=0.2, color="black")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_diablo, name="Diablo (raw)", marker=list(opacity=0.2, color="#F0E442")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_sanramon, name="San Ramon (raw)", marker=list(opacity=0.2, color="#56B4E9")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_dublin, name="Dublin (7 day ave.)", line=list(shape="spline", color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_pleasanton, name="Pleasanton (7 day ave.)", line=list(shape="spline", color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_livermore, name="Livermore (7 day ave.)", line=list(shape="spline", color="blue")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_alamo, name="Alamo (7 day ave.)", line=list(shape="spline", color="#CC79A7")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_danville, name="Danville (7 day ave.)", line=list(shape="spline", color="black")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_diablo, name="Diablo (7 day ave.)", line=list(shape="spline", color="#F0E442")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_sanramon, name="San Ramon (7 day ave.)", line=list(shape="spline", color="#56B4E9")) %>%
	layout(title="# of new COVID-19 cases per 100K population, by day\nDots = raw numbers, lines = 7 day average",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))




##################
# Make dashboard #
##################

# make dashboard, copy to root directory
rmarkdown::render(input = "index.Rmd")
file.copy("index.html", "../docs/", overwrite=T)
