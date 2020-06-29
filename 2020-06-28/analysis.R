library(data.table)


# Get current time for later use
currenttime <- paste("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), " (Pacific Time)", sep="")

# Read data from https://data.acgov.org/datasets/
# Also save a copy

byplace <- fread("https://opendata.arcgis.com/datasets/f6195a2de0b0440c99ceb9290fa95316_0.csv")
download.file("https://opendata.arcgis.com/datasets/f6195a2de0b0440c99ceb9290fa95316_0.csv", "f6195a2de0b0440c99ceb9290fa95316_0.csv")

cumulative_byplace <- fread("https://opendata.arcgis.com/datasets/e9ed9fa8d5d54d0e80dbfb95b2e142f5_0.csv")
download.file("https://opendata.arcgis.com/datasets/e9ed9fa8d5d54d0e80dbfb95b2e142f5_0.csv", "e9ed9fa8d5d54d0e80dbfb95b2e142f5_0.csv")




############################
# Data wrangling / summary #
############################

# Change "DtCreate" to a date vector
cumulative_byplace[, record_date := as.Date(DtCreate)]


# Sorry - only interested in Trivalley for now
# Subset dates and Trivalley data
# Sort by date
trivalley <- cumulative_byplace[, list(record_date, Dublin, Pleasanton, Livermore)]
setorder(trivalley, record_date)

# get the earliest and latest dates in dataset
dayEarliest <- min(trivalley$record_date, na.rm=T)
dayLatest <- max(trivalley$record_date, na.rm=T)



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


# Calculate day change
trivalley[, day_dublin := c(NA, diff(cumul_dublin))]
trivalley[, day_pleasanton := c(NA, diff(cumul_pleasanton))]
trivalley[, day_livermore := c(NA, diff(cumul_livermore))]


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




# Get population numbers as well
pop_dublin <- byplace[Geography=="Dublin"]$Population
pop_pleasanton <- byplace[Geography=="Pleasanton"]$Population
pop_livermore <- byplace[Geography=="Livermore"]$Population


# Get cumulative per 100k rate
trivalley[, cumul_per100k_dublin := round(cumul_dublin / pop_dublin * 100000, 2)]
trivalley[, cumul_per100k_pleasanton := round(cumul_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, cumul_per100k_livermore := round(cumul_livermore / pop_livermore * 100000, 2)]


# Calculate day changes per 100k rates
trivalley[, day_per100k_dublin := round(day_dublin / pop_dublin * 100000, 2)]
trivalley[, day_per100k_pleasanton := round(day_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, day_per100k_livermore := round(day_livermore / pop_livermore * 100000, 2)]

trivalley[, day_7mean_per100k_dublin := round(day_7mean_dublin / pop_dublin * 100000, 2)]
trivalley[, day_7mean_per100k_pleasanton := round(day_7mean_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, day_7mean_per100k_livermore := round(day_7mean_livermore / pop_livermore * 100000, 2)]





##############
# Make plots #
##############

library(plotly)

# cumulative
fig_cumul <- plot_ly() %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_dublin, name="Dublin", line=list(color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_pleasanton, name="Pleasanton", line=list(color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_livermore, name="Livermore", line=list(color="blue")) %>%
	layout(title="Cumulative # of COVID-19 cases",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))

# cumulative per 100K
fig_cumul_100K <- plot_ly(mode="lines") %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_dublin, name="Dublin", line=list(color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_pleasanton, name="Pleasanton", line=list(color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$cumul_per100k_livermore, name="Livermore", line=list(color="blue")) %>%
	layout(title="Cumulative # of COVID-19 cases, per 100K population",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))


# Day changes
fig_day <- plot_ly() %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_dublin, name="Dublin (raw)", marker=list(opacity=0.2, color="green")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_pleasanton, name="Pleasanton (raw)", marker=list(opacity=0.2, color="orange")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_livermore, name="Livermore (raw)", marker=list(opacity=0.2, color="blue")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_dublin, name="Dublin (7 day ave.)", line=list(shape="spline", color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_pleasanton, name="Pleasanton (7 day ave.)", line=list(shape="spline", color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_livermore, name="Livermore (7 day ave.)", line=list(shape="spline", color="blue")) %>%
	layout(title="# of new COVID-19 cases by day\nDots = raw numbers, lines = 7 day average",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))

# Day changes, per 100K
fig_day_100K <- plot_ly() %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_dublin, name="Dublin (raw)", marker=list(opacity=0.2, color="green")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_pleasanton, name="Pleasanton (raw)", marker=list(opacity=0.2, color="orange")) %>%
	add_trace(type="scatter", mode="markers", x=trivalley$record_date, y=trivalley$day_per100k_livermore, name="Livermore (raw)", marker=list(opacity=0.2, color="blue")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_dublin, name="Dublin (7 day ave.)", line=list(shape="spline", color="green")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_pleasanton, name="Pleasanton (7 day ave.)", line=list(shape="spline", color="orange")) %>%
	add_trace(type="scatter", mode="lines", x=trivalley$record_date, y=trivalley$day_7mean_per100k_livermore, name="Livermore (7 day ave.)", line=list(shape="spline", color="blue")) %>%
	layout(title="# of new COVID-19 cases per 100K population, by day\nDots = raw numbers, lines = 7 day average",
		xaxis=list(title="Dates", range=c(dayEarliest-1, dayLatest+1)))




##################
# Make dashboard #
##################

# make dashboard, copy to root directory
rmarkdown::render(input = "index.Rmd")
file.copy("index.html", "../", overwrite=T)
