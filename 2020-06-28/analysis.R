library(data.table)

# Read data from https://data.acgov.org/datasets/

byplace <- fread("https://opendata.arcgis.com/datasets/f6195a2de0b0440c99ceb9290fa95316_0.csv")

cummulative_byplace <- fread("https://opendata.arcgis.com/datasets/e9ed9fa8d5d54d0e80dbfb95b2e142f5_0.csv")


# Work on the cummulative dataset

# Change "DtCreate" to a date vector
cummulative_byplace[, record_date := as.Date(DtCreate)]


# Sorry - only interested in Trivalley for now
# Subset dates and Trivalley data
# Sort by date
trivalley <- cummulative_byplace[, list(record_date, Dublin, Pleasanton, Livermore)]
setorder(trivalley, record_date)

# Data has both "0" cases and "<10" cases
# For ease of plotting, change them to NAs

remove_tens <- function(x){
	gsub("^0|<10", NA, x)
}

trivalley <- trivalley[, lapply(.SD, remove_tens)]

# Rename colums for clarity, fix classes
names(trivalley) <- c("record_date", "cummul_dublin", "cummul_pleasanton", "cummul_livermore")
trivalley[, record_date := as.Date(record_date)]
trivalley[, cummul_dublin := as.integer(cummul_dublin)]
trivalley[, cummul_pleasanton := as.integer(cummul_pleasanton)]
trivalley[, cummul_livermore := as.integer(cummul_livermore)]


# Calculate day change
trivalley[, day_dublin := c(NA, diff(cummul_dublin))]
trivalley[, day_pleasanton := c(NA, diff(cummul_pleasanton))]
trivalley[, day_livermore := c(NA, diff(cummul_livermore))]


# Get population numbers as well
pop_dublin <- byplace[Geography=="Dublin"]$Population
pop_pleasanton <- byplace[Geography=="Pleasanton"]$Population
pop_livermore <- byplace[Geography=="Livermore"]$Population


# Get cummulative per 100k rate
trivalley[, cummul_per100k_dublin := round(cummul_dublin / pop_dublin * 100000, 2)]
trivalley[, cummul_per100k_pleasanton := round(cummul_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, cummul_per100k_livermore := round(cummul_livermore / pop_livermore * 100000, 2)]

# Calculate day change
trivalley[, day_per100k_dublin := round(day_dublin / pop_dublin * 100000, 2)]
trivalley[, day_per100k_pleasanton := round(day_pleasanton / pop_pleasanton * 100000, 2)]
trivalley[, day_per100k_livermore := round(day_livermore / pop_livermore * 100000, 2)]



# plot_ly()




