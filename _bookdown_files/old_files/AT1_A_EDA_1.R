## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = hook_purl)

## ----Package Install, eval = FALSE--------------------------------------------
#  install.packages(tidyverse)

## ----Load Libraries-----------------------------------------------------------
library(tidyverse)

## ----Custom Function----------------------------------------------------------
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## ----Export-Code-Chunks, eval = FALSE-----------------------------------------
#  p <- purl("AT1_A_EDA_1.Rmd", output = /chunks/)
#  read_chunk(p)
#  chunks <- knitr:::knit_code$get()
#  invisible(mapply(function(chunk, name) {
#      writeLines(c(paste0("## ----",name,"----"), chunk), paste0("chunk-",name,".R"))
#  }, chunks, names(chunks)))
#  unlink(p) # delete the original purl script
#  knitr:::knit_code$restore() # remove chunks from current knitr session

## -----------------------------------------------------------------------------
transactions <- read_csv("transactions.csv")

## ----test, purl = TRUE--------------------------------------------------------
names(transactions)
str(transactions)
summary(transactions)
range(transactions$date) # range doesn't work on dates? 
sapply(transactions, function(x) length(unique(x))) #number of unique entries for each variable


## -----------------------------------------------------------------------------
nrow(transactions) == nrow(na.omit(transactions))

# A. No


## -----------------------------------------------------------------------------
transactions$date <- as.Date(transactions$date, format = "%d/%m/%Y")
range(transactions$date) # range doesn't work on dates? 
# date was being read incorrectly

## ---- eval=FALSE--------------------------------------------------------------
#  #not used yet due to legacy code
#  tname <- names(transactions)
#  tname[1] <- "trdate"
#  names(transactions) <- tname

## -----------------------------------------------------------------------------
number_of_transactions_per_customer <- as.data.frame(table(transactions$customer_id))
head(number_of_transactions_per_customer)

# A. no

## -----------------------------------------------------------------------------
# ggplot(transactions, aes(y = customer_id, x= date)) + geom_point(alpha = 0.25)
# too much data for small area

transactions$duplicated <- transactions %>%
  select(date, customer_id) %>%
  duplicated()

transactions %>% 
  filter(duplicated == "TRUE")

#A. No.

## -----------------------------------------------------------------------------
transactions <- transactions %>% 
  select(-duplicated)

## -----------------------------------------------------------------------------
transactions$customer_id <- as.factor(transactions$customer_id) 

## -----------------------------------------------------------------------------
# table(transactions$customer_id)
# Alot

length(unique(transactions$customer_id))

## -----------------------------------------------------------------------------
transactions$industry <- as.factor(transactions$industry)

## -----------------------------------------------------------------------------
unique(transactions$industry)

#A. Yes

## -----------------------------------------------------------------------------

# transactions$multi_industry <- 
  
industry_check <- transactions %>%
  select(industry, customer_id) %>%
  distinct()

industry_check$multi_industry <- industry_check %>%
  select(customer_id) %>%
  duplicated()

industry_check %>% 
  filter(multi_industry == "TRUE")

# A. Yes


## -----------------------------------------------------------------------------
transactions %>%
  distinct(customer_id, industry) %>%
ggplot(aes(x = industry)) + geom_bar()

## -----------------------------------------------------------------------------
customers_industry_6 <- transactions %>%
  distinct(customer_id, industry) %>%
  filter(industry == 6) 

count(customers_industry_6)

customers_industry_10 <- transactions %>%
  distinct(customer_id, industry) %>%
  filter(industry == 10) 

count(customers_industry_10)

#A. Both still significant, even though under represented

## -----------------------------------------------------------------------------
transactions$location <- as.factor(transactions$location)

## -----------------------------------------------------------------------------
unique(transactions$location)

# A. Yes

## -----------------------------------------------------------------------------
location_check <- transactions %>%
  select(location, customer_id) %>%
  distinct()

location_check$multi_location <- location_check %>%
  select(customer_id) %>%
  duplicated()

location_check %>% 
  filter(multi_location == "TRUE")

# A. Yes

## -----------------------------------------------------------------------------
transactions %>%
  distinct(customer_id, location) %>%
ggplot(aes(x = location)) + geom_bar()

## -----------------------------------------------------------------------------
zero_trans <- transactions %>%
  filter(monthly_amount == 0)

zero_trans

#A. only one transaction with amount 0, error?

## -----------------------------------------------------------------------------
transactions %>%
  filter(customer_id == zero_trans$customer_id) %>%
  nrow()

## -----------------------------------------------------------------------------
large_trans <- transactions %>%
  filter(monthly_amount == 100000000)

large_trans

#only one transaction of 100mm

largeish_trans <- transactions %>%
  filter(monthly_amount >= 50000000)

largeish_trans


## -----------------------------------------------------------------------------
ggplot(transactions, aes(x = monthly_amount)) + geom_histogram(bins = 100)

# A. Distribution is heavily skewed toward the lower end of the range

transactions %>%
  filter(monthly_amount <= 1000000) %>%

ggplot(aes(x = monthly_amount)) + geom_histogram(bins  = 100)

# Still skewed heavily

transactions %>%
  filter(monthly_amount <= 100000) %>%

ggplot(aes(x = monthly_amount)) + geom_histogram(bins  = 100)

#It appears most billing is cut

transactions %>%
  filter(monthly_amount <= 80000 & monthly_amount >= 50000) %>%

ggplot(aes(x = monthly_amount)) + geom_histogram(binwidth = 2000)

# almost no transactions below 64000

transactions %>%
  filter(monthly_amount <= 64000 & monthly_amount >= 50000) %>%

ggplot(aes(x = monthly_amount)) + geom_histogram(binwidth = 100)

transactions %>%
  filter(monthly_amount <= 75000 & monthly_amount >= 62000) %>%

ggplot(aes(x = monthly_amount)) + geom_histogram(binwidth = 10)

transactions %>%
  filter(monthly_amount >= 10000000) %>%

ggplot(aes(x = monthly_amount)) + geom_histogram(bins = 1000)


## -----------------------------------------------------------------------------
ggplot(transactions, aes(x = date, y = monthly_amount)) + geom_point()

ggplot(filter(transactions, industry != 6 & industry != 10), aes(x = date, y = monthly_amount)) + geom_point()


## -----------------------------------------------------------------------------
transactions %>%
  filter(monthly_amount < 4000000) %>%
ggplot(aes(x = date, y = monthly_amount)) + geom_point(alpha = 0.1)

## -----------------------------------------------------------------------------
# plot(transactions)
# long load

## -----------------------------------------------------------------------------
for (indust in 1:10){
  dfname <- paste("transactions_industry", indust, sep = "_")
  dfind <- transactions %>%
    filter(industry == indust)
  assign(dfname, dfind)
}

plotlist_industry <- as.list(1:10)

plotlist_industry <- lapply(plotlist_industry, function(ind){
  ggplot(eval(as.symbol(paste("transactions_industry_", ind, sep=""))), aes(x = date, y = monthly_amount)) + geom_point() + labs (title = paste("industry", ind, sep = " "))
  })

multiplot(plotlist = plotlist_industry, cols = 2)


## -----------------------------------------------------------------------------
for (locat in 1:10){
  dfname <- paste("transactions_location", locat, sep = "_")
  dfind <- transactions %>%
    filter(location== locat)
  assign(dfname, dfind)
}

plotlist_location <- as.list(1:10)

plotlist_location <- lapply(plotlist_location, function(loc){
  ggplot(eval(as.symbol(paste("transactions_location_", loc, sep=""))), aes(x = date, y = monthly_amount)) + geom_point() + labs (title = paste("location", loc, sep = " "))
  })

multiplot(plotlist = plotlist_location, cols = 2)

## -----------------------------------------------------------------------------
# modified code from CANVAS Announcement
# industry_location is the aggregated data frame
plotlist_loc_ind <- data.frame()
industries = unique(transactions$industry)
locations = unique(transactions$location)
for (ind in industries) {
for (loc in locations) {
  temp_name <- paste("industry", ind, "location", loc, sep = "_")
# create a subset of the data
temp = transactions[transactions$industry == ind &
transactions$location == loc, ]
assign(temp_name, temp)
row <- cbind(temp_name)
plotlist_loc_ind <- rbind(plotlist_loc_ind, row)
}
}

plotlist_loc_ind <- as.list(as.character(plotlist_loc_ind$temp_name))

plotlist_loc_ind <- lapply(plotlist_loc_ind, function(plotlist_loc_ind){
  ggplot(eval(as.symbol(paste(plotlist_loc_ind))), aes(x = date, y = monthly_amount)) + geom_point(alpha = 0.25) + theme_void() + labs (title = paste(plotlist_loc_ind))
  })

multiplot(plotlist = plotlist_loc_ind, cols = 5)


