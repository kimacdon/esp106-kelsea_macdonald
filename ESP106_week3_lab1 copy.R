##########################

# ESP 106
# Lab 3 (Monday) - graphing

##########################

#In this lab we will start by reading merging in data on economic development and indoor and outdoor air pollution. Then we will practice making some graphs with it.



#1. First read in the three csv files: gdppercapiandgini and airpollution
gdpp <- read.csv("gdppercapiandgini.csv")
airp <- read.csv("airpollution.csv")

#Both datasets are from Our World in Data: ourworldindata.org
#The GDP dataset has GDP per capita and the GINI index (a measure of income inequality: https://en.wikipedia.org/wiki/Gini_coefficient)
#The air pollution dataset has death rates from indoor and outdoor air pollution - units are in deaths per 100,000 people
#Indoor air pollution is the Household Air Pollution from Solid Fules
#Outdoor air pollution is split into particulate matter and ozone

#Hint: The column names are long and cumbersome (because they contain information about units et) - you might want to rename some of the columns to make them easier to work with
colnames(gdpp) <- c("Entity","Code","Year","Total.pop","Continent", 
                    "Gini.coeff", "GDP.per.cap")
colnames(airp) <- c("Entity", "Code","Year", "Death.Rate.PM", 
                    "Death.Rate.household","Death.Rate.ozone",
                    "Death.Rate.airpollution")
head(airp)
#2. Chose two countries that you are interested in and make a plot showing the death rates from indoor air pollution and outdoor air pollution (sum of particulate matter and ozone) over time
#Distinguish the countries using different colored lines and the types of pollution using different line types
#Make sure to add a legend and appropriate titles for the axes and plot 
unique(airp$Entity) # From this list I am choosing Canada and Cuba
Death.rate.outdoor <- (airp$Death.Rate.PM + airp$Death.Rate.ozone) #total outdoor air pollution deaths
airp2 <- cbind(airp, Death.rate.outdoor) # adding the outdoor deaths column to the data frame
Can.Cub.airp <- subset(airp2, Entity %in% c('Canada', 'Cuba')) # subset including only Canada and Cuba
mycolor <- c("red","blue")[as.factor(Can.Cub.airp$Entity)] 
par(mfrow=c(1,1))
plot(Can.Cub.airp$Year, Can.Cub.airp$Death.rate.outdoor, xlab='Year', 
     ylab='Death Rate', col=mycolor, pch=1, ylim = c(0,50), 
     main='Death Rates of Indoor and Outdoor Air Pollution \nfor Canada and Cuba',
     las=1, cex=2, cex.lab=1.5, cex.axis=1.5)
points(Can.Cub.airp$Year, Can.Cub.airp$Death.Rate.household, col=mycolor, pch=20, cex=2)
legend('topright', legend = c("Canada-indoor", "Cuba-indoor", "Canada-outdoor", 
      "Cuba-outdoor"), col = c("red","blue"), pch = c(20, 20, 1, 1), cex=.7)

#Hint: you can see all the different country names using unique(x$Entity) where x is the data frame containing the air pollution data
#Then create two new data frames that contain only the rows corresponding to each of the two countries you want to look at
#Create a new column of total outdoor air pollution deaths by summing death rates from particulate matter and ozone
#Use these to make your plot and add the lines you need

#Hint: you might have to set the y scale manually to make sure your plot is wide enough to show both countries. You can do this using the "ylim" argument in plot

#3. Merge the air pollution data with the gdp data using merge()
# Merge is a function that combines data across two data frames by matching ID rows
#By default merge will identify ID rows as those where column names are the same between datasets, but it is safer to specify the columns you want to merge by yourself using "by"
#In our case, we want to merge both by country (either the "Entity" or "Code" columns) and year columns
#Note that by default, the merge function keeps only the entries that appear in both data frames - that is fine for this lab. If you need for other applications, you can change using the all.x or all.y arguments to the function - check out the documentation at ?merge
airp.gdpp <- merge(airp2, gdpp, by=c("Entity","Year"))

#4. Make a plot with two subplots - one showing a scatter plot between log of per-capita GDP (x axis) and indoor air pollution death rate (y axis) and one showing log of per-capita GDP (x axis) and outdoor air pollution (y axis)
#Make sure to add appropriate titles to the plots and axes
#Use ylim to keep the range of the y axis the same between the two plots - this makes it easier for the reader to compare across the two graphs
#STRETCH GOAL CHALLENGE - color the points based on continent. NOT REQUIRED FOR FULL POINTS - a challenge if you want to push yourself - continent info is included in the GDP dataset, but it is only listed for the year 2015
#If you are trying this and getting stuck ASK FOR HELP - there are some tips and tricks for making it easier 
par(mfrow=c(1,2), mar=c(5,5,5,1.5))
airp.gdpp.2015 <- subset(airp.gdpp, Year %in% 2015) 
# I chose only 2015 samples since those are the ones with Continents labeled
# Indoor Air pollution plot:
plot(log(airp.gdpp.2015$GDP.per.cap), airp.gdpp.2015$Death.Rate.household,
     xlab = 'log of per-capita GDP', ylab = 'indoor air pollution death rate',
     pch=20, cex=1.5, cex.lab=1.5, cex.axis=1.5, main='2015 Indoor Air Pollution by GDP',
     col= factor(airp.gdpp.2015$Continent))
legend('topright', legend = c(unique(airp.gdpp.2015$Continent)),
       title.cex=2, cex=.5, col= factor(airp.gdpp.2015$Continent),
       pch=20)
# Outdoor Air pollution plot:
plot(log(airp.gdpp.2015$GDP.per.cap), airp.gdpp.2015$Death.rate.outdoor,
     xlab = 'log of per-capita GDP', ylab = 'outdoor air pollution death rate',
     pch=20, cex=1.5, cex.lab=1.5, cex.axis=1.5, main = '2015 Outdoor Air Pollution by GDP',
     col= factor(airp.gdpp.2015$Continent))
legend('topright', legend = c(unique(airp.gdpp.2015$Continent)),
       title.cex=2, cex=.5, col= factor(airp.gdpp.2015$Continent),
       pch=20)
# I could not figure out how to label it but I believe the unlabeled color in the legend
# represents the points where a continent is not given
#Testing 12345