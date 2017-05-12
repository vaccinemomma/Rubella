library(MASS)
library(foreign)
library(arm)
library(R2OpenBUGS)
library(coda)

measles = read.csv("measles.csv")
pop = 321000000

#basic linear regression
incidence = measles$percent_incidence
coverage = measles$coverage
basic = lm(incidence ~ coverage)
summary(basic)
(basic$coefficients[[1]] + basic$coefficients[[2]])*pop
(basic$coefficients[[1]])*pop

#basic linear regression using only US data
index = (measles$country=="United States of America")
incidence = measles$percent_incidence[index]
coverage = measles$coverage[index]
basicUS = lm(incidence ~ coverage)
summary(basicUS)
(basicUS$coefficients[[1]] + basicUS$coefficients[[2]])*pop
(basicUS$coefficients[[1]])*pop

#auto-regressive (1)
incidence = measles$percent_incidence
coverage = measles$coverage
prev_incidence = measles$prev_percent_incidence
prev_coverage = measles$prev_coverage
ar1 = lm(incidence ~ coverage + prev_incidence + prev_coverage)
summary(ar1)
(ar1$coefficients[[1]] + ar1$coefficients[[4]]*0.92)*pop + ar1$coefficients[[3]]*4.058836e-07*pop
(ar1$coefficients[[1]])*pop

#auto-regressive US data only
index = (measles$country=="United States of America")
incidence = measles$percent_incidence[index]
coverage = measles$coverage[index]
prev_incidence = measles$prev_percent_incidence[index]
prev_coverage = measles$prev_coverage[index]
ar1 = lm(incidence ~ coverage + prev_incidence + prev_coverage)
summary(ar1)
(ar1$coefficients[[1]] + ar1$coefficients[[2]])*pop
(ar1$coefficients[[1]])*pop

countries=list()
countries[["country"]] = unique(measles$country)
numCountries = length(countries[["country"]])
countries[["countryVal"]] = seq(1:numCountries)
measles = merge(measles,countries,by="country")
measles.data <- list(n=length(incidence), numCountries=numCountries, diseaseRate=incidence, country=measles$countryVal, vaxRate=coverage) 
numChains = 3
iterations = 10000
measles.inits <- function() {
	list(countryVal=rep(0,numCountries),vaxFactor=0,mu.country=0,sigma.diseaseRate=1,sigma.country=1)
}
measles.parameters<-c("countryVal","vaxFactor","mu.country","sigma.diseaseRate","sigma.country")
measles.countryHier <- bugs(data=measles.data, inits=measles.inits, parameters=measles.parameters,model.file="countryHierarchy.txt", n.chains=numChains, n.iter=iterations, debug=TRUE)
attach(measles.countryHier) 
#DIC=-51180.0
#(measles.countryHier$mean$countryVal[164]+measles.countryHier$mean$vaxFactor)*pop
#(measles.countryHier$mean$countryVal[164])*pop

regions=list()
regions[["region"]] = unique(measles$region)
numRegions = length(regions[["region"]])
regions[["regionVal"]] = seq(1:numRegions)
measles = merge(measles,regions,by="region")
regionList = unique(measles[,c("countryVal","regionVal")])
measles.data <- list(n=length(incidence),numRegions=numRegions, numCountries=numCountries, diseaseRate=incidence, country=measles$countryVal, regionCountries = regionList$countryVal,regionRegions = regionList$regionVal, vaxRate=coverage) 
measles.inits <- function() {
	list(regionVal=rep(0.002,numRegions),countryVal=rep(0.002,numCountries),vaxFactor=-0.0018,mu.region=0,sigma.region=1,sigma.country=1,sigma.diseaseRate=1)
}
measles.parameters<-c("regionVal","countryVal","vaxFactor","mu.region","sigma.region","sigma.country","sigma.diseaseRate")
measles.regionHier <- bugs(data=measles.data, inits=measles.inits, parameters=measles.parameters,model.file="regionHierarchy.txt", n.chains=numChains, n.iter=iterations, debug=TRUE)
attach(measles.regionHier) 
#DIC = -51290
#(measles.regionHier$mean$countryVal[164]+measles.regionHier$mean$vaxFactor)*pop
#(measles.regionHier$mean$countryVal[164])*pop

diseaseRatePrior = measles$prev_percent_incidence
prevVaxRate = measles$prev_coverage
measles.data <- list(n=length(incidence),numRegions=numRegions, numCountries=numCountries, diseaseRate=incidence, country=measles$countryVal, regionCountries = regionList$countryVal,regionRegions = regionList$regionVal, vaxRate=coverage, prevVaxRate=prevVaxRate, diseaseRatePrior=diseaseRatePrior) 
measles.inits <- function() {
	list(regionVal=rep(0.002,numRegions),countryVal=rep(0.002,numCountries),vaxFactor=-0.0018,ar1Factor = 1.0,arVaxFactor = 0.,mu.region=0,sigma.region=1,sigma.country=1,sigma.diseaseRate=1)
}
measles.parameters<-c("regionVal","countryVal","vaxFactor","ar1Factor","arVaxFactor","mu.region","sigma.region","sigma.country","sigma.diseaseRate")
measles.ar1RegionHier <- bugs(data=measles.data, inits=measles.inits, parameters=measles.parameters,model.file="ar1RegionalHierarchy.txt", n.chains=numChains, n.iter=iterations, debug=TRUE)
attach(measles.ar1RegionHier) 
#DIC = -51290
#(measles.ar1RegionHier$mean$countryVal[164]+measles.regionHier$mean$vaxFactor+measles.ar1RegionHier$mean$ar1Factor*0.00000208797264211)*pop
#(measles.ar1RegionHier$mean$countryVal[164]+measles.ar1RegionHier$mean$ar1Factor*0.00000208797264211)*pop

index = (measles$country=="United States of America")
incidence = measles$percent_incidence[index]
coverage = measles$coverage[index]
prev_incidence = measles$prev_percent_incidence[index]
prev_coverage = measles$prev_coverage[index]
ar1 = lm(incidence ~ coverage + prev_incidence + prev_coverage)
summary(ar1)
(ar1$coefficients[[1]] + ar1$coefficients[[2]])*pop
(ar1$coefficients[[1]])*pop