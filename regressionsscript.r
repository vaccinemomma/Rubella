library(MASS)
library(foreign)
library(arm)
library(R2OpenBUGS)
library(coda)

args <- commandArgs(trailingOnly = TRUE)
dataFile <- args[1]
useAR <- args[2]
print(args)
#dataFile <- "yellowFever.csv"
#useAR <- "notAR"
disease = read.csv(dataFile)
pop = 321773600

if(useAR != "AR"){
	#basic linear regression
	incidence = disease$percent_incidence
	coverage = disease$coverage
	basic = lm(incidence ~ coverage)
	print("basic model, global data")
	print(summary(basic))
	(basic$coefficients[[1]] + basic$coefficients[[2]])*pop
	(basic$coefficients[[1]])*pop

	#basic linear regression using only US data
	index = (disease$country=="United States of America")
	incidence = disease$percent_incidence[index]
	coverage = disease$coverage[index]
	if(length(incidence) > 0){
		basicUS = lm(incidence ~ coverage)
		print("basic model, US data")
		print(summary(basicUS))
		(basicUS$coefficients[[1]] + basicUS$coefficients[[2]])*pop
		(basicUS$coefficients[[1]])*pop
	}
} else {
	#auto-regressive (1)
	incidence = disease$percent_incidence
	coverage = disease$coverage
	prev_incidence = disease$prev_percent_incidence
	prev_coverage = disease$prev_coverage
	ar1 = lm(incidence ~ coverage + prev_incidence + prev_coverage)
	print("AR1 model, global data")
	print(summary(ar1))
	(ar1$coefficients[[1]] + ar1$coefficients[[4]]*0.95)*pop + ar1$coefficients[[3]]*3.13039376628E-09*pop
	(ar1$coefficients[[1]])*pop

	#auto-regressive US data only
	index = (disease$country=="United States of America")
	incidence = disease$percent_incidence[index]
	coverage = disease$coverage[index]
	prev_incidence = disease$prev_percent_incidence[index]
	prev_coverage = disease$prev_coverage[index]
	if(length(incidence) > 0){
		ar1 = lm(incidence ~ coverage + prev_incidence + prev_coverage)
		print("AR1 model, US data")
		print(summary(ar1))
		(ar1$coefficients[[1]] + ar1$coefficients[[4]]*0.95)*pop + ar1$coefficients[[3]]*3.13039376628E-09*pop
		(ar1$coefficients[[1]])*pop
	}
}
	
if(useAR != "AR"){
	countries=list()
	countries[["country"]] = unique(disease$country)
	numCountries = length(countries[["country"]])
	countries[["countryVal"]] = seq(1:numCountries)
	disease = merge(disease,countries,by="country")
	incidence = disease$percent_incidence
	coverage = disease$coverage
	disease.data <- list(n=length(incidence), numCountries=numCountries, diseaseRate=incidence, country=disease$countryVal, vaxRate=coverage) 
	numChains = 3
	iterations = 10000
	disease.inits <- function() {
		list(countryVal=rep(0,numCountries),vaxFactor=0,mu.country=0,sigma.diseaseRate=1,sigma.country=1)
	}
	disease.parameters<-c("countryVal","vaxFactor","mu.country","sigma.diseaseRate","sigma.country")
	disease.countryHier <- bugs(data=disease.data, inits=disease.inits, parameters=disease.parameters,model.file="countryHierarchy.txt", n.chains=numChains, n.iter=iterations, debug=TRUE)
	attach(disease.countryHier) 
	write.csv(disease.countryHier$summary,"countryHier.csv")
	print(paste('hierarchical country DIC:',disease.countryHier$DIC,sep=""))
	#DIC=-51180.0
	#(disease.countryHier$mean$countryVal[154]+disease.countryHier$mean$vaxFactor)*pop
	#(disease.countryHier$mean$countryVal[154])*pop

	regions=list()
	regions[["region"]] = unique(disease$region)
	numRegions = length(regions[["region"]])
	regions[["regionVal"]] = seq(1:numRegions)
	disease = merge(disease,regions,by="region")
	regionList = unique(disease[,c("countryVal","regionVal")])
	disease.data <- list(n=length(incidence),numRegions=numRegions, numCountries=numCountries, diseaseRate=incidence, country=disease$countryVal, regionCountries = regionList$countryVal,regionRegions = regionList$regionVal, vaxRate=coverage) 
	disease.inits <- function() {
		list(regionVal=rep(0.002,numRegions),countryVal=rep(0.002,numCountries),vaxFactor=-0.0018,mu.region=0,sigma.region=1,sigma.country=1,sigma.diseaseRate=1)
	}
	disease.parameters<-c("regionVal","countryVal","vaxFactor","mu.region","sigma.region","sigma.country","sigma.diseaseRate")
	disease.regionHier <- bugs(data=disease.data, inits=disease.inits, parameters=disease.parameters,model.file="regionHierarchy.txt", n.chains=numChains, n.iter=iterations, debug=TRUE)
	attach(disease.regionHier)
	write.csv(disease.regionHier$summary,"regionHier.csv")
	print(paste('hierarchical region DIC:',disease.regionHier$DIC,sep=""))
	
} else {
#DIC = -51290
#(disease.regionHier$mean$countryVal[164]+disease.regionHier$mean$vaxFactor)*pop
#(disease.regionHier$mean$countryVal[164])*pop
	numChains = 3
	iterations = 10000
	
	countries=list()
	countries[["country"]] = unique(disease$country)
	numCountries = length(countries[["country"]])
	countries[["countryVal"]] = seq(1:numCountries)
	disease = merge(disease,countries,by="country")
	regions=list()
	regions[["region"]] = unique(disease$region)
	numRegions = length(regions[["region"]])
	regions[["regionVal"]] = seq(1:numRegions)
	disease = merge(disease,regions,by="region")
	regionList = unique(disease[,c("countryVal","regionVal")])
	incidence = disease$percent_incidence
	coverage = disease$coverage
	diseaseRatePrior = disease$prev_percent_incidence
	prevVaxRate = disease$prev_coverage
	disease.data <- list(n=length(incidence),numRegions=numRegions, numCountries=numCountries, diseaseRate=incidence, country=disease$countryVal, regionCountries = regionList$countryVal,regionRegions = regionList$regionVal, vaxRate=coverage, prevVaxRate=prevVaxRate, diseaseRatePrior=diseaseRatePrior) 
	disease.inits <- function() {
		list(regionVal=rep(0.002,numRegions),countryVal=rep(0.002,numCountries),vaxFactor=-0.0018,ar1Factor = 1.0,arVaxFactor = 0.,mu.region=0,sigma.region=1,sigma.country=1,sigma.diseaseRate=1)
	}
	disease.parameters<-c("regionVal","countryVal","vaxFactor","ar1Factor","arVaxFactor","mu.region","sigma.region","sigma.country","sigma.diseaseRate")
	disease.ar1RegionHier <- bugs(data=disease.data, inits=disease.inits, parameters=disease.parameters,model.file="ar1RegionalHierarchy.txt", n.chains=numChains, n.iter=iterations, debug=TRUE)
	attach(disease.ar1RegionHier) 
	write.csv(disease.ar1RegionHier$summary,"ARregionHier.csv")
	print(paste('AR hierarchical region DIC:',disease.ar1RegionHier$DIC,sep=""))
	
#DIC = -51290
#(disease.ar1RegionHier$mean$countryVal[164]+disease.regionHier$mean$vaxFactor+disease.ar1RegionHier$mean$ar1Factor*0.00000208797264211)*pop
#(disease.ar1RegionHier$mean$countryVal[164]+disease.ar1RegionHier$mean$ar1Factor*0.00000208797264211)*pop
}
