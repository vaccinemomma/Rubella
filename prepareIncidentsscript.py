#script to prepare the data for hierarchical regression
import pandas
import sys

#argv[1] - the sheet name of the disease
#argv[2] - the sheet name of the antigen
#argv[3] - the name of the output csv, must include ".csv" in the name
#argv[4] - AR or something else, to determine if AR1 is to be used.
#argv[5] - the sheet name of the second antigen

def change_names(df,column):
	df[column] = df[column].str.replace(' \(the\)','')
	df[column] = df[column].str.replace('Dem. ','Democratic ')
	df[column] = df[column].str.replace('Fed. ','Federated ')
	df[column] = df[column].str.replace('The former Yugoslav Republic of','TFYR')
	df[column] = df[column].str.replace('of Great Britain and Northern Ireland','')

if __name__ == "__main__":
	incidenceFile = 'incidence_series.xls'
	sheet = sys.argv[1]
	df = pandas.read_excel(incidenceFile,sheet)
	change_names(df,'Cname')
	incidence = pandas.melt(df,id_vars=list(df.columns[0:4]),value_vars=list(df.columns[4:]))
	incidence.rename(columns={'variable':'year','value':'incidents'},inplace=True)
	incidence = incidence.dropna()
	populationFile = 'population.csv'
	df = pandas.read_csv(populationFile)
	change_names(df,'country')
	population = pandas.melt(df,id_vars=list(df.columns[0:3]),value_vars=list(df.columns[3:]))
	population.rename(columns={'variable':'year','value':'population'},inplace=True)
	coverageFile = 'coverage_series.xls'
	sheet = sys.argv[2]
	df = pandas.read_excel(coverageFile,sheet)
	change_names(df,'Cname')
	coverage = pandas.melt(df,id_vars=list(df.columns[0:4]),value_vars=list(df.columns[4:]))
	coverage.rename(columns={'variable':'year','value':'coverage'},inplace=True)
	coverage = coverage.dropna()
	coverage.coverage = coverage.coverage/100.0
	try:
		if sys.argv[5] != None:	
			print 'using second vaccine'
			sheet2 = sys.argv[5]
			df2 = pandas.read_excel(coverageFile,sheet2)
			change_names(df2,'Cname')
			coverage2 = pandas.melt(df2,id_vars=list(df2.columns[0:4]),value_vars=list(df2.columns[4:]))
			coverage2.rename(columns={'variable':'year','value':'coverage2'},inplace=True)
			coverage2 = coverage2.dropna()
			coverage2.coverage2 = coverage2.coverage2/100.0
			coverage = coverage.merge(coverage2,how='inner',left_on=['Region','Cname','year'],right_on=['Region','Cname','year'])[['Cname','year','coverage','coverage2']]
	except IndexError:
		pass
	if sys.argv[4] != 'AR':
	#we want a new table with the year, region, country, incidence percent, coverage percent
		incidence2 = incidence.merge(population,how='inner',left_on=['Cname','year'],right_on=['country','year'])[['WHO_REGION','region','country','year','incidents','population']]
		incidence2['percent_incidence'] = incidence2.incidents / incidence2.population / 1000.
		try:
			if sys.argv[5] != None:
				data = coverage.merge(incidence2,how='inner',left_on=['Cname','year'],right_on=['country','year'])[['WHO_REGION','region','country','year','percent_incidence','coverage','coverage2']]
		except IndexError:
			data = coverage.merge(incidence2,how='inner',left_on=['Cname','year'],right_on=['country','year'])[['WHO_REGION','region','country','year','percent_incidence','coverage']]
		data.to_csv(sys.argv[3],header=True,index=False)
	else:
		incidence2 = incidence.merge(population,how='inner',left_on=['Cname','year'],right_on=['country','year'])[['WHO_REGION','region','country','year','incidents','population']]
		incidence2['percent_incidence'] = incidence2.incidents / incidence2.population / 1000.
		try:
			if sys.argv[5] != None:
				data = coverage.merge(incidence2,how='inner',left_on=['Cname','year'],right_on=['country','year'])[['WHO_REGION','region','country','year','percent_incidence','coverage','coverage2']]
		except IndexError:
			data = coverage.merge(incidence2,how='inner',left_on=['Cname','year'],right_on=['country','year'])[['WHO_REGION','region','country','year','percent_incidence','coverage']]
		data.year = data.year.astype(int)
		data['prevYear'] = data.year - 1
		try:
			if sys.argv[5] != None:
				data2 = data.merge(data,how='inner',left_on=['country','prevYear'],right_on=['country','year'])[['WHO_REGION_x','region_x','country','year_x','percent_incidence_x','coverage_x','coverage2_x','percent_incidence_y','coverage_y','coverage2_y']]
				data2.columns = ['WHO_REGION','region','country','year','percent_incidence','coverage','coverage2','prev_percent_incidence','prev_coverage','prev_coverage2']
		except IndexError:
			data2 = data.merge(data,how='inner',left_on=['country','prevYear'],right_on=['country','year'])[['WHO_REGION_x','region_x','country','year_x','percent_incidence_x','coverage_x','percent_incidence_y','coverage_y']]
			data2.columns = ['WHO_REGION','region','country','year','percent_incidence','coverage','prev_percent_incidence','prev_coverage']
		data2.to_csv(sys.argv[3],header=True,index=False)

	
	#CRS
	#Diptheria DTP1, DTP3
	#JapEnc
	#Measles MCV1, MCV2
	#Mumps
	#Ntetanus DTP1, DTP3
	#Pertusis DTP1, DTP3
	#Polio IPV1, Pol3
	##Rubella RCV1
	#Ttetanus DTP1, DTP3
	##Yfever YFV