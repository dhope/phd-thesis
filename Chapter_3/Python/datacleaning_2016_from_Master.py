#!/usr/bin/python3
'''
This script should provide a record of all 
edits made to the original data that was either
entered by the observer online or by David Hope manually
This script is based on ImportingfromMasterFiles.ipnb. 
See that file for more details.
'''

import numpy as np
import pandas as pd
import re
#import csvimport numpy as np
import pandas as pd
import re
import csv
SSDB = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/'
inputfolder = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Python/input/'
outputfolder = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Python/output/'
masterfiles = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/MasterFiles/'


#############################
#------------------------- 2016 ------------------------------------
##
##############################


# Import Data entered into Google Drive and then downloaded unedited.
onlineDownloaded = pd.read_excel(masterfiles + "DownloadedFromDrive/2016_Shorebird_Data_Form (Responses).xlsx",
                             sheetname = 0, na_values=['NA'] )
siteNames = onlineDownloaded['Survey Site'].unique() # Assign sites to download and enter siteIDs
dates = onlineDownloaded['Survey Date'].unique()

################## DATA to MANUALY CORRECT !!!!!!!!!!!!!! WARNING!!############################
# This removes duplicates, remove this peice to discover dupliates again.

# WHIF July 15. Entered online as July 16. Correct date from hard copy
onlineDownloaded['Survey Date'][onlineDownloaded.Timestamp == pd.to_datetime("2016-07-15 12:35:08.942")] = "7/15"

#MAPL Aug 21, Entered as Aug 16 online. Correct from scan and surveyor package start time.
onlineDownloaded['Survey Date'][onlineDownloaded.Timestamp == pd.to_datetime("2016-08-21 20:56:03.775")] = "8/21"

# VRYL Aug 20. Entered as Aug 16 online. Correct from hard copy.
onlineDownloaded['Survey Date'][onlineDownloaded.Timestamp == pd.to_datetime("2016-08-21 16:43:56.792")] = "8/20"
# How is this so fucking hard? Jon the moron entered it wrong also.
# ENBO
onlineDownloaded['Survey Date'][onlineDownloaded.Timestamp == pd.to_datetime("2016-09-23 09:15:31.860")] = "8/20"




# ---------------------------------------------------------------------
onlineDownloadedClean = onlineDownloaded.reset_index(drop=True).copy()

# Split month Day
# splitdates = pd.DataFrame(onlineDownloadedClean['Survey Date'].str.split("/",1).tolist(), 
# 	columns = ["Month", "Day"])

# onlineDownloadedClean["Month"] = splitdates["Month"]
# onlineDownloadedClean["Day"] = splitdates["Day"]
onlineDownloadedClean['Month'], onlineDownloadedClean['Day'] = \
		zip(*onlineDownloadedClean['Survey Date'].apply(lambda x: x.split('/', 1)))


# Rename columns based on standard protocol
# Import previously used data 
old_online_2015 = pd.read_table(inputfolder + '2015_OnlineData_Feb.txt', sep = '\t', na_values=['NA'])

# 1 Export columns
columnNames = np.array(onlineDownloadedClean.columns)
masterCol = np.array(old_online_2015.columns)

dic = dict(zip(columnNames, masterCol))

onlineDownloadedClean.rename(columns = dic, inplace = True)

# 5 Read in edited SiteID as dataframe and join to original
siteCodes = pd.read_table(SSDB + 'Python/keys/SiteCodes.txt',delimiter="\t")

##### Create clean siteID column and create recordID
onlineDownloadedClean["Year"] = "2016"
online_2016 = pd.merge(onlineDownloadedClean, siteCodes, left_on=["Site"], right_on = ["OnlineName"], how='left')
online_2016['RecordID'] = online_2016['SiteID'] + online_2016['Year'].map(str) + \
                online_2016['Month'].map(str) +  \
                online_2016['Day'].map(str)

# print(online_2016)

### --------------Update to datacleaning_2016.py -----------------------
online_2016['Source'] = 'Online'

# record_columns = ['RecordID', 'SiteID','Year', 'Month', 'Day' ]
observer_columns = ['RecordID', 'SiteID', 'Timestamp', 'Obs1', 'Obs2', 'Address', 'Email', 'SiteName', 'Source']
observerOnline2016 = online_2016.ix[:,observer_columns].copy() #Observer data frame with contact info

site_columns = ['RecordID', 'SiteID', 'SiteName', 'Collaborative', 'Ratio',\
       'Time_Start', 'Tide_Start', 'Time_End', 'Tide_End', 'Date', 'Month',\
       'Day', 'Weather', 'Precipitation', 'Sea_State', 'Tide_State_Start',\
       'Tide_State_End', 'Visibility', 'Vis_Reason', 'Vis_Reason_Other',\
       'Equipment', 'NoActivity', 'Walkers', 'Dogs', 'Power_Boats',\
       'Unpowered_Boat', 'Other_Activity', 'PhotoFiles', 'YES_NO_WESA_Obs',\
       'Comments', 'Source']

site_infoOnline2016 = pd.DataFrame(online_2016.ix[:,site_columns]).copy() # Site info data frame


from combineCountsFun import combineCounts

## Convert WESA counts to rows
WESA_2016 = combineCounts(online_2016, "WESA")
print("Null IDs from WESA. Should be empty\n", WESA_2016[pd.isnull(WESA_2016.ID)]) 

# Convert falcon columns to dataframe rows
falc_2016 = combineCounts(online_2016, "Falcon")

## No observed Counts------------------------------------------

no_obs_Online2016 = site_infoOnline2016[site_infoOnline2016.YES_NO_WESA_Obs == "I did not observe any Western Sandpipers during the entire survey"]
#'I saw at least 1 WESA during the survey']

columns_Iwant = ['RecordID','SiteID','Collaborative','Time_Start','Time_End','Date','Month','Day']
no_obs_Online2016_short = no_obs_Online2016.loc[:,columns_Iwant]
no_obs_Online2016_short['Count'] = 0
# print(no_obs_Online2016_short.head())
no_obs_Online2016_short.to_csv(outputfolder + 'noObsOnline2016_Fra_Master.csv', sep = '\t')


### Extract manual data from Master Files
manuallyEntered_2016 =  pd.read_excel(masterfiles + "2016_ManuallyEntered.xlsx",
                             sheetname = None, na_values=['NA'] )
volunteer_datasheets2016 = pd.read_excel(masterfiles + "2016_DataSheets_fromObservers.xlsx",
                                     sheetname = None, na_values = ['NA'])

## ------------ Observer and Site Info -------------------------
observer_manual2016 = pd.read_excel(masterfiles + "2016_ManuallyEntered.xlsx",
                             sheetname = 'Observer Information', na_values=['NA'], skiprows = 3 ) 
observer_datasheets2016 = pd.read_excel(masterfiles + "2016_DataSheets_fromObservers.xlsx",
                                     sheetname = 'Observer Information', na_values = ['NA'], skiprows = 3)
# manuallyEntered_2013['Observer Information'].copy()
site_manual2016 = pd.read_excel(masterfiles + "2016_ManuallyEntered.xlsx",
                             sheetname = 'Site Information', na_values=['NA'], skiprows = 1 ) 
site_datasheets2016 = pd.read_excel(masterfiles + "2016_DataSheets_fromObservers.xlsx",
                                     sheetname = 'Site Information', na_values = ['NA'], skiprows = 1)

# manuallyEntered_2013['Site Information'].copy()
WESA_manual_2016 = manuallyEntered_2016['WESA Counts'].copy().ix[1:].copy()
WESA_datasheets_2016 = volunteer_datasheets2016['WESA Counts'].copy().ix[1:].copy()
falcon_manual_2016 = manuallyEntered_2016['Falcon Obs'].copy()
falcon_datasheets_2016 = volunteer_datasheets2016['Falcon Obs'].copy()

colnames_obs = ['Obs1', 'Obs2', 'OtherObs',  'Address', 'Email', 'RecordID']
observer_manual2016.columns = colnames_obs
observer_datasheets2016.columns = colnames_obs

### Add the source of the data as a column
observer_manual2016['Source'] = 'Manual'
site_manual2016['Source'] = 'Manual'
falcon_manual_2016['Source'] = 'Manual'
WESA_manual_2016['Source'] = 'Manual'

observer_datasheets2016['Source'] = 'Datasheet'
site_datasheets2016['Source'] = 'Datasheet'
falcon_datasheets_2016['Source'] = 'Datasheet'
WESA_datasheets_2016['Source'] = 'Datasheet'

# Standardize columns
colnames_sites_man    = ['SiteName', 'RecordID', 'SiteID', 'Date', 'Year', 'Month', 'Day', 'Time_Start', 'Time_End', \
                         'Tide_Start', 'Tide_End', 'Collaborative', 'Ratio_WESA', 'Ratio_LESA', 'Weather', 'Precipation',\
                         'Sea_State', 'Tide_State_Start', 'Tide_State_End', 'Visibility', 'Vis_Reason', 'Vis_Reason_Other', \
                         'Equipment', 'Walkers', 'Dogs', 'Power_Boats', 'Unpowered_Boat', 'Other', \
                         'PhotoFiles', 'YES_NO_WESA_Obs', 'Comments', 'Source']

site_manual2016.columns = colnames_sites_man # Rename colums
site_datasheets2016.columns = colnames_sites_man

### Fix RecordID

# site_manual2016, observer_manual2016, WESA_manual_2016, falcon_manual_2016
# []


# ------------Combine all source data together #########

all_obs_info_2016 = pd.concat([observer_manual2016, observerOnline2016, observer_datasheets2016], ignore_index=True).copy()
all_site_info_2016 = pd.concat([site_manual2016, site_infoOnline2016, site_datasheets2016], ignore_index=True).copy()
WESA_all_2016 = pd.concat([WESA_2016, WESA_manual_2016, WESA_datasheets_2016],ignore_index=True).copy()


falcon_2016_manual_and_datasheets = falcon_manual_2016.copy()

falc_col = ['Date', 'Site', 'Year', 'Month', 'Day', 'RecordID', 'SiteID', 'Group', 'ID', \
                                             'Time', 'ObsNum', 'Species', 'Count', 'Behaviour', 'AttackLength', 'Success', \
                                             'PreySpecies', 'Attack_Type', 'Attack_Source', 'Comments', 'Source']
falcon_2016_manual_and_datasheets.columns = falc_col
falcon_datasheets_2016.columns = falc_col

falcon_all_2016 = pd.concat([falc_2016, falcon_2016_manual_and_datasheets, falcon_datasheets_2016], ignore_index=True).copy()

### ----------- Export data to csv -------------------------

all_obs_info_2016.to_csv(outputfolder + 'FromMaster_Observers_2016.csv',sep = '\t')
all_site_info_2016.to_csv(outputfolder + 'FromMaster_Sites_2016.csv',sep = '\t')
WESA_all_2016.to_csv(outputfolder + 'FromMaster_WESA_Counts_2016.csv',sep = '\t')
falcon_all_2016.to_csv(outputfolder + "FromMasterFalconCounts2016.csv", sep = '\t')



#### ----------------Check for any duplicates.------------------
'''This is how I originally found the duplicate counts 
fixed above. It should now only print out from ALHE, which is being
excluded from the data. If there are others found will need to 
adjust above fixes. '''
all_obs_info_2016['SiteID2'] = all_obs_info_2016.RecordID.str[:4]
nrecords = all_obs_info_2016.groupby("RecordID").count().sort("SiteID2", ascending = False)

duplicates = list(nrecords[nrecords.SiteID2 > 1].index)
print(duplicates)
print(all_obs_info_2016[all_obs_info_2016.RecordID.isin(duplicates)].sort_values("RecordID"))

#### WARNING for 2016 and 2016 I have manually fixed some dupliates that were true second observations
# and where were entered manually by adjusting the recordID in the master spreadsheet. One will have an 'a'
# appended to the record ID in the master file.

