# U.Chicago-Capstone--City-of-Chicago-Building-Inspection-Optimization

Chicago’s Department of Buildings (DOB) supports the safety and quality of life for residents through enforcement of the city’s Building Code. A crucial part of this is conducting building inspections to ensure the code’s high-quality design and operating standards are being met. Unsafe and blighted buildings are an ongoing health and safety issue for Chicago’s residents and visitors and the DOB inspections help identify these risks to mitigate any possible hazards.

Part of this responsibility includes responding to public complaints about potentially unsafe buildings. Chicago residents can use the city’s 311 service to register building complaints which might require a DOB inspection. At this nexus, Chicago 311 and the DOB work collaboratively to identify and respond to these building complaints with inspectors being dispatched to verify any property issues. Additionally, DOB inspectors conduct ongoing, routine building inspections for new and existing construction permits, public buildings (such as schools), business licensing issuance, and circuit court follow ups to ensure past building violations have been addressed.

As a result, the DOB has to conduct an immense number of inspections in a given year. Compounding this work load further are mandatory service standards the department has to adhere to. These include a requirement to launch an inspection to investigate a complaint within 21 days of receiving the inquiry. However, a recent audit conducted by the City of Chicago’s Office of the Inspector General found the DOB only meets its complaint response benchmark for 36.5% of calls and inquiries . The audit report outlines a need for the department to better address incoming complaints and adhere to its statutory service standards set out by the Municipal Code of Chicago (MCC). Given this, the project is about developing a data-driven approach for helping the DOB better address building violation complaints in a timely and efficient manner while meeting their service response benchmark.

More specifically, this project aims to develop a methodology for prioritizing and streamlining inspector workloads while considering both complaints and routine inspections. Despite the stark audit findings, the city has a wealth of data that might positively shape an inspection prioritization solution. Chicago has a comprehensive Open Data Portal containing 311 complaints data, building violation inspection records, and the results from building violations court proceedings. The common data resources from the Open Data Portal will be used to address a complex problem spanning several city departments.

# Contents
Below are the script required to replicate the results of the analysis. Note that these scripts still are based on running updates through a local machine. File names and locations may need to be updated. These scripts have not been converted to functions at this time. Details about each script are listed below, roughly in the order that they should be run. Some scripts such as the address master list were created after the initial scripts, and the other scripts were updated after it was run.

### 1) Data_Downloads_from_Portal.R
This script provides the process for downloading and saving the various 311 files collected prior to the new 311 system update.

### 2) Create_Address_Master_List.R
A total list of addresses that did not match up to the building footprint set were collected here. While many building were matched using a tool owned by the city of Chicago, there were still nearly 400k unmatched addresses found. This script simply extracts the coordinates for each building in the set, and then splits the city up into squares and matches each missing building to the closest building found in the footprints set.

### 3) Aggregating_311_Requests.R
This script takes the data downloaded from the portal, adds in the relevant data from the new system (currenlty based on a static extract) and aggregates by each building footprint and year.

### 4) Create_Footprint_Datasets_by_Year.R
Script to download the buildings footprint data, add additional data elements (311, violations, shapefiles, etc.) and split into the two data sets used for modelling.

### 5) Create_Building_Complaints.R
This script takes all 311 complaints in the new system (only available as a static download at the time of creation) and extracts just building related complaints.

### 6) Updated_Risk_Score_Model_Transformed.R
This script takes the two datasets created by Create_Footprint_Datasets_by_Year.R, creates the models, and applies them to the open complaints (currently static). Note that some unused techniques and EDA exist in the this script, and it would need to be cleaned and condensed to turn into a repeatable function.

### 7) inspctR
This folder contains the necessary scripts and other elements to run the shiny app that was created
