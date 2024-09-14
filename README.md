# IndividualNicheBats
Description of the data and file structure

This data set contains data and scripts necessary to model individual niches of two bat species. Data files include raw recordings from an automated Very-High-Frequency (VHF) tracking system, excel files of metabarcoding data of bat guano and land cover data. The scripts comprise the workflow to filter data, calculate bearings and triangulations and build dynamic Brownian bridge movement models (dBMMs). Finally, the dBMMs are used to calculate temporal explicit habitat selection ratios and to model individual niches of bats.


a_TriangulationEstimation
This folder contains the R-scripts required to triangulate the pre-filtered data. First step is to pre-filter data based on the interval between consecutive signals. Then, the bearings are calculated using a cosine approach. Finally, the bearings are triangulated using Azimuthal Telemetry Models (ATM).

The fits of the ATM have a size of 56.42 GB. Thus, they are not provided on github, but can be shared on request.

Input: Raw data
output: Triangulated location estimates including inaccuracy estimates

b_filterRawPoints
This folder contains the scripts to filter the triangulated locations based on activity (moving vs roosting) and speed.

Input: Triangulated location estimates including inaccuracy estimates
Output: Filtered location estimates

c_MovementModels
This folder contains the scripts to transform the location data to dynamic Brownian Bridge movement models and burst the data to 30 min, 10 min, and 1 hour intervals.

Input: Filtered location estimates
Output: Movement models, burst to 30 min, 10 min, and 1 hour intervals

d_landscapePreparation
This folder contains the scripts to crop the land cover data and calculate selection ratios of the respective land cover types.

Input: land cover data (Corine, Land Burgenland), movement models
Output: Temporal explicit selection ratios

e_ nicheModels
This folder contains the scripts to build niche models based on the temporal explicit selection ratios

Input: Temporal explicit selection ratios
Output: Individual niche models

f_metabarcoding
This folder contains the analysis of the metabarcoding.

Input: Output of metabarcoding analysis
Output: PERMANOVA of metabarcoding results, NMDS and Pianka’s niche overlap
