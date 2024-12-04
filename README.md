## This repository contains all R code included in the publication "Cooling outweighs warming across phenological transitions in the Northern Hemisphere" (Li et al.)

### Workflow

1. 01 Images mosaic
    - Satellite-derived data preprocessing - Images mosaic from each latitude zone;
                                       Band splitting for both Eurasia (EA) and NorthernAmerica (NA);
                                       Group by year                    
    - Run `001 Koppen climate Images Mosaic for EA(NA).R' for climate types mask and analysis
    - Run `002 LST Images Mosaic for EA(NA).R` for the land surface temperature analysis
    - Run `003 PHE Images Mosaic for EA(NA).R` for the vegetation phenology analysis
    -- EA = `Eurasia`
    -- NA = `North America`
    -- NH = `Northern Hemisphere`

2. 02 Analysis of ΔLST
    - Use ATC model to analyze the effects of vegetation following each phenological transition on LST 
    - Run `001 nlstest_LST_meanDiff_EA(NA)_test_each year_up to_slurm.R` for each year ATC fitting
    - Run the code like`2013-nlstest_LST_meanDiff_EA(NA)_test.R` in  the `001_nlstest_LST_meanDiff_EA(NA)_test_up_to_slurm` files to files to conduct the NLS fitting test for each year
    - Run `002 meanDiff_6events_common_EA(NA)_each year.R`  to select the pixels with all six ΔLSTs.
    - Run `003 Merge_EA+NA_common_for_meanDiff_each year.R` for the analysis and figures
    - Run `004 meanDiff_Polar+Histogram_NA+EA--Fig.2.R` for the analysis and figures
    - Run `005 meanDiff-data_analysis--Table.S4.R` for the analysis


3. 03 Evaluation of ATC model
    - Evaluation of ATC model performance in simulating annual LST
    - Run `001  nlstest_Evaluation of ATC model_EA(NA)_up_to_slurm_for a single year.R` for each year 
    - Run `002  nlstest_Evaluation of ATC_average_parameters_EA(NA).R` to select pixels that have all ΔLST post six phenological events for each year and then to analyze the average evaluation indicators
    - Run `003 Polarplot_Evaluation of ATC_parameters_NA+EA-Fig.S3.R` for the analysis and figures


4. 04 Analysis of Accumulated ΔLST
    - Use ATC model to analyze the cumulative vegetation on LST during the growing season across climate types
    - Run `001 nlstest_LST_SumDiff_EA(NA)_test_up_to_slurm_for a single year.R` for each year ATC fitting
    - Run the code like`2013-nlstest_LST_SumDiff_EA(NA)_test.R` in  the `001 nlstest_LST_SumDiff_EA_test_up to_slurm` files to files to calculate cumulative ΔLST of phenological phases for each year
    - Run `002 SumDiff_6events_common_EA(NA)_each year.R`  to select the pixels matched with all six ΔLSTs post six phenological events
    - Run `003 Merge_EA+NA_common_for_SumDiff_each year.R` for each year to the next step analysis 
    - Run `003 Merge_SumDiff_6phases_common_EA+NA.R`to merge rasters  for the analysis and figures
    - Run `004 004 SumDiff_Polarplot--Fig.3-1_range-3500-3500.R` for the analysis and figures
    - Run `005 Select KG-Global-ClimateType_for_Research.R` to select the objective climate types 
    - Run `006 SumDiff&Climate_type_barplot-Fig.3-2_By_T.R` for the analysis and figures
    - Run `007 SumDiff&Climate_type_barplot-Fig.S5-By_P.R` for the analysis and figures


5. 05 Analysis of ΔLST sensitivity
    - Ground-sourced phenology observations for Eastern North America (Harvard forest)
    - Run `001 annual_mean_temperature_EA(NA).R` to calculate the annual mean LST for each year
    - Run `002 annual_actLST_common_EA(NA)_each year.R` extracts the common pixels matched with all six ΔLSTs post six phenological events
    - Run `003 Merge_actLST_common_EA+NA.R` to merge the LST rasters for each year 
    - Run `004 NA_EA_annual_mean_T--Fig.S4.R` to analyze annual mean LST across EA and NA
    - Run `005 k-Climate_type_lineplot-Fig.S6--T.R` and `005 koppen_31subtype-4Ktypes-polar-Fig.S6-7T.R` to calculate temperature sensitivity across climate type classified by temperature and generate figures
    - Run `006 k-Climate_type_lineplot-Fig.S7--P.R` and `006 koppen_31subtype-4Ktypes-polar-Fig.S7-6P.R` to calculate temperature sensitivity across climate type classified by precipitation and generate figures
    -  Run `007 k-of-LSTdiff&LST_yrmean_common pixels.R` to remove outliers in temperature sensitivity values using three times the standard deviation method, and retain only the pixels that have valid sensitivity values after all six phenological transitions.
    -  Run `008 k-Polar-lmtest_LST_Diff&LST_yrmean--E_Fig.2.R` and `008 k-Polar+5Barplot+np_Histogram--E_Fig.2+legend.R` for the analysis and figures


6. 06 Analysis of ΔLST sensitivity pattern
    - To generate the patterns of vegetation's effect on LST (ΔLST) and its temperature sensitivity
    - Run `001 Annual_ΔLST_PatternType_lineplot_NH_Fig.S8.R`  to analyze the  annual ΔLST difference following phenological transitions for the four patterns across the Northern Hemisphere
    - Run `001 Annual_ΔLST_PatternType_lineplot_NA_Fig.S9.R`  to analyze the  annual ΔLST difference following phenological transitions for the four patterns across North America
    - Run `001 Annual_ΔLST_PatternType_lineplot_EA_Fig.S10.R`  to analyze the  annual ΔLST difference following phenological transitions for the four patterns across Eurasia
    - Run `002 ΔLST_PatternType_Polarmap+Histogram_NA+EA--Fig.4.R` to analyze ΔLST sensitivity pattern following phenological transitions for the four patterns across the Northern Hemisphere


7. 07 AmerifluxData_Analysis
    - Ground-sourced LST and phenology observations analysis across North America
    - Step 1: Run `001 Amerifux1330LSTData_PhenoCamData_Match.R` for site Information integration and filter out deciduous and mixed forest sites. Note: Perform separate analysis for Noen and non-Noen sites, as the selected temperature columns are different. For the Noen station, tau_corrected_surfaceTemp_97 was selected for LST analysis, while for the normal stations, corrected_surfaceTemp_97 is recommended.
    - Step 2: Run `002 nlstest_LST_meanDiff_test_NoenSites_for a single year.R` and `002 nlstest_LST_meanDiff_test_NormalSites_for a single year.R`  for ATC fitting of NoenSites and NormalSites, respectively, for each year.
    - Step 3: Run `003 Combine_Noen+Normal_sites_Results.R` to merge site ATC fitting results and integrate information.
    -Step 4: Retrieve site information (latitude, longitude, elevation, vegetation type, climate classification, temperature, and precipitation) from the AmeriFlux website, add it to the results; then run `004 Add_VegType_Select_Noen+Normal_sites.R` to filter sites with specific vegetation types (DBF, MF, and DNF, the latter absent in the current data) to generate the final dataset
    -Step 5: Run`005 AmeriFluxSites_ 4_Plot-Fig.S1.R` for the analysis and figures
    -Step 6: Run`006 AmeriFluxSites_Climates_Analysis_Table S8.R` for the analysis across climate types and figures


8. 08 Analysis of Phe_Events_DOY
    - Run `001 PheDOY_6events_common_EA(NA)_each year.R`  to select pixels that match both PHE values and ΔLST values for all six phenological events each year
    - Run `002 Merge_EA+NA_common_for_PheDOY_each year.R` to merge EA and NA rasters for the analysis 
    - Run `003 EA+NA_Annual_mean_phe_DOY-Analysis_TableS3.R`  to analyze the average date and the trend of phenological transitions 