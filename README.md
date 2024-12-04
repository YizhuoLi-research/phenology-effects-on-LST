## This repository contains all R code included in the publication "Cooling outweighs warming across phenological transitions in the Northern Hemisphere" (Li et al.)

### Workflow

1. 01 Images mosaic
    - Data preprocessing - Images mosaic from each latitude zone;
                                       Band splitting for both Eurasia (EA) and NorthernAmerica (NA);
                                       Group by year                    
    - Run `001 Koppen climate Images Mosaic for EA(NA).R'_ for climate types analysis
    - Run `002 LST Images Mosaic for EA.R` for the land surface temperature analysis
    - Run `003 PHE Images Mosaic for EA.R` for the vegetation phenology analysis


2. 02 Analysis of ΔLST
    - Use ATC model to analyze the effects of vegetation following each phenological transitions on LST 
    - Run `001 nlstest_LST_meanDiff_EA(NA)_test_each year_up to_slurm.R` for each year ATC fitting
      Run the code like`2013-nlstest_LST_meanDiff_EA(NA)_test` in  `001 nlstest_LST_meanDiff_EA(NA)_test_up_to_slurm` files to conduct the nls fiting test for each year
    - Run `002 meanDiff_6events_common_EA(NA)_each year` to select the pixels that have all six ΔLSTs 
    - Run `003 Merge_EA+NA_common_for_meanDiff_each year.R` for the analysis and figures
    - Run `004 meanDiff_Polar+Histogram_NA+EA--Fig.2.R` for the analysis and figures
    - Run `005 meanDiff-data_analysis--Table.S4.R` for the analysis

 以下未更改

3. 03 Evaluation of ATC model
    - Use ATC model to analysis the effects of vegetation following each phenologial events on LST 
    - Run `001 nlstest_LST_meanDiff_EA(NA)_test_for a single year.R` for each year ATC fitting
    - Run `002 meanDiff_6phase_common_EA(NA)_9year.R` to select pixels that have all ΔLST post six phenological events for each year.
    - Run `003 Diff_Polar+Histogram_NA+EA--Fig.2.R` for the analysis and figures
3. FluxNet analysis
    - Analyse autumn photosynthetic declines using flux tower measurements 
    - Run `FluxNet_analysis.Rmd` for the analysis and figures
4. Harvard analysis
    - Ground-sourced phenology observations for Eastern North America (Harvard forest)
    - Run `Harvard_models.Rmd` to create models and figures
5. PEP725 analysis
    - Ground-sourced phenology observations for Europe (PEP725 data)
    - Step 1: `1_Data_extraction` - download `www.PEP725.eu` data, GLDAS climate data and other info
    - Step 2: `2_Add_drivers` - generate seasonal climate and photosynthesis drivers
    - Step 3: `3_Analysis` - run models and generate figures
6. Remote sensing analysis
    - Analysis of satellite-derived phenology observations
    - Step 1: `1_Data_extraction` - extract photoperiod info
    - Step 2: `2_Add_drivers` - generate seasonal climate and photosynthesis drivers
        - EOS10 = Date when EVI last crossed 90% threshold
        - EOSstart = Date of maximum EVI curvature change rate (breakpoint)
        - EOS50 = Date when EVI last crossed 50% threshold
        - EOS85 = Date when EVI last crossed 15% threshold
    - Step 3: `3_Analysis` - run models and generate figures
        - `3.1` Check sample sizes, `3.2` add preseason temperatures, `3.3` run models and `3.4` create figures
        - EOS10 folder = `EOS10_SenescenceStart`
        - EOSstart folder = `EOSstart_VNP`
        - EOS50 foler = `EOS50_MidGreendown`
        - EOS85 folder = `EOS85_Dormancy`
