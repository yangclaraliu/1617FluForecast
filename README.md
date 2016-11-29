# 1617FluForecast
## How to update environmental database from MERRA-2 online database
1. Run **update-Environmental_v1.R**: Read new data in the format of "nc4", extract targeted environmental variables: TS_min, TS_mean, TS_max, T2M_min, T2M_mean, T2M_max, T2MDEW_mean, QV2M. By the end of this step, the environmental variables are in the daily scale ("recordx.RData").
2. Run **dialyweekly_conversion_v1.R**: Read daily environmental data and save as weekly data ("weekly_environmentx.RData").
3. Run **timealign_va.R**: Match the time steps of the environmental data and the health data ("timealignx.RData").

## How to update model in use
4. Run **model_selected.R** one 3 is renewed. The output of this step will be a new set of model and a new set of variable lags.
5. Run **fore_tab_v1.R** to get an updated regression table for prediction.

## How to get run model for results if no environmental data needs to be updated
6. Run **fore_tab_all_v1.R**: this step creates environmental dataset. Permutation is done through bootstrapping.
7. Run **fore_tab_v1.R**: this step draws the regression table.
8. Run **runModel.R**: core to results generation.
9. Run **WriteTableUUS.R** fills in the sumbission for HHS.
10. Run **WriteTableNational.R** does the same as 9 only on the national level. This step also reads a model that translates from regional to national level ILI\%.

## How to look at the results
* 4wk_Results: created as a set of forecast is generated; contains forecast plots 4 weeks into the future at the time of forecast.
* Forecast_Comparison: created when a set of forecast is returned after that week's true ILI\% has been published. This folder cotnains result of comparing UMN model performance with all other competing models. 
* Submission: contains all submission sheets.

## On-going models
*
**Environmental Model 1**: is the environmental model that contains only local public health and environmental information.
*
**SARIMA Model**: is the modified SARIMA model.
*
**PArabola**: Parabola model proposed by Dr. J.Lega
