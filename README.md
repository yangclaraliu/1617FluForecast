# 1617FluForecast
## 4wk_Results
contains forecast plots 4 weeks into the future at the time of forecast.
## Environmental Model 1
is the environmental model that contains only local public health and environmental information.
### Run Modules
1. Run **fore_tab_all_v1.R**
2. Run **fore_tab_v1.R**
### How to update Environmental Dataset using MERRA-2
1. Run **update-Environmental_v1.R**: Read new data in the format of "nc4", extract targeted environmental variables: TS_min, TS_mean, TS_max, T2M_min, T2M_mean, T2M_max, T2MDEW_mean, QV2M. By the end of this step, the environmental variables are in the daily scale ("recordx.RData").
2. Run **dialyweekly_conversion_v1.R**: Read daily environmental data and save as weekly data (weekly_environmentx.RData).
3. Run **timealign_va.R**: Match the time steps of the environmental data and the health data.
## Forecast Comparison
is after the forecast results are returned by CDC, plots on how our result compares to other models.
## Parabola
J. Lega's approach and some variations.
## SARIMA model
is a simpler time series model for influenza forecasts.
## Submisison
contains all submission sheets.
