Please run the code line by line and the results will be generated.

Following steps will are being performed in the code:
1. Load Data
2. Check for missing values
3. Basic Cleaning of data
4. Group demand by product and take the one with largest demand for analysis
5. Basic preprocessing of the data of the product with largest demand.
6. Add the missing dates in the data and interpolate the values
7. Split into test and train 
8. Convert it to a time series, clean and plot it again
9. Determine a moving avaerage of 30 days
10. Deseasonalize the data
11. Check for stationarity (Dickey Fuller Test)
12. Plot ACF and PACF
13. Perform differencing.
14. Perform Dickey fuller test again
15. Plot ACF nad PACF for differenced series.
16. Fit Auto ARIMA model.
17. Perform the same steps for monthly data.
18. We fit another model on the montly data to get better results.
19. Evaluation of the results.
