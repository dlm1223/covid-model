# Models for projecting future Covid cases
Run the following 2 scripts to get model projections:

- 1-organize data.R which combines various external datasets with the Johns Hopkins data, such as demographic datasets from ourworldindata.org, World Bank, Covid testing data, and more. In addition, the script calculates various lagged-stats and other features.

- 2-model fitting.R which trains various models to project future cases, fatalities, and recoveries
- These models are:
	1. GBM model to project days 1, 2, 3 etc in future using various lagged stats and demographic variables
	2. Linear model to project days 1, 2, 3 etc in future using various lagged stats and demographic variables
	3. Geometric series model based on past observations only (inspired by [this Kaggle kernel](https://www.kaggle.com/gaborfodor/covid-19-w3-a-few-charts-and-submission)) 
	4. ARIMA model based on past observations only (inspired by [this Kaggle kernel](https://www.kaggle.com/neg0000273/auto-arima-week-4)) 
- Models are then ensembled based on previous predictive performance 
