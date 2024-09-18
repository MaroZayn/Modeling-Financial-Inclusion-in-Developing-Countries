# Modeling-Financial-Inclusion-in-Developing-Countries

## Overview
This project analyzes the relationship between digital finance and financial inclusion in developing countries using data from the World Development Indicators (WDI) and the World Bank’s Global Findex. We conducted econometric estimations using both OLS (Ordinary Least Squares) and instrumental variable methods to study the impact of various socio-economic factors on financial inclusion.

## Data Collection
1. **Developing Countries**: We referred to the list of 152 developing countries provided by the United Nations.
2. **Data Availability**: Due to missing data, we reduced the sample to less than 150 countries for 2018, and after further filtering, the final sample consists of 92 countries.
3. **Data Sources**: Data was gathered from multiple databases:
   - **World Development Indicators (WDI)**: Various socio-economic variables.
   - **Global Findex**: Data on financial inclusion.

## Variables Used
- **country_name**: Names of the countries selected for this study.
- **deposit_account**: Share of the population with a deposit account.
- **loans**: Share of the population with loans.
- **savings**: Share of the population with savings.
- **digital_fin**: Digital finance usage captured through internet or mobile usage for financial transactions.
- **acces_elec**: Share of the population with access to electricity.
- **employ_agri**: Share of agricultural employment.
- **self_employ**: Share of self-employment.
- **foreign_DI**: Foreign direct investment as a share of GDP.
- **gdp**: Gross domestic product (GDP) at constant 2015 prices.
- **CPI**: Consumer price index, used to capture inflation.
- **urban_pop**: Urban population.
- **rural_pop**: Rural population.
- **total_pop**: Total population.
- **pop_density**: Population density.
- **HCI (Human Capital Index)**: A measure between 0 and 1 capturing the level of education and access to healthcare.
- **FI_Index (Financial Inclusion Index)**: An index (0-1) constructed following the method of Oumarou and Celestin (2021).

## Data Preprocessing
- **Transformation of Variables**: We converted variables into appropriate formats (e.g., factors, numeric, date objects) to prepare them for analysis.
- **Handling Missing and Outlier Values**:
  - Missing values in some variables like `sesso` (gender) were replaced with "Unknown".
  - Outliers in variables like birth year (`data di nascita`) were handled.
- **Merging Data**: After preprocessing, we merged the datasets and encoded binary variables to create a clean dataset (`base_esti`) for our estimations.

## Model Estimation

### Ordinary Least Squares (OLS)
We estimated four models using OLS to examine the different dimensions of financial inclusion:
1. **Model 1**: Deposit account as the dependent variable.
2. **Model 2**: Loans as the dependent variable.
3. **Model 3**: Savings as the dependent variable.
4. **Model 4**: Financial Inclusion Index as the dependent variable.

We ran diagnostics on normality and goodness-of-fit for each model, including residual analysis and the Breusch-Pagan test for heteroskedasticity.

### Instrumental Variables (IV) Estimation
We re-estimated the models using an instrumental variable approach (2SLS) with the following instruments:
- **log_urban_pop** (urban population in log form).
- **log_pop_density** (population density in log form).

### Variables in Logarithmic Form
Some variables were transformed into logarithmic form to interpret results in terms of elasticity. These include:
- **log_dep_account**: Log of deposit accounts.
- **log_loans**: Log of loans.
- **log_savings**: Log of savings.
- **log_digital_fin**: Log of digital finance usage.
- **log_gdp**: Log of GDP.
- **log_CPI**: Log of consumer price index.
- **log_urban_pop**: Log of urban population.
- **log_pop_density**: Log of population density.

## Conclusion
Our models provide insights into the key determinants of financial inclusion in developing countries, with digital finance playing a significant role. By improving access to digital financial services and infrastructure (e.g., electricity), governments can promote greater financial inclusion.
