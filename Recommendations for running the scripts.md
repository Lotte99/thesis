The Python and R scripts follow the following order:
1. Run Python 10-K annual reports_v8.py to get the business sections of 10-K reports. Be aware that this script requires significant running time (for me +30 hours).
2. Run Preprocessing.R, phase 1. Not yet the rest.
   2a. Use files "extracted_business_sections_2000-2020.csv", "extracted_business_sections_2020-2023.csv", "WRDS_ticker_data.csv", "WRDS_ROA_data.csv", "WRDS_CIK_data.csv",
       "LSEG_ESG_scores.xlsx", "data.LSEG.CSwalk.xlsx".
3. Run Seeded_LDA.R and Talk Quantity.R. Be careful not to run separately from each other, as they contain some of the same object names, which may have different codes.
   In this case, the resulting variables and topic distributions may not align with the thesis' methodology.
   3a. Use "A_data.csv" if you did not run or save phase 1 from Preprocessing.R.
4. Return to Preprocessing.R and run phase 2-8. Talk Quantity and the topic distributions from the previous step are added to the data in phase 2 of Preprocessing.R
5. Run Regression_models.R.
   5a. Use "data.standardized.csv" if you did not run or save the preprocessed data from phase 7 in Preprocessing.R.
