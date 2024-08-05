
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Replication Material for the Paper “Distribution Regression for Partially Observed Discrete, Ordered Outcomes - The Case of Adolescent Substance Abuse”

<!-- badges: start -->
<!-- badges: end -->

The code in this repository generates the results of the empirical
application of the working paper Gutknecht, D., Liu, C. and Wermuth,
J.-L. (2024), Distribution Regression for Partially Observed Discrete,
Ordered Outcomes - The Case of Adolescent Substance Abuse available on
[SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4904286).

## Data Availability

We unfortunately do not have the licence to make the data publicly
available. Therefore, replication is only possible by downloading the
2021 data from
[YRBS](https://www.cdc.gov/yrbs/data/national-yrbs-datasets-documentation.html).
The data based on which the covariates are constructed are in the folder
‘data’.

## Application

The files for the applications are available in the folder
‘applications’. The file ‘DataPreparation.R’ merges the YRBS data with
the other datasets. The files with the ending ’\_functions.R’ contain
all the functions necessary to perform the analysis in ‘Misreporting.R’,
where our model is estimated, and ‘NoMisreporting.R’, where the
competing model without the possibility to misreport is estimated.
