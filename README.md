
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Replication Material for the Paper “Distribution Regression for Partially Observed Discrete, Ordered Outcomes - The Case of Adolescent Substance Abuse”

<!-- badges: start -->
<!-- badges: end -->

The code in this repository generates the results of the empirical
application of the working paper Gutknecht, D., Liu, C. and Wermuth,
J.-L. (2024), ‘Distribution Regression for Partially Observed Discrete,
Ordered Outcomes - The Case of Adolescent Substance Abuse’ available on
[SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4904286).

## Data Availability

We unfortunately do not have the licence to make the data publicly
available. Therefore, replication is only possible by downloading the
20219 data from
[YRBS](https://www.cdc.gov/yrbs/data/national-yrbs-datasets-documentation.html).
The YRBS data consists of repeated cross-sectional samples and contains
information about the consumption behavior of illicit substances among
high-school students across U.S. states. On the other hand, data on
additional state-level control variables, which is also used in the
empirical analysis, can be found in the folder ‘data’.

## Application

The files for the applications are available in the folder
‘applications’. To replicate the results from the paper, proceed as
follows:

1.  Execute the file ‘DataPreparation.R’ to merge the YRBS data with the
    other data files.

2.  The file ‘Misreporting.R’ performs the estimation described in the
    paper in equation (11).

3.  The files ‘MisreportingY.R’ and ‘MisreportingC.R’ perform all the
    remaining calculations that are needed to produce the respective
    plots for Y and C with misreporting. The functions needed in those
    scripts are collected in the files ‘MisreportingY_functions.R’ and
    ‘MisreportingC_functions.R’.

4.  The file ‘NoMisreporting.R’ replicates the results for the model
    without misreporting calling the necessary functions in the script
    ‘NoMisreporting_functions.R’.
