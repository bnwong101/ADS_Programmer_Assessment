# ADS_Programmer_Assessment
This repository contains my submission for the ADS Programmer Assessment. It demonstrates SDTM derivation, ADaM dataset creation, and safety TLG generation using R.

The project is organized by question for clarity and reproducibility.

question_1_sdtm
-> 01_create_ds_domain.R: Derives the SDTM Disposition (DS) domain
-> question_1_sdtm_dataset.csv: Output dataset

-> Logs included as 01_create_ds_domain_logs

question_2_adam
-> create_adsl.R: Derives subject-level ADSL dataset
-> question_2_adam_dataset.csv: Final ADSL output

-> Logs included as create_adsl_logs

question_3_tlg
-> 01_create_ae_summary_table.R: Generates AE summary table
-> Output: 01_ae_summary_table.html

-> 02_create_visualizations.R: Generates ggplot2 visualizations for adverse events reporting
-> Output: ae_severity_distribution.png & ae_frequency_top10.png

-> Logs are included for both R scripts.
