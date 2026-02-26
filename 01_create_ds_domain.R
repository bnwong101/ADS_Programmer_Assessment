#ADS Programmer Coding Assessment
#Question 1 - Brandon Wong

#Install packages
#install.packages(c("admiral", "sdtm.oak", "gt", "ggplot2", "pharmaverseraw", "pharmaversesdtm"))


#Read in data
library(sdtm.oak)
library(pharmaverseraw)
library(dplyr)
library(pharmaversesdtm)

#Read in DS raw data
ds_raw <- pharmaverseraw::ds_raw
View(ds_raw)

#Read in DM domain
dm <- pharmaversesdtm::dm
View(dm)


#Inspect data
colnames(ds_raw)
head(ds_raw, 10)
colnames(dm)
head(dm, 10)

#IT.DSSDAT misnamed as per eCRF for ds_raw
names(ds_raw) [names(ds_raw) == "IT.DSSTDAT"] <- "IT.DSSDAT"

#Check
colnames(ds_raw)

#Create oak_id_vars
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

View(ds_raw)


study_ct <- read.csv("sdtm_ct.csv")

ds <- 
  # Derive topic variable
  # Map DSTERM using assign_no_ct, raw_var = IT.DSTERM, tgt_var = DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  # Map DSTERM when OTHERSP is not N/A using assign_no_ct, raw_var = OTHERSP, tgt var = DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) 

#Validation check
View(ds)

ds <- ds %>%
  #Map DSSTDTC using assign_datetime, raw_var = IT.DSSDAT
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    raw_unk = c("UN","UNK"),
    id_vars = oak_id_vars()
  ) %>%
  #Map DSDECOD using assign_ct, raw_var = IT.DSDECOD
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  #Map DSDECOD when OTHERSP is not N/A using assign_no_ct, raw_var = IT.DSDECOD
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  #Map DSDTC using assign_datetime, raw_var = DSDTCOL + DSTMCOL
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M"),
    raw_unk = c("UN","UNK"),
    id_vars = oak_id_vars()
  ) %>%
  #Map DSCAT using hardcode_no_ct, raw_var = IT.DSDECOD
  #hardcoded value "DISPOSITION EVENT" when IT.DSDECOD != "Randomized"
  hardcode_no_ct(
    tgt_val = "DISPOSITION EVENT",
    raw_dat = condition_add(dat = ds_raw, IT.DSDECOD != "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
  ) %>%
  #Map DSCAT using hardcode_no_ct, raw_var = IT.DSDECOD
  #hardcoded value "PROTOCOL MILESTONE" when IT.DSDECOD = "Randomized"
  hardcode_no_ct(
    tgt_val = "PROTOCOL MILESTONE",
    raw_dat = condition_add(dat = ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
  ) %>%
  #Map DSCAT using hardcode_no_ct, raw_var = OTHERSP
  #hardcoded value "OTHER EVENT" when OTHERSP != NA
  hardcode_no_ct(
    tgt_val = "OTHER EVENT",
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISIT from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # Map VISITNUM from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )
  
#Validation check
View(ds)

#Create SDTM Derived variables
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    DSTERM = toupper(DSTERM),
    DSDECOD = toupper(DSDECOD),
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_var = c("USUBJID", "DSTERM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", 
    "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )

View(ds)
write.csv(ds, file = "question_1_sdtm_dataset.csv")
