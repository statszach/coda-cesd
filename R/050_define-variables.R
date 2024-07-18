rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% "params")])
source(here::here("R", "010_paths-and-files.R"))
source(here::here("R", "020_libraries.R"))

# Import data

tracker <- readRDS(here::here(RDS_path, "010_tracker.rds"))
hrs1998_a <- readRDS(here::here(RDS_path, "010_hrs1998_a.rds"))
hrs1998_c <- readRDS(here::here(RDS_path, "010_hrs1998_c.rds"))
hrs2000_c <- readRDS(here::here(RDS_path, "010_hrs2000_c.rds"))
hrs2002_d <- readRDS(here::here(RDS_path, "010_hrs2002_d.rds"))
hrs2004_d <- readRDS(here::here(RDS_path, "010_hrs2004_d.rds"))
hrs2006_d <- readRDS(here::here(RDS_path, "010_hrs2006_d.rds"))
hrs2008_d <- readRDS(here::here(RDS_path, "010_hrs2008_d.rds"))
hrs2010_d <- readRDS(here::here(RDS_path, "010_hrs2010_d.rds"))
hrs2012_d <- readRDS(here::here(RDS_path, "010_hrs2012_d.rds"))
hrs2014_d <- readRDS(here::here(RDS_path, "010_hrs2014_d.rds"))
hrs2016_d <- readRDS(here::here(RDS_path, "010_hrs2016_d.rds"))
hrs2018_d <- readRDS(here::here(RDS_path, "010_hrs2018_d.rds"))
hrs2020_d <- readRDS(here::here(RDS_path, "010_hrs2020_d.rds"))
hrs2022_d <- readRDS(here::here(RDS_path, "010_hrs2022_d.rds"))
CODA_IDs <- readRDS(here::here(RDS_path, "040_CODA-IDS.Rds"))

hrs1998_scored <- hrs1998_c %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>% 
  dplyr::mutate(CESD1 = case_when(F1493 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  F1493 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(F1494 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  F1494 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(F1495 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  F1495 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(F1496 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  F1496 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(F1497 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  F1497 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(F1498 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  F1498 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(F1499 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  F1499 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(F1500 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  F1500 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2000_scored <- hrs2000_c %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(G1669 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  G1669 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(G1670 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  G1670 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(G1671 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  G1671 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(G1672 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  G1672 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(G1673 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  G1673 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(G1674 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  G1674 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(G1675 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  G1675 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(G1676 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  G1676 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8 )

hrs2002_scored <- hrs2002_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(HD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  HD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(HD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  HD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(HD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  HD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(HD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  HD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(HD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  HD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(HD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  HD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(HD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  HD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(HD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  HD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2004_scored <- hrs2004_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(JD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  JD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(JD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  JD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(JD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  JD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(JD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  JD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(JD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  JD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(JD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  JD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(JD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  JD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(JD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  JD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2006_scored <- hrs2006_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(KD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  KD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(KD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  KD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(KD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  KD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(KD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  KD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(KD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  KD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(KD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  KD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(KD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  KD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(KD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  KD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2008_scored <- hrs2008_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(LD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  LD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(LD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  LD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(LD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  LD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(LD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  LD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(LD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  LD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(LD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  LD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(LD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  LD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(LD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  LD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2010_scored <- hrs2010_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(MD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  MD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(MD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  MD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(MD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  MD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(MD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  MD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(MD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  MD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(MD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  MD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(MD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  MD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(MD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  MD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2012_scored <- hrs2012_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(ND110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  ND110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(ND111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  ND111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(ND112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  ND112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(ND113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  ND113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(ND114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  ND114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(ND115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  ND115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(ND116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  ND116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(ND117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  ND117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2014_scored <- hrs2014_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(OD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  OD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(OD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  OD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(OD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  OD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(OD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  OD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(OD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  OD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(OD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  OD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(OD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  OD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(OD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  OD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2016_scored <- hrs2016_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(PD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  PD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(PD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  PD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(PD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  PD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(PD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  PD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(PD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  PD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(PD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  PD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(PD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  PD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(PD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  PD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2018_scored <- hrs2018_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(QD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  QD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(QD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  QD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(QD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  QD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(QD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  QD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(QD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  QD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(QD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  QD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(QD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  QD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(QD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  QD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

hrs2020_scored <- hrs2020_d %>%
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::mutate(CESD1 = case_when(RD110 == 1 ~ 1,    # ...felt depressed 1 = Yes 5 = No
                                  RD110 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD2 = case_when(RD111 == 1 ~ 1,      # ...everything effort 1 = Yes 5 = No
                                  RD111 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD3 = case_when(RD112 == 1 ~ 1,      # ...sleep restless 1 = Yes 5 = No
                                  RD112 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD4 = case_when(RD113 == 1 ~ 0,      # ...were happy 1 = No, 5 = Yes
                                  RD113 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD5 = case_when(RD114 == 1 ~ 1,      # ...felt lonely 1 = Yes, 5 = No
                                  RD114 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD6 = case_when(RD115 == 1 ~ 0,      # ...enjoyed life 1 = No, 5 = Yes
                                  RD115 == 5 ~ 1,
                                  TRUE ~ NA_real_),
                CESD7 = case_when(RD116 == 1 ~ 1,      # ...felt sad 1 = Yes, 5 = No
                                  RD116 == 5 ~ 0,
                                  TRUE ~ NA_real_),
                CESD8 = case_when(RD117 == 1 ~ 1,      # ..could not get going 1 = Yes, 5 = No
                                  RD117 == 5 ~ 0,
                                  TRUE ~ NA_real_)) %>%
  dplyr::select( #get scored measures and depression items
    HRS_ID, CESD1:CESD8, mplusid) %>% 
  dplyr::mutate(CESDSUM = CESD1 + CESD2 + CESD3 + CESD4 + CESD5 + CESD6 + CESD7 + CESD8)

####

hrs1998_reduced <- hrs1998_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD98 = CESDSUM)

hrs2000_reduced <- hrs2000_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD00 = CESDSUM)

hrs2002_reduced <- hrs2002_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD02 = CESDSUM)

hrs2004_reduced <- hrs2004_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD04 = CESDSUM)

hrs2006_reduced <- hrs2006_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD06 = CESDSUM)

hrs2008_reduced <- hrs2008_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD08 = CESDSUM)

hrs2010_reduced <- hrs2010_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD10 = CESDSUM)

hrs2012_reduced <- hrs2012_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD12 = CESDSUM)

hrs2014_reduced <- hrs2014_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD14 = CESDSUM)

hrs2016_reduced <- hrs2016_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD16 = CESDSUM)

hrs2018_reduced <- hrs2018_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD18 = CESDSUM)

hrs2020_reduced <- hrs2020_scored %>% 
  dplyr::select(HRS_ID, CESDSUM) %>% 
  dplyr::rename(CESD20 = CESDSUM)

hrscesd_wide <- hrs1998_reduced %>% 
  left_join(hrs2000_reduced, by = "HRS_ID") %>% 
  left_join(hrs2002_reduced, by = "HRS_ID") %>% 
  left_join(hrs2004_reduced, by = "HRS_ID") %>% 
  left_join(hrs2006_reduced, by = "HRS_ID") %>% 
  left_join(hrs2008_reduced, by = "HRS_ID") %>% 
  left_join(hrs2010_reduced, by = "HRS_ID") %>% 
  left_join(hrs2012_reduced, by = "HRS_ID") %>% 
  left_join(hrs2014_reduced, by = "HRS_ID") %>% 
  left_join(hrs2016_reduced, by = "HRS_ID") %>% 
  left_join(hrs2018_reduced, by = "HRS_ID") %>% 
  left_join(hrs2020_reduced, by = "HRS_ID") 

tracker_demos <- tracker %>% 
  tidyr::unite("HRS_ID", HHID, PN, remove = F) %>% 
  left_join(CODA_IDs, by = "HRS_ID") %>%
  filter(HRS_ID %in% CODA_IDs$HRS_ID) %>%
  dplyr::select(HRS_ID, GENDER, HISPANIC, RACE, SCHLYRS, FAGE) %>% 
  dplyr::mutate(vdfem = dplyr::if_else(GENDER == 2, 1, 0),
                vdhisp = dplyr::case_when(HISPANIC == 0 ~ NA_real_,
                                          HISPANIC == 1 ~ 1,
                                          HISPANIC == 2 ~ 1,
                                          HISPANIC == 3 ~ 1,
                                          HISPANIC == 5 ~ 0),
                vdothrac = dplyr::if_else(RACE == 7, 1, 0),
                vdblack = dplyr::if_else(RACE == 2, 1, 0),
                vdedu = dplyr::na_if(SCHLYRS, 99),
                vdage = dplyr::na_if(FAGE, 999)) %>% 
  dplyr::select(HRS_ID, vdfem, vdhisp, vdothrac, vdblack, vdedu, vdage)