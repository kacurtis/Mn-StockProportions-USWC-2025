sight %<>% mutate(year = if_else(is.na(date), year, as.integer(year(date)))) %>% 
  # drop records not yet merged with HW
  filter(is.na(DateTimeAdded) | DateTimeAdded < "2025-01-14") %>% 
  # add field indicating whether Record Source is HW
  mutate(hw=grepl("HW",Record.Source)) %>% 
  # filter out poor quality (Q=3,4 or PQ, idq=1 or 2 without mitigating Q from double scoring yrs, or HW source and idq=0)
  filter((!hw & (is.na(Quality) | !(Quality %in% c("3","4","5","PQ","3 PQ","3 pq")))) | 
           (hw & ((is.na(HW_media_idq) & (is.na(Quality) | Quality %in% c("A","B",1:2))) | 
                    ((HW_media_idq %in% 1:2) & Quality %in% c("A","B",1:2)) | 
                    HW_media_idq %in% 3:5))) %>% 
  # filter out is.na(year)
  filter(!is.na(year)) %>% 
  # drop Dead animals
  filter(!Dead) %>% 
  # correct any negative longitudes after filtering out Japan and Russia
  filter(!(loc %in% c("6","91","97"))) %>% 
  mutate(lon = if_else(lon>0, -1*lon, lon)) %>% 
  # eliminate sightings south of Panama
  filter(lat>7.2) %>% 
  # group by id and date for filtering and create variables with which to identify problems
  group_by(id, year, date) %>%   # include year for grouping in case date missing
  mutate(nidd=n()) %>% 
  # drop records with NA positions on days with known-position record
  filter(nidd==1 | all(is.na(lat)) | !is.na(lat)) %>%
  # recalculate records per day
  mutate(nidd=n()) %>% 
  # keep only "PRECISE" and "Precise" when "GENERAL" or "General" or "Estimated" in same day
  mutate(postype = any(Position.Type %in% c("GENERAL","General","Estimated")) & any(Position.Type %in% c("PRECISE","Precise"))) %>%
  filter((postype & Position.Type %in% c("PRECISE","Precise")) | !postype) %>% 
  ungroup()
