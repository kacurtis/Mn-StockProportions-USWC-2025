# load libraries
library(magrittr)
library(lubridate)

# unlist data
sight <- mnid$sight
loccode <- mnid$loccode
biop <- mnid$biop

# edit field types, rename, select
sight %<>% rename(id = CRC.ID, loc = Loc.Code, year = Year, date = Date, 
                  vessel = Vessel, lat = Dec.Lat, lon = Dec.Long) %>%
                  # note that there are some records with date==NA but with a year
  mutate(id = as.character(id), loc = as.character(loc), 
         mm = month(date), mmlab = month(date, label = TRUE)) %>% 
  select(CRC.Record, Record.Source, Research.Group, Sharing.Status, Contributor, id, loc, 
         Locality, year, date, mm, mmlab, vessel, Sighting, Time, lat, lon, Position.Type, 
         Depth, Group.Size, Number.Calves, Group.Type, Group.Behavior, Behavioral.Role, 
         Field.ID, Sample.Number, AGEREP, Quality, CRC, Survey, Num.IDs, Comments, 
         Beh.Sex, Catalog.Date, Tag.Event, Tag.Type, Dead, SourceDB, DateTimeAdded, 
         HW_enc_id, HW_surveys, HW_ind_comments, HW_media_idq, HW_enc_admin_comments, HW_enc_loc_precision) %>% 
  arrange(date, id)
loccode %<>% rename(loc = Loc.Code, rgn = Region, subarea = Sub.area) %>% 
  mutate(loc = as.character(loc))
biop %<>% rename(id = CRC.ID, loc = Loc.Code, lat = Dec.Lat, lon = Dec.Long) %>% 
  mutate(id = as.character(id), loc = as.character(loc)) %>% 
  select(-c(Preservation, Reaction, Sampled.by, Sample.Photo, SourceDB, DateTimeAdded)) %>% 
  arrange(Date)

# merge regions from loccode to sight based on loc
sight %<>% left_join(loccode %>% select(loc, rgn, subarea) %>% distinct()) %>%
  select(1:7, rgn, subarea, everything())
