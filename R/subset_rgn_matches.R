# get matched IDs for each stock and USWC
## U.S. WEST COAST
### filter to US EEZ polygon
#### load US West Coast EEZ
load("USWestCoastEEZ.rda")
#### convert sight.uswc to sf
sight.sf <- sight %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = st_crs(useez.wc))
#### subset to points in USWC EEZ and convert back to df
sight.sf$ineez <- as.vector(st_intersects(useez.wc, sight.sf, sparse = FALSE))
sight.uswc <- bind_cols(st_drop_geometry(sight.sf[sight.sf$ineez,]),
                        st_coordinates(sight.sf[sight.sf$ineez,])) %>%
  select(-ineez) %>% rename(lon=X, lat=Y) %>% 
  # get rid of single remaining sighting just north of 49th parallel
  filter(lat <= 49) %>% 
  # keep only USWC sightings with date so can aggregate to monthly
  filter(!is.na(date)) %>%
  # create occ
  mutate(occ = as.numeric(year))
rm(useez.wc)

## Select sightings in wintering areas and filter to winter
sight <- bind_cols(st_drop_geometry(sight.sf), st_coordinates(sight.sf)) %>%
  rename(lon=X, lat=Y)
sight.wg <- sight %>% filter(!ineez & lat<33) %>% 
  select(-ineez) %>% 
  # eliminate non-winter sightings south of the Mexican border (includes Hawaii)
  filter(!(mm %in% 6:10)) %>% 
  # create occ to shift Nov/Dec into next year
  mutate(occ = if_else(mm %in% 11:12, year+1, as.numeric(year)))
rm(sight.sf)

## HAWAII
sight.hi <- sight.wg %>%
  # filter Hawaii sightings
  filter(rgn=="Hawaii" | (lat>15 & lat<30 & lon>(-180) & lon<(-150))) %>% 
  # filter to only those IDs matched along USWC, for which we reliably have HI data and that we know use USWC
  filter(id %in% sight.uswc$id) %>% 
  # add labels
  mutate(stock="HI", abbr="H")

## CENTRAL AMERICA / SOUTHERN MEXICO - CA/OR/WA
### create list of IDs seen in southern hemisphere
match.sh <- sight %>% filter(subarea=="Peru-Antarc") %>% select(id)
### select CASM sightings
sight.casm <- sight.wg %>%
  # filter Central America and S Mexico sightings
  filter((lat<18 & lon>(-102.2) & lon<(-75)) | 
           (is.na(lat) & rgn %in% c("Central America", "S Mexico"))) %>% 
  # eliminate IDs that match to southern hemisphere
  filter(!(id %in% match.sh$id)) %>% 
  # add labels
  mutate(stock="CASM", abbr="C")
sight.cam <- sight.casm %>%
  # filter Central America only sightings
  filter((lat<14.5) | (is.na(lat) & rgn %in% c("Central America")))

## MEXICO
sight.mex.all <- sight.wg %>%
  # filter Mexico sightings 
  filter(lat>18 & lat<33 & lon>(-120) & lon<(-100)) %>% 
  # exclude Pacific Coast of Mainland Mexico including southern Jalisco, Colima, and Michoacan
  filter(!(lat<20 & lon>(-106))) %>%
  # # filter out Revillagigedo
  # filter(!(lat<21 & lon<(-108))) %>% 
  # # filter only/out Baja (add "!" to filter out)
  # filter((lat>20 & lon<(-107.5))) %>%
  # filter to only those IDs matched along USWC and CASM, for which we reliably have MX data and that we know use USWC
  filter((id %in% sight.uswc$id) | (id %in% sight.casm$id))
### MAINLAND MEXICO - CA/OR/WA
sight.mxcow.omnisc <- sight.mex.all %>% 
  filter(!(id %in% sight.casm$id)) %>% 
  filter(!(id %in% match.nw$id)) %>%
  # add labels
  mutate(stock="MX-COW", abbr="M")
sight.mxcow <- sight.mex.all %>% 
  filter(!(id %in% (sight.casm %>% filter(occ %in% yy.wg))$id)) %>% 
  filter(!(id %in% match.nw$id)) %>%
  # add labels
  mutate(stock="MX-COW", abbr="M")
### MEXICO - NORTH PACIFIC 
sight.mxnp.omnisc <- sight.mex.all %>% 
  filter(!(id %in% sight.casm$id)) %>% 
  filter(id %in% match.nw$id) %>%
  # add labels
  mutate(stock="MX-NP", abbr="P")
sight.mxnp <- sight.mex.all %>% 
  filter(!(id %in% (sight.casm %>% filter(occ %in% yy.wg))$id)) %>% 
  filter(id %in% match.nw$id) %>%
  # add labels
  mutate(stock="MX-NP", abbr="P")
## Eliminate individuals seen in more than one wintering area and including Hawaii
ids.mstock <- sight.hi %>% filter((id %in% c(sight.casm$id, sight.mex.all$id))) %>% 
  select(id) %>% distinct() %>% unlist()
sight.hi <- sight.hi %>% filter(!(id %in% ids.mstock))
sight.casm <- sight.casm %>% filter(!(id %in% ids.mstock))
sight.cam <- sight.cam %>% filter(!(id %in% ids.mstock))
sight.mex.all <- sight.mex.all %>% filter(!(id %in% ids.mstock))
sight.mxcow <- sight.mxcow %>% filter(!(id %in% ids.mstock))
sight.mxnp <- sight.mxnp %>% filter(!(id %in% ids.mstock))
sight.mxcow.omnisc <- sight.mxcow.omnisc %>% filter(!(id %in% ids.mstock))
sight.mxnp.omnisc <- sight.mxnp.omnisc %>% filter(!(id %in% ids.mstock))
## subset years of interest
sight.uswc.yy <- sight.uswc %>% filter(year %in% yy.wc)
match.hi.yy <- sight.hi %>% filter(occ %in% yy.wg) %>% 
  select(id, stock) %>% distinct() 
match.cam.yy <- sight.cam %>% filter(occ %in% yy.wg) %>% 
  select(id, stock) %>% distinct()
match.casm.yy <- sight.casm %>% filter(occ %in% yy.wg) %>% 
  select(id, stock) %>% distinct()
match.mex.all.yy <- sight.mex.all %>% filter(occ %in% yy.mex) %>% 
  select(id) %>% distinct()
match.mxcow.yy <- sight.mxcow %>% filter(occ %in% yy.mex) %>% 
  select(id, stock) %>% distinct()
match.mxnp.yy <- sight.mxnp %>% filter(occ %in% yy.mex) %>% 
  select(id, stock) %>% distinct()

# create data frame with stock assignments
df.stock.yywg <- bind_rows(match.hi.yy, match.casm.yy, match.mxcow.yy, match.mxnp.yy)
df.stock.all <- bind_rows(sight.hi, sight.casm, sight.mxcow.omnisc, sight.mxnp.omnisc) %>%
  select(id, stock) %>% distinct() %>%
  rename(stockall=stock)
