#### FMD STANDSTILL SCRIPT ####
# michael deason: 2015
# Used for:
# processing the cattle and sheep movement networks 
# identifying exemptions  
# rewiring the network 
# checking standstill  
# creating input for further FMD modeling 

#### government standstill exemptions ####
# record those movements breaking the standstill legally from those that aren't
# Animals between premises occupied by the same person or business
# Animals from any of the Scottish Island areas going to a market
# Animals moving direct from premises to a slaughterhouse
# Animals moving direct from premises to a market for animals intended for immediate slaughter
# Calves under 30 days old from holding of birth moving for fostering
# Imported animals; import locations all converted to 0 in the data
# Exported animals; not important
# Animals to a show/exhibition

#setwd('C:/Users/Michael/SAM') 
#rm(list=ls()) # clears objects
#rm(list=ls(all=TRUE)) # clears environment if needed
#load('fmd_standstill_test.RData') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### constrain to 2010-2011 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

x2010 <- as.Date('01/01/2010', format='%d/%m/%Y')
y2011 <- as.Date('31/12/2011', format='%d/%m/%Y')

library(lubridate) # used to manipulate dates
library(plyr) # used for the count function; much faster than as.data.frame(table())
library(raster) # used to crop the map
library(rgeos) # uses to calculate the centroid of a spatialpolygon; here, parishes

#### animals info ####
# used to determine animal type and calculate age of animal at time of movement
# imported animals assumed to be over 30 days old
#~~~~#
# animals2.txt created in UNIX using raw CTS data
# sed '1, 18d' animals | head -n -7 > animals2.txt # removes header and footer

# animals <- read.delim(file = '../cts201404/animals2.txt', 
#                       na.strings='\\N',
#                       fill = TRUE, # allows rows to have different lengths
#                       header=FALSE,
#                       stringsAsFactors=FALSE)
# names(animals) <- c('animal.id', 'sex', 'breed', 'eartag', 'cts.indicator', 'birth.date', 'death.date', 'import.country.code', 'import.date')
# animals$birth.date <- as.Date(animals$birth.date) # convert birth date to class date
# animals$death.date <- as.Date(animals$death.date) # convert death date to class date
# animals$import.date <- as.Date(animals$import.date) # convert import date to class date
# save(animals, file='animals.RData') # save in order to allow for quicker loading time just in case
load('animals.RData') # nrow=45360053

#### process animal info ####  

animals.temp1 <- animals[animals$death.date >= x2010 - days(280) & animals$birth.date > y2011, ] # remove animals that have died before start date or born after end date; extra 9 months for bulls
animals.temp1 <- animals.temp1[!is.na(animals.temp1$animal.id), ] # remove any rogue NAs introduced through subsetting
animals.temp2 <- animals[is.na(animals$death.date), ] # animals that should still be alive

animals.new <- rbind(animals.temp1, animals.temp2) # stack two data.frames
animals.new <- animals.new[!is.na(animals.new$animal.id), ] # nrow = 8284601
rm(animals, animals.temp1, animals.temp2)

#### bulls ####
# use preprocessed animals file to generate a list of bull ids
# 280 days before the bull is first recorded as a sire
#~~~~#
# bulls_ids.txt created in UNIX from raw CTS animal relationships data
# grep CS animal_relationships > bull_1.txt # returns a list of relationships with a bull
# cut -f5, 8 bulls_1.txt > bulls_ids.txt # create list of bull animal ids

bulls.ids <- read.delim('../cts201404/bulls_ids.txt', 
                        header=FALSE, 
                        fill = TRUE, # rows have different lengths
                        na.strings='\\N',
                        stringsAsFactors=FALSE)
names(bulls.ids) <- c('animal.id', 'relationship.date')
bulls.ids$relationship.date <- as.Date(bulls.ids$relationship.date, format='%Y-%m-%d')
bulls.ids <- bulls.ids[complete.cases(bulls.ids), ] 
bulls.ids <- bulls.ids[order(bulls.ids$relationship.date), ] # sort by dates
sire.date <- bulls.ids[!duplicated(bulls.ids$animal.id), ] # take the earliest date for each bull by removing all duplicated entries; nrow = 139666 

bulls.info <- animals.new[animals.new$animal.id %in% sire.date$animal.id, ] # subset male cattle ids for the reported bull ids
bulls.info <- merge(bulls.info, sire.date, all.x=TRUE) # nrow = 29230
bulls.info <- bulls.info[c('animal.id', 'birth.date', 'death.date', 'import.date', 'relationship.date')] # subset for relevant columns
names(bulls.info) <- c('animal.id', 'birth.date', 'death.date', 'import.date', 'first.bull')
bulls.info$bull.date <- bulls.info$first.bull - days(280) # 280 days before first birth of a calf; gestation is normally 277
rm(bulls.ids, sire.date)

#### location data ####

sam.coords <- read.csv('C:/Users/Michael/cts201404/locations.csv', header=TRUE, stringsAsFactors=FALSE) # read in location information
sam.coords <- sam.coords[c('location_id', 'premises_type_code', 'cph', 'x', 'y')] # subset for only these columns; 133108 obs
sam.coords.table <- count(sam.coords, c('cph'))
sam.coords.table <- sam.coords.table[sam.coords.table$cph != '', ]
sam.coords.table.duplicates <- sam.coords.table[sam.coords.table$freq > 1, ]$cph
sam.coords.duplicates <- sam.coords[sam.coords$cph %in% sam.coords.table.duplicates, ] # 47 obs; 15 unique cphs
sam.coords.duplicates <- sam.coords.duplicates[!sam.coords.duplicates$premises_type_code == '', ] # remove cphs without premises types

sam.coords.no.duplicates <- sam.coords[!sam.coords$cph %in% sam.coords.duplicates$cph, ] # remove the duplicated cphs from sam.coords, replace with sam.coords.duplicates
sam.coords <- rbind(sam.coords.duplicates, sam.coords.no.duplicates) # 133076
rm(sam.coords.duplicates, sam.coords.no.duplicates, sam.coords.table, sam.coords.table.duplicates)

sam.coords$cph <- substr(sam.coords$cph, 1, nchar(sam.coords$cph) - 3)
sam.coords$cph <- ifelse(sam.coords$cph == '', '00/000/0000', sam.coords$cph)

# for missing coords for 89/719/0038, use googled eastings and northings for postcode FK8 3QS
# 247915, 699991
# use parish centroid eventually
sam.coords[sam.coords$cph == '89/719/0038', ]$x <- 247915
sam.coords[sam.coords$cph == '89/719/0038', ]$y <- 699991

#### determine purpose of animals ####
# Gates & Woolhouse 2015
# diary breeding = female, at least one calf, lived longer than 30 months, breed
# beef breeding = female, at least one calf, lived longer than 30 months, breed
# store = male store, female store, bulls

#### dames ####
# store cattle are either males or females that won't give birth
#~~~~#
# dam_CB_ids.txt created in UNIX from raw CTS animal relationships data
# grep CB animal_relationships > dam_CB.txt # calf births
# cut -f5 > dam_CB_ids # list of birth dam animal ids

birth.dams <- read.delim('../cts201404/dam_CB_ids.txt', 
                         header=FALSE, 
                         fill = TRUE, # rows have different lengths
                         na.strings='\\N',
                         stringsAsFactors=FALSE)

store <- animals.new[!animals.new$animal.id %in% birth.dams$V1, ] # not dams 
dams <- animals.new[animals.new$animal.id %in% birth.dams$V1, ] # cattle that are registered as having given birth; n.b. 1850 males, though likely a databasing error
rm(birth.dams) 

# BSE rule is 30 months; produces NA if birth.date+30months is not a calendar day e.g. 31 Feb.
# google says 30 months is 913.1 days, use 914 days from birth for BSE date
dams <- dams[!is.na(dams$birth.date), ] # 4458651 obs; 10459 without birth.date; only 2 were imports from 1996
dams$bse <- dams$birth.date + days(914) # assign BSE date
dams$death.date[is.na(dams$death.date)] <- as.Date("20/02/2020", "%d/%m/%Y") # assign arbitrary death date to dams without a death date 
dams$store.dam <- ifelse(dams$bse > dams$death.date, TRUE, FALSE) 

store.dam <- dams[dams$store.dam, ] # 18582 obs
store.stack <- rbind(store, store.dam[names(store.dam) %in% names(store)]) # 14295151 obs
rm(store, store.dam)

not.store.dam <- dams[!dams$store.dam, ] # 4440069 obs


breeds <- read.csv('C:/Users/Michael/cts201404/breed.csv',
                   header=TRUE,
                   stringsAsFactor=FALSE)
dairy.breeds <- breeds[breeds$breed_type == 'D', ]$breed_code
dairy.dams <- not.store.dam[not.store.dam$breed %in% dairy.breeds, ] # 2446683 obs
beef.dams <- not.store.dam[!not.store.dam$breed %in% dairy.breeds, ] # the rest; 1993386 obs
rm(not.store.dam, dams, breeds, dairy.breeds)

#### movements ####
# cattle movement data
#~~~~#
# movements_indirect.txt created in UNIX from the raw CTS movement data
# includes the transitional location
# sed '1, 18d' movements.txt | head -n -7 > movements2.txt # removes header and footer; 299049870 lines
# cut -f2, 4, 5, 6, 7, 8, 9 movements2.txt > movements3a.txt # cuts animal.ids, date, off.id, is.trans, trans.id, on.id and is.birth
#  sed 's/\\N/NA/' movements3a.txt | # replaces //N with NA; done multiple times just in case there are multiple NAs in the row
#   sed 's/\\N/NA/' | # replaces //N with NA
#   sed 's/\\N/NA/' | # replaces //N with NA
#   awk '$4 != "t" { next } { print }' | # only keep is.trans=t
#   awk '$6 == "NA" { next } { print }' | # removes lines without on location
#   awk '$3 == $6 { next } {print}' | # if out == in, skip
#   awk '$7 == "t" { next } {print}' | # removes births
#   cut -f1, 2, 3, 5, 6 > movements_indirect.txt # cut out useful columns
#~~~~#

movements <- read.table("C:/Users/Michael/cts201404/movements_indirect.txt", 
                        header=FALSE, 
                        stringsAsFactors=FALSE)
names(movements) <- c("animal.id", "date", "off", "trans", "on")
movements$date <- as.Date(movements$date, format='%Y-%m-%d')
movements <- movements[movements$date >= x2010 & movements$date <= y2011, ] # constrain movements to 2010-2011 

names(sam.coords) <- c('location_id', 'premises_type_code', 'cph', 'x', 'y')
other.types.id <- sam.coords[sam.coords$premises_type_code %in% c('AI', 'CA', 'CC', 'EX', 'SG'), ]$location_id
movements <- movements[!movements$off %in% other.types.id, ]
movements <- movements[!movements$on %in% other.types.id, ]
rm(other.types.id)

###

slaughter.ids <- sam.coords[sam.coords$premises_type_code %in% c('CR', 'CS', 'HK', 'KY', 'IP', 'MP', 'PP', 'SM', 'SR', 'SW'), ]$location_id
new.movements <- movements[!movements$on %in% slaughter.ids, ] # remove movements to slaughter locations
new.movements <- new.movements[!new.movements$off %in% slaughter.ids, ] # remove movements from slaughter locations
rm(movements, slaughter.ids)

#### add cphs to movements ####

names(sam.coords) <- c('off', 'premises_type_code.off', 'cph.off', 'off.x', 'off.y') 
new.movements <- merge(new.movements, sam.coords[c('off', 'cph.off')], all.x=TRUE)
names(sam.coords) <- c('on', 'premises_type_code.on', 'cph.on', 'on.x', 'on.y') 
new.movements <- merge(new.movements, sam.coords[c('on', 'cph.on')], all.x=TRUE)
names(sam.coords) <- c('trans', 'premises_type_code.trans', 'cph.trans', 'trans.x', 'trans.y') 
new.movements <- merge(new.movements, sam.coords[c('trans', 'cph.trans')], all.x=TRUE)

#### combine animal production information with movements

new.movements$production.type <- ifelse(new.movements$animal.id %in% beef.dams$animal.id, 'beef.dam',
                                        ifelse(new.movements$animal.id %in% dairy.dams$animal.id, 'dairy.dam', 'store'))
rm(beef.dams, dairy.dams, store.stack)

#### bull movements ####

bull.movements <- new.movements[new.movements$animal.id %in% bulls.info$animal.id, ] # bull movements
bull.movements <- merge(bull.movements, bulls.info) # merge bull.movements with bull information
bull.movements$bull.move <- ifelse(bull.movements$date >= bull.movements$bull.date, 'TRUE', 'FALSE')
bull.move <- bull.movements[bull.movements$bull.move == TRUE, ]
bull.move <- bull.move[c('date', 'cph.off', 'cph.on')]
bull.move <- bull.move[!is.na(bull.move$date), ]
rm(bulls.info, bull.movements)

#### bull batches ####

bull.batches <- count(bull.move, c('date', 'cph.off', 'cph.on')) # 5276 obs
names(bull.batches) <- c('date', 'cph.off', 'cph.on', 'no.bulls') # rename columns
rm(bull.move)

#### calves ####

movements.scot.animal.info <- merge(new.movements, animals.new[c('animal.id', 'birth.date')]) # 778329 obs
movements.scot.animal.info$move.age <- as.integer(movements.scot.animal.info$date - movements.scot.animal.info$birth.date) # subtract birth date from movement date to get age at move

calves.movements <- movements.scot.animal.info[movements.scot.animal.info$move.age <= 30, ] 
calves.movements <- calves.movements[!is.na(calves.movements$animal.id), ] # 26652 obs

#### calf batches ####

calf.batches <- count(calves.movements, c('date', 'cph.off', 'cph.on')) # 12824 obs
names(calf.batches) <- c('date', 'cph.off', 'cph.on', 'no.calves') # rename columns
rm(animals.new, movements.scot.animal.info, calves.movements)

# needs to be batched for off/trans/on/production
batches <- count(new.movements, c('date', 'cph.off', 'cph.trans', 'cph.on', 'production.type'))
names(batches) <- c('date', 'cph.off', 'cph.trans', 'cph.on', 'production.type', 'no.animals') # rename columns

# merge locations file and batches movements
names(sam.coords) <- c('off', 'premises_type_code.off', 'cph.off', 'x.off', 'y.off')
batches.merge <- merge(batches, sam.coords[c('cph.off', 'premises_type_code.off', 'x.off', 'y.off')], all.x=TRUE)
###
names(sam.coords) <- c('on', 'premises_type_code.on', 'cph.on', 'x.on', 'y.on')
batches.merge <- merge(batches.merge, sam.coords[c('cph.on','premises_type_code.on', 'x.on', 'y.on')], all.x=TRUE)
###
names(sam.coords) <- c('trans', 'premises_type_code.trans', 'cph.trans', 'x.trans', 'y.trans')
batches.merge <- merge(batches.merge, sam.coords[c('cph.trans','x.trans', 'y.trans')], all.x=TRUE) # 

# type.codes of transitional cphs
# AI     CA     MA     SG 
# 10     40 268252    205

batches.merge$cph.trans[is.na(batches.merge$cph.trans)] <- 'direct' # direct movements to be taken into account; still says NA in cat
batches.merge$cph.off[is.na(batches.merge$cph.off)] <- 'import'

rm(new.movements, batches)

#### only movements that are either to or from scotland ####
# add county to batches


batches.merge$county.off <- as.integer(substr(batches.merge$cph.off, 1, 2))
batches.merge$county.on <- as.integer(substr(batches.merge$cph.on, 1, 2))
batches.scot.1 <- batches.merge[batches.merge$county.on >= 66 & # scottish counties are 66 and above 
                                  batches.merge$county.on <= 98, ]
batches.scot.2 <- batches.merge[batches.merge$county.off >= 66 &
                                  batches.merge$county.off <= 98, ] 
batches.scot <- rbind(batches.scot.1, batches.scot.2)
rm(batches.scot.1, batches.scot.2)

batches.scot <- unique(batches.scot) # remove duplicates; if both on and off locations are scottish, then the row would be duplicated
batches.scot <- batches.scot[!is.na(batches.scot$date), ] # remove rogue NAs

batches.scot <- batches.scot[!batches.scot$county.on == 99, ] # remove odd type
batches.scot <- batches.scot[batches.scot$county.off != 99 | # remove odd type
                               is.na(batches.scot$county.off), ] # don't remove imports
batches.scot <- batches.scot[!names(batches.scot) %in% c('county.off', 'county.on')]
batches.scot$year <- format(batches.scot$date, '%Y')
#rm(batches.merge)

#### Misha's cleaned SAMU data ####
misha.sheep <- read.csv('../Desktop/Framework/Framework/executables/input/cs/SAMU_2010_11_Misha.csv', 
                        header=TRUE,
                        stringsAsFactors=FALSE)

#### match sheep data to cattle data ####
# Species, Movement_Year, Movement_Date, Number_of_Animals_Moved, Best_CPH_Dep, Location_Type_Dep, Best_Easting_Dep, Best_Northing_Dep, Best_CPH_Mkt, Best_Easting_Mkt, Best_Northing_Mkt, Best_CPH_Dest, Location_Type_Dest, Best_Easting_Dest, Best_Northing_Dest
names(misha.sheep) <- c('production.type', 'year', 'date', 'no.animals', 'cph.off', 'premises_type_code.off', 'x.off', 'y.off', 'cph.trans', 'x.trans', 'y.trans', 'cph.on', 'premises_type_code.on', 'x.on', 'y.on')

misha.sheep$premises_type_code.off <- ifelse(misha.sheep$premises_type_code.off == 'Market', 'MA',
                                             ifelse(misha.sheep$premises_type_code.off == 'Slaughterhouse (both)', 'SR', 'UNKNOWN'))
misha.sheep$premises_type_code.on <- ifelse(misha.sheep$premises_type_code.on == 'Market', 'MA',
                                            ifelse(misha.sheep$premises_type_code.on == 'Slaughterhouse (both)', 'SR', 'UNKNOWN'))

names(sam.coords) <- c('off', 'premises_type_code.off.2', 'cph.off', 'x.off.2', 'y.off.2') # rename to facilitate merging
sheep.merge <- merge(misha.sheep, sam.coords[c('cph.off', 'premises_type_code.off.2', 'x.off.2', 'y.off.2')], all.x=TRUE)

rm(misha.sheep)

###
names(sam.coords) <- c('on', 'premises_type_code.on.2', 'cph.on', 'x.on.2', 'y.on.2')
sheep.merge <- merge(sheep.merge, sam.coords[c('cph.on', 'premises_type_code.on.2', 'x.on.2', 'y.on.2')], all.x=TRUE)
###
sheep.merge$premises_type_code.off <- ifelse(sheep.merge$premises_type_code.off == 'UNKNOWN',
                                             sheep.merge$premises_type_code.off.2, 
                                             sheep.merge$premises_type_code.off)
sheep.merge$x.off <- ifelse(is.na(sheep.merge$x.off),
                            sheep.merge$x.off.2,
                            sheep.merge$x.off)
sheep.merge$y.off <- ifelse(is.na(sheep.merge$y.off),
                            sheep.merge$y.off.2,
                            sheep.merge$y.off)
###
sheep.merge$premises_type_code.on <- ifelse(sheep.merge$premises_type_code.on == 'UNKNOWN',
                                            sheep.merge$premises_type_code.on.2, 
                                            sheep.merge$premises_type_code.on)
sheep.merge$x.on.x <- ifelse(is.na(sheep.merge$x.on),
                             sheep.merge$x.on.2,
                             sheep.merge$x.on)
sheep.merge$y.on.y <- ifelse(is.na(sheep.merge$y.on),
                             sheep.merge$y.on.2,
                             sheep.merge$y.on)
sheep.merge <- sheep.merge[c('production.type', 'year', 'date', 'no.animals', 'cph.off', 'premises_type_code.off', 'x.off', 'y.off', 'cph.trans', 'x.trans', 'y.trans', 'cph.on', 'premises_type_code.on', 'x.on', 'y.on')]

# remove movements to and from slaughter and other location types; quite a lot of movements to and from Showgrounds for being transitional data...
sheep.merge <- sheep.merge[!sheep.merge$premises_type_code.off %in% c('CA', 'CC', 'SR', 'SG'), ]
sheep.merge <- sheep.merge[!sheep.merge$premises_type_code.on %in% c('CA', 'CC', 'EX', 'SG', 'SR'), ]
sheep.merge$cph.trans[sheep.merge$cph.trans == ''] <- 'direct'

#### combine with cattle data ####

batches.combined <- rbind(batches.scot, sheep.merge)
batches.combined <- batches.combined[!is.na(batches.combined$date), ]
#### add a column for exemptions, these'll be removed before the rewiring loop, then tacked on at the end :P ####

#### movements from scottish islands to markets are exempt ####
# island parishes identified manually using ag parish map http://www.gov.scot/Topics/Statistics/19972/21083
# counties and parishes would be of more use

island.parishes <- c(147:151, 153, 164:168, 274, 275, 279, 443, 444, 456:465, 610:634, 753:756, 870:891)

names(sam.coords) <- c('location_id', 'premises_type_code', 'cph', 'x', 'y')
sam.coords$county <- as.integer(substr(sam.coords$cph, 1, 2))
sam.coords.scotland <- sam.coords[sam.coords$county >= 66 &
                                    sam.coords$county < 99, ]
sam.coords$county <- NULL # clean up sam.coords
sam.coords.scotland$parish <- as.integer(substr(sam.coords.scotland$cph, 4, 6))
sam.coords.scotland$islands <- ifelse(sam.coords.scotland$parish %in% island.parishes, TRUE, FALSE)
sam.coords.scotland$county.parish <- paste0(sam.coords.scotland$county, sam.coords.scotland$parish)
scottish.islands <- sam.coords.scotland[sam.coords.scotland$islands == TRUE, ]
scottish.islands <- scottish.islands[scottish.islands$county != 83, ] # remove mislabled farm from lanarkshire
island.county.parish <- scottish.islands$county.parish
rm(sam.coords.scotland, scottish.islands)

# generate parishes from cph's
batches.combined$county.parish.off <- as.integer(substr(gsub('/', '', batches.combined$cph.off), 1, 5))
batches.combined$county.parish.on <- as.integer(substr(gsub('/', '', batches.combined$cph.on), 1, 5))
  
batches.combined$island.exempt <- ifelse(batches.combined$county.parish.off %in% island.county.parish &
                                           !batches.combined$county.parish.on %in% island.county.parish, TRUE, FALSE)
batches.combined$county.parish.off <- NULL
batches.combined$county.parish.on <- NULL

#### Seperation aggreements ####
# read in seperation aggreements
# incomplete data
# make an informed decision i.e. if they are present at all in any of the lists, include treat them as having and agreement
# also, if any holding has an agreement in place, assume that any movement within 13 day standstill is exempt; no way to tell otherwise
# there are duplicates

seperation.agreements <- c('separation_agreements_13-01-2010.csv',
                           'separation_agreements_18-12-2013.csv',
                           'separation_agreements_18-12-2013.csv')
sep.agreements <- c()

for (i in seperation.agreements) {
  
  csv <- read.csv(i, stringsAsFactors=FALSE, header=TRUE) # read.csv
  names(csv) <- c('area.office', 'cph', 'start', 'end')
  sep.agreements <- append(sep.agreements, csv$cph)
                            
}

sep.agreements <- unique(sep.agreements) # takes unique CPHs
rm(i, csv, seperation.agreements) # clear 

batches.combined$sep.agreements.on <- ifelse(batches.combined$cph.on %in% sep.agreements, TRUE, FALSE) # only the destination matters
rm(sep.agreements)

#### bull movements ####

batches.combined$concat <- paste0(batches.combined$date, '_', batches.combined$cph.off, '_', batches.combined$cph.on)
bull.batches$concat <- paste0(bull.batches$date, '_', bull.batches$cph.off, '_', bull.batches$cph.on)
batches.combined$bull.move <- ifelse(batches.combined$concat %in% bull.batches$concat, TRUE, FALSE)
rm(bull.batches)

#### calve movements exempt ####
# assuming only calves are being moved together; not mixed batches

calf.batches$concat <- paste0(calf.batches$date, '_', calf.batches$cph.off, '_', calf.batches$cph.on)
batches.combined$calf.move <- ifelse(batches.combined$concat %in% calf.batches$concat, TRUE, FALSE)
rm(calf.batches)
batches.combined$concat <- NULL

#### CPH owner info ####
# exemption states that movements between premises occupied by the same person or business are exempt
# multiple owners and occupiers for some farms with multiple CPHHs

cph.keeper <- read.csv('REQUESTqryMDeason_CPHOwners_v2_MDcleaned.txt', 
                       header=TRUE, 
                       stringsAsFactors=FALSE) # cleaned to remove obvious typos; e.g. 2209 instead of 2009
names(cph.keeper) <- c('cph', 'cphh', 'lookup.description', 'id', 'status', 'from.date', 'to.date')
cph.keeper <- cph.keeper[cph.keeper$lookup.description == 'Is primary keeper of', ] # only interested in keepers
cph.keeper$lookup.description <- NULL
cph.keeper$cphh <- NULL
cph.keeper$status <- NULL
cph.keeper$from.date <- as.Date(cph.keeper$from.date, format='%d/%m/%Y') # 1970-01-01 very likely an error
cph.keeper$to.date <- as.Date(cph.keeper$to.date, format='%d/%m/%Y')
cph.keeper <- cph.keeper[!cph.keeper$from.date > y2011, ] # remove those records that begin after the end date
cph.keeper <- cph.keeper[!cph.keeper$to.date < x2010, ] # remove those records that begin after the end date
cph.keeper <- cph.keeper[cph.keeper$cph %in% unique(c(batches.combined$cph.off, batches.combined$cph.on)), ] # only those farms in movement data

# loop through the result file to find the keeper id that fits the movement date
# occationally more than one keeper per CPH

# batches.combined.keeper.loop <- batches.combined[c('cph.off', 'cph.on', 'date')] # should have the same order as the original data.frame
batches.combined$keeper.exempt <- NA

# cphs may have multiple keeper ids; one for each cphh
# calculate the exemption in this loop

for (i in 1:nrow(batches.combined)) {
  
  if (i %%  5000 == 0) { # progress bar; prevents over printing
    cat(i, 'of', nrow(batches.combined), '\n')
  }
  
  cph.off <- batches.combined$cph.off[i]
  cph.on <- batches.combined$cph.on[i]
  date <- batches.combined$date[i]
  
  keeper.off <- unique(cph.keeper[cph.keeper$cph == cph.off &
                                    cph.keeper$from.date < date &
                                    cph.keeper$to.date > date, ]$id)
  
  if (length(keeper.off) == 0) {
    
    keeper.off <- -99
    
  }
  
  keeper.on <- unique(cph.keeper[cph.keeper$cph == cph.on &
                                   cph.keeper$from.date < date &
                                   cph.keeper$to.date > date, ]$id)
  
  if (length(keeper.on) == 0) {
    
    keeper.on <- -99
    
  }
  
  if (any(keeper.off == -99) & any(keeper.on == -99)) {
    
    # if they're both NA, not exempt
    batches.combined$keeper.exempt[i] <- FALSE
    
  } else {
    
    # any of the keepers in keeper.off and keeper.on are the same, exempt, else not exempt
    batches.combined$keeper.exempt[i] <- any(keeper.off %in% keeper.on)
    
  }
}

rm(keeper.off, keeper.on)

### merge with batched data ###

batches.combined$standstill.exempt <- ifelse(batches.combined$sep.agreements.on == TRUE | # farm has a seperation agreement or
                                               batches.combined$bull.move == TRUE | # exempt bull move or
                                               batches.combined$calf.move == TRUE | # exempt calf move or
                                               batches.combined$keeper.exempt == TRUE | # movements from islands are exempt
                                               batches.combined$island.exempt == TRUE, 'exempt', 'not_exempt') # movement between same keeper


#### create a list of market days and the type of animals moved on that day ####

market.days <- count(batches.combined, c('cph.trans', 'date', 'production.type'))
market.days$cph.trans[is.na(market.days$cph.trans)] <- 'direct'

#~~~~#
#load('parish_market_dist.RData') # list of closest markets by scottish agricultural parish; calculated in scot_parish.R and needs to be moved here
library(rgdal)
library(rgeos)

par <- readOGR('par', 'Agricultural_parishes') # coordinates in eastings and northings
parish.centroids <- gCentroid(par, byid = TRUE) # PARCODE corresponds to parish in CPH :)

market.coords <- sam.coords[sam.coords$premises_type_code == 'MA', ][c('cph', 'x', 'y')]
market.coords <- market.coords[market.coords$cph %in% market.days$cph.trans, ] # only use market that appear in the data to cut out far flung english markets

# for each 891 scottish parishs, order the closest markets
parish.centroids.df <- data.frame(parish.centroids, 
                                  row.names = 1:length(parish.centroids)) # removes 0 from row names
parish.centroids.df$parish <- 1:nrow(parish.centroids.df)
parish.market.dist <- list() # list of cphs

for (i in 1:891) {
  
  if (i %%  100 == 0) { # progress bar; prevents over printing
    cat(i, 'of 891\n')
  }
  
  par.coords <- parish.centroids.df[i, ][c('x', 'y')] # for parish i
  par.dist <- data.frame(cph = numeric(0), 
                         dist = numeric(0))
  
  for (j in 1:nrow(market.coords)) {
    
    par.dist[j, 1] <- market.coords$cph[j]
    par.dist[j, 2] <- sqrt(sum((par.coords - market.coords[c('x', 'y')][j, ]) ^ 2)) # calculate euclidean distance
    par.dist <- par.dist[order(par.dist$dist), ] # order from smallest to largest   
    
  }
  
  parish.market.dist[[i]] <- par.dist$cph # store cphs in list
}

rm(par, par.coords, parish.centroids, parish.centroids.df, market.coords)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#### run network variations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

batches.test <- batches.combined[batches.combined$date >= '2010-09-15' & 
                                   batches.combined$date <= '2010-10-31', ]

# network.stubs <- batches.test[rep(seq(nrow(batches.test)), batches.test$no.animals), ] # create stubs by lengthening movement list by the number of animals in a batch   
# network.stubs$ref <- 1:nrow(network.stubs) # create a unique animal movement index
# network.stubs <- network.stubs[sample(nrow(network.stubs), nrow(network.stubs)), ] # shuffle dataframe to randomise order of farm ids

# network.stubs$cph.trans[is.na(network.stubs$cph.trans)] <- 'direct' # direct movements to be taken into account; still says NA in cat
# network.stubs$cph.off[is.na(network.stubs$cph.off)] <- 'import'

batches.test$ref <- 1:nrow(batches.test) # create a unique movement index

# keep no.animals for in/out degree measures? no, not practical to implement

#stubs <- stubs[, c('ref', 'date', 'cph.off', 'cph.trans', 'production.type', 'cph.on')] 

#~~~~#
# shunt movements
#~~~~#

stub.dates <- seq(from = as.Date("2010-09-15"), # start date
                  by='day', 
                  length.out = 300) # number of days to run the model

stubs <- batches.test
# stubs <- stubs[stubs$cph.off == '75/312/0030' & # temporary debugging subset
#                  stubs$cph.on == '75/312/0030', ]

standstill.duration <- 6 # set the standstill duration
nearest.market <- TRUE # create functionality in loop to allow for this to be either TRUE or FALSE
standstills.exempt <- FALSE # TRUE: exemptions considered; FALSE: exemptions ignored; movements will simply be removed at the beginning and added at the end.

#~~~~#

new.stubs <- data.frame()
standstill.list <- data.frame() # may need to define columns 
stored.moves <- data.frame() # initialise stored.moves
standstill.chain.test <- data.frame() # check the amount of shunted movements chained together
# create empty data.frame with the correct names
legal.stored.moves <- data.frame(ref = numeric(0), 
                                 date = numeric(0), 
                                 cph.off = numeric(0),
                                 cph.trans = numeric(0),
                                 production.type = numeric(0),
                                 cph.on = numeric(0),
                                 next.move= numeric(0))

if (standstills.exempt == TRUE) {
  
  stubs <- batches.test[batches.test$standstill.exempt == 'not_exempt', ]
  stubs <- stubs[!is.na(stubs$date), ]
  stubs.exempted <- batches.test[batches.test$standstill.exempt == 'exempt', ]
  stubs.exempted <- stubs.exempted[!is.na(stubs.exempted$date), ]
  
} 

for (i in stub.dates) { # maybe use a while loop and increment by days instead of a days vector; this way is easier
  
  cat('~~~ date =', paste(as.Date(i, origin='1970-01-01')), 
      'standstill.list size = ', nrow(standstill.list),
      '| illegal.stored.moves =', nrow(stored.moves), 
      '| legal.stored.moves =', nrow(legal.stored.moves), '~~~\n') 
  cat('nrow new.stubs =', nrow(new.stubs), 'of a possible', nrow(stubs), '\n')
  
  standstill.chain.test <- unique(rbind(standstill.chain.test, standstill.list))
  
  loop.movements <- data.frame() # initialise or clear variable
  standstill.list <- standstill.list[standstill.list$next.move.date > i, ] # update standstill.list to remove those farms that are no longer on restriction
  day.stubs <- stubs[stubs$date == i, ]
  
  if (nrow(stored.moves) != 0) {
    
    legal.stored.moves <- rbind(legal.stored.moves,
                                stored.moves[stored.moves$next.move <= i, ])
    legal.stored.moves <- legal.stored.moves[!duplicated(legal.stored.moves$ref, fromLast=TRUE), ] # removes potential duplicates in legal moves list
    day.stubs <- rbind(day.stubs, legal.stored.moves[, c(1:length(legal.stored.moves) - 1)]) # reintroduce stored moves from previous timesteps; previously illegal movements need to go back on the pile
    
  } else {
    
    day.stubs <- rbind(day.stubs, legal.stored.moves[, c(1:length(legal.stored.moves) - 1)]) # rbind stored.moves even if there are no new stored.moves
    day.stubs <- day.stubs[!is.na(day.stubs$date), ]
    
  }

  if (nrow(day.stubs) == 0) 
    break # prevents loop from running through all possible days in stub.dates
  
  if (nearest.market == FALSE) { # use the market data in movement records
    
    for (j in unique(stubs$cph.trans)) { 
      
      for (k in unique(stubs$production.type)) {
        
        if (nrow(market.days[market.days$date == i & 
                               market.days$cph.trans == j & 
                               market.days$production.type == k, ]) != 0) {
          
          loop.stubs <- day.stubs[day.stubs$cph.trans == j &
                                    day.stubs$production.type == k, ]
          loop.movements <- rbind(loop.movements, loop.stubs)
        
        }
      } 
    } 
    
  } else { # calculate the nearest market for the given day using parish.market.dist
    
    for (k in unique(stubs$production.type)) {
      
      loop.market <- market.days[market.days$date == i & 
                                   market.days$production.type == k, ]
      
      if (nrow(loop.market) != 0) {
        
        loop.stubs <- day.stubs[day.stubs$production.type == k, ]
        loop.stubs <- loop.stubs[!is.na(loop.stubs$date), ]
        
        drop.list <- numeric()
        
        if (nrow(loop.stubs) > 0) {
          
          for (key in 1:nrow(loop.stubs)) {
            
            # if cph.trans is in loop market, just use that cph.trans  
            if (!loop.stubs$cph.trans[key] %in% loop.market$cph.trans) { # if the cph.trans in loop.stubs is NOT present in loop.market...
              
              closest.markets <- parish.market.dist[[as.integer(substr(loop.stubs$cph.off[key], 4, 6))]] # parse out the parish of the cph.off; store vector of market cphs; use only observed markets in scottish data eventually
              new.markets <- closest.markets[closest.markets %in% loop.market$cph.trans]
              
              if (length(new.markets) == 0) {
                
                new.loop.stubs <- cbind(loop.stubs[key, ], as.Date(i, origin='1970-01-01'))
                names(new.loop.stubs) <- names(stored.moves)
                stored.moves <- rbind(stored.moves, new.loop.stubs) 
                
                drop.list <- append(drop.list, key)
                
              } else {
                
                loop.stubs$cph.trans[key] <- new.markets[1] # assign new cph.trans; NB assumes the observed destination is the same
                
              }
            }
          }
        } 
        
        if (length(drop.list) > 0) {
          
          loop.stubs <- loop.stubs[-c(drop.list), ] # remove these rows from loop.stubs to keep things simple
          
        }
        
        loop.movements <- rbind(loop.movements, loop.stubs)
        
      }
    }
  } # end of else loop
  
  if (nrow(loop.movements) > 0) {
    
    for (test in 1:nrow(loop.movements)) { 
      
      x <- standstill.list[standstill.list$cph.on == loop.movements$cph.off[test], ]
      y <- loop.movements[test, ][loop.movements$date[test] < x$next.move.date, ]
      
      if (nrow(y) != 0) {
        
        y$next.move <- x$next.move.date
        stored.moves <- rbind(stored.moves, y) # store illegal moves, wait until they're legal
        stored.moves <- stored.moves[!duplicated(stored.moves$ref, fromLast=TRUE), ] # only keep the most recent next.move
        
      }
      
      rm(x,  y)
      
    }
    
    stored.moves.update <- stored.moves[stored.moves$next.move > i, ] # update list of stored moves by subsetting for those that are still illegal
    loop.movements <- loop.movements[!loop.movements$ref %in% stored.moves.update$ref, ] # if the stored moves are illegal, remove them from loop.stubs
    legal.stored.moves <- legal.stored.moves[!legal.stored.moves$ref %in% loop.movements$ref, ] # if the move are being used for new.stubs, remove them from legal.stored.moves
    
    if (nrow(loop.movements) > 0) { # only proceed if there were data in the subset            
      
      standstill.list <- rbind(standstill.list, unique(data.frame(date = as.Date(i, origin='1970-01-01'), # list of farms moving stock on for a particular day
                                                                  cph.on = loop.movements$cph.on, 
                                                                  next.move.date = as.Date(i, origin='1970-01-01') + days(standstill.duration), #  days to make maths easier
                                                                  ref = loop.movements$ref,
                                                                  stringsAsFactors = FALSE)))
      
      loop.movements$date <- as.Date(i, origin='1970-01-01')
      
      new.stubs <- rbind(new.stubs, loop.movements) # store output
      
      stored.moves <- stored.moves[stored.moves$ref %in% stored.moves.update$ref, ] 
      
    }
    
    standstill.list <- standstill.list[!duplicated(standstill.list$cph.on, fromLast = TRUE), ] # if the cph is already present, take the earlier value
    
  }
}

if (standstills.exempt == TRUE) {
  
  new.stubs <- rbind(new.stubs, stubs.exempted)
  
}

# sanity checks
nrow(batches.test)
nrow(new.stubs) 
new.stubs.unique <- unique(new.stubs) 
nrow(legal.stored.moves) 
nrow(unique(rbind(legal.stored.moves[1:length(legal.stored.moves) - 1], new.stubs.unique))) 


#### rebatch ####

# library(plyr)
# new.batches <- count(new.stubs, c('date', 'cph.off', 'cph.trans', 'production.type', 'cph.on'))
# names(new.batches) <- c('date', 'cph.off', 'cph.trans', 'production.type', 'cph.on', 'no.animals')
new.batches <- new.stubs

#### create new list of scot.farms ####

cphs <- unique(c(new.batches$cph.off, 
                 new.batches$cph.on)) 
counties <- as.integer(substr(cphs, 1, nchar(cphs) - 9)) 
scot.counties <- counties >= 66 # TRUE/FALSE vector
scot.farms <- cphs[scot.counties] 
scot.farms <- scot.farms[!is.na(scot.farms)]
rm(cphs, counties, scot.counties)

#### calculate the amount of time between each on and subsequent off movement ####
# will need to run through entire list of scottish on locations

test.results <- data.frame() # initialise data frame

for (farm in scot.farms) { # for each on location; a list of scottish farms
  
  if (which(farm == scot.farms) %%  500 == 0) { # progress bar; prevents over printing
    cat(which(farm == scot.farms), 'of', length(scot.farms), '\n') # only print ever 500 lines
  }
  
  movements.subset <- new.batches[new.batches$cph.on == farm | new.batches$cph.off ==  farm, ] # subset rows where the farm is present in either on or off
  movements.subset <- movements.subset[!is.na(movements.subset$date), ] # remove any rogue NAs
  ordered.movements <- movements.subset[order(movements.subset$date), ] # order the dates
  
  if (nrow(ordered.movements) <= 1) { # only proceed if there are more than 1 movement; a single movement seems to cause the loop to fall over
    next
  }
  
  differences <- data.frame() # initialise data.frame
  difference.in.days <- 0
  
  pass.key <- 0 # initialise/reset 'token'
  first.move.index <- 0 # initialise/reset index of first movment
  
  for (i in 1:(nrow(ordered.movements) - 1)) { # -1 to prevent looping through an on movement in the last row
    
    if (ordered.movements$cph.on[i] == farm) { # Is this date associated with an on movement?
      
      pass.key <- 'token' # store a token 
      first.move.index <- i # store the index value
      
    } else if (ordered.movements$cph.off[i] == farm & # off location is a farm
                 pass.key == 'token') { # this is the next off movement
      
      difference.in.days <- ordered.movements$date[i] - ordered.movements$date[first.move.index] # subtract the on movement date from the off movement date
      
      if (difference.in.days > 0) { # make sure it's greater than 0
        
        differences <- rbind(differences, 
                             cbind(rbind(ordered.movements[first.move.index, ], 
                                         ordered.movements[i, ]),
                                   rbind(difference.in.days, 
                                         difference.in.days)))
      }
      
      pass.key <- 0 # reset 'token'
      
    }
  } 
  
  test.results <- rbind(test.results, differences) # store in result
  
}

test.results$diff <- test.results$'rbind(difference.in.days, difference.in.days)' # rename the variable
test.results$'rbind(difference.in.days, difference.in.days)' <- NULL
table(test.results$diff)
hist(test.results$diff, breaks = 0:max(test.results$diff))

#~~~~~#
#save.image('fmd_standstill_test.RData') # 24-09-2015
#~~~~~#

#### format for FMD model ####
# AH -> 'Unknown'; MA -> 'Market'
# 'direct' -> ''
# cph 99/999/0013 for exports
# cph 99/999/0200 for imports

input  <- new.stubs[c('production.type', 'year', 'date', 'no.animals',  'cph.off', 'premises_type_code.off', 'x.off', 'y.off', 'cph.trans', 'x.trans', 'y.trans', 'cph.on', 'premises_type_code.on', 'x.on', 'y.on')]
input <- input[input$date >= '2010-10-01', ]
input$x.trans[is.na(input$x.trans)] <- 0
input$y.trans[is.na(input$y.trans)] <- 0
input$cph.trans[input$cph.trans == 'direct'] <- ''
input$premises_type_code.off <- ifelse(input$premises_type_code.off == 'MA', 'Market', 'Unknown')
input$premises_type_code.on <- ifelse(input$premises_type_code.on == 'MA', 'Market', 'Unknown')
input$cph.off[input$cph.off == 'import'] <- '99/999/0200' # cph code for imports
names(input) <- c('Species', 'Movement_Year', 'Movement_Date', 'Number_of_Animals_Moved', 'Best_CPH_Dep', 'Location_Type_Dep', 'Best_Easting_Dep', 'Best_Northing_Dep', 'Best_CPH_Mkt', 'Best_Easting_Mkt', 'Best_Northing_Mkt', 'Best_CPH_Dest', 'Location_Type_Dest', 'Best_Easting_Dest', 'Best_Northing_Dest')

write.csv(input, paste0('C:/Users/Michael/Google Drive/FMD model imput/fmd_rewire_', standstill.duration, '_nearest_market_', nearest.market, '_exemptions_', standstills.exempt, '_01-10-2010.csv'), row.names=FALSE)

# input  <- batches.combined[c('production.type', 'year', 'date', 'no.animals',  'cph.off', 'premises_type_code.off', 'x.off', 'y.off', 'cph.trans', 'x.trans', 'y.trans', 'cph.on', 'premises_type_code.on', 'x.on', 'y.on')]
# input$x.trans[is.na(input$x.trans)] <- 0
# input$y.trans[is.na(input$y.trans)] <- 0
# input$cph.trans[input$cph.trans == 'direct'] <- ''
# input$premises_type_code.off <- ifelse(input$premises_type_code.off == 'MA', 'Market', 'Unknown')
# input$premises_type_code.on <- ifelse(input$premises_type_code.on == 'MA', 'Market', 'Unknown')
# input$cph.off[input$cph.off == 'import'] <- '99/999/0200' # cph code for imports
# names(input) <- c('Species', 'Movement_Year', 'Movement_Date', 'Number_of_Animals_Moved', 'Best_CPH_Dep', 'Location_Type_Dep', 'Best_Easting_Dep', 'Best_Northing_Dep', 'Best_CPH_Mkt', 'Best_Easting_Mkt', 'Best_Northing_Mkt', 'Best_CPH_Dest', 'Location_Type_Dest', 'Best_Easting_Dest', 'Best_Northing_Dest')
# write.csv(input, 'fmd_rewire_observed_complete.csv', row.names=FALSE)