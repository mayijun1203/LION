#  10/8/2017
#  Lion-Street-Roadbed: SegmentTyp in ( 'B', 'R', 'E', 'T', 'C', 'S', 'U') AND FeatureTyp in ( '0', '6', 'C', 'W', 'A')

library(data.table)
library(dplyr)

# Choose the exported lion table
path=choose.files()

#  Create a cleaned LION table
lionstreetroadbeds=read.csv(path,stringsAsFactors = F,colClasses = 'character')
lion=select(lionstreetroadbeds,PhysicalID,SegmentID,SeqNum,StreetCode,Street,NodeIDFrom,NodeIDTo)
lion=mutate(lion,PhysicalID2=ifelse(PhysicalID=='0',yes=as.character(90000000+as.integer(SegmentID)),no=PhysicalID)) # Add 90000000 to the SegmentIDs as PhysicalIDs
lion=mutate(lion,SegmentID2=paste(lion$SegmentID,lion$NodeIDFrom,lion$NodeIDTo,sep = '|')) # To deal with the coincident segments with reverse topological directions
lion=mutate(lion,StreetCode2=paste(lion$StreetCode,lion$Street,sep = '|')) # To deal with different segments with same streetcode
lion=unique(lion)
lion=select(lion,PhysicalID,PhysicalID2,SegmentID,SegmentID2,SeqNum,StreetCode,StreetCode2,Street,NodeIDFrom,NodeIDTo)

## Prioritize the street names based on the frequencies of PhysicalIDs in the entire network
streetcode=unique(select(lion,StreetCode,StreetCode2,Street,PhysicalID,PhysicalID2))
streetcode=arrange(streetcode,StreetCode,StreetCode2,Street,PhysicalID,PhysicalID2)
streetcode=data.frame(streetcode %>% group_by(StreetCode,StreetCode2,Street) %>% mutate(Priority1=n()))
streetcode=unique(select(streetcode,StreetCode,StreetCode2,Street,Priority1))
streetcode=streetcode %>% mutate(Priority1=ifelse(grepl('HIGHWAY',Street)|grepl('PKWY',Street)|grepl('PARKWAY',Street)|grepl('EXPWY',Street)|grepl('EXPRESSWAY',Street)|
     grepl(' ENTRANCE',Street)|grepl('EXIT',Street)|grepl('RAMP',Street)|grepl('PEDESTRIAN',Street)|grepl('BIKE',Street)|grepl('BICYCLE',Street)|grepl('ALLEY',Street)|
     grepl('DRIVEWAY',Street)|grepl('UNNAMED',Street)|grepl('CONNECTOR',Street),yes=0,no=Priority1)) # Assign 0 to the priorities of street names with highway, pkwy...
streetcode=arrange(streetcode,StreetCode,StreetCode2,desc(Priority1),Street)
lion=merge(lion,streetcode,by=c('StreetCode','StreetCode2'),all.x=T)
lion=select(lion,PhysicalID,PhysicalID2,SegmentID,SegmentID2,SeqNum,StreetCode,StreetCode2,Street.y,NodeIDFrom,NodeIDTo,Priority1)
colnames(lion)=c('PhysicalID','PhysicalID2','SegmentID','SegmentID2','SeqNum','StreetCode','StreetCode2','Street','NodeIDFrom','NodeIDTo','Priority1')
lion=unique(lion)

## List all the names related to each SegmentID and choose one name based on the frequencies of PhysicalIDs in the entire road network
segment=unique(select(lion,SegmentID,SegmentID2,SeqNum,StreetCode,StreetCode2,Street,Priority1))
segment=arrange(segment,SegmentID,SegmentID2,desc(Priority1),StreetCode,StreetCode2,SeqNum)
segment=data.frame(segment %>% group_by(SegmentID,SegmentID2) %>% mutate(StreetList=paste0(Street,collapse=' / '))) # Create a list of all the street names for each SegmentID
segment=subset(segment,!duplicated(SegmentID2))
lion=merge(lion,segment,by=c('SegmentID','SegmentID2'),all.x=T)
lion=select(lion,PhysicalID,PhysicalID2,SegmentID,SegmentID2,SeqNum.y,StreetCode.y,StreetCode2.y,Street.y,StreetList,NodeIDFrom,NodeIDTo,Priority1.y)
colnames(lion)=c('PhysicalID','PhysicalID2','SegmentID','SegmentID2','SeqNum','StreetCode','StreetCode2','Street','StreetList','NodeIDFrom','NodeIDTo','Priority1')
lion=unique(lion)

## List all the names related to each PhysicalID and choose one name based on the frequencies of names in the entire road network
physical=unique(select(lion,PhysicalID,PhysicalID2,StreetCode,StreetCode2,Street,StreetList,Priority1))
physical=arrange(physical,PhysicalID,PhysicalID2,desc(Priority1),StreetCode,StreetCode2)
physical=data.frame(physical %>% group_by(PhysicalID,PhysicalID2) %>% mutate(StreetList=paste0(StreetList,collapse=' / '))) # Create a list of all the street names for each PhysicalID
physical=subset(physical,!duplicated(PhysicalID2))
lion=merge(lion,physical,by=c('PhysicalID','PhysicalID2'),all.x=T)
lion=select(lion,PhysicalID,PhysicalID2,SegmentID,SegmentID2,SeqNum,StreetCode.y,StreetCode2.y,Street.y,StreetList.y,NodeIDFrom,NodeIDTo,Priority1.y)
colnames(lion)=c('PhysicalID','PhysicalID2','SegmentID','SegmentID2','SeqNum','StreetCode','StreetCode2','Street','StreetList','NodeIDFrom','NodeIDTo','Priority1')
lion=unique(lion)

## Remove duplicated street names in the lists
removeduplicates=function(x){
	x=strsplit(x,' / ')[[1]]
	x=unique(x)
	x=paste0(x,collapse=' / ')
	return(x)
}
lion=data.frame(lion %>% rowwise() %>% mutate(StreetList=removeduplicates(StreetList)))

## Keep StreetCode, Street, and StreetList consistent
streetcode=unique(select(lion,StreetCode,StreetCode2,Street,StreetList))
streetcode=aggregate(StreetList~StreetCode+StreetCode2+Street,data=streetcode,paste,collapse=' / ')
streetcode=data.frame(streetcode %>% rowwise() %>% mutate(StreetList=removeduplicates(StreetList)))
lion=merge(lion,streetcode,by=c('StreetCode','StreetCode2'),all.x=T)
lion=select(lion,PhysicalID,PhysicalID2,SegmentID,SegmentID2,SeqNum,StreetCode,StreetCode2,Street.y,StreetList.y,NodeIDFrom,NodeIDTo,Priority1)
colnames(lion)=c('PhysicalID','PhysicalID2','SegmentID','SegmentID2','SeqNum','StreetCode','StreetCode2','Street','StreetList','NodeIDFrom','NodeIDTo','Priority1')






#  Create Node Table
## List all the NodeIDs with related streets
node1=select(lion,NodeIDFrom,StreetCode,StreetCode2,Street,StreetList,Priority1)
colnames(node1)=c('NodeID','StreetCode','StreetCode2','Street','StreetList','Priority1') # Find From Node and the related street names
node2=select(lion,NodeIDTo,StreetCode,StreetCode2,Street,StreetList,Priority1)
colnames(node2)=c('NodeID','StreetCode','StreetCode2','Street','StreetList','Priority1') # Find To Node and the related street names
node=rbind(node1,node2) # Combine two lists together
node=arrange(node,NodeID,StreetCode,StreetCode2)

## Count the number of records for each NodeID
node=data.frame(node %>% group_by(NodeID) %>% mutate(Count=n()))

## Prioritize the street names based on the frequencies for each NodeID
node=data.frame(node %>% group_by(NodeID,StreetCode,StreetCode2) %>% mutate(Priority2=n()))
node=select(node,NodeID,StreetCode,StreetCode2,Street,StreetList,Count,Priority1,Priority2)

## Assign 0 to the priority2 of street names that contain PEDESTRIAN, ALLEY... (using the same criteria above in the pr0 function)
node=node %>% mutate(Priority2=ifelse(grepl('HIGHWAY',Street)|grepl('PKWY',Street)|grepl('PARKWAY',Street)|grepl('EXPWY',Street)|grepl('EXPRESSWAY',Street)|
     grepl(' ENTRANCE',Street)|grepl('EXIT',Street)|grepl('RAMP',Street)|grepl('PEDESTRIAN',Street)|grepl('BIKE',Street)|grepl('BICYCLE',Street)|grepl('ALLEY',Street)|
     grepl('DRIVEWAY',Street)|grepl('UNNAMED',Street)|grepl('CONNECTOR',Street),yes=0,no=Priority2)) # Assign 0 to the priorities of street names with highway, pkwy...
node=select(node,NodeID,StreetCode,StreetCode2,Street,StreetList,Priority1,Priority2)






#  Find From/To NodeID for each PhysicalID
## Find the from node of lowest seqnum and to node of highest seqnum
lion=mutate(lion,Flag1=ifelse(PhysicalID=='0',yes=1,no=0)) # Flag the segments with no PhysicalIDs
dupsegment=unique(filter(lion,duplicated(SegmentID))$SegmentID)
lion=mutate(lion,Flag2=ifelse(SegmentID %in% dupsegment,yes=1,no=0)) # Flag the coincident segments with reverse topological direction
lion1=filter(lion,Flag1==1) # Segments with no PhysicalIDs
lion2=filter(lion,Flag2==1) # Coincident segments with reverse topological directions
lion3=filter(lion,Flag1==0&Flag2==0) # Normal segments
lion3=arrange(lion3,PhysicalID,PhysicalID2,SeqNum,StreetCode,StreetCode2)
fromnode=select(filter(lion3,!duplicated(PhysicalID2)),PhysicalID,PhysicalID2,SeqNum,StreetCode,StreetCode2,Street,StreetList,NodeIDFrom)
tonode=select(filter(lion3,!duplicated(PhysicalID2,fromLast = T)),PhysicalID,PhysicalID2,NodeIDTo)
physicalfromto=merge(fromnode,tonode,by=c('PhysicalID','PhysicalID2'),all=T)


#  Find from street name for each PhysicalID for normal segments
fromstreet=merge(physicalfromto,node,by.x='NodeIDFrom',by.y='NodeID',all.x=T)
fromstreet=data.frame(fromstreet %>% group_by(PhysicalID,PhysicalID2) %>% mutate(Count=n()))

## If only one record for the node, simply use the street name even though the name may be the same as the on street name
fromstreet1=filter(fromstreet,Count==1)
fromstreet1=select(fromstreet1,PhysicalID,PhysicalID2,StreetCode.y,StreetCode2.y,Street.y,StreetList.y)
colnames(fromstreet1)=c('PhysicalID','PhysicalID2','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList')

## If multiple records, filter out those have the same names as the on street names, choose the one with the highest priorities, and list all the street names
fromstreet2=filter(fromstreet,Count!=1)
fromstreet2=filter(fromstreet2,StreetCode2.x!=StreetCode2.y)
fromstreet2=arrange(fromstreet2,PhysicalID,PhysicalID2,desc(Priority2),desc(Priority1),StreetCode.y,StreetCode2.y)
fromstreet2=data.frame(fromstreet2 %>% group_by(PhysicalID,PhysicalID2) %>% mutate(FromStreetList=paste0(StreetList.y,collapse=' / '))) # Create a list of all the from street names
fromstreet2=filter(fromstreet2,!duplicated(PhysicalID2)) # Choose the from street names with the highest priorities
fromstreet2=select(fromstreet2,PhysicalID,PhysicalID2,StreetCode.y,StreetCode2.y,Street.y,FromStreetList)
colnames(fromstreet2)=c('PhysicalID','PhysicalID2','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList')

## Combine the two from street tables and remove duplicated street names in the from street lists
fromstreet=rbind(fromstreet1,fromstreet2)
fromstreet=arrange(fromstreet,PhysicalID,PhysicalID2)
fromstreet=data.frame(fromstreet %>% rowwise() %>% mutate(FromStreetList=removeduplicates(FromStreetList)))


# Find to street names for each PhysicalID for normal segments
tostreet=merge(physicalfromto,node,by.x='NodeIDTo',by.y='NodeID',all.x=T)
tostreet=data.frame(tostreet %>% group_by(PhysicalID,PhysicalID2) %>% mutate(Count=n()))

## If only one record for the node, simply use the street name even though the name may be the same as the on street name
tostreet1=filter(tostreet,Count==1)
tostreet1=select(tostreet1,PhysicalID,PhysicalID2,StreetCode.y,StreetCode2.y,Street.y,StreetList.y)
colnames(tostreet1)=c('PhysicalID','PhysicalID2','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')

## If multiple records, filter out those have the same names as the on street names, choose the one with the highest priorities, and list all the street names
tostreet2=filter(tostreet,Count!=1)
tostreet2=filter(tostreet2,StreetCode2.x!=StreetCode2.y)
tostreet2=arrange(tostreet2,PhysicalID,PhysicalID2,desc(Priority2),desc(Priority1),StreetCode.y,StreetCode2.y)
tostreet2=data.frame(tostreet2 %>% group_by(PhysicalID,PhysicalID2) %>% mutate(ToStreetList=paste0(StreetList.y,collapse=' / '))) # Create a list of all the to street names
tostreet2=filter(tostreet2,!duplicated(PhysicalID2)) # Choose the to street names with the highest priorities
tostreet2=select(tostreet2,PhysicalID,PhysicalID2,StreetCode.y,StreetCode2.y,Street.y,ToStreetList)
colnames(tostreet2)=c('PhysicalID','PhysicalID2','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')

## Combine the two to street tables and remove duplicated street names in the to street lists
tostreet=rbind(tostreet1,tostreet2)
tostreet=arrange(tostreet,PhysicalID,PhysicalID2)
tostreet=data.frame(tostreet %>% rowwise() %>% mutate(ToStreetList=removeduplicates(ToStreetList)))


#  Join from/to streets for normal segments
physicalfromto=merge(physicalfromto,fromstreet,by=c('PhysicalID','PhysicalID2'),all.x=T)
physicalfromto=merge(physicalfromto,tostreet,by=c('PhysicalID','PhysicalID2'),all.x=T)
physicalfromto=select(physicalfromto,PhysicalID,PhysicalID2,StreetCode,StreetCode2,Street,StreetList,FromStreetCode,FromStreetCode2,FromStreet,FromStreetList,ToStreetCode,ToStreetCode2,ToStreet,ToStreetList)
colnames(physicalfromto)=c('PhysicalID','PhysicalID2','OnStreetCode','OnStreetCode2','OnStreet','OnStreetList','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')
physicalfromto=arrange(physicalfromto,PhysicalID,PhysicalID2)





# Special Cases1: Segments with no PhysicalIDs
segmentfromto=select(lion1,SegmentID,SegmentID2,StreetCode,StreetCode2,Street,StreetList,NodeIDFrom,NodeIDTo)

#  Find from street name for each SegmentID with no PhysicalID
fromstreet=merge(segmentfromto,node,by.x='NodeIDFrom',by.y='NodeID',all.x=T)
fromstreet=data.frame(fromstreet %>% group_by(SegmentID,SegmentID2) %>% mutate(Count=n()))

## If only one record for the node, simply use the street name even though the name may be the same as the on street name
fromstreet1=filter(fromstreet,Count==1)
fromstreet1=select(fromstreet1,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,StreetList.y)
colnames(fromstreet1)=c('SegmentID','SegmentID2','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList')

## If multiple records, filter out those have the same names as the on street names, choose the one with the highest priorities, and list all the street names
fromstreet2=filter(fromstreet,Count!=1)
fromstreet2=filter(fromstreet2,StreetCode2.x!=StreetCode2.y)
fromstreet2=arrange(fromstreet2,SegmentID,SegmentID2,desc(Priority2),desc(Priority1),StreetCode.y,StreetCode2.y)
fromstreet2=data.frame(fromstreet2 %>% group_by(SegmentID,SegmentID2) %>% mutate(FromStreetList=paste0(StreetList.y,collapse=' / '))) # Create a list of all the from street names
fromstreet2=filter(fromstreet2,!duplicated(SegmentID2)) # Choose the from street names with the highest priorities
fromstreet2=select(fromstreet2,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,FromStreetList)
colnames(fromstreet2)=c('SegmentID','SegmentID2','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList')

## Combine the two from street tables and remove duplicated street names in the from street lists
fromstreet=rbind(fromstreet1,fromstreet2)
fromstreet=arrange(fromstreet,SegmentID,SegmentID2)
fromstreet=data.frame(fromstreet %>% rowwise() %>% mutate(FromStreetList=removeduplicates(FromStreetList)))


# Find to street names for each SegmentID with no PhysicalID
tostreet=merge(segmentfromto,node,by.x='NodeIDTo',by.y='NodeID',all.x=T)
tostreet=data.frame(tostreet %>% group_by(SegmentID,SegmentID2) %>% mutate(Count=n()))

## If only one record for the node, simply use the street name even though the name may be the same as the on street name
tostreet1=filter(tostreet,Count==1)
tostreet1=select(tostreet1,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,StreetList.y)
colnames(tostreet1)=c('SegmentID','SegmentID2','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')

## If multiple records, filter out those have the same names as the on street names, choose the one with the highest priorities, and list all the street names
tostreet2=filter(tostreet,Count!=1)
tostreet2=filter(tostreet2,StreetCode2.x!=StreetCode2.y)
tostreet2=arrange(tostreet2,SegmentID,SegmentID2,desc(Priority2),desc(Priority1),StreetCode.y,StreetCode2.y)
tostreet2=data.frame(tostreet2 %>% group_by(SegmentID,SegmentID2) %>% mutate(ToStreetList=paste0(StreetList.y,collapse=' / '))) # Create a list of all the to street names
tostreet2=filter(tostreet2,!duplicated(SegmentID2)) # Choose the to street names with the highest priorities
tostreet2=select(tostreet2,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,ToStreetList)
colnames(tostreet2)=c('SegmentID','SegmentID2','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')

## Combine the two to street tables and remove duplicated street names in the to street lists
tostreet=rbind(tostreet1,tostreet2)
tostreet=arrange(tostreet,SegmentID,SegmentID2)
tostreet=data.frame(tostreet %>% rowwise() %>% mutate(ToStreetList=removeduplicates(ToStreetList)))


#  Join from/to streets for SegmentIDs with no PhysicalIDs
segmentfromto=merge(segmentfromto,fromstreet,by=c('SegmentID','SegmentID2'),all.x=T)
segmentfromto=merge(segmentfromto,tostreet,by=c('SegmentID','SegmentID2'),all.x=T)
segmentfromto=select(segmentfromto,SegmentID,SegmentID2,StreetCode,StreetCode2,Street,StreetList,FromStreetCode,FromStreetCode2,FromStreet,FromStreetList,ToStreetCode,ToStreetCode2,ToStreet,ToStreetList)
colnames(segmentfromto)=c('SegmentID','SegmentID2','OnStreetCode','OnStreetCode2','OnStreet','OnStreetList','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')
segmentfromto=arrange(segmentfromto,SegmentID,SegmentID2)






# Special Cases2: Coincident segments with reverse topological directions
segmentid2fromto=select(lion2,SegmentID,SegmentID2,StreetCode,StreetCode2,Street,StreetList,NodeIDFrom,NodeIDTo)

#  Find from street name for each coincident segment with reverse topological directions
fromstreet=merge(segmentid2fromto,node,by.x='NodeIDFrom',by.y='NodeID',all.x=T)
fromstreet=data.frame(fromstreet %>% group_by(SegmentID,SegmentID2) %>% mutate(Count=n()))

## If only one record for the node, simply use the street name even though the name may be the same as the on street name
fromstreet1=filter(fromstreet,Count==1)
fromstreet1=select(fromstreet1,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,StreetList.y)
colnames(fromstreet1)=c('SegmentID','SegmentID2','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList')

## If multiple records, filter out those have the same names as the on street names, choose the one with the highest priorities, and list all the street names
fromstreet2=filter(fromstreet,Count!=1)
fromstreet2=filter(fromstreet2,StreetCode2.x!=StreetCode2.y)
fromstreet2=arrange(fromstreet2,SegmentID,SegmentID2,desc(Priority2),desc(Priority1),StreetCode.y,StreetCode2.y)
fromstreet2=data.frame(fromstreet2 %>% group_by(SegmentID,SegmentID2) %>% mutate(FromStreetList=paste0(StreetList.y,collapse=' / '))) # Create a list of all the from street names
fromstreet2=filter(fromstreet2,!duplicated(SegmentID2)) # Choose the from street names with the highest priorities
fromstreet2=select(fromstreet2,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,FromStreetList)
colnames(fromstreet2)=c('SegmentID','SegmentID2','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList')

## Combine the two from street tables and remove duplicated street names in the from street lists
fromstreet=rbind(fromstreet1,fromstreet2)
fromstreet=arrange(fromstreet,SegmentID,SegmentID2)
fromstreet=data.frame(fromstreet %>% rowwise() %>% mutate(FromStreetList=removeduplicates(FromStreetList)))


# Find to street names for each coincident segment with reverse topological directions
tostreet=merge(segmentid2fromto,node,by.x='NodeIDTo',by.y='NodeID',all.x=T)
tostreet=data.frame(tostreet %>% group_by(SegmentID,SegmentID2) %>% mutate(Count=n()))

## If only one record for the node, simply use the street name even though the name may be the same as the on street name
tostreet1=filter(tostreet,Count==1)
tostreet1=select(tostreet1,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,StreetList.y)
colnames(tostreet1)=c('SegmentID','SegmentID2','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')

## If multiple records, filter out those have the same names as the on street names, choose the one with the highest priorities, and list all the street names
tostreet2=filter(tostreet,Count!=1)
tostreet2=filter(tostreet2,StreetCode2.x!=StreetCode2.y)
tostreet2=arrange(tostreet2,SegmentID,SegmentID2,desc(Priority2),desc(Priority1),StreetCode.y,StreetCode2.y)
tostreet2=data.frame(tostreet2 %>% group_by(SegmentID,SegmentID2) %>% mutate(ToStreetList=paste0(StreetList.y,collapse=' / '))) # Create a list of all the to street names
tostreet2=filter(tostreet2,!duplicated(SegmentID2)) # Choose the to street names with the highest priorities
tostreet2=select(tostreet2,SegmentID,SegmentID2,StreetCode.y,StreetCode2.y,Street.y,ToStreetList)
colnames(tostreet2)=c('SegmentID','SegmentID2','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')

## Combine the two to street tables and remove duplicated street names in the to street lists
tostreet=rbind(tostreet1,tostreet2)
tostreet=arrange(tostreet,SegmentID,SegmentID2)
tostreet=data.frame(tostreet %>% rowwise() %>% mutate(ToStreetList=removeduplicates(ToStreetList)))


#  Join from/to streets for each coincident segment with reverse topological directions
segmentid2fromto=merge(segmentid2fromto,fromstreet,by=c('SegmentID','SegmentID2'),all.x=T)
segmentid2fromto=merge(segmentid2fromto,tostreet,by=c('SegmentID','SegmentID2'),all.x=T)
segmentid2fromto=select(segmentid2fromto,SegmentID,SegmentID2,StreetCode,StreetCode2,Street,StreetList,FromStreetCode,FromStreetCode2,FromStreet,FromStreetList,ToStreetCode,ToStreetCode2,ToStreet,ToStreetList)
colnames(segmentid2fromto)=c('SegmentID','SegmentID2','OnStreetCode','OnStreetCode2','OnStreet','OnStreetList','FromStreetCode','FromStreetCode2','FromStreet','FromStreetList','ToStreetCode','ToStreetCode2','ToStreet','ToStreetList')
segmentid2fromto=arrange(segmentid2fromto,SegmentID,SegmentID2)





# Export physicalfromto, segmentfromto, and segmentid2fromto tables to the folder of lion table
write.csv(physicalfromto,paste0(dirname(path),'/physicalfromto.csv'),row.names=F)
write.csv(segmentfromto,paste0(dirname(path),'/segmentfromto.csv'),row.names=F)
write.csv(segmentid2fromto,paste0(dirname(path),'/segmentid2fromto.csv'),row.names=F)



# Join the lookup tables back to the orignal lion table
lionstreetroadbeds1=filter(lionstreetroadbeds,PhysicalID=='0')
lionstreetroadbeds1$SegmentID2=paste(lionstreetroadbeds1$SegmentID,lionstreetroadbeds1$NodeIDFrom,lionstreetroadbeds1$NodeIDTo,sep='|')
lionstreetroadbeds2=filter(lionstreetroadbeds,SegmentID %in% dupsegment)
lionstreetroadbeds2$SegmentID2=paste(lionstreetroadbeds2$SegmentID,lionstreetroadbeds2$NodeIDFrom,lionstreetroadbeds2$NodeIDTo,sep='|')
lionstreetroadbeds3=filter(lionstreetroadbeds,(PhysicalID!='0')&!(SegmentID %in% dupsegment))
lionstreetroadbeds1=merge(lionstreetroadbeds1,segmentfromto,by=c('SegmentID','SegmentID2'),all.x=T)
lionstreetroadbeds1=select(lionstreetroadbeds1,-SegmentID2,-OnStreetCode,-OnStreetCode2,-FromStreetCode,-FromStreetCode2,-ToStreetCode,-ToStreetCode2)
lionstreetroadbeds2=merge(lionstreetroadbeds2,segmentid2fromto,by=c('SegmentID','SegmentID2'),all.x=T)
lionstreetroadbeds2=select(lionstreetroadbeds2,-SegmentID2,-OnStreetCode,-OnStreetCode2,-FromStreetCode,-FromStreetCode2,-ToStreetCode,-ToStreetCode2)
lionstreetroadbeds3=merge(lionstreetroadbeds3,physicalfromto,by='PhysicalID',all.x=T)
lionstreetroadbeds3=select(lionstreetroadbeds3,-PhysicalID2,-OnStreetCode,-OnStreetCode2,-FromStreetCode,-FromStreetCode2,-ToStreetCode,-ToStreetCode2)
newlionstreetroadbeds=rbind(lionstreetroadbeds1,lionstreetroadbeds2,lionstreetroadbeds3)
write.csv(newlionstreetroadbeds,paste0(dirname(path),'/new_lion_street_roadbeds.csv'),row.names=F)

