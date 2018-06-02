from arcpy import *
lion="LION Streets - Roadbeds"
AddField_management(lion,"Segment","DOUBLE")
CalculateField_management(lion,"Segment","int( !SegmentID!)","PYTHON")
AddField_management(lion,"SegmentID2","TEXT")
CalculateField_management(lion,"SegmentID2","!SegmentID!+'|'+!NodeIDFrom!+'|'+!NodeIDTo!","PYTHON")
AddField_management(lion,"FromStreet","TEXT")
AddField_management(lion,"ToStreet","TEXT")

AddJoin_management(lion,"PhysicalID","physicalfromto.csv","PhysicalID","KEEP_ALL")
CalculateField_management(lion,"lion.FromStreet","!physicalfromto.csv.FromStreet!","PYTHON")
CalculateField_management(lion,"lion.ToStreet","!physicalfromto.csv.ToStreet!","PYTHON")
RemoveJoin_management(lion,"physicalfromto.csv")

AddJoin_management(lion,"Segment","segmentfromto.csv","SegmentID","KEEP_ALL")
SelectLayerByAttribute_management(lion,"NEW_SELECTION","lion.FromStreet IS NULL")
CalculateField_management(lion,"lion.FromStreet","!segmentfromto.csv.FromStreet!","PYTHON")
CalculateField_management(lion,"lion.ToStreet","!segmentfromto.csv.ToStreet!","PYTHON")
RemoveJoin_management(lion,"segmentfromto.csv")
SelectLayerByAttribute_management(lion,"CLEAR_SELECTION")

AddJoin_management(lion,"Segment","segmentid2fromto.csv","SegmentID2","KEEP_ALL")
SelectLayerByAttribute_management(lion,"NEW_SELECTION","lion.FromStreet IS NULL")
CalculateField_management(lion,"lion.FromStreet","!segmentid2fromto.csv.FromStreet!","PYTHON")
CalculateField_management(lion,"lion.ToStreet","!segmentid2fromto.csv.ToStreet!","PYTHON")
RemoveJoin_management(lion,"segmentid2fromto.csv")
SelectLayerByAttribute_management(lion,"CLEAR_SELECTION")

DeleteField_management(lion,["Segment","SegmentID2"])
