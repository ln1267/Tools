#####################################################
## NAME: BatchASCIItoGRID
## Source Name: BatchASCIItoGRID.py
## Version: ArcGIS 9.2
## Author: Joe Wheaton & Pete Bunting (IGES, UWA)


## Description:  Converts ASCII rasters to other format.
## Date: 19 April 2007
## Updated: 27 July 2007
#####################################################

import sys, os, arcgisscripting, string

gp = arcgisscripting.create()

msgNonExist="Output location does not exist: "
msgSuccess="Successfully converted: "
msgFail="Failed to convert: "

gp.AddMessage("STARTED")

try:


# Argument 1 is the list of Rasters to be converted
    inRasters = gp.GetParameterAsText(0)
    ##gp.AddMessage("inRasters: " + inRasters)   

    # The list is split by semicolons ";"
    inRasters_list = string.split(inRasters,";")
    ##gp.AddMessage("inRasters_list: " + inRasters_list)
    
# Argument 2 is the path to place the new rasters in    
    # The output workspace where the shapefiles are created
    outWorkspace = gp.GetParameterAsText(1)
    ##gp.AddMessage("outWorkspace: " + outWorkspace)  

    # Error trapping, in case the output workspace doesn't exist
    if not gp.Exists(outWorkspace):
        raise Exception, msgNonExist + "%s" % (outWorkspace)


# Argument 3 is the file extension or format to convert the rasters to 
    ext = gp.GetParameterAsText(2)
    ##gp.AddMessage("EXT: " + ext)  
    ##gp.AddMessage("Got Arguments")
    # Get proper extension based on the format string
    if (ext == "IMAGINE Image"):
        ext = ".img"
    elif (ext == "TIFF"):
        ext = ".tif"
    elif (ext == "GRID"):
        ext = ""
 
    ##gp.AddMessage("EXT: " + ext)    
        
# Argument 4 is the type of raster (Integer or Float)    
    format = gp.GetParameterAsText(3)

    
    # Loop through the list of input Rasters and convert/copy each to the output geodatabase or folder
    for inRaster in inRasters_list:
        # Get filename correct
        rasterBaseWithExt = os.path.split(inRaster)[1]
        rasterBase = string.split(rasterBaseWithExt,".")[0]
        ourRasterBase = rasterBase
        rasterBase = "\\" + rasterBase
        outRaster = outWorkspace + rasterBase + ext
        
        try:
            
            # Copy/Convert the inRaster to the outRaster
            gp.ASCIIToRaster_conversion(inRaster, outRaster, format)
##            gp.ASCIIToRaster_conversion(inRaster, outRaster, "FLOAT")
            # If the Copy/Convert was successfull add a message stating this
            gp.AddMessage(msgSuccess + "%s To %s \n\n" % (rasterBaseWithExt, ourRasterBase))
            

        except Exception, ErrorDesc:
            # Except block for the loop. If the tool fails to convert one of the Rasters, it will come into this block
            #  and add warnings to the messages, then proceed to attempt to convert the next input Raster.
            WarningMessage = (msgFail + "%s" % (rasterBase))

            if gp.GetMessages(2) != "":
                WarningMessage = WarningMessage + ". " + (gp.GetMessages(2))
            elif ErrorDesc != "":
                WarningMessage = WarningMessage + (str(ErrorDesc))

            # Add the message as a warning.
            gp.AddWarning(WarningMessage)

except Exception, ErrorDesc:
    # Except block if the tool could not run at all.
    #  For example, not all parameters are provided, or if the output path doesn't exist.
    gp.AddError(str(ErrorDesc))
    
