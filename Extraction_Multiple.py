 #Extract files with specific extension from multiple ZIP files.
import zipfile
target = [ #only .tif files, that store accumulated rainfall in a given pixel
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S000000-E002959.0000.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S003000-E005959.0030.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S010000-E012959.0060.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S013000-E015959.0090.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S020000-E022959.0120.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S023000-E025959.0150.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S030000-E032959.0180.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S033000-E035959.0210.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S040000-E042959.0240.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S043000-E045959.0270.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S050000-E052959.0300.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S053000-E055959.0330.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S060000-E062959.0360.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S063000-E065959.0390.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S070000-E072959.0420.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S073000-E075959.0450.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S080000-E082959.0480.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S083000-E085959.0510.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S090000-E092959.0540.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S093000-E095959.0570.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S100000-E102959.0600.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S103000-E105959.0630.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S110000-E112959.0660.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S113000-E115959.0690.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S120000-E122959.0720.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S123000-E125959.0750.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S130000-E132959.0780.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S133000-E135959.0810.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S140000-E142959.0840.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S143000-E145959.0870.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S150000-E152959.0900.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S153000-E155959.0930.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S160000-E162959.0960.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S163000-E165959.0990.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S170000-E172959.1020.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S173000-E175959.1050.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S180000-E182959.1080.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S183000-E185959.1110.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S190000-E192959.1140.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S193000-E195959.1170.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S200000-E202959.1200.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S203000-E205959.1230.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S210000-E212959.1260.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S213000-E215959.1290.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S220000-E222959.1320.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S223000-E225959.1350.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S230000-E232959.1380.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150826-S233000-E235959.1410.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S000000-E002959.0000.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S003000-E005959.0030.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S010000-E012959.0060.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S013000-E015959.0090.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S020000-E022959.0120.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S023000-E025959.0150.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S030000-E032959.0180.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S033000-E035959.0210.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S040000-E042959.0240.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S043000-E045959.0270.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S050000-E052959.0300.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S053000-E055959.0330.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S060000-E062959.0360.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S063000-E065959.0390.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S070000-E072959.0420.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S073000-E075959.0450.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S080000-E082959.0480.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S083000-E085959.0510.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S090000-E092959.0540.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S093000-E095959.0570.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S100000-E102959.0600.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S103000-E105959.0630.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S110000-E112959.0660.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S113000-E115959.0690.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S120000-E122959.0720.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S123000-E125959.0750.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S130000-E132959.0780.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S133000-E135959.0810.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S140000-E142959.0840.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S143000-E145959.0870.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S150000-E152959.0900.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S153000-E155959.0930.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S160000-E162959.0960.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S163000-E165959.0990.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S170000-E172959.1020.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S173000-E175959.1050.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S180000-E182959.1080.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S183000-E185959.1110.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S190000-E192959.1140.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S193000-E195959.1170.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S200000-E202959.1200.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S203000-E205959.1230.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S210000-E212959.1260.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S213000-E215959.1290.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S220000-E222959.1320.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S223000-E225959.1350.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S230000-E232959.1380.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150827-S233000-E235959.1410.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S000000-E002959.0000.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S003000-E005959.0030.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S010000-E012959.0060.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S013000-E015959.0090.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S020000-E022959.0120.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S023000-E025959.0150.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S030000-E032959.0180.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S033000-E035959.0210.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S040000-E042959.0240.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S043000-E045959.0270.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S050000-E052959.0300.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S053000-E055959.0330.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S060000-E062959.0360.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S063000-E065959.0390.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S070000-E072959.0420.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S073000-E075959.0450.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S080000-E082959.0480.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S083000-E085959.0510.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S090000-E092959.0540.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S093000-E095959.0570.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S100000-E102959.0600.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S103000-E105959.0630.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S110000-E112959.0660.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S113000-E115959.0690.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S120000-E122959.0720.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S123000-E125959.0750.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S130000-E132959.0780.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S133000-E135959.0810.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S140000-E142959.0840.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S143000-E145959.0870.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S150000-E152959.0900.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S153000-E155959.0930.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S160000-E162959.0960.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S163000-E165959.0990.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S170000-E172959.1020.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S173000-E175959.1050.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S180000-E182959.1080.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S183000-E185959.1110.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S190000-E192959.1140.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S193000-E195959.1170.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S200000-E202959.1200.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S203000-E205959.1230.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S210000-E212959.1260.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S213000-E215959.1290.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S220000-E222959.1320.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S223000-E225959.1350.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S230000-E232959.1380.V06B.zip',
'3B-HHR-GIS.MS.MRG.3IMERG.20150828-S233000-E235959.1410.V06B.zip'
]
for x in target:
        #handle = zipfile.ZipFile(x)
       # handle.extractall('C:\MGEO YEAR 2\MSC THESIS\pythonProject'+ x)

        handle = zipfile.ZipFile(x)
        for x in handle.namelist():
            if x.endswith('.liquid.accum.tif'):
                handle.extract(x, 'Rainfall') #saved in folder called Rainfall