#
# PPS Python ftps script
# Usage:  python script.py   (requires Python 3)
#

import os, sys, hashlib
from ftplib import FTP_TLS
import urllib
import urllib.request

downloadCount=0
skipCount=0
ftpConnection=None
forceHttps=False
userid='c.nabukulu@student.utwente.nl'

def usage():
  print ('Usage:  python script.py [https]  (requires Python 3)')
  print ('Connects to the PPS ftps server and pulls files generated during order processing.')
  print ('Confirms download with a SHA checksum calculation.')
  print ('Will use https requests if connection to ftps server fails.')
  print ('Argument:  https - force https connection')

def hashfile(filename, blocksize=65536):
  hasher = hashlib.sha1()
  with open(filename, 'rb') as localfile:
    buf = localfile.read(blocksize)
    while len(buf) > 0:
      hasher.update(buf)
      buf = localfile.read(blocksize)
  return hasher.hexdigest()

def calcCksum(filename,cksum=None):
  if (cksum):
    sha = hashfile(filename)
    print (' cksum Pass' if cksum==sha else ' cksum FAIL')

def get(filepath,cksum=None):
  if (ftpConnection):
    getFtpFile(filepath,cksum)
  else:
    getHttpsFile(filepath,cksum)

def getFtpFile(filepath,cksum=None):
  global ftpConnection,downloadCount,skipCount
  path,filename=os.path.split(filepath)
  ftpConnection.cwd(path)
  download=True
  ftpSize=ftpConnection.size (filename)
  # determine if file exists in local directory
  if (os.path.exists(filename)):
    # check size and cksum
    if (cksum):
      sha = hashfile(filename)
      download=(cksum != sha)
    else:
      filesize=os.path.getsize(filename)
      download=(ftpSize!=filesize)

  if (download):
    # if not exists or file checks do not match, get file.
    downloadCount+=1
    sys.stdout.write( str(downloadCount)+') Downloading '+filename+'   '+str(ftpSize)+' bytes')
    sys.stdout.flush()
    with open(filename, 'wb') as localfile:
      ftpConnection.retrbinary('RETR ' + filename, localfile.write, 1024)
    calcCksum(filename,cksum)
  else:
    print ('Already downloaded '+filename)
    skipCount+=1

def getHttpsFile(filepath,cksum=None):
  global downloadCount
  path,filename=os.path.split(filepath)
  with urllib.request.urlopen('https://arthurhouhttps.pps.eosdis.nasa.gov'+filepath) as response, open(filename, 'wb') as localfile:
    downloadCount+=1
    sys.stdout.write(str(downloadCount)+') Downloading '+filename)
    sys.stdout.flush()
    localfile.write(response.read())
    localfile.close()
    calcCksum(filename,cksum)

def getPpsFiles():
  global ftpConnection,forceHttps
  print ('Connecting to PPS')
  if forceHttps==False:
    try:
      ftpConnection = FTP_TLS('arthurhouftps.pps.eosdis.nasa.gov')
      print (ftpConnection.getwelcome())
      ftpConnection.login(userid,userid)
      ftpConnection.sendcmd('TYPE i')
      print ('Connected.  Getting files...')
    except Exception as e:
      print ('Failed to connect to the PPS ftps server due to ' + str(e))
      print ('Trying https')
      ftpConnection=None

  if (ftpConnection==None):
    password_mgr = urllib.request.HTTPPasswordMgrWithDefaultRealm()
    password_mgr.add_password(None,'https://arthurhouhttps.pps.eosdis.nasa.gov',userid,userid)
    handler = urllib.request.HTTPBasicAuthHandler(password_mgr)
    opener = urllib.request.build_opener(handler)
    urllib.request.install_opener(opener)

  # The following is the list of PPS files to transfer:
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S000000-E002959.0000.V06B.zip','ce82a32890c2b891eafdfaebaa2d107c538bd9b6')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S003000-E005959.0030.V06B.zip','33da0df515f21fae42329ed83f04f6b1a857175e')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S010000-E012959.0060.V06B.zip','99e7154baf640900083b55172825230c2200599d')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S013000-E015959.0090.V06B.zip','6fc80450b77a8d8e4257eec9cc8ad070a58627fc')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S020000-E022959.0120.V06B.zip','f74f4ee73333dda7c426fb2b56092de77ff8a727')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S023000-E025959.0150.V06B.zip','8fb23ea8b38abe91abf16c86de82a2e571d42bff')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S030000-E032959.0180.V06B.zip','53959d210c4401140691c9d28723a3f56dc5a60b')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S033000-E035959.0210.V06B.zip','1b773294f449e5b379777d3a02141207521a0c31')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S040000-E042959.0240.V06B.zip','094e6b2550368219144bfaa82f777e0207a3a8ea')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S043000-E045959.0270.V06B.zip','f4632100f6826cfbedf9fbef34f2e79584ad3ca5')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S050000-E052959.0300.V06B.zip','fadfce2182686cd6e7e52e02820e327cf30ce48c')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S053000-E055959.0330.V06B.zip','f628751c2ef625bd6a603e92cecac69fcc8a0bb1')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S060000-E062959.0360.V06B.zip','e6082a960b38eb7863aa456a67738ffba1c21bee')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S063000-E065959.0390.V06B.zip','9ccca67db72a4aa98aaa1a621cc45f7d10a72799')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S070000-E072959.0420.V06B.zip','78fbea6837987ec7ad9eef48cace9ae398e22b2f')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S073000-E075959.0450.V06B.zip','d2d3f6f972e5a4fd5fe19002b6b0bf88a6ed867b')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S080000-E082959.0480.V06B.zip','ad55f523e2cec0e4b6cb9148175819b1acfd9ba8')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S083000-E085959.0510.V06B.zip','ddb641fadf9df87ac5f2733bcb9784bbe505e31d')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S090000-E092959.0540.V06B.zip','4bb606027c6359d3b59bca6420c1b1c0d6d5f44a')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S093000-E095959.0570.V06B.zip','2c2ad36c23645286b54c476e7d56648e2dfe4426')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S100000-E102959.0600.V06B.zip','26c103abd68721a388af108c25ad9b9008db6bcc')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S103000-E105959.0630.V06B.zip','b6985532ab631800f945ca68f243d4cfd009d750')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S110000-E112959.0660.V06B.zip','9333172d77f7f25f8df4558bae45bdc5c81d8fd2')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S113000-E115959.0690.V06B.zip','5bce0f1f5d52fbaab062b283d5b612e1f7215c3a')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S120000-E122959.0720.V06B.zip','5686f4afb2ae750c449664bc84c487f874b762a8')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S123000-E125959.0750.V06B.zip','c67f4bc665289b12531623346e744725079cd014')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S130000-E132959.0780.V06B.zip','56043e6650920cc447763cbb0cfc26097ada104a')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S133000-E135959.0810.V06B.zip','895f1c03ab457fd496cc93e98855bd2314f76d53')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S140000-E142959.0840.V06B.zip','c1f438461031505dc4f6571b4b9500a5ed3ed2d5')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S143000-E145959.0870.V06B.zip','68aed6165f7d6d10f5c368308b189427e17605fd')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S150000-E152959.0900.V06B.zip','f5d15b0d3ba45f42b98b710348f1a9a8bc5391e2')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S153000-E155959.0930.V06B.zip','1c6cd9ed50534b68368a4f3ff9465d27b4ea9b74')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S160000-E162959.0960.V06B.zip','85605ce4a96672ca38288a09ba606ab45918467c')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S163000-E165959.0990.V06B.zip','264f851646becd8d2cddf12fdf16f5940fbbd1a8')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S170000-E172959.1020.V06B.zip','35c92a72d8924d23dab92fcd7e4749f37ed55330')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S173000-E175959.1050.V06B.zip','c4351aec55f88be87e613aa98ea86df1dba84ec6')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S180000-E182959.1080.V06B.zip','0d1209dd8abc9dc3b5a9f3b63af2d80598923407')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S183000-E185959.1110.V06B.zip','2bee9f971b9ff7e5b94ab8c7556860dbdf1c82f9')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S190000-E192959.1140.V06B.zip','507906bc28ac6f125ef4ec417ea1b5e9c8aff3bc')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S193000-E195959.1170.V06B.zip','eace5c76336fb62c6743c7b73a4197828f360e6c')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S200000-E202959.1200.V06B.zip','cbcd8e090f11ec5879e0afa63344957dd4d6d74c')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S203000-E205959.1230.V06B.zip','3ba05cb83f72e2246ddedb1260029bde11e341c1')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S210000-E212959.1260.V06B.zip','337d000609646665b7ce45cd4b9198c0efed9e0e')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S213000-E215959.1290.V06B.zip','632609817a31f3ae40e8d9e8f4cc83b7ba0feefd')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S220000-E222959.1320.V06B.zip','8d25c208d713dd9b9b49f857a187c3a92d93a9ee')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S223000-E225959.1350.V06B.zip','67cbf6b57b1e97186cb0c0fc95362f1f1abe8be8')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S230000-E232959.1380.V06B.zip','4f7c16ce9dd0a0e723a3c46f8ec37a4f007f421a')
  get('/gpmallversions/V06/2015/08/26/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150826-S233000-E235959.1410.V06B.zip','532238d12c4ed37d4ffd39872e7ef4603650d5ee')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S000000-E002959.0000.V06B.zip','70f07e79472364124d387d9292a797d88a4ff9bd')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S003000-E005959.0030.V06B.zip','d661f48d3665c2f778f57350f498b1cf1973a061')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S010000-E012959.0060.V06B.zip','77e7fb92f460a8de1f13731c5222fda8386e6441')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S013000-E015959.0090.V06B.zip','e2ecab2f92454878acce5596955beb2ad7f294fd')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S020000-E022959.0120.V06B.zip','2cb7fb6e0a811a8bfa1369607c4b1fd65217ad07')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S023000-E025959.0150.V06B.zip','e8ef0cc8ad19244d7733172ce921ac5ffc41cf50')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S030000-E032959.0180.V06B.zip','aba06d4fa1a184f35c17bc844a5f107b6c2d0197')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S033000-E035959.0210.V06B.zip','69af687244be0db96c2fe174c854ef30e46a41d9')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S040000-E042959.0240.V06B.zip','557e5a863cedafa7417fd18ee6b27a3161e38099')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S043000-E045959.0270.V06B.zip','308699bad3690324ee094ce7c783a14bcaacbfae')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S050000-E052959.0300.V06B.zip','4df9a1b513b725b377cfbe6661b2a16c64ccfa61')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S053000-E055959.0330.V06B.zip','0b31205ea68e18f9a8c011223d8c38defbece6ea')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S060000-E062959.0360.V06B.zip','e5934be9d1cf741eee2dd3ee4b33eaee22f258d5')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S063000-E065959.0390.V06B.zip','380525497b03617967aafcba50cbe3b2d761fc61')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S070000-E072959.0420.V06B.zip','100d5b9ae4e29d8fd557764551f3a5c55dafa7a7')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S073000-E075959.0450.V06B.zip','ad6cd0b49ae10c31e9b0c68b50f659b3c85dc31f')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S080000-E082959.0480.V06B.zip','903171e42be3ea5c047af5b068233233009bf926')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S083000-E085959.0510.V06B.zip','3b4bfa0bae2a5c19bc65b63ba95abda21922b42b')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S090000-E092959.0540.V06B.zip','dbd013d29b47595bad5ff31899ddc6fa4fd510eb')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S093000-E095959.0570.V06B.zip','072da429e45f9f53c6d0e8d5504df83e014130e9')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S100000-E102959.0600.V06B.zip','b4ca0b5d6eb145684b808776d3c701e9ba9892d2')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S103000-E105959.0630.V06B.zip','b31e48d9126414e707fea6de910277e52b89f749')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S110000-E112959.0660.V06B.zip','00e0b10b118ba3080e68ee52f672569667466a77')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S113000-E115959.0690.V06B.zip','c6a12867db202bb1bf4f9a28eb8a241bf201b320')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S120000-E122959.0720.V06B.zip','cc408441b70a2dfcc8f8407dc5a87ddcede066c2')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S123000-E125959.0750.V06B.zip','4493231149bb793412b1926978510ad5820cfa25')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S130000-E132959.0780.V06B.zip','87c986f2790b8629d3e7694e6277f5f40676c9bf')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S133000-E135959.0810.V06B.zip','178c95370ffc00694ab5bbf529c4d63f4ac725e4')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S140000-E142959.0840.V06B.zip','f4f06605433505d18c592f78e96928ad066958b4')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S143000-E145959.0870.V06B.zip','03a71bc70ba440f952b845c9588afb8a39dc7766')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S150000-E152959.0900.V06B.zip','6442627583596bcd6c41b7ce8422864718fd4621')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S153000-E155959.0930.V06B.zip','1342185a4c6fbd930f59d7a57f15a77b68e16441')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S160000-E162959.0960.V06B.zip','ccdc99282d268761e0a17152464678c6c5752fff')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S163000-E165959.0990.V06B.zip','9573a15b1f9d5cf99472f4d61093645a8bf4b959')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S170000-E172959.1020.V06B.zip','e77a3074a931999a965537f04f888e3ca5533051')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S173000-E175959.1050.V06B.zip','3509e1897fbf9a68ea5d1fbfaa94e6ca34957249')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S180000-E182959.1080.V06B.zip','475ca3ed075d6b9dc9b0743ed5d7579403f68305')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S183000-E185959.1110.V06B.zip','97c14b5ceabbb5768545feae0d413a512c66377a')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S190000-E192959.1140.V06B.zip','dac080f82cbdee41a5b5ae70a6347ab19699c962')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S193000-E195959.1170.V06B.zip','848cbd730bfa8a56efb9b8e33e785374fc4e801e')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S200000-E202959.1200.V06B.zip','4e8854fda21be052927c62bf421f5d6343a880c3')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S203000-E205959.1230.V06B.zip','30c078859d4bfc58bb43399e124ea14ea0f72c73')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S210000-E212959.1260.V06B.zip','19e8cb2d1a66ad7939ecbc7221ab3532149a5f86')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S213000-E215959.1290.V06B.zip','3988b384ee7299f8ebb3538c1a4aedfd0030a817')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S220000-E222959.1320.V06B.zip','6232ddae3206f7a17c72cffbd85b63a207722b92')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S223000-E225959.1350.V06B.zip','0f882a3c9e57678c8418c1746569278964d37b92')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S230000-E232959.1380.V06B.zip','b0f0fb3f9b6786bbe84221503bb8e537cda3f472')
  get('/gpmallversions/V06/2015/08/27/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150827-S233000-E235959.1410.V06B.zip','2a4203601af327ef24cd516085efebe4d24bbfc5')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S000000-E002959.0000.V06B.zip','58487d226fa900cefbcec68729f045ffaa1b0a67')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S003000-E005959.0030.V06B.zip','16d902a5a4886c783524711424d8e0add8c35e59')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S010000-E012959.0060.V06B.zip','9c9f0df2d993bcf13e4234dc1e01a6f1476e5973')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S013000-E015959.0090.V06B.zip','0603895a21d847f3e16b2fa38f27c48a192f0e0c')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S020000-E022959.0120.V06B.zip','fe0320929d835ca77096098bdd6513c6bb2ee144')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S023000-E025959.0150.V06B.zip','38a4c677a82abf33b9dc6551d4b7f6597bc06b4c')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S030000-E032959.0180.V06B.zip','93ac35129a9a44ea914f6bad7da22547ce06c841')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S033000-E035959.0210.V06B.zip','11d5cc513bca86045c90fda255421364c64a7a79')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S040000-E042959.0240.V06B.zip','cea37719ff156efbd70aca45db78aa4de100f5d2')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S043000-E045959.0270.V06B.zip','1fe1d26c829d73e61835a413a43bddc26121217d')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S050000-E052959.0300.V06B.zip','8f1fd0b1a169d1dd548fdb4252b4c68124db598f')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S053000-E055959.0330.V06B.zip','fabee236c7a9bccb0ed3dfac3d7957c68518cc64')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S060000-E062959.0360.V06B.zip','08ddd01afbe7638ab46aa145d65a9481b9acf8bd')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S063000-E065959.0390.V06B.zip','610703f508efded5c2e4ef6ee1aaead86b231d45')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S070000-E072959.0420.V06B.zip','4d88394d7fd02b815559e8cffa7012ea21ed7850')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S073000-E075959.0450.V06B.zip','20795a512835e5eabc7cccf8d75638e127e5f89e')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S080000-E082959.0480.V06B.zip','f8d2773c4ca0d687543f598958ed14672595f4ed')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S083000-E085959.0510.V06B.zip','820ee74b264f9f0494b81ae98cbd9b79f3fec453')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S090000-E092959.0540.V06B.zip','58b106a19d8f9741453a22182df41f4b72d44377')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S093000-E095959.0570.V06B.zip','067545336085aab6082b1b8c8b9a9ed3da366d41')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S100000-E102959.0600.V06B.zip','61bcec3de607f5ac3677be7ad630222b9ed6b17b')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S103000-E105959.0630.V06B.zip','5436204fa4db45db75da0bc1bc462f3c4a6350b0')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S110000-E112959.0660.V06B.zip','82241babfcee0941bdb7f41e778485e4c8ad1432')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S113000-E115959.0690.V06B.zip','eb519d7d6f1313da2fe48226855e6b282e60508f')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S120000-E122959.0720.V06B.zip','cfc532ad1da3fbc4c0560332d40656b483a224d2')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S123000-E125959.0750.V06B.zip','7519e9c704037c13b223152176a6886a182173d1')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S130000-E132959.0780.V06B.zip','196b1cf9120e09d5b1c991afbaab46a69513807e')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S133000-E135959.0810.V06B.zip','710a4fa13defa152b047250f90a889c70937f1eb')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S140000-E142959.0840.V06B.zip','35cad97f714f7136fc83bdf6d1e7775a5575cff1')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S143000-E145959.0870.V06B.zip','e4828e406dbe6783b678688d129e50e55a90d294')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S150000-E152959.0900.V06B.zip','d6c2e5ea3d07c2dc6529c526235aebd1c0480be0')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S153000-E155959.0930.V06B.zip','623def2ec9360a86e5028d4e7039d1f26b03dacd')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S160000-E162959.0960.V06B.zip','800355b5064a3a5fef801fb6bf39cbbb5f308ffe')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S163000-E165959.0990.V06B.zip','dbd979c4a2f6fc59a9cffff147c0f66681ff3cbe')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S170000-E172959.1020.V06B.zip','0744f545a3f952971a3d4eea3474a888714ba44a')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S173000-E175959.1050.V06B.zip','1f2ada94a2a535e6cc5798831877b09f70a05703')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S180000-E182959.1080.V06B.zip','6470ee124fce804f4c468363ea1c4a78a0ed09ee')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S183000-E185959.1110.V06B.zip','504d8572c436bfaef3295b3fea47a3ec9740914d')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S190000-E192959.1140.V06B.zip','6f79ce9fb94f6a914d3e1867572ea4172153c1b9')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S193000-E195959.1170.V06B.zip','0503bd087ecd0c514e6c9f6f37cd610ccfbeccae')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S200000-E202959.1200.V06B.zip','40a51c4df1fe8bc606653bbe39c7716fb5e3be3a')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S203000-E205959.1230.V06B.zip','daee0abf507ec6421a2bdbbbcbe008f208b46834')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S210000-E212959.1260.V06B.zip','346a247a7befbded9a16dd981fe606907abf1654')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S213000-E215959.1290.V06B.zip','3ce546f261a16c011adfb20fac91046c7799afad')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S220000-E222959.1320.V06B.zip','96d771d897fcebb211b464c4da0ec71eff8873ea')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S223000-E225959.1350.V06B.zip','45d514bba27fb00e2c815b36b6ececae0db29059')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S230000-E232959.1380.V06B.zip','842d54b61e5afc6a135d5cf946f05b8b0481119c')
  get('/gpmallversions/V06/2015/08/28/gis/3B-HHR-GIS.MS.MRG.3IMERG.20150828-S233000-E235959.1410.V06B.zip','cae3cb42c5f19f46dd59119115b445cd097bbb8a')

  # Transfer complete; close connection

  if ftpConnection:
    ftpConnection.quit()
  print ('Number of files downloaded: '+str(downloadCount))
  if (skipCount>0):
    print ('Number of files already downloaded: '+str(skipCount))

  sys.exit(0)


if __name__ == '__main__':
  if sys.version_info[0]<3:
    raise Exception('Must use Python 3')
  if (len(sys.argv)>1):
    if (sys.argv[1].find('http')!=-1):
      forceHttps=True
    else:
      usage()
      sys.exit(1)
  getPpsFiles()
