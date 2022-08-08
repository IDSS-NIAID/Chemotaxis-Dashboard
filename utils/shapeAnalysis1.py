import os
import pandas as pd
import SimpleITK as sitk
import numpy as np
import time
from csv import reader
import sys

from skimage.measure import label, regionprops

### NOTES FOR RUNNING IN BIOWULF ###
## Rilyn 8/5/22 ##
#
# Set up a custom Python environment with the proper packages loaded
# instructions here: https://hpc.nih.gov/apps/python.html#envs 
# 1. set up Mamba
# wget https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-Linux-x86_64.sh
# bash Mambaforge-Linux-x86_64.sh -p /data/$USER/conda -b
# rm Mambaforge-Linux-x86_64.sh
# 2. activate base Mamba environment
# source /data/$USER/conda/etc/profile.d/conda.sh && source /data/$USER/conda/etc/profile.d/mamba.sh
# export MAMBA_NO_BANNER=1
# mamba activate base
# which python3
# mamba update --all
# mamba clean --all --yes
# 3. deactivate base mamba and set up a custom environment, we will call it project1
# mamba create -n project1 python=3.8 numpy pandas SimpleITK scikit-image time
# 4. activate project1 and run your code
# mamba activate project1
# python3 shapeAnalysis1.py <DATASET (without .csv)>
#
### END OF NOTES ###


ROOT = '/vf/users/IDSS_projects/liuy5/Chemotaxis' 

TRACK = 'trackResults'
SEG = 'segResults'

#DATASET = '20180215_CH5_RT_fMLF'
#write name of data set, without .csv, after python3 shapeAnalysis1.py
DATASET = sys.argv[1]

print('Processing dataset:', DATASET)

FRAMES = '/data/IDSS_projects/good_frames1'
#DAT_SPL = DATASET.split("_")
#DATE = DAT_SPL[0]
#CH = DAT_SPL[1]

#load the csv file and the nii.gz mask file
df = pd.read_csv(os.path.join(ROOT, TRACK, DATASET, DATASET + '.csv'))
msk = sitk.ReadImage(os.path.join(ROOT, SEG, DATASET + '.nii.gz'))

#Read in the good frames file -- will only select and compute statistics for frames in this file
with open(os.path.join(FRAMES,DATASET+"_goodFrames.csv")) as read_obj:
  csv_reader = reader(read_obj)
  frames_list = list(csv_reader)
  for i in range(len(frames_list)):
    for j in range(len(frames_list[i])):
      if frames_list[i][j] != ' ':
        frames_list[i][j] = int(frames_list[i][j])
  #print(frames_list)

#get the numpy representation of the segmentation mask
#needed for skimage processing
mskNpy = sitk.GetArrayFromImage(msk)

print('Image shape:',mskNpy.shape)

#compute regionprops(label measurements) for each frame
st = time.time()
props = []
for z in range(mskNpy.shape[0]):
  lbl = label(mskNpy[z])
  pps = regionprops(lbl)
  props.append(pps)

et = time.time()
print('Regionprops calculated in:', et-st, 'seconds')

#find track ids
tracks = list(set(df['Track']))

# start with empty list and populate it with the relevant statistics at 'good frames'
d = []
for i in range(len(tracks)):
  #use the first track as an example
  track = tracks[i]
  
  t = [track]
  
  #find frames for the current track
  frames = df[df['Track'].isin(t)].values
  #frames = good_frames
  
  #good frames for first track
  check_frames = frames_list[i]
  #print(check_frames)
  
  if(check_frames != ['']):
  
  
    #print("frame num", len(frames))
    for f in frames:
      #z: current frame
      z = f[1]
      x = f[2]
      y = f[3]
    
      #only want shape data for 'good frames' which are in the list check_frames
      if (z in check_frames):
      #get the regionprops in the current frame
        pps = props[z]
        
        for pp in pps:
          #find the label containing the (x,y) coords
          #right now I don't have a better/faster way to check which prop contains the current point
          #for more regionprops details including different measurements
          #see https://scikit-image.org/docs/stable/api/skimage.measure.html#skimage.measure.regionprops
          #print(pp.coords.shape)
  
          for coord in pp.coords:
            if y == coord[0] and x == coord[1]:
             #print('Track:',t,'Frame:',z,'X:',x,'Y:',y,'Perimeter:',pp.perimeter,'Area:',pp.area)
             d.append(
             {
              'Track': t,
              'Frame': z,           
              'X':  x,
              'Y': y,
              'Perimeter': pp.perimeter,
              'Area': pp.area         
             }
            )
#save as dataframe for easier conversion to csv        
shape_dat = pd.DataFrame(d)
#print(shape_dat)
DASH = '~/Desktop/Chemotaxis-Dashboard'
#save data in csv
shape_dat.to_csv(os.path.join(DASH,'shape_data',DATASET+'_shape.csv'))
print('done')
