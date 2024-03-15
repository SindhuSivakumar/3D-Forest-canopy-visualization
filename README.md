# 3D Forest canopy visualization of ETH canopy height dataset (Bavaria)

3D visualization of ETH canopy height (2020) data for the state of Bavaria using ggplot2 and rayshader in R.

### Data : 
1. ETH_GlobalCanopyHeight_10m_2020_version1
Global canopy height map for the year 2020 derived from Sentinel-2 and GEDI with a spatial resolution of 10m. Combined GEDI and Sentinel-2 to construct a probabilistic deep learning model to obtain canopy height from Sentinel-2 photos anywhere on Earth and quantify the uncertainty in these estimations.(https://www.research-collection.ethz.ch/handle/20.500.11850/609802)
2. GADM for state boundary (https://gadm.org/index.html)

### R Packages used:
tidyverse, sf, geodata, terra, classInt, ggplot2, rayshader

### Result Visualization:
![Alt text](https://github.com/SindhuSivakumar/3D-Forest-canopy-visualization/blob/main/Bavaria-forest-height.png)
