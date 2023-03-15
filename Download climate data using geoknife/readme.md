These scripts download climate data in the format needed by the climate library: averages across each ecoregion. They rely on the USGS geoknife package, which uses remote servers to do all the processing and returns you tabular data that we can then rework into the right format. At the moment, the scripts only do one climate region, but when I add the functionality to do multiple ecoregions I'll update here. For now hopefully it's not too onerous to run the script a few times for each ecoregion.

The two scripts are slightly different, in that they access different datasets. I figure I'd upload them both so folks can see how you might have to change the script to access different data. It comes down to what URLs you're iterating over, and how you specify those URLs. That formatting might depend from dataset to dataset, or THREDDS server to THREDDS server. They're well commented, so hopefully it all makes sense. There's also excellent tutorials available, linked in the scripts.

Sam Flake
swflake@ncsu.edu
