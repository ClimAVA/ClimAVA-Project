The ClimAVA dataset provides high-resolution (4km) future climate projections derived from seventeen CMIP6 General Circulation Models. It includes three key variables—precipitation, minimum, and maximum temperature—for three Shared Socioeconomic Pathways (SSP245, SSP370, SSP585) on a daily scale. The initial release, ClimAVA-SW, covers the entire U.S. Southwest region, encompassing all watersheds within it.

Employing the newly developed Spatial Pattern Interactions Downscaling (SPID) method, ClimAVA ensures high-quality downscaling by utilizing machine learning models. These models capture the relationship between spatial patterns at Global Circulation Model (GCM) resolution and fine-resolution pixel values. Essentially, a random forest model is trained for each pixel, using the finer reference data as a predictand and nine pixels from the spatially resampled (coarser) version of the reference data as predictors. These models are then utilized to downscale the bias-corrected GCM data. Results from this method have proven to maintain climate realism and greatly represent extreme events.

Use this link to download the data directly: https://home.chpc.utah.edu/~u6047395/ClimAVA_sw_data/

A download script named "ClimAVA_sw_downloading_script.R" is available here for users preferring to download the data using R. 
