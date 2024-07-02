The ClimAVA dataset provides high-resolution (4km) future climate projections derived from seventeen CMIP6 General Circulation Models. It includes three key variables—precipitation, minimum, and maximum temperature—for three Shared Socioeconomic Pathways (SSP245, SSP370, SSP585) on a daily scale. The initial release, ClimAVA-SW, covers the entire U.S. Southwest region, encompassing all watersheds within it.

Employing the newly developed Spatial Pattern Interactions Downscaling (SPID) method, ClimAVA ensures high-quality downscaling by utilizing machine learning models. These models capture the relationship between spatial patterns at Global Circulation Model (GCM) resolution and fine-resolution pixel values. Essentially, a random forest model is trained for each pixel, using the finer reference data as a predictand and nine pixels from the spatially resampled (coarser) version of the reference data as predictors. These models are then utilized to downscale the bias-corrected GCM data. Results from this method have proven to maintain climate realism and greatly represent extreme events.

To download the data directly use this link: https://home.chpc.utah.edu/~u6047395/ClimAVA_sw_data/

To access and download the data, please visit data. A download script is available here for users preferring to download the data using R. Additionally, all scripts and accessory data involved in creating ClimAVA can be found here, along with the data used in training the downscaling process and its validation.
