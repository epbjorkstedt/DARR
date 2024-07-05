  DARR_v2.03_R  -- Eric P. Bjorkstedt, NOAA Fisheries, SWFSC  5 July 2024

  This version of DARR updates the mode of use to facilitate batch processing or integration in replicable workflows by
  substuting functional arguments for 'point-and-click' operations to designate data files, number of traps in an experiment,
  and any strata to be pooled a priori.

  Use will still proceed as sourcing this file (DARR_v2.03.R) to bring in the various functions (some of which revised)
  for subsequent invocation as "Darr_R(...)"

  Note that the operational code has not been altered or updated to the tidyverse, but default plot has been updated to
  use ggplot and to indicate pooled groups of strata by color

  Arguments are:
     csvDataFileName (default = 'exampleMR_csvDataFile.csv' [included in repo])
     NTraps (default = 1)
     pool.apriori (default = NULL)
     makePlot (default = TRUE)

  Original documents presenting DARR are included in repo.
  
  Citation should continue to be 
  Bjorkstedt, E. 2005. DARR, 2.0: updated software for estimating abundance from stratified mark-recapture data. 
  NOAA Technical Memorandum NMFSSWFSC-368.
