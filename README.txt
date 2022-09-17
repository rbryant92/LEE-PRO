Abstract:
This app provides a number of visualizations pertaining to Patient Reported Outcome (PRO), efficacy comparisons for PFS and OS, and Adverse Events for six studies.

Key Functions:
This app utilizes the functions getData() and manipulateData().   
The getData() function loads the datasets ADAE, ADSL, ADSLSUPP, ADQS, ADTTEQS, and ADRECSL.   There is some naming change here in order to keep names consistent across studies.
The manipulateData() function creates the datasets which are passed to the tabs.  These datasets are called: tteData, proData, aeData, and comparisonData.  
Subgroups for PFS_3Month, AESI_3Month etc. are created by making data frames called survData and aesiData.

Sidebar UI Elements:
There are the following elements on the sidebar: study, UI elements varSel, valueSel, singleSelection, SliderRng, pparaSelect, compvarm, compvarm_yn1, compvarm_sw1, compvarm_sd1, compvarm_gr1, compvarm_tx1, compvarm_keep1, compvarm_yn2, compvarm_sd2, compvarm_gr2, compvarm_tx2.  The code to create these is in the server.R file in the Shiny app, and these feed the pop.data(), subpop.data(), paratte.data(), subvartte.data(), and ttevent functions.   The code to perform this is virtually identical to the code from the KM_Cox_20190911 (SOLAR1) and CTL-019 apps.  These create the Subgroup and Comparison variable features on the sidebar.
  
Tabs:
All of these pull the subpop.data() reactive and filter subject IDs based on this, in order to filter using the subgroup.   At this time, only the Efficacy tab is equipped to make use of the Comparison variable features.

  * PRO
    - Utilizes the proData data table.
    - Four graphs, visualizing baseline PRO, individual line plots of PRO progression (Level = Individual only), change from baseline PRO, and percent change from baseline PRO
  * Efficacy
    - Utilizes the pop.data and ttevent functions.
    - Three graphs displaying forest plot, variable comparison plot, and KM curves followed by Cox model output (virtually identical to )
  * Safety
    - Utilizes the aeData data table.
    - Graph displaying time until AEs and DT displaying all AEs
  * Bubble Plot
    - Utilizes the comparisonData data table.
    - Visualizes relationship between OS, PFS (divided into Censor/Event times) or AE SOC classes over time and PRO deterioration (configurable)
  * Heatmap
    - Utilizes the proData and aeData data tables and creates the mapData data table from these.
    - Visualizes PRO over time with configurable groupings
  
  