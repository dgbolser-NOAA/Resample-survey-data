#V3.30.23.1;_safe;_compile_date:_Dec  5 2024;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#_data_and_control_files: 2017pop.dat // 2017pop.ctl
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS3)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)
#_Cond sd_ratio_rd < 0: platoon_sd_ratio parameter required after movement params.
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
2 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
4 #_Nblock_Patterns
 5 1 1 1 #_blocks_per_pattern 
# begin and end years of blocks
 1918 1991 1992 2001 2002 2007 2008 2008 2009 2010
 1918 2010
 1995 2004
 1918 1999
#
# controls for all timevary parameters 
1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds
#
# AUTOGEN
 1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: P(y)=f(TVP,env_Zscore) w/ logit to stay in min-max;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  5: like 4 with logit transform to stay in base min-max
#_DevLinks(more):  21-25 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, wt-len, maturity, fecundity, (hermaphro), recr_distr, cohort_grow, (movement), (age error), (catch_mult), sex ratio 
#_NATMORT
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity;_6=Lorenzen_range
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
3 #_Age(post-settlement) for L1 (aka Amin); first growth parameter is size at this age; linear growth below this
20 #_Age(post-settlement) for L2 (aka Amax); 999 to treat as Linf
0.055 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
3 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
0 #_First_Mature_Age
2 #_fecundity_at_length option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
2 #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)
#_** in option 1, any male parameter with value = 0.0 and phase <0 is set equal to female parameter
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
 0.02 0.1 0.054 -2.92 0.44 3 -5 0 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 15 25 20.7538 20.8 10 0 3 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 35 45 41.6011 41.4 10 0 2 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.1 0.4 0.166779 0.166 0.05 0 3 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.03 5 1.34872 1.31 0.5 0 4 0 0 0 0 0 0 0 # SD_young_Fem_GP_1
 0.03 5 2.56049 2.68 0.5 0 4 0 0 0 0 0 0 0 # SD_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 0 3 1.003e-05 1.003e-05 99 0 -99 0 0 0 0 0 0 0 # Wtlen_1_Fem_GP_1
 2 4 3.1026 3.1026 99 0 -99 0 0 0 0 0 0 0 # Wtlen_2_Fem_GP_1
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 20 40 32.1 32.1 99 0 -99 0 0 0 0 0 0 0 # Mat50%_Fem_GP_1
 -2 4 -1 -1 99 0 -99 0 0 0 0 0 0 0 # Mat_slope_Fem_GP_1
 0 6 8.66e-10 1 99 0 -99 0 0 0 0 0 0 0 # Eggs_scalar_Fem_GP_1
 -3 5 4.9767 5 99 0 -99 0 0 0 0 0 0 0 # Eggs_exp_len_Fem_GP_1
# Sex: 2  BioPattern: 1  NatMort
 -1 1 0 0.05 0.1 6 -5 0 0 0 0 0 0 0 # NatM_uniform_Mal_GP_1
# Sex: 2  BioPattern: 1  Growth
 -1 1 0 0 0.1 0 -2 0 0 0 0 0 0 0 # L_at_Amin_Mal_GP_1
 -1 1 -0.0664828 -0.066 0.1 0 2 0 0 0 0 0 0 0 # L_at_Amax_Mal_GP_1
 -1 1 0.169132 0.167 0.1 0 3 0 0 0 0 0 0 0 # VonBert_K_Mal_GP_1
 -5 5 0 0 0.5 0 -5 0 0 0 0 0 0 0 # SD_young_Mal_GP_1
 -5 5 -0.115863 0 0.5 0 5 0 0 0 0 0 0 0 # SD_old_Mal_GP_1
# Sex: 2  BioPattern: 1  WtLen
 0 3 9.881e-06 9.881e-06 99 0 -99 0 0 0 0 0 0 0 # Wtlen_1_Mal_GP_1
 2 4 3.1039 3.1039 99 0 -99 0 0 0 0 0 0 0 # Wtlen_2_Mal_GP_1
# Hermaphroditism
#  Recruitment Distribution 
 0 2 1 1 99 0 -99 0 0 0 0 0 0 0 # RecrDist_GP_1
 0 2 1 1 99 0 -99 0 0 0 0 0 0 0 # RecrDist_Area_1
 0 2 1 1 99 0 -99 0 0 0 0 0 0 0 # RecrDist_month_1
#  Cohort growth dev base
 0 2 1 1 99 0 -99 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Platoon StDev Ratio 
#  Age Error from parameters
#  catch multiplier
#  fraction female, by GP
 0.01 0.99 0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#  M2 parameter for each predator fleet
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
             5            20        9.4018            10             5             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1           0.5          0.72          0.15             2         -2          0          0          0          0          0          0          0 # SR_BH_steep
           0.5           1.2           0.7           0.7            99             0         -6          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0            99             0        -99          0          0          0          0          0          0          0 # SR_regime
             0             2             0             1            99             0        -99          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1940 # first year of main recr_devs; early devs can precede this era
2014 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 1900 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 3 #_recdev_early_phase
 5 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1955 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1975 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2012 #_last_yr_fullbias_adj_in_MPD
 2014 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)
 0.7 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -6 #min rec_dev
 6 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_year Input_value
#
# all recruitment deviations
#  1900E 1901E 1902E 1903E 1904E 1905E 1906E 1907E 1908E 1909E 1910E 1911E 1912E 1913E 1914E 1915E 1916E 1917E 1918E 1919E 1920E 1921E 1922E 1923E 1924E 1925E 1926E 1927E 1928E 1929E 1930E 1931E 1932E 1933E 1934E 1935E 1936E 1937E 1938E 1939E 1940R 1941R 1942R 1943R 1944R 1945R 1946R 1947R 1948R 1949R 1950R 1951R 1952R 1953R 1954R 1955R 1956R 1957R 1958R 1959R 1960R 1961R 1962R 1963R 1964R 1965R 1966R 1967R 1968R 1969R 1970R 1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015F 2016F 2017F 2018F 2019F 2020F 2021F 2022F 2023F 2024F 2025F 2026F 2027F 2028F
#  0.00265608 0.00279212 0.00293307 0.00307876 0.00322844 0.00338211 0.00353892 0.00369816 0.00385838 0.00401863 0.00417636 0.00432964 0.0044803 0.00463293 0.00479019 0.00495215 0.00511808 0.00528793 0.00546234 0.00563745 0.00581655 0.00599472 0.00617121 0.00634328 0.00650789 0.00666408 0.00680719 0.00692984 0.00703633 0.00711744 0.00717595 0.00721668 0.00725513 0.00732359 0.00748037 0.00781971 0.00848328 0.00965425 0.0115364 0.0143116 0.0221458 0.0269695 0.0327411 0.0393303 0.0466054 0.0547083 0.0638741 0.0750287 0.088651 0.104353 0.120292 0.131999 0.132745 0.115696 0.0792704 0.0280154 -0.0281599 -0.0788303 -0.115391 -0.126247 -0.0962772 -0.0157145 0.0622514 0.01427 -0.128862 -0.230559 -0.269888 -0.256351 -0.12836 0.26688 0.787791 0.0275395 -0.284252 -0.26552 -0.234534 0.0239733 -0.241949 -0.00827132 -0.367436 -0.259743 -0.205514 0.0587824 0.468089 0.38752 0.100145 0.069809 0.11028 0.0556485 0.27116 0.821469 0.779449 -0.0407904 -0.481965 -0.414715 0.291588 0.310015 -0.357392 -0.459205 -0.77423 1.0363 1.42974 0.227421 -0.383356 -1.08877 -0.168291 -0.901104 -0.816967 -0.871984 2.62262 -0.591714 -0.139602 0.561692 -1.42002 1.11781 -0.812716 -0.00405196 0 0 0 0 0 0 0 0 0 0 0 0 0
#
#Fishing Mortality info 
0.03 # F ballpark value in units of annual_F
-1999 # F ballpark year (neg value to disable)
1 # F_Method:  1=Pope midseason rate; 2=F as parameter; 3=F as hybrid; 4=fleet-specific parm/hybrid (#4 is superset of #2 and #3 and is recommended)
0.9 # max F (methods 2-4) or harvest fraction (method 1)
# F_Method 1:  no additional input needed
#
#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 0
#_for unconstrained init_F, use an arbitrary initial catch and set lambda=0 for its logL
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#
# F rates by fleet x season
#_year:  1918 1919 1920 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 1.05416e-05 3.92297e-06 3.2643e-06 3.05327e-06 1.82468e-06 2.38578e-06 4.9024e-06 6.19943e-06 9.86322e-06 1.28158e-05 1.14553e-05 9.27149e-06 9.86578e-06 8.23473e-06 6.78813e-06 1.02549e-05 2.4035e-05 7.14545e-05 1.77521e-05 2.46634e-05 5.03343e-05 8.80473e-05 0.000193977 0.000253605 0.000399664 0.00480327 0.00796187 0.0189207 0.00938781 0.00501258 0.00609306 0.0080304 0.0124624 0.0145017 0.0216565 0.0162513 0.0236565 0.0187304 0.0301402 0.026246 0.0208979 0.0123493 0.0231434 0.0328341 0.0444833 0.0609834 0.067706 0.0896166 0.0418693 0.0345965 0.0556361 0.0352394 0.038396 0.0208326 0.0195142 0.0181665 0.0173836 0.0134057 0.0260973 0.0261655 0.0404888 0.0305178 0.034387 0.0283357 0.0215321 0.0338855 0.0353028 0.0303403 0.0297146 0.0243401 0.0302651 0.0320569 0.0225901 0.0305087 0.0251193 0.0296417 0.0227167 0.0183821 0.0171819 0.0135028 0.0129215 0.00991033 0.00340347 0.00614904 0.00391304 0.00330391 0.00320706 0.00160386 0.00167482 0.00296147 0.00214661 0.0033421 0.0028708 0.000910019 0.000956045 0.00089268 0.000819431 0.000720262 0.000703676 0.0029523 0.0028479 0.0396695 0.0396695 0.0396695 0.0396695 0.0396695 0.0396695 0.0396695 0.0396695 0.0396695 0.0396695
# ASHOP 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.0020111 0.00101915 0.000119764 0.00048225 0.000474232 0.00147384 0.000483704 0.000890335 0.000342246 7.29944e-05 0.000364279 0.000637374 0.000173319 0.000146613 0.000140547 0.00261814 0.00147945 0.0119341 2.76339e-05 0.00264258 0.00144548 0.000191141 0.000188458 0.000642357 0.000464898 0.000278398 0.000564933 0.000101435 0.000161893 2.67643e-05 4.16825e-05 7.47608e-05 9.34838e-05 0.000357123 3.4e-05 0.000359047 0.000190624 9.151e-05 0.00010589 7.35566e-05 0.000155496 0.00017444 0.000531095 0.000507219 0.009834 0.009834 0.009834 0.009834 0.009834 0.009834 0.009834 0.009834 0.009834 0.009834
# Foreign 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.191444 0.186409 0.120019 0.0096153 0.00907293 0.0186649 0.0196899 0.0369102 0.0308999 0.0106064 0.00501902 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3.38329e-12 3.38329e-12 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08
# POP 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.000593823 0 0 0 0 0 0.000246039 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3.38329e-12 3.38329e-12 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08
# Triennial 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.000409253 0 0 0.000169275 0 0 0.000165133 0 0 5.15531e-05 0 0 0.000146695 0 0 0.000123411 0 0 8.76312e-05 0 0 0.000120402 0 0 5.45776e-05 0 0 4.97099e-05 0 0 0 0 0 0 0 0 0 0 0 0 3.38329e-12 3.38329e-12 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08
# AFSCSlope 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1.43444e-05 0 0 0 8.42603e-06 0 3.10945e-07 0 1.83821e-05 2.62995e-06 0 1.20636e-07 1.75344e-05 1.61693e-06 0 1.47504e-05 3.2348e-06 8.57286e-06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3.38329e-12 3.38329e-12 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08
# NWFSCSlope 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6.1152e-06 5.96898e-06 8.40954e-06 4.80709e-06 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3.38329e-12 3.38329e-12 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08 5.083e-08
# NWFSCcombo 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4.55764e-05 7.52805e-06 1.51467e-05 1.43745e-05 6.71022e-06 9.17008e-06 3.10304e-05 1.75374e-05 1.1781e-05 1.49439e-05 1.52234e-05 4.78104e-06 1.2336e-05 2.34707e-05 3.38329e-12 3.38329e-12 0.00132315 0.00132315 0.00132315 0.00132315 0.00132315 0.00132315 0.00132315 0.00132315 0.00132315 0.00132315
#
#_Q_setup for fleets with cpue or survey or deviation data
#_1:  fleet number
#_2:  link type: 1=simple q; 2=mirror; 3=power (+1 parm); 4=mirror with scale (+1p); 5=offset (+1p); 6=offset & power (+2p)
#_     where power is applied as y = q * x ^ (1 + power); so a power value of 0 has null effect
#_     and with the offset included it is y = q * (x + offset) ^ (1 + power)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         4         1         0         0         0         1  #  POP
         5         1         0         1         0         1  #  Triennial
         6         1         0         0         0         1  #  AFSCSlope
         7         1         0         0         0         1  #  NWFSCSlope
         8         1         0         1         0         1  #  NWFSCcombo
-9999 0 0 0 0 0
#
#_Q_parameters
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -15            15     -0.217115             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_POP(4)
           -15            15      -2.01109             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Triennial(5)
             0           0.5        0.3853             0             1             0         -2          0          0          0          0          0          0          0  #  Q_extraSD_Triennial(5)
           -15            15      -2.67499             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_AFSCSlope(6)
           -15            15      -3.04717             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_NWFSCSlope(7)
           -15            15      -2.73349             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_NWFSCcombo(8)
             0           0.5       0.01779             0             1             0          2          0          0          0          0          0          0          0  #  Q_extraSD_NWFSCcombo(8)
#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0;  parm=0; selex=1.0 for all sizes
#Pattern:_1;  parm=2; logistic; with 95% width specification
#Pattern:_5;  parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_11; parm=2; selex=1.0  for specified min-max population length bin range
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6;  parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (mean over bin range)
#Pattern:_8;  parm=8; double_logistic with smooth transitions and constant above Linf option
#Pattern:_9;  parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2*special; non-parm len selex, read as N break points, then N selex parameters
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_2;  parm=6; double_normal with sel(minL) and sel(maxL), using joiners, back compatibile version of 24 with 3.30.18 and older
#Pattern:_25; parm=3; exponential-logistic in length
#Pattern:_27; parm=special+3; cubic spline in length; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=special+3+2; cubic spline; like 27, with 2 additional param for scaling (mean over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 24 1 0 0 # 1 Fishery
 24 0 0 0 # 2 ASHOP
 15 0 0 1 # 3 Foreign
 1 0 0 0 # 4 POP
 24 0 0 0 # 5 Triennial
 24 0 0 0 # 6 AFSCSlope
 24 0 0 0 # 7 NWFSCSlope
 24 0 0 0 # 8 NWFSCcombo
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic. Recommend using pattern 18 instead.
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (mean over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (mean over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 10 0 0 0 # 1 Fishery
 10 0 0 0 # 2 ASHOP
 10 0 0 0 # 3 Foreign
 10 0 0 0 # 4 POP
 10 0 0 0 # 5 Triennial
 10 0 0 0 # 6 AFSCSlope
 10 0 0 0 # 7 NWFSCSlope
 10 0 0 0 # 8 NWFSCcombo
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   Fishery LenSelex
            20            45       37.0908            28            10             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_Fishery(1)
            -6             4            -5            -1             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_Fishery(1)
            -1             9       3.47683             4             2             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_Fishery(1)
            -9             9         -1.65             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_Fishery(1)
            -5             9       -3.2223             4             2             0          4          0          0          0          0          0          0          0  #  Size_DblN_start_logit_Fishery(1)
            -5             9    0.00856049            -2             2             0          4          0          0          0          0          0          4          2  #  Size_DblN_end_logit_Fishery(1)
            15            45       28.4526            35            10             0          1          0          0          0          0        0.5          0          0  #  Retain_L_infl_Fishery(1)
           0.1            10      0.985719             1             2             0          1          0          0          0          0        0.5          2          1  #  Retain_L_width_Fishery(1)
           -10            10       7.11797           6.5             2             0          1          0          0          0          0        0.5          1          2  #  Retain_L_asymptote_logit_Fishery(1)
             0             0             0             0            99             0         -3          0          0          0          0        0.5          0          0  #  Retain_L_maleoffset_Fishery(1)
# 2   ASHOP LenSelex
            20          49.5       49.4956            28            10             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_ASHOP(2)
            -6             4            -5            -1             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_ASHOP(2)
            -1             9       5.15704             4             2             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_ASHOP(2)
            -1             9             1             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_ASHOP(2)
            -9             9         -4.35            -4             2             0         -4          0          0          0          0          0          0          0  #  Size_DblN_start_logit_ASHOP(2)
            -5           999           999           999             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_ASHOP(2)
# 3   Foreign LenSelex
# 4   POP LenSelex
            20            70       25.1237            30            10             0          1          0          0          0          0          0          0          0  #  Size_inflection_POP(4)
         0.001            50        11.654            15             5             0          3          0          0          0          0          0          0          0  #  Size_95%width_POP(4)
# 5   Triennial LenSelex
            20            45       27.6542            28            10             0         -1          0          0          0          0          0          0          0  #  Size_DblN_peak_Triennial(5)
            -6             4            -5            -1             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_Triennial(5)
            -1             9           5.5             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_Triennial(5)
            -1             9        3.2432             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_Triennial(5)
            -5             9            -5            -5             2             0         -4          0          0          0          0          0          0          0  #  Size_DblN_start_logit_Triennial(5)
            -5             9      -0.67802            -2             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_Triennial(5)
# 6   AFSCSlope LenSelex
            20            45       21.5056            28            10             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_AFSCSlope(6)
            -6             4            -5            -1             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_AFSCSlope(6)
            -1             9       1.14059             4             2             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_AFSCSlope(6)
            -1             9             1             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_AFSCSlope(6)
            -9             9            -9            -9             2             0         -4          0          0          0          0          0          0          0  #  Size_DblN_start_logit_AFSCSlope(6)
            -5           999           999           999             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_AFSCSlope(6)
# 7   NWFSCSlope LenSelex
            20            45       35.9371            28            10             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_NWFSCSlope(7)
            -6             4            -5            -1             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_NWFSCSlope(7)
            -1             9       1.84591             4             2             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_NWFSCSlope(7)
            -1             9             1             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_NWFSCSlope(7)
            -9             9            -9            -9             2             0         -4          0          0          0          0          0          0          0  #  Size_DblN_start_logit_NWFSCSlope(7)
            -5           999           999           999             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_NWFSCSlope(7)
# 8   NWFSCcombo LenSelex
            18          49.5       21.1613            28            10             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_NWFSCcombo(8)
            -6             4            -5            -1             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_NWFSCcombo(8)
            -1             9       3.02794             4             2             0          3          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_NWFSCcombo(8)
            -1             9             1             4             2             0         -3          0          0          0          0          0          0          0  #  Size_DblN_descend_se_NWFSCcombo(8)
            -9             9            -9            -4             2             0         -4          0          0          0          0          0          0          0  #  Size_DblN_start_logit_NWFSCcombo(8)
            -5           999           999           999             2             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_NWFSCcombo(8)
# 1   Fishery AgeSelex
# 2   ASHOP AgeSelex
# 3   Foreign AgeSelex
# 4   POP AgeSelex
# 5   Triennial AgeSelex
# 6   AFSCSlope AgeSelex
# 7   NWFSCSlope AgeSelex
# 8   NWFSCcombo AgeSelex
#_No_Dirichlet parameters
# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
            -5             9       1.50688            -2             2             0      2  # Size_DblN_end_logit_Fishery(1)_BLK4repl_1918
           0.1            10       1.26058             1             2             0      2  # Retain_L_width_Fishery(1)_BLK2add_1918
           -10            10       9.58289           3.9             2             0      4  # Retain_L_asymptote_logit_Fishery(1)_BLK1repl_1918
           -10            10       2.58069           1.7             2             0      4  # Retain_L_asymptote_logit_Fishery(1)_BLK1repl_1992
           -10            10       1.91825           0.6             2             0      4  # Retain_L_asymptote_logit_Fishery(1)_BLK1repl_2002
           -10            10      0.689664             0             2             0      4  # Retain_L_asymptote_logit_Fishery(1)_BLK1repl_2008
           -10            10     0.0280968             0             2             0      4  # Retain_L_asymptote_logit_Fishery(1)_BLK1repl_2009
# info on dev vectors created for selex parms are reported with other devs after tag parameter section 
#
0   #  use 2D_AR1 selectivity? (0/1)
#_no 2D_AR1 selex offset used
#_specs:  fleet, ymin, ymax, amin, amax, sigma_amax, use_rho, len1/age2, devphase, before_range, after_range
#_sigma_amax>amin means create sigma parm for each bin from min to sigma_amax; sigma_amax<0 means just one sigma parm is read and used for all bins
#_needed parameters follow each fleet's specifications
# -9999  0 0 0 0 0 0 0 0 0 0 # terminator
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#      5     6     1     4     2     0     0     0     0     0     0     0
#      5     8     2     2     1     0     0     0     0     0     0     0
#      5     9     3     1     2     0     0     0     0     0     0     0
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_factor  fleet  value
      4      1 0.0955977
      4      2  0.104648
      4      4         1
      4      6  0.076979
      4      7  0.564539
      4      8  0.030745
      5      1  0.217385
      5      2    0.0318
      5      4         1
      5      7  0.303584
      5      8  0.363011
      4      5 0.0232231
      5      5  0.232127
 -9999   1    0  # terminator
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 12 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
 4 1 1 0.5 1
 4 2 1 0.5 1
 4 4 1 0.5 1
 4 6 1 0.5 1
 4 7 1 0.5 1
 5 1 1 0.5 1
 5 2 1 0.5 1
 5 4 1 0.5 1
 5 7 1 0.5 1
 1 5 1 0 1
 4 5 1 0 1
 5 5 1 0 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  0 #_CPUE/survey:_3
#  1 #_CPUE/survey:_4
#  0 #_CPUE/survey:_5
#  1 #_CPUE/survey:_6
#  1 #_CPUE/survey:_7
#  1 #_CPUE/survey:_8
#  1 #_discard:_1
#  0 #_discard:_2
#  0 #_discard:_3
#  0 #_discard:_4
#  0 #_discard:_5
#  0 #_discard:_6
#  0 #_discard:_7
#  0 #_discard:_8
#  0.5 #_lencomp:_1
#  0.5 #_lencomp:_2
#  0 #_lencomp:_3
#  0.5 #_lencomp:_4
#  0 #_lencomp:_5
#  0.5 #_lencomp:_6
#  0.5 #_lencomp:_7
#  1 #_lencomp:_8
#  0.5 #_agecomp:_1
#  0.5 #_agecomp:_2
#  0 #_agecomp:_3
#  0.5 #_agecomp:_4
#  0 #_agecomp:_5
#  0 #_agecomp:_6
#  0.5 #_agecomp:_7
#  1 #_agecomp:_8
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_init_equ_catch4
#  1 #_init_equ_catch5
#  1 #_init_equ_catch6
#  1 #_init_equ_catch7
#  1 #_init_equ_catch8
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio
 # 0 2 0 0 # Selectivity: (1) fleet, (2) 1=len/2=age/3=both, (3) year, (4) N selex bins
 # 0 0 # Growth: (1) growth pattern, (2) growth ages
 # 0 0 0 # Numbers-at-age: (1) area(-1 for all), (2) year, (3) N ages
 # -1 # list of bin #'s for selex std (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 # -1 # list of ages for NatAge std (-1 in first bin to self-generate)
999

