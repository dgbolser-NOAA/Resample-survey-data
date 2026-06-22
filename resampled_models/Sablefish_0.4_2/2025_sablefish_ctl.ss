#C file created using an r4ss function
#C file write time: 2026-06-18  16:46:02
#
1 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
4 # recr_dist_method for parameters
1 # not yet implemented; Future usage:Spawner-Recruitment; 1=global; 2=by area
1 # number of recruitment settlement assignments 
0 # unused option
# for each settlement assignment:
#_GPattern	month	area	age
1	1	1	0	#_recr_dist_pattern1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
3 #_Nblock_Patterns
2 1 1 #_blocks_per_pattern
#_begin and end years of blocks
1890 2010 2011 2018
1890 2010
1890 2001
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=Maunder_M;_6=Age-range_Lorenzen
#_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0.5 #_Age(post-settlement)_for_L1;linear growth below this
30 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
5 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
3 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
 0.04	  0.13	   0.084261	  -2.631	0.31	3	  2	0	0	0	0	0	0	0	#_NatM_p_1_Fem_GP_1  
   20	    35	    25.1822	    24.9	  99	0	 -2	0	0	0	0	0	0	0	#_L_at_Amin_Fem_GP_1 
   55	    70	    60.7182	      66	  99	0	 -2	0	0	0	0	0	0	0	#_L_at_Amax_Fem_GP_1 
 0.15	  0.55	   0.342744	    0.34	  99	0	 -2	0	0	0	0	0	0	0	#_VonBert_K_Fem_GP_1 
0.001	     1	  0.0972141	     0.1	  99	0	 -2	0	0	0	0	0	0	0	#_CV_young_Fem_GP_1  
 0.01	     1	  0.0905326	    0.15	  99	0	 -2	0	0	0	0	0	0	0	#_CV_old_Fem_GP_1    
    0	     1	3.41065e-06	3.41e-06	  99	6	-50	0	0	0	0	0	0	0	#_Wtlen_1_Fem_GP_1   
    0	     4	     3.2667	     3.3	  99	6	-50	0	0	0	0	0	0	0	#_Wtlen_2_Fem_GP_1   
   53	    59	      55.19	      55	  99	6	-50	0	0	0	0	0	0	0	#_Mat50%_Fem_GP_1    
   -3	     3	     -0.421	   -0.25	  99	6	-50	0	0	0	0	0	0	0	#_Mat_slope_Fem_GP_1 
   -3	     3	          1	       1	  99	6	-50	0	0	0	0	0	0	0	#_Eggs_alpha_Fem_GP_1
   -3	     3	          0	       0	  99	6	-50	0	0	0	0	0	0	0	#_Eggs_beta_Fem_GP_1 
    0	  0.13	          0	       0	0.31	6	 -3	0	0	0	0	0	0	0	#_NatM_p_1_Mal_GP_1  
   15	    35	    26.6461	    25.5	  99	0	 -2	0	0	0	0	0	0	0	#_L_at_Amin_Mal_GP_1 
   50	    60	    56.0591	      57	  99	0	 -2	0	0	0	0	0	0	0	#_L_at_Amax_Mal_GP_1 
  0.2	  0.55	   0.351087	   0.423	  99	0	 -2	0	0	0	0	0	0	0	#_VonBert_K_Mal_GP_1 
0.001	     1	  0.0857992	       0	  99	0	 -2	0	0	0	0	0	0	0	#_CV_young_Mal_GP_1  
 0.01	     1	  0.0733395	     0.1	  99	0	 -2	0	0	0	0	0	0	0	#_CV_old_Mal_GP_1    
    0	     1	3.70085e-06	 3.7e-06	  99	6	-50	0	0	0	0	0	0	0	#_Wtlen_1_Mal_GP_1   
    0	     4	    3.24693	     3.2	  99	6	-50	0	0	0	0	0	0	0	#_Wtlen_2_Mal_GP_1   
  0.1	    10	          1	       1	   1	0	-50	0	0	0	0	0	0	0	#_CohortGrowDev      
1e-06	0.9999	        0.5	     0.5	 0.5	0	-99	0	0	0	0	0	0	0	#_FracFemale_GP_1    
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; 2=Ricker (2 parms); 3=std_B-H(2); 4=SCAA(2);5=Hockey(3); 6=B-H_flattop(2); 7=Survival(3);8=Shepard(3);9=Ricker_Power(3);10=B-H_a,b(4)
0 # 0/1 to use steepness in initial equ recruitment calculation
0 # future feature: 0/1 to make realized sigmaR a function of SR curvature
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn # parm_name
  8	 12	10.4	10.5	  99	0	  1	0	0	0	0	0	0	0	#_SR_LN(R0)  
0.2	  1	0.75	0.75	0.15	2	 -7	0	0	0	0	0	0	0	#_SR_BH_steep
0.2	1.5	 1.4	 0.6	  99	0	-50	0	0	0	0	0	0	0	#_SR_sigmaR  
 -1	  1	   0	   0	  99	0	-50	0	0	0	0	0	0	0	#_SR_regime  
 -1	  1	   0	   0	  99	0	-50	0	0	0	0	0	0	0	#_SR_autocorr
#_no timevary SR parameters
2 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1975 # first year of main recr_devs; early devs can preceed this era
2023 # last year of main recr_devs; forecast devs start in following year
2 #_recdev phase
1 # (0/1) to read 13 advanced options
1890 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
-3 #_recdev_early_phase
3 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
1974 #_last_yr_nobias_adj_in_MPD; begin of ramp
1979 #_first_yr_fullbias_adj_in_MPD; begin of plateau
2023 #_last_yr_fullbias_adj_in_MPD
2024 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
0.96 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-4 #min rec_dev
4 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
#Fishing Mortality info
0.02 # F ballpark
-2000 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
3 # max F or harvest rate, depends on F_Method
4 # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#
#_Q_setup for fleets with cpue or survey data
#_fleet	link	link_info	extra_se	biasadj	float  #  fleetname
    7	1	0	1	0	1	#_Triennial_Early
    8	1	0	1	0	1	#_Triennial_Late 
    9	1	0	1	0	1	#_NWFSC_Slope    
   10	1	0	1	0	1	#_WCGBT          
   11	5	0	0	0	0	#_ENV            
-9999	0	0	0	0	0	#_terminator     
#_Q_parms(if_any);Qunits_are_ln(q)
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
  -15	  15	 0.167784	0	99	0	-1	0	0	0	0	0	0	0	#_LnQ_base_Triennial_Early(7) 
0.001	 0.5	    0.001	0	99	0	-2	0	0	0	0	0	0	0	#_Q_extraSD_Triennial_Early(7)
  -15	  15	 0.336667	0	99	0	-1	0	0	0	0	0	0	0	#_LnQ_base_Triennial_Late(8)  
0.001	0.75	 0.185526	0	99	0	 1	0	0	0	0	0	0	0	#_Q_extraSD_Triennial_Late(8) 
  -15	  15	-0.644843	0	99	0	-1	0	0	0	0	0	0	0	#_LnQ_base_NWFSC_Slope(9)     
0.001	0.75	 0.147979	0	99	0	 2	0	0	0	0	0	0	0	#_Q_extraSD_NWFSC_Slope(9)    
  -15	  15	 -1.2e-05	0	99	0	-1	0	0	0	0	0	0	0	#_LnQ_base_WCGBT(10)          
1e-04	 0.3	    1e-04	0	99	0	-2	0	0	0	0	0	0	0	#_Q_extraSD_WCGBT(10)         
    0	  10	      0.5	0	99	0	 1	0	0	0	0	0	0	0	#_Q_base_ENV(11)              
  -15	  15	        0	0	99	0	-1	0	0	0	0	0	0	0	#_Q_offset_ENV(11)            
#_no timevary Q parameters
#
#_size_selex_patterns
#_Pattern	Discard	Male	Special
0	0	0	0	#_1 TWL            
0	0	0	0	#_2 HKL            
0	0	0	0	#_3 Pot            
0	0	0	0	#_4 TWL_Discards   
0	0	0	0	#_5 HKL_Discards   
0	0	0	0	#_6 Pot_Discard    
0	0	0	0	#_7 Triennial_Early
0	0	0	0	#_8 Triennial_Late 
0	0	0	0	#_9 NWFSC_Slope    
0	0	0	0	#_10 WCGBT         
0	0	0	0	#_11 ENV           
#
#_age_selex_patterns
#_Pattern	Discard	Male	Special
20	0	0	0	#_1 TWL            
20	0	3	0	#_2 HKL            
15	0	0	2	#_3 Pot            
20	0	0	0	#_4 TWL_Discards   
20	0	0	0	#_5 HKL_Discards   
20	0	0	0	#_6 Pot_Discard    
20	0	0	0	#_7 Triennial_Early
20	0	0	0	#_8 Triennial_Late 
20	0	0	0	#_9 NWFSC_Slope    
20	0	0	0	#_10 WCGBT         
11	0	0	0	#_11 ENV           
#
#_SizeSelex
#_No size_selex_parm
#_AgeSelex
0.01	10	  3.59766	   1	99	0	  3	0	0	0	0	0	3	2	#_AgeSel_P_1_TWL(1)            
 -10	10	 -1.77183	 0.3	99	0	  5	0	0	0	0	0	0	0	#_AgeSel_P_2_TWL(1)            
 -10	10	 0.899472	   5	99	0	  4	0	0	0	0	0	3	2	#_AgeSel_P_3_TWL(1)            
 -10	10	      -10	   4	99	0	 -5	0	0	0	0	0	0	0	#_AgeSel_P_4_TWL(1)            
 -10	10	 -9.99999	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_TWL(1)            
 -10	10	 0.565923	  -5	99	0	  5	0	0	0	0	0	0	0	#_AgeSel_P_6_TWL(1)            
0.01	20	  5.32135	   1	99	0	  3	0	0	0	0	0	3	2	#_AgeSel_P_1_HKL(2)            
  -5	 5	       -4	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_HKL(2)            
 -10	10	 0.915386	   5	99	0	  5	0	0	0	0	0	0	0	#_AgeSel_P_3_HKL(2)            
 -10	10	  4.06564	   4	99	0	  5	0	0	0	0	0	0	0	#_AgeSel_P_4_HKL(2)            
 -10	10	       -5	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_HKL(2)            
 -10	10	 -1.35505	  -5	99	0	  5	0	0	0	0	0	0	0	#_AgeSel_P_6_HKL(2)            
 -20	20	-0.104302	   0	99	0	  3	0	0	0	0	0	0	0	#_AgeSel_PMalOff_1_HKL(2)      
  -5	 5	        0	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_PMalOff_2_HKL(2)      
 -10	10	        0	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_PMalOff_3_HKL(2)      
 -20	10	-0.636248	   0	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_PMalOff_4_HKL(2)      
0.01	 1	 0.279859	   1	99	0	  3	0	0	0	0	0	0	0	#_AgeSel_PMalOff_5_HKL(2)      
0.01	20	 0.205644	   1	99	0	 -4	0	0	0	0	0	1	2	#_AgeSel_P_1_TWL_Discards(4)   
 -10	10	 -3.73792	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_TWL_Discards(4)   
 -10	10	  9.99916	   5	99	0	 -5	0	0	0	0	0	0	0	#_AgeSel_P_3_TWL_Discards(4)   
 -10	10	 -2.74909	   4	99	0	  5	0	0	0	0	0	2	2	#_AgeSel_P_4_TWL_Discards(4)   
 -10	10	 -3.30918	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_TWL_Discards(4)   
 -10	10	 -1.90549	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_TWL_Discards(4)   
0.01	20	  4.47293	   1	99	0	  3	0	0	0	0	0	1	2	#_AgeSel_P_1_HKL_Discards(5)   
 -10	10	 -5.95332	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_HKL_Discards(5)   
 -10	10	   1.0867	   5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_3_HKL_Discards(5)   
 -10	10	 -7.81765	   4	99	0	 -5	0	0	0	0	0	0	0	#_AgeSel_P_4_HKL_Discards(5)   
 -10	10	       -5	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_HKL_Discards(5)   
 -10	10	 0.565083	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_HKL_Discards(5)   
0.01	20	  2.94442	   1	99	0	  3	0	0	0	0	0	2	2	#_AgeSel_P_1_Pot_Discard(6)    
 -10	10	 -3.23371	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_Pot_Discard(6)    
 -10	10	-0.567118	   5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_3_Pot_Discard(6)    
 -10	10	 -5.49683	   4	99	0	 -5	0	0	0	0	0	0	0	#_AgeSel_P_4_Pot_Discard(6)    
 -10	10	       -5	   0	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_Pot_Discard(6)    
 -10	10	  2.67639	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_Pot_Discard(6)    
0.01	12	 0.581097	   1	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_1_Triennial_Early(7)
 -10	10	 -3.34652	 0.3	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_2_Triennial_Early(7)
 -10	10	 -9.72391	   5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_3_Triennial_Early(7)
 -10	10	 -8.64738	   4	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_4_Triennial_Early(7)
 -10	 5	       -5	  -5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_Triennial_Early(7)
 -10	10	 -5.32407	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_Triennial_Early(7)
0.01	12	  1.04968	   1	99	0	  3	0	0	0	0	0	0	0	#_AgeSel_P_1_Triennial_Late(8) 
 -10	10	      -10	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_Triennial_Late(8) 
 -10	10	 -9.72391	   5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_3_Triennial_Late(8) 
 -10	10	  1.36842	   4	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_4_Triennial_Late(8) 
 -10	 5	       -5	  -5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_Triennial_Late(8) 
 -10	10	 -3.69724	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_Triennial_Late(8) 
0.01	15	  3.73483	   1	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_1_NWFSC_Slope(9)    
 -10	10	 -5.67511	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_NWFSC_Slope(9)    
 -10	10	  1.54477	   5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_3_NWFSC_Slope(9)    
 -10	10	 -6.94214	   4	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_4_NWFSC_Slope(9)    
 -10	10	 -4.71773	  -5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_NWFSC_Slope(9)    
 -10	10	  1.28127	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_NWFSC_Slope(9)    
0.01	10	     0.25	   1	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_1_WCGBT(10)         
 -10	10	 -9.99987	 0.3	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_2_WCGBT(10)         
 -10	10	  4.34053	   5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_3_WCGBT(10)         
 -10	10	     -9.9	-5.5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_4_WCGBT(10)         
 -10	 5	    -2.75	  -5	99	0	 -4	0	0	0	0	0	0	0	#_AgeSel_P_5_WCGBT(10)         
 -10	10	  1.44385	  -5	99	0	  4	0	0	0	0	0	0	0	#_AgeSel_P_6_WCGBT(10)         
   0	10	        0	  -5	99	0	-99	0	0	0	0	0	0	0	#_AgeSel_P_1_ENV(11)           
   0	10	        0	  -5	99	0	-99	0	0	0	0	0	0	0	#_AgeSel_P_2_ENV(11)           
# timevary selex parameters 
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE
0.01	20	   1.1218	1	99	0	 6	#_AgeSel_P_1_TWL(1)_BLK3repl_1890         
 -10	10	 -7.27287	5	99	0	-4	#_AgeSel_P_3_TWL(1)_BLK3repl_1890         
   1	20	  4.63651	1	99	0	 6	#_AgeSel_P_1_HKL(2)_BLK3repl_1890         
0.01	20	 0.406885	1	99	0	 6	#_AgeSel_P_1_TWL_Discards(4)_BLK1repl_1890
0.01	20	0.0911056	1	99	0	-6	#_AgeSel_P_1_TWL_Discards(4)_BLK1repl_2011
 -10	10	      -10	4	99	0	-7	#_AgeSel_P_4_TWL_Discards(4)_BLK2repl_1890
   1	20	  2.11471	1	99	0	-6	#_AgeSel_P_1_HKL_Discards(5)_BLK1repl_1890
   1	20	  3.77859	1	99	0	 7	#_AgeSel_P_1_HKL_Discards(5)_BLK1repl_2011
   1	20	  2.09978	1	99	0	 6	#_AgeSel_P_1_Pot_Discard(6)_BLK2repl_1890 
# info on dev vectors created for selex parms are reported with other devs after tag parameter section
#
0 #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
# Tag loss and Tag reporting parameters go next
0 # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# Input variance adjustments factors: 
#_factor	fleet	value
    5	 1	0.188333	#_Variance_adjustment_list1 
    5	 2	0.334454	#_Variance_adjustment_list2 
    5	 3	0.126024	#_Variance_adjustment_list3 
    5	 4	0.389079	#_Variance_adjustment_list4 
    5	 5	0.077398	#_Variance_adjustment_list5 
    5	 6	0.085798	#_Variance_adjustment_list6 
    5	 7	       1	#_Variance_adjustment_list7 
    5	 8	       1	#_Variance_adjustment_list8 
    5	 9	0.118175	#_Variance_adjustment_list9 
    5	10	0.118982	#_Variance_adjustment_list10
-9999	 0	       0	#_terminator                
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 0 changes to default Lambdas (default value is 1.0)
-9999 0 0 0 0 # terminator
#
0 # 0/1 read specs for more stddev reporting
#
999
