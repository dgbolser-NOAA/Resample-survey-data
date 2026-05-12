#C file created using an r4ss function
#C file write time: 2026-05-12  15:11:07
#
0 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
2 # recr_dist_method for parameters
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
4 #_Nblock_Patterns
5 1 1 1 #_blocks_per_pattern
#_begin and end years of blocks
1918 1991 1992 2001 2002 2007 2008 2008 2009 2010
1918 2010
1995 2004
1918 1999
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
3 #_Age(post-settlement)_for_L1;linear growth below this
20 #_Growth_Age_for_L2 (999 to use as Linf)
0.055 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
3 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
0 #_First_Mature_Age
2 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
2 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
0.02	 0.1	     0.054	    -2.92	0.44	3	 -5	0	0	0	0	0	0	0	#_NatM_p_1_Fem_GP_1  
  15	  25	   20.7538	     20.8	  10	0	  3	0	0	0	0	0	0	0	#_L_at_Amin_Fem_GP_1 
  35	  45	   41.6011	     41.4	  10	0	  2	0	0	0	0	0	0	0	#_L_at_Amax_Fem_GP_1 
 0.1	 0.4	  0.166779	    0.166	0.05	0	  3	0	0	0	0	0	0	0	#_VonBert_K_Fem_GP_1 
0.03	   5	   1.34872	     1.31	 0.5	0	  4	0	0	0	0	0	0	0	#_CV_young_Fem_GP_1  
0.03	   5	   2.56049	     2.68	 0.5	0	  4	0	0	0	0	0	0	0	#_CV_old_Fem_GP_1    
   0	   3	 1.003e-05	1.003e-05	  99	0	-99	0	0	0	0	0	0	0	#_Wtlen_1_Fem_GP_1   
   2	   4	    3.1026	   3.1026	  99	0	-99	0	0	0	0	0	0	0	#_Wtlen_2_Fem_GP_1   
  20	  40	      32.1	     32.1	  99	0	-99	0	0	0	0	0	0	0	#_Mat50%_Fem_GP_1    
  -2	   4	        -1	       -1	  99	0	-99	0	0	0	0	0	0	0	#_Mat_slope_Fem_GP_1 
   0	   6	  8.66e-10	        1	  99	0	-99	0	0	0	0	0	0	0	#_Eggs_alpha_Fem_GP_1
  -3	   5	    4.9767	        5	  99	0	-99	0	0	0	0	0	0	0	#_Eggs_beta_Fem_GP_1 
  -1	   1	         0	     0.05	 0.1	6	 -5	0	0	0	0	0	0	0	#_NatM_p_1_Mal_GP_1  
  -1	   1	         0	        0	 0.1	0	 -2	0	0	0	0	0	0	0	#_L_at_Amin_Mal_GP_1 
  -1	   1	-0.0664828	   -0.066	 0.1	0	  2	0	0	0	0	0	0	0	#_L_at_Amax_Mal_GP_1 
  -1	   1	  0.169132	    0.167	 0.1	0	  3	0	0	0	0	0	0	0	#_VonBert_K_Mal_GP_1 
  -5	   5	         0	        0	 0.5	0	 -5	0	0	0	0	0	0	0	#_CV_young_Mal_GP_1  
  -5	   5	 -0.115863	        0	 0.5	0	  5	0	0	0	0	0	0	0	#_CV_old_Mal_GP_1    
   0	   3	 9.881e-06	9.881e-06	  99	0	-99	0	0	0	0	0	0	0	#_Wtlen_1_Mal_GP_1   
   2	   4	    3.1039	   3.1039	  99	0	-99	0	0	0	0	0	0	0	#_Wtlen_2_Mal_GP_1   
   0	   2	         1	        1	  99	0	-99	0	0	0	0	0	0	0	#_RecrDist_GP_1      
   0	   2	         1	        1	  99	0	-99	0	0	0	0	0	0	0	#_RecrDist_Area_1    
   0	   2	         1	        1	  99	0	-99	0	0	0	0	0	0	0	#_RecrDist_month_1   
   0	   2	         1	        1	  99	0	-99	0	0	0	0	0	0	0	#_CohortGrowDev      
0.01	0.99	       0.5	      0.5	 0.5	0	-99	0	0	0	0	0	0	0	#_FracFemale_GP_1    
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
  5	 20	9.4018	  10	   5	0	  1	0	0	0	0	0	0	0	#_SR_LN(R0)  
0.2	  1	   0.5	0.72	0.15	2	 -2	0	0	0	0	0	0	0	#_SR_BH_steep
0.5	1.2	   0.7	 0.7	  99	0	 -6	0	0	0	0	0	0	0	#_SR_sigmaR  
 -5	  5	     0	   0	  99	0	-99	0	0	0	0	0	0	0	#_SR_regime  
  0	  2	     0	   1	  99	0	-99	0	0	0	0	0	0	0	#_SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1940 # first year of main recr_devs; early devs can preceed this era
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
2014 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
0.7 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-6 #min rec_dev
6 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
#Fishing Mortality info
0.03 # F ballpark
-1999 # F ballpark year (neg value to disable)
1 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
0.9 # max F or harvest rate, depends on F_Method
#
#_initial_F_parms; count = 0
#
#_Q_setup for fleets with cpue or survey data
#_fleet	link	link_info	extra_se	biasadj	float  #  fleetname
    4	1	0	0	0	1	#_POP       
    5	1	0	1	0	1	#_Triennial 
    6	1	0	0	0	1	#_AFSCSlope 
    7	1	0	0	0	1	#_NWFSCSlope
    8	1	0	1	0	1	#_NWFSCcombo
-9999	0	0	0	0	0	#_terminator
#_Q_parms(if_any);Qunits_are_ln(q)
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
-15	 15	-0.217115	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_POP(4)        
-15	 15	 -2.01109	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_Triennial(5)  
  0	0.5	   0.3853	0	1	0	-2	0	0	0	0	0	0	0	#_Q_extraSD_Triennial(5) 
-15	 15	 -2.67499	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_AFSCSlope(6)  
-15	 15	 -3.04717	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_NWFSCSlope(7) 
-15	 15	 -2.73349	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_NWFSCcombo(8) 
  0	0.5	  0.01779	0	1	0	 2	0	0	0	0	0	0	0	#_Q_extraSD_NWFSCcombo(8)
#_no timevary Q parameters
#
#_size_selex_patterns
#_Pattern	Discard	Male	Special
24	1	0	0	#_1 Fishery   
24	0	0	0	#_2 ASHOP     
15	0	0	1	#_3 Foreign   
 1	0	0	0	#_4 POP       
24	0	0	0	#_5 Triennial 
24	0	0	0	#_6 AFSCSlope 
24	0	0	0	#_7 NWFSCSlope
24	0	0	0	#_8 NWFSCcombo
#
#_age_selex_patterns
#_Pattern	Discard	Male	Special
10	0	0	0	#_1 Fishery   
10	0	0	0	#_2 ASHOP     
10	0	0	0	#_3 Foreign   
10	0	0	0	#_4 POP       
10	0	0	0	#_5 Triennial 
10	0	0	0	#_6 AFSCSlope 
10	0	0	0	#_7 NWFSCSlope
10	0	0	0	#_8 NWFSCcombo
#
#_SizeSelex
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
   20	  45	   37.0908	 28	10	0	 1	0	0	0	0	  0	0	0	#_SizeSel_P_1_Fishery(1)   
   -6	   4	        -5	 -1	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_2_Fishery(1)   
   -1	   9	   3.47683	  4	 2	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_3_Fishery(1)   
   -9	   9	     -1.65	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_4_Fishery(1)   
   -5	   9	   -3.2223	  4	 2	0	 4	0	0	0	0	  0	0	0	#_SizeSel_P_5_Fishery(1)   
   -5	   9	0.00856049	 -2	 2	0	 4	0	0	0	0	  0	4	2	#_SizeSel_P_6_Fishery(1)   
   15	  45	   28.4526	 35	10	0	 1	0	0	0	0	0.5	0	0	#_SizeSel_PRet_1_Fishery(1)
  0.1	  10	  0.985719	  1	 2	0	 1	0	0	0	0	0.5	2	1	#_SizeSel_PRet_2_Fishery(1)
  -10	  10	   7.11797	6.5	 2	0	 1	0	0	0	0	0.5	1	2	#_SizeSel_PRet_3_Fishery(1)
    0	   0	         0	  0	99	0	-3	0	0	0	0	0.5	0	0	#_SizeSel_PRet_4_Fishery(1)
   20	49.5	   49.4956	 28	10	0	 1	0	0	0	0	  0	0	0	#_SizeSel_P_1_ASHOP(2)     
   -6	   4	        -5	 -1	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_2_ASHOP(2)     
   -1	   9	   5.15704	  4	 2	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_3_ASHOP(2)     
   -1	   9	         1	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_4_ASHOP(2)     
   -9	   9	     -4.35	 -4	 2	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_ASHOP(2)     
   -5	 999	       999	999	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_6_ASHOP(2)     
   20	  70	   25.1237	 30	10	0	 1	0	0	0	0	  0	0	0	#_SizeSel_P_1_POP(4)       
0.001	  50	    11.654	 15	 5	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_2_POP(4)       
   20	  45	   27.6542	 28	10	0	-1	0	0	0	0	  0	0	0	#_SizeSel_P_1_Triennial(5) 
   -6	   4	        -5	 -1	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_2_Triennial(5) 
   -1	   9	       5.5	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_3_Triennial(5) 
   -1	   9	    3.2432	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_4_Triennial(5) 
   -5	   9	        -5	 -5	 2	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_Triennial(5) 
   -5	   9	  -0.67802	 -2	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_6_Triennial(5) 
   20	  45	   21.5056	 28	10	0	 1	0	0	0	0	  0	0	0	#_SizeSel_P_1_AFSCSlope(6) 
   -6	   4	        -5	 -1	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_2_AFSCSlope(6) 
   -1	   9	   1.14059	  4	 2	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_3_AFSCSlope(6) 
   -1	   9	         1	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_4_AFSCSlope(6) 
   -9	   9	        -9	 -9	 2	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_AFSCSlope(6) 
   -5	 999	       999	999	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_6_AFSCSlope(6) 
   20	  45	   35.9371	 28	10	0	 1	0	0	0	0	  0	0	0	#_SizeSel_P_1_NWFSCSlope(7)
   -6	   4	        -5	 -1	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_2_NWFSCSlope(7)
   -1	   9	   1.84591	  4	 2	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_3_NWFSCSlope(7)
   -1	   9	         1	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_4_NWFSCSlope(7)
   -9	   9	        -9	 -9	 2	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_NWFSCSlope(7)
   -5	 999	       999	999	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_6_NWFSCSlope(7)
   18	49.5	   21.1613	 28	10	0	 1	0	0	0	0	  0	0	0	#_SizeSel_P_1_NWFSCcombo(8)
   -6	   4	        -5	 -1	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_2_NWFSCcombo(8)
   -1	   9	   3.02794	  4	 2	0	 3	0	0	0	0	  0	0	0	#_SizeSel_P_3_NWFSCcombo(8)
   -1	   9	         1	  4	 2	0	-3	0	0	0	0	  0	0	0	#_SizeSel_P_4_NWFSCcombo(8)
   -9	   9	        -9	 -4	 2	0	-4	0	0	0	0	  0	0	0	#_SizeSel_P_5_NWFSCcombo(8)
   -5	 999	       999	999	 2	0	-2	0	0	0	0	  0	0	0	#_SizeSel_P_6_NWFSCcombo(8)
#_AgeSelex
#_No age_selex_parm
# timevary selex parameters 
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE
 -5	 9	  1.50688	 -2	2	0	2	#_SizeSel_P_6_Fishery(1)_BLK4repl_1918   
0.1	10	  1.26058	  1	2	0	2	#_SizeSel_PRet_2_Fishery(1)_BLK2add_1918 
-10	10	  9.58289	3.9	2	0	4	#_SizeSel_PRet_3_Fishery(1)_BLK1repl_1918
-10	10	  2.58069	1.7	2	0	4	#_SizeSel_PRet_3_Fishery(1)_BLK1repl_1992
-10	10	  1.91825	0.6	2	0	4	#_SizeSel_PRet_3_Fishery(1)_BLK1repl_2002
-10	10	 0.689664	  0	2	0	4	#_SizeSel_PRet_3_Fishery(1)_BLK1repl_2008
-10	10	0.0280968	  0	2	0	4	#_SizeSel_PRet_3_Fishery(1)_BLK1repl_2009
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
    4	1	0.095621	#_Variance_adjustment_list1 
    4	2	0.101899	#_Variance_adjustment_list2 
    4	4	       1	#_Variance_adjustment_list3 
    4	6	0.075996	#_Variance_adjustment_list4 
    4	7	0.558441	#_Variance_adjustment_list5 
    4	8	0.038117	#_Variance_adjustment_list6 
    5	1	0.221823	#_Variance_adjustment_list7 
    5	2	0.031795	#_Variance_adjustment_list8 
    5	4	       1	#_Variance_adjustment_list9 
    5	7	0.301951	#_Variance_adjustment_list10
    5	8	0.411592	#_Variance_adjustment_list11
    4	5	 0.02264	#_Variance_adjustment_list12
    5	5	0.215469	#_Variance_adjustment_list13
-9999	0	       0	#_terminator                
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 12 changes to default Lambdas (default value is 1.0)
#_like_comp	fleet	phase	value	sizefreq_method
    4	1	1	0.5	1	#_length_Fishery_sizefreq_method_1_Phz1   
    4	2	1	0.5	1	#_length_ASHOP_sizefreq_method_1_Phz1     
    4	4	1	0.5	1	#_length_POP_sizefreq_method_1_Phz1       
    4	6	1	0.5	1	#_length_AFSCSlope_sizefreq_method_1_Phz1 
    4	7	1	0.5	1	#_length_NWFSCSlope_sizefreq_method_1_Phz1
    5	1	1	0.5	1	#_age_Fishery_Phz1                        
    5	2	1	0.5	1	#_age_ASHOP_Phz1                          
    5	4	1	0.5	1	#_age_POP_Phz1                            
    5	7	1	0.5	1	#_age_NWFSCSlope_Phz1                     
    1	5	1	  0	1	#_Surv_Triennial_Phz1                     
    4	5	1	  0	1	#_length_Triennial_sizefreq_method_1_Phz1 
    5	5	1	  0	1	#_age_Triennial_Phz1                      
-9999	0	0	  0	0	#_terminator                              
#
0 # 0/1 read specs for more stddev reporting
#
999
