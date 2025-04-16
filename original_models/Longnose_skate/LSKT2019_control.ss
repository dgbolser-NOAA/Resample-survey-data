#C LSKT control file
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  1=like 3.24; 2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # Recruitment: 1=global; 2=by area (future option)
1 #  number of recruitment settlement assignments 
0 # year_x_area_x_settlement_event interaction requested (only for recr_dist_method=1)
#GPat month  area age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 #_N_movement_definitions
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) if do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, GP=1, source=1 dest=2, age1=4, age2=10
#
2 #_Nblock_Patterns
1	23 	#_blocks_per_pattern
1995	2004	#Triennial Q offset
1995	1995	1996	1996	1997	1997 1998	1998	1999	1999 2000	2000	2001	2001 2002	2002	2003	2003 2004	2004	2005	2005 2006	2006	2007	2007 2008	2008	2009	2009	2010	2010 2011	2011	2012	2012 2013	2013	2014	2014 2015	2015	2016	2016 2017	2018	
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
1 1 1 1 1 # autogen
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if min=-12345
# 1st element for biology, 2nd for SR, 3rd for Q, 5th for selex, 4th reserved
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
0 		#_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
1 		# GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
0 		#_Growth_Age_for_L1
30 		#_Growth_Age_for_L2 (999 to use as Linf)
-999 	#_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0 		#_placeholder for future growth feature
0 		#_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
2 		#_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 		#_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
8 		#_First_Mature_Age
1 		#_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 		#_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 		#_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_ LO 			HI 				INIT 					PRIOR 					PR_SD 			PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
 0.01 			0.8 			0.20769668 	 -1.57167654 		0.4384383 		 3 				 2 		0 				0 			0 				0 				0.5 				0 		0 	#M, Hamel prior
 0 					40 				26.958 	  		26.958 					99 					 0 				 2 		0 				0 			0 				0 				0.5 				0 		0		#L_at_Amin                                      
 70 				150 			109.74 		  	109.74 					99 					 0 				 2 		0 				0 			0 				0 				0.5 				0 		0		#L_at_Amax                                      
 0.035 			0.15			0.047	   			0.047 					99 					 0 			   1 		0 				0 			0 				0 				0.5 				0 		0		#VBK           
 0.5 				15 				3.98641 			2 							99					 0				 5 		0 				0 			0 				0 				0.5 				0 		0 	#SD_young
 0.5 				15 				7.38093 			8 							99 					 0 				 5 		0 				0 			0 				0 				0.5 				0 		0 	#SD_old
                                                                 
-3 					3 				4.288369e-06	4.288369e-06 		99 					 0 				-3 		0 				0 			0 				0 				0.5 				0 		0 	#wt-len-1
 2 					4 				3.068629    	3.068629 				99 					 0 				-3 		0 				0 			0 				0 				0.5 				0 		0		#wt-len-2   
                                                                 
10 					140 			101.5262    	120.753 				99 					 0 				-3 		0 				0 			0 				0 				0.5 				0 		0		#Mat50%_Fem        
-0.09			 -0.05			-0.12997    -0.0985876 				99 					 0 				-3 		0 				0 			0 				0 				0.5 				0 		0		#Mat_slope_Fem 
                                                                 
-3					3 				1 					1 								99					 0 			 	-3 		0 				0 			0 				0 				0.5 				0 		0		#Female eggs/gm intercept
-3					3 				0 						0 							99					 0 			 	-3 		0 				0 			0 				0 				0.5 				0 		0		#Female eggs/gm slope 

#
# 0 2 1 1 99 0 -5 0 0 0 0 0 0 0 # RecrDist_GP_1
# 0 2 1 1 99 0 -5 0 0 0 0 0 0 0 # RecrDist_Area_1
# 0 2 1 1 99 0 -5 0 0 0 0 0 0 0 # RecrDist_Bseas_1
  0 2 1 1 99 0 -5 0 0 0 0 0 0 0 # CohortGrowDev
 1e-006 0.999999 0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1

#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 	0 	0 	0 	0 	0 	0 	0 	0 	0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_LO          HI         INIT      PRIOR        PR_SD      PR_type   PHASE	    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn 
  5           15         13         11.1        10  				 0      1                0          0          0          0          0          0          0 			# SR_LN(R0)
  0.2          1         0.4      	0.6     		0.2          0     -3                0          0          0          0          0          0          0 			# SR_BH_steep
  0            0.4       0.3     		0.3       	0.8          0     -2                0          0          0          0          0          0          0 			# SR_sigmaR
 -2            2         0          0           99           0     -1                0          0          0          0          0          0          0 			# SR_regime
  0            0         0          0           0            0     -99               0          0          0          0          0          0          0 			# SR_autocorr

1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1985 # first year of main recr_devs; early devs can preceed this era
2016 # last year of main recr_devs; forecast devs start in following year
-3 #_recdev phase 
1 # (0/1) to read 13 advanced options
1900 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
-6 #_recdev_early_phase
-4 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
1988.4 #_last_yr_nobias_adj_in_MPD; begin of ramp
1996.9 #_first_yr_fullbias_adj_in_MPD; begin of plateau
2015.1 #_last_yr_fullbias_adj_in_MPD
2017.9 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
0.6132 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-5 #min rec_dev
5 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1916R 1917F 1918F 1919F 1920F 1921F 1922F 1923F 1924F 1925F 1926F 1927F 1928F 1929F 1930F 1931F 1932F 1933F 1934F 1935F 1936F 1937F 1938F 1939F 1940F 1941F 1942F 1943F 1944F 1945F 1946F 1947F 1948F 1949F 1950F 1951F 1952F 1953F 1954F 1955F 1956F 1957F 1958F 1959F 1960F 1961F 1962F 1963F 1964F 1965F 1966F 1967F 1968F 1969F 1970F 1971F 1972F 1973F 1974F 1975F 1976F 1977F 1978F 1979F 1980F 1981F 1982F 1983F 1984F 1985F 1986F 1987F 1988F 1989F 1990F 1991F 1992F 1993F 1994F 1995F 1996F 1997F 1998F 1999F 2000F 2001F 2002F 2003F 2004F 2005F 2006F 2007F 2008F 2009F 2010F 2011F 2012F 2013F 2014F 2015F 2016F 2017F 2018F 2019F 2020F 2021F 2022F
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0 0 0 0 0 0 0 0 0 0 0 0
#
#Fishing Mortality info 
0.2 # F ballpark
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
4  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
##
#_Q_setup
#_    fleet       link  link_info   extra_se    biasadj      float  #  fleetname
          5          1          0          1          0          0  #  5_NWFSC_shelf_slope
          6          1          0          1          0          0  #  6_Triennial 			
          7          1          0          1          0          1  #  7_AFSC_slope  			
          8          1          0          1          0          1  #  8_NWFSC_slope  			
          9          1          0          1          0          1  #  9_IPHC  			
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_         LO           HI         INIT        PRIOR        PR_SD      PR_type     PHASE   env-var   use_dev  dev_mnyr  dev_mxyr    dev_PH     Block   Blk_Fxn  #  parm_name
           -7          5           -0.19        -0.19        0.187         6        1         0         0         0         0         0         0         0 			#  LnQ_base_5_NWFSC_shelf_slope   
          	0          5    		 		0            0.01        99            0       -5         0         0         0         0         0         0         0 			#  Q_extraSDe_5_NWFSC_shelf_slope
           -7          0           -0.6          0           99            0        1         0         0         0         0         0         1         2 			#  LnQ_base_6_Triennial  			    
          	0          5    		 		0            0.01        99            0        5         0         0         0         0         0         0         0 			#  Q_extraSD_6_Triennial
           -7          0           -0.6          0           99            0       -1         0         0         0         0         0         0         0 			#  LnQ_base_7_AFSC_slope  			  
          	0          5    				0            0.01        99            0       -5         0         0         0         0         0         0         0 			#  Q_extraSD_7_AFSC_slope
           -7          0           -0.6          0           99            0       -1         0         0         0         0         0         0         0 			#  LnQ_base_8_NWFSC_slope 			  
          	0          5    		 		0            0.01        99            0       -5         0         0         0         0         0         0         0 			#  Q_extraSD_8_NWFSC_slope
           -7          0           -0.6          0           99            0       -1         0         0         0         0         0         0         0 			#  LnQ_base_9_IPHC  			  
          	0          5    				0            0.01        99            0        5         0         0         0         0         0         0         0 			#  Q_extraSD_9_IPHC
           
#_timevary Q parameters
#	HI	LO		init			PRIOR		PR_SD		PRIOR_TYPE PHASE
-7       0       -0.6    0       99       0      1     	# LnQ_base_4_Triennial 1995-2004 block  			    

#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
24 2 0 0 #  1_Fishery_current  		
15 0 0 1 #  2_Fishery_historical_discard  		
15 0 0 1 #  3_Fishery_historical_landings  		
15 0 0 1 #  4_Fishery_tribal  		
24 0 0 0 #  5_NWFSC_shelf_slope   
24 0 0 0 #  6_Triennial  			  	
24 0 0 0 #  7_AFSC_slope  			
15 0 0 7 #  8_NWFSC_slope 				
24 0 0 0 #  9_IPHC  			

#_age_selex_types
#_Pattern Discard Male Special
 0 0 0 0 #	1_Fishery_current  		        
 0 0 0 0 #	2_Fishery_historical_discard  
 0 0 0 0 #	3_Fishery_historical_landings 
 0 0 0 0 #	4_Fishery_tribal  		        
 0 0 0 0 #	5_NWFSC_shelf_slope           
 0 0 0 0 #	6_Triennial  			  	        
 0 0 0 0 #	7_AFSC_slope  			          
 0 0 0 0 #	8_NWFSC_slope 				        
 0 0 0 0 #	9_IPHC  			                

#_size_selex_settings
#_LO  HI    INIT  PRIOR    PR_SD    PR_type     PHASE env-var   use_dev  dev_mnyr  dev_mxyr    dev_PH     Block   Blk_Fxn #  parm_name
#_size_sel: 1_fishery 
  60 		150 	85 		85 			 	99 			0 				 4 			0 			0 			0 				0 					0 				0 			0 			# PEAK
 -15  	4    -15   -15       	99      0        	-5      0       0       0         0        		0       	0       0  			# TOP:_width of plateau
 -1 		9 		5.8  	5.8 		 	99 			0 				 4 			0 			0 			0 				0 					0 				0 			0 			# Asc_width  
 -1 		20 		8.3  	6.7 		 	99 			0 				 5 			0 			0 			0 				0 					0 				0 			0 			# Desc_width
 -5 		9  		-5  	-5 		 		99 			0 				-4 			0 			0 			0 				0 					0 				0 			0 			# INIT:_selectivity_at_fist_bin
 -999		9 		 9  	9 		 		99 			0 				-5 			0 			0 			0 				0 					0 				0 			0 			# FINAL:_selectivity_at_last_bin
#Retention
#_LO  HI    INIT  PRIOR PR_SD   PR_type PHASE   env-var use_dev dev_min dev_max dev_std Block   Block_Fxn
15			150		27		35			 	99			0			 	 2			0				0				0					0							0				0				0				#Inflection               
0.1			10		2			1				 	99			0			 	 2			0				0				0					0							0				0				0				#Slope                    # 1 means that parm’ = baseparm + blockparm
-10			10		10		10			 	99			0			 	-3			0				0				0					0							0				2				2				#Asymptotic retention     # 2 means that parm’ = blockparm
0				0			0			0				 	99			0			 	-3			0				0				0					0							0				0				0	      #Male offset To inflection
#Discard mortality
#_LO  HI    INIT  PRIOR PR_SD   PR_type PHASE   env-var use_dev dev_min dev_max dev_std Block   Block_Fxn
5				15		5			5					99			0			  -4			0				0				0					0							0				0				0				#Descending inflection               
0.001		10		0.1		0.1				99			0			  -4			0				0				0					0							0				0				0				#Descending slope 
0				1			0.5		0.5				99			0			  -5			0				0				0					0							0				0				0				#Maximum discard mortality
0				0			0			0					99			0			  -5			0				0				0					0							0				0				0	      #Male offset to descending inflection (arithmetic, not multiplicative)

#_size_sel: 2_NWFSC_shelf_slope 		    				    		                                    		              	  			
  22.5	100		50 		50 		 		99 			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# PEAK                           
 -15  	4    -15   -15      	99      0        	-5      0       0       0         0        			0       0       0  			# TOP:_width of plateau
 -1 		9 		9 		9 		 		99 			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# Asc_width                      
 -1 		20 		6 		6 		 		99 			0 				 5 			0 			0 			0 				0 						0 			0 			0 			# Desc_width                     
 -5 		9  		-5  	-5 		 	 99 			0 				-4 			0 			0 			0 				0 						0 			0 			0 			# INIT:_selectivity_at_fist_bin
 -999		9  	-999  	-999 		  99 			0 				-5 			0 			0 			0 				0 						0 			0 			0 			# FINAL:_selectivity_at_last_bin

#_size_sel: 4_Triennial         		    				      	                                    		              	  			
  40 		130 	75 	  75 			 	99			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# PEAK                           
 -15  	4    -15   -15        99      0        	-5      0       0       0         0        			0       0       0  			# TOP:_width of plateau
 -1 		20 		9 		9 		 		99			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# Asc_width                      
 -1 		20	  	7.2   7.2 	 	99			0 			   5 			0 			0 			0 				0 						0 			0 			0 			# Desc_width                     
 -5 		9  	-5  		-5 		   	99 			0 				-4 			0 			0 			0 				0 						0 			0 			0 			# INIT:_selectivity_at_fist_bin
 -999		9  	-999   -999 		  99 			0 				-5 			0 			0 			0 				0 						0 			0 			0 			# FINAL:_selectivity_at_last_bin

#_size_sel: 5_Slope             		    				      	                                    		              	  			
  20 		100    45 	  45 	 		99			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# PEAK                           
 -15  	4    -15   -15      	99      0        	-5      0       0       0         0        			0       0       0  			# TOP:_width of plateau
 -1 	  9 	  5 	  5 	 			99			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# Asc_width                     
 -1 	  20 	  7.7 	7.7  			99			0 				 5 			0 			0 			0 				0 						0 			0 			0 			# Desc_width                     
 -5 		9  	-5  		-5 		 	  99 			0 				-4 			0 			0 			0 				0 						0 			0 			0 			# INIT:_selectivity_at_fist_bin
 -999		9  	-999  -999 				99 			0 				-5 			0 			0 			0 				0 						0 			0 			0 			# FINAL:_selectivity_at_last_bin

#_size_sel: IPHC             		    				      	                                    		              	  			
  20 		150   45 	  45 	 			99			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# PEAK                           
 -15  	4    -15   -15      	99      0        	-5      0       0       0         0        			0       0       0  			# TOP:_width of plateau
 -1 	  9 	  5 	  5 	 			99			0 				 4 			0 			0 			0 				0 						0 			0 			0 			# Asc_width                     
 -1 	  20 	  7.7 	7.7  			99			0 				 5 			0 			0 			0 				0 						0 			0 			0 			# Desc_width                     
 -5 		9  	-5  		-5 			  99 			0 				-4 			0 			0 			0 				0 						0 			0 			0 			# INIT:_selectivity_at_fist_bin
 -999		9  	-999  -999 				99 			0 				-5 			0 			0 			0 				0 						0 			0 			0 			# FINAL:_selectivity_at_last_bin
#
#_LO    HI INIT    PRIOR PR_SD PR_type PHASE   env-var use_dev  dev_mnyr  dev_mxyr    dev_PH     Block   Blk_Fxn   # parm_name
-5      5  0     0     99    0       2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_1
-5      5  0     0     99    0       2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_2
-5      5  0     0     99    0       2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_3
-5      5  5     0     99    0       -2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_4
-5      5  5     0     99    0       -2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_5
-5      5  5     0     99    0       -2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_6
-5      5  5     0     99    0       -2       0      		0 			0 				0 						0 			0 			0 			 # ln(EffN_mult)_7


# timevary selex parameters 
#23 blocks
           -10            10    			0.23 				0.23   				 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.78        0.78           99             0      4  # Retain_P3_Fishery_current
           -10            10    			1           1              99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.88        0.88           99             0      4  # Retain_P3_Fishery_current
           -10            10    			1           1              99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.95        0.95           99             0      4  # Retain_P3_Fishery_current
           -10            10    			1           1              99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.53        0.53           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.78        0.78           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.44        0.44           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.59        0.59           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.85        0.85           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.58        0.58           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.68        0.68           99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.61 				0.61    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.75 				0.75    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.78 				0.78    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.83 				0.83    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.83 				0.83    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.81 				0.81    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.83 				0.83    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.81 				0.81    			 99             0      4  # Retain_P3_Fishery_current
           -10            10    			0.78 				0.78    			 99             0      4  # Retain_P3_Fishery_current
#
0  #_ 0/1 to request experimental 2D_AR selectivity smoother options
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp

 -9999   1    0  # terminator
#
1 #_maxlambdaphase
1 #_sd_offset
# read 0 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

