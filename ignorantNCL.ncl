;*****************************************************
;  NCL program developed by ignorant
;  
;  History:
;           before 2010/10/25  ignorant_contour,ignorant_nc_contour
;           2010/10/25         ignorant_HMLcloud, first release
;           2010/11/07         ignorant_LWP, first release
;           2010/12/27         ignorant_write_nc, first release
;                              test OK in float varible, character & string varible is not test
;           2010/12/31         CHANGE *ignorant_write_nc*, [opt] statement
;                              old: opt=0,1,2 ; new: opt=-1,-2,N
;                              for writing value in file more flexibility, see discription of it
;
;           2011/01/04         CHANGE *ignorant_write_nc*, [opt] statement
;                              old: opt=-1,-2,N ; new: opt=-1,-2,-3,N
;                              opt=-3 allow automatically create non-pre-existing varible and dimensions
;
;           2011/08/29         ADD function "ignorant_SelectColorMap"
;
;           2011/09/30         ADD function "ignorant_merge_colormaps"
; 
;           2011/10/31         ADD function "ignorant_xy"
;           2011/12/02         ADD function "ignorant_combine_colormaps"
;           2011/12/07         CHANGE function name "ignorant_SelectColorMap" 
;                              to "ignorant_select_colormap"
;           2013/04/16         ADD function "ignorant_get_var_files"
;           2013/11/08         note that function "ignorant_select_colormap" MUST call 
;                              before call "gsn_open_wks", otherwise it would show error message:
;                              "fatal:_NclCreateHLUObjOp: Parent value is not an hlu object or is undefined"
;                              in RCEC nuwa machine
;           2013/12/10         ADD a choice in function "ignorant_select_colormap", let user can select all colors
;                              in given color map.
;           2014/04/03         CHANGE function "ignorant_select_colormap", add extra input argument to include/exclude
;                              foreground and backgroung colors
;           2014/04/24         ADD funtion "gsn_csm_contour_map_avg"
;           2014/04/25         ADD function "diff"
;           2014/09/25         ADD function "multiply"
;           2014/09/29         ADD function "divide"
;           2016/02/20         ADD function "lat_wgt_avg"
;           2016/02/21         REWRITE function "lat_wgt_avg", to avoid missing values in data_in
;           2016/02/20         ADD function "plus"
;           2016/03/20         ADD function "binmap"
;           2016/03/29         ADD attribute "pdf" in function binmap
;           2016/05/10         CHANGE function binmap sorting code to speed up
;           2016/10/07         ADD function "taylor_diagram"
;           2016/10/21         MODIFY cosine latitude weighted method in function "gsn_csm_contour_map_avg"
;                              MODIFY function ignorant_select_colormap by using read_colormap_file
;           2016/11/03         FIX a bug at "ignorant_select_colormap" by adding fore/background colors
;           2017/02/17         REWRITE function "lat_wgt_avg" by applying NCL function
;           2017/08/30         CHANGE adding mean values in "ignorant_xy" legends, by setting opt@legend_mean = True,False and/or opt@MeanFormat = "%5.2f"
;           2017/09/27         ADD function "unit_convert"
;           2017/10/18         ADD function "sort_for_xyplot"
;           2018/02/20         ADD procedure "cnColors_PosNeg"
;           2018/02/25         DEBUG procedure "cnColors_PosNeg", when num_positive and num_negative equals one, fspan will fail.
;                              ADD return opt in function "cnColors_PosNeg"
;           2018/03/19         MODIFY "ignorant_get_var_files" to get coordinate variables
;           2018/05/03         ADD "ignorant_get_vars_file"
;           2018/05/04         ADD "ignorant_convert_2Ddata"
;           2018/05/10         ADD opt_2Ddata@return_type = "lat_time-lon_avg" into "ignorant_convert_2Ddata"
;           2018/05/11         MODIFY "ignorant_convert_2Ddata"
;           2018/05/17         ADD "pressure_lat-timelon_avg","time_lat_lon-plev" into ignorant_convert_2Ddata 
;           2018/05/19         ADD ms2knots in "unit_convert"
;                              MODIFY "ignorant_convert_2Ddata"
;           2018/05/30         ADD W/m22mm/day in "unit_convert"
;           2018/05/31         ADD mm/day2m/s in "unit_convert"
;                              MODIFY "unit_convert", not do if input units differs the unit attribute
;           2018/06/07         ADD function gsn_csm_contour_map_siglvl
;           2018/06/08         ADD procedure change_var_atts
;           2018/06/14         ADD set cn fill colors in the procedure "change_var_atts"
;           2018/06/16         MODIFY longname setup in the case "cnColors_PosNeg"
;           2018/07/11         MODIFY ignorant_convert_2Ddata to make lon conversion automatically
;           2018/07/15         MODIFY reture_type name of ignorant_convert_2Ddata
;           2018/07/26         DEBUG in "cnColors_PosNeg", moving CopyAtt otherwise the previous cmnmap will override the correct one
;                              CHANGE dry_wet colors in "cnColors_PosNeg"
;           2018/08/02         DEBUG gsn_csm_contour_map_siglvl, user can decide whether use cnColors_PosNeg 
;           2018/08/04         ADD opt_chgatt@chg_cncolors in "change_var_atts"
;           2018/08/04         ADD a new procedure "ignorant_convert_1Ddata"
;           2018/08/05         MODIFY "ignorant_convert_1Ddata"
;           2018/08/06         ADD "tlji2pn_crossji" into ignorant_convert_2Ddata
;           2018/08/06         MIDIFY "change_var_atts"
;
;*****************************************************

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;  Content of ignorant NCL program
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
; load "/home/chishin/ncarg/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "/home/chishin/ncarg/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
;
; procedure change_var_atts (varname[1]:string, varin, res[1]:logical, opt_chgatt[1]:logical)
;   change units and resources of the input variable
;     varname	: a string that decides which kind of changes will be applied
;     varin  	: input variable
;     res    	: a NCL res
;     opt_chgatt: option. Turn on/off this procedure
;       opt_chgatt@chg_cncolors = True/False ; turn on/off changing contour colors
;
; function gsn_csm_contour_map_siglvl(wks:graphic, varvar_any[*][*][*]:numeric, varvar_base[*][*][*]:numeric, res_var[1]:logical, res_sig[1]:logical, opt[1]:logical)
;   plot the difference between varvar_any and varvar_base over map and overlay the significance level from the t-test results
;     wks		: work station
;     varvar_any	: a three dimension variable (time,lat,lon)
;     varvar_base	: a three dimension variable (time,lat,lon)
;     res_var		: NCL res for plotting varvar_any minus varvar_base
;     res_sig		: NCL res for plotting significance level
;     opt 		: options
;	opt@siglvl = 0.01  		; set significance level (the default is 0.05)
;       opt@cnColors_PosNeg = False	; turn off cnColors_PosNeg
;
; procedure ignorant_convert_1Ddata (infilenames[*]:string, varvars[*]:string, res[1]:logical, opt_1Ddata[1]:logical)
;   convert input variable to 1-D for plotting a XY plot
;     Return	: var_xx = opt@var_xx
;		: var_yy = opt@var_yy
;		:   if opt@var_xx_number = True, return var_xx = 0.,1.,2.,...N
;		:   if opt@var_yy_number = True, return var_yy = 0.,1.,2.,...N
;
;     infilename: input file name (CESM for now)
;     varvar    : variables in infilename
;     opt_1Ddata: option
;       opt_1Ddata@return_type = "vtji2t_vt_avgji"	; input: CESM (variables,time,lat,lon), return: var_xx=(time), var_yy=(varible,time), averaged over lat and lon
;       opt_1Ddata@return_type = "vtlji2vp_p_avgtji"	; input: CESM (variables,lev/ilev,time,lat,lon), return: var_xx=(variable,pressure), var_yy=(pressure), averaged over time, lat, and lon
;       opt_1Ddata@return_type = "ftlji2fp_p_avgtji"	; input: CESM (files,time,lev/ilev,lat,lon), return: var_xx=(files,pressure), var_yy=(pressure), averaged over time, lat, and lon
;       opt_1Ddata@return_type = "ftji2t_ft_avgji"	; input: CESM (files,time,lat,lon), return: var_xx=(time), var_yy=(files,time), averaged over lat and lon
;
;       opt_1Ddata@@plev = (/250., 300., 350., 400., 450., 500./)	; pressure levels for interpolation
;
;       opt_1Ddata@@update_res = False	; turn off updating res
;
;       opt_1Ddata@x_coordinate = "12months_number" 		; set x-coordinate to 1,2,3,...,12
;       opt_1Ddata@x_coordinate = "12months_1characters" 	; set x-coordiante to J,F,M,...,D
;       opt_1Ddata@x_coordinate = "12months_3characters" 	; set x-coordinate to Jan,Feb,Mar,...,Dec
;
; function ignorant_convert_2Ddata (infilename[1]:string, varvar[1]:string, opt_2Ddata:logical)
;   convert input dataset to 2-D dataset for plotting a contour
;     infilename: input file name (CESM for now)
;     varvar    : variable in infilename
;     opt_2Ddata: option
;
;      if applicable,    opt_2Ddata@time_step   = 1	; set return time step
;
;        opt_2Ddata@return_type = "tlji2pj_avgti"	; input: CESM var(time,lev/ilev,lat,lon). return var(pressure,lat) averaged over time & lon 
;
;        opt_2Ddata@return_type = "tlji2pt_avgji"	; input: CESM var(time,lev/ilev,lat,lon). return var(pressure,time) averaged over lat & lon 
;           opt_2Ddata@plev        = (/500.,880.1000./)	;   pressure levels (hPa) for interpolation
;
;        opt_2Ddata@return_type = "tlji2tji_intp"	; input: CESM var(time,lev/ilev,lat,lon). return var(time,lat,lon) at the given pressure level
;           opt_2Ddata@plev        = 300.		;   interpolate the var at this pressure level (hPa)
;
;        opt_2Ddata@return_type = "tji2jt_avgi"         ; input: var(time,lat,lon). return var(latitdue,time) averaged over lon
;
;        opt@return_type = "tlji2pn_crossji"		; input: CESM var(time,lev/ilev,lat,lon). return var(time,pressure,npoints), vertical cross section between two points
;           opt@linint2_points_cyclic = True  		; For function linint2_points. This should be set to True only if you have global data, but your longitude values don't quite wrap all the way around the globe. For example, if your longitude values go from, say, -179.75 to 179.75, or 0.5 to 359.5, then you would set this to True.
;           opt@leftlat  =  -30.			; lat/lon of the left point
;           opt@leftlon  =  20.				
;           opt@rightlat =  -60.			; lat/lon of the right point
;           opt@rightlon =  -60.
;           opt@npts     =  100				; number of points of resulting transection
;           opt@time_step= "avg" or n			; "avg" plot the time-averaged; n=0,N plot at n time step
;           opt@plev     = (/250., 300., 350., 400., 450., 500./)       ; pressure levels for interpolation

;
; procedure cnColors_PosNeg (varvar[*][*]:numeric, res[1]:logical, opt_cncolors[1]:logical)
;   assign colors to positive and negative values in NCL resources for plotting contour
;   return: res@cnFillColors
;     varvar       : 2-D array for plotting
;     res          : NCL resources
;     opt_cncolors : a logical varialbe to control availabe features in this procedure
;        opt_cncolors@max_levels = 15  ; level for using function nice_mnmxintvl
;      
;        opt_cncolors@cnlevels_predefine = False    ; True  - contour levels are given in res
;                                                   ; False - contour levels will be computed in this procedure
;      
;        opt_cncolors@colomap_positive = "WhiteYellowOrangeRed"  ; colormap for plotting positive colors
;        opt_cncolors@colomap_positive_index_start = 2           ; the least positive color
;        opt_cncolors@colomap_positive_index_end = 999           ; the most  positive color
;      
;        opt_cncolors@colomap_negative = "MPL_GnBu"              ; colormap for plotting negative colors
;        opt_cncolors@colomap_negative_index_start = 127         ; the most  negative color
;        opt_cncolors@colomap_negative_index_end = 2             ; the least negative color

; function binmap2(xdata[*]:numeric, ydata:numeric, nbin:integer, opt:logical)
;          sort ydata into xdata bin based on user defined min,max,interval values
;          this is an updated version of binmap2
;            xdata: an input 1-D data
;            ydata: an input 1D or 2D data, the rightmost dimension must be the same as xdata
;            nbin : number of bins for xdata
;            opt  : logical variable
;              opt@x_max = 100.  ; max value for bin x, if not set, max(xdata) will be used
;              opt@x_min =   0.  ; min value for bin x, if not set, min(xdata) will be used
;
;          return: a 3D array, [variable | 1] x [index | 5] x [bin | 4]
;                     dim variable: the same as the first dimension of ydata
;                     dim index   : Index 0: number in each bin; 
;                                   Index 1: Sum in each bin; 
;                                   Index 2: Average value in each bin; 
;                                   Index 3: standard deviation in each bin; 
;                                   Index 4: PDF of each bin
;                     dim bin     : number of bins
;
; function sort_for_xyplot(xdata[*]:numeric, ydata[*]:numeric, zdata[*]:numeric, nbin:integer)
;          sort input zdata into several bins, and then return a data that can use for plotting a XY plot
;            xdata: an input 1-D data that would be plot on X-axis
;            ydata: an input 1-D data that would be plot on Y-axis
;            zdata: an input 1-D data that would be sorted into several bins
;            nbin : number of bins for zdata
;
;          return: a 3-D data plotvars (2,nbin,dimension of X,Y)
;                   plotvars@index = "0: x_plot_var, 1: y_plot_var"
;                   plotvars@nbin_labels = labels
;                   plotvars@nbin_mid    = layers
;                   plotvars@nbin_edge   = levels
;
; function unit_convert(var_from:numeric, unit_from:string, unit_to:string)
;          convert the unit of an input variable, var_from
;          var_from : any numerical variable
;          unit_from: unit of var_from. Can set "" if @units is an attribute of var_from
;          unit_to  : new unit
;
; function taylor_diagram (wks:graphic ,RATIO[*][*]:numeric, CC[*][*]:numeric \
;                                     ,rOpts:logical)
;          wks  : work station
;          RATIO: ratio of the normalized standard deviation respect to the reference variable  
;          CC   : correlation coefficient
;          rOpts: NCL resources for taylor diagram
;
;          ref: http://www.ncl.ucar.edu/Applications/taylor.shtml
;
; function binmap(xx:numeric,yy:numeric,xx_min[1]:numeric,xx_max[1]:numeric,xx_int[1]:numeric)
;          sort yy into xx bin based on user defined min,max,interval values
;          Return: zz(3,binsize), Index 0: number in each bin; Index 1: Average value in each bin; Index 2: standard deviation in each bin
;          xx    : multidimension array
;          yy    : multidimension array
;          xx_min: minimum value of bin
;          xx_max: maximum value of bin
;          xx_int: bin interal
;
; function lat_wgt_avg(data_in:numeric)
;          compute latitude-weighted mean fields
;          Return  : longitude and latitude-weighted mean fields
;          data_in : numeric array, acceptable dimension orders are (lat,lon),(time,lat,lon),(time,?,lat,lon), (time,?,?,lat,lon).
;
; function plus(a:numeric, b:numeric)
;          compute a plus b, return variable's dimension,attributes,coordinated is the same as a.
;          Return  : a plus b
;          a       : numeric array
;          b       : numeric number 
;
; function divide(a:numeric, b:numeric)
;          compute a divided by b value, return variable's dimension,attributes,coordinated is the same as a.
;          Return  : a divide b
;          a       : numeric array
;          b       : numeric number 
;
; function multiply(a:numeric, b:numeric[1])
;          compute a multiply b value, return variable's dimension,attributes,coordinated is the same as a.
;          Return  : a multiply b
;          a       : numeric array
;          b       : numeric number 
;
; function diff(a:numeric, b:numeric)
;          compute a minus b value, return variable's dimension,attributes,coordinated is the same as a.
;          Return  : a minus b
;          a       : numeric array
;          b       : numeric array, dimension is must the same as a
;
; function gsn_csm_contour_map_avg(wks[1]:graphic, data_input[*][*]:numeric, \
;                                  nclres[1]:logical, opt[1]:logical)
;          using NCL funtion gsn_csm_contour_map and ADD 2D-data average at @gsnCenterString 
;          there are also user-defined resources, opt
;
;          Return    : graphic, contour at map plot
;          wks       : work station
;          data_input: input 2-D data array
;          nclres    : any resources at NCL
;          opt       : user-define options
;            opt@LatWeighted = False    ; do average considering latitude transformation effects
;                                       ; if turn on, data_input coordinate variable's MUST in correct format
;            opt@MeanFormat = "%7.3f"   ; set mean value format
;            opt@MeanPrefix = "mean: "  ; set prefix of mean value
;
; function ignorant_get_var_files(infile_names:string, read_var_name[1]:string)
;          get same varible from different files, if [read_var_name] has dimension as (time,lat,lon)
;          the return dimension could be (file,time,lat,lon)
;
;          Return         : same type as [read_var_name], but add a extra dimension [file]
;          infile_names   : one-dimension string, full path of read-in files
;          read_var_name  : string, ONE varible name in infile_names
;
; function ignorant_get_vars_file(infilenames[0]:string, varvars[*]:string)
;          get all varibles from a file, if [varvars] has dimension as (time,lat,lon)
;          the return dimension could be (variable,time,lat,lon)
;
;          Return         : same type as [read_var_name], but add a extra dimension [variable]
;          infilename s   : string, ONE path of the read-in file
;          read_var_name  : string, read-in variables with the SAME dimension
;
; function ignorant_combine_colormaps( cmap1[*][3]:numeric, cmap2[*][3]:numeric, \
;                                      choice:integer  )
;          combine 2 RGB array to one RGB array.
;          Can use for combine different color array.
;           
;          Return      : numeric, combined color map array
;          cmap1,cmap2 : First RGB array, in dimension [*]{3}.
;          choice      : .eq. 1 -> combine total cmap1,cmap2
;                        else   -> don't combine cmap2 fore/background color
;
; function ignorant_xy(wks:graphic, xx:numeric, yy:numeric , \
;                      labels:string, nclres:logical, opt:logical)
;          ignorant defined XY-plot function. gsn_csm_xy is the basic.
;
;          Return : graphic, XY plot.
;          wks    : work station
;          xx     : x-value
;          yy     : y-value
;          labels : each line title
;          nclres : any resources in NCL
;          opt    : user resources, not define
;            opt@legend_mean = True or False   ; turn on/off mean values on legends
;            opt@MeanFormat = "%6.2f"          ; format of the mean values
;
; function ignorant_merge_colormaps(icmap)
;          merge NCL predifined color map.
;          Return : merged-colormap RGB value array.
;          icmap  : an array contain NCL colormaps name, ex:(/"Cat12", "gsltod", "BrownBlue12"/)
;
; function ignorant_select_colormap(icmap_name:string, ocmap_index[*]:numeric, \
;                                   option_foreback:logical)
;          select some color in NCL-named colormap, and write out to an array, which can used to
;          define a new colormap.
;          icmap_name      : NCL-named colormap, ex: "gui_default"
;          ocmap_index     : an index array of choosed color in icmap_name, ex: (/3,6,8/)
;                            if ocmap_index = 999 then return total colors in given colormap
;          option_foreback : option that add fore- and background colors in return variables
;                            .True.  : add fore- and background colors
;                            .False. : not add fore- and background colors
;
; procedure ignorant_write_nc(filename:string,outvarname:string,invar,opt:integer)
;           Write value to an existing netCDF file with existing varible
;           filename: file which write in, note should be a support file type of NCL
;           outvarname : varible name in *filename* file, must be existing
;           invar : value, dimension MUST be equal to *outvarname* in *filename*, exclude unlimited dimension
;           opt : option, INTEGER, -1   -- outfile have unlimited time, overwrite from max time
;                                  -2   -- Don't write in unlimited time varible
;                                  -3   -- *outvarname* not exist in *filename*
;                                          create new varible and new dimsensions  (lazy method, not recommended)
;                                N.ge.0 -- outfile have unlimited time, overwrite in N time
;
; function ignorant_LWP(qc[*][*][*],p[*][*][*])
;          Calculate liquid water path using NCL "prcwater_dp" function
;          qc : cloud/rain/ice water mixing ratio, dimension is nz*ny*nx, unit in kg/kg
;           p : pressure, dimension is same as qc, unit in Pa
;
;          Return: a two dimension array ny*nx, mean every grid column liquid water path
;
;
; function ignorant_HMLcloud(P[*][*][*]:numeric,Tk[*][*][*]:numeric,qv[*][*][*]:numeric,qall[*][*][*]:numeric)
;          Calculate every grid box High(300-550mb),Middle(550-800mb),Low(800-1000mb) cloud amount using maximum overlap assumption
;          algorithm is same as Xu and Krueger(1991), Mon. Wea. Rev. 119, 342-367
;             P: pressure, dimension is nz*ny*nx , unit in hPa
;            Tk: temperature, dimension is same as P, unit in K
;            qv: water vapor mixing ratio, dimension is same as P, no unit (ex: kg/kg)
;          qall: condenced water mixing ratio, dimension is same as P, no unit (ex: kg/kg)
;
;          Return: High,Middle,Low level cloud amount, 3*ny*nx dimension array, index 0,1,2 corresponding to L,M,H cloud
;
;
; function ignorant_contour(wks[1]:graphic,data[*][*]:numeric,opt[1]:logical)
;          Default Behavier: Filled Color Contour with Label Bar
;
;
; function ignorant_nc_contour( wks[1]:graphic, nc_file:file, data[*][*]:numeric, opt:logical, nclres:logical )
;          Default Behavier: Draw Filled Color Contour on Map
;
;          opt@WRFfile = True        ; Set True if nc_file is WRF output
;
;          opt@REF_LAT = 20.023      ; Use in Map Projection
;          opt@REF_LON = 117.2858    ; KNOWNI,KNOWNJ correspond lon,lat value
;          opt@KNOWNI = 0            ;
;          opt@KNOWNJ = 0            ;
;
;          opt@XLeft   = XL          ; Set Map Area
;          opt@XRight  = XR          ; default is full range
;          opt@YTop    = YT          ;
;          opt@YBottom = YB          ;
;
;          nclres : Any resources in NCL
;
;
;!!!!!!!!!!!!!!!!!!!!!!
;  Program Code
;!!!!!!!!!!!!!!!!!!!!!!

;###################################################
;  BUG: 1. sometimes it will have this error message "fatal:ContourPlotGetValues: error copying cnFillColors GenArray". When it happens, turn off this procedure
;          FIX. This error is because I also set cnFillPalette to something. If I turn off this cnFillPalette, this error is gone.
undef("cnColors_PosNeg")
procedure cnColors_PosNeg (varvar[*][*]:numeric, res[1]:logical, opt_cncolors[1]:logical)
begin

;-------------
; initialize
;-------------
  option_cmap = "default"

  if ( isatt(varvar,"long_name") ) then
    longname = varvar@long_name
  else
    longname = "slkdjflksjdfljasldfjlaks"
  end if

  ;if (\
  ;        longname.eq."" \
  ;   .or. longname.eq."" \
  ;   )
  ;  option_cmap = ""
  ;end if 

  ;if ( \
  ;         .not.ismissing(str_match(longname,"")) \
  ;    .or. .not.ismissing(str_match(longname,"")) \
  ;   ) then
  ;   option_cmap = ""
  ;end if

  if ( \
           .not.ismissing(str_match(longname,"Vertical velocity")) \
      .or. .not.ismissing(str_match(longname,"OMEGA")) \
     ) then
     option_cmap = "red_blue"
  end if

  if ( \
           .not.ismissing(str_match(longname,"Zonal wind")) \
      .or. .not.ismissing(str_match(longname,"zonal wind")) \
      .or. .not.ismissing(str_match(longname,"Meridional wind")) \
      .or. .not.ismissing(str_match(longname,"meridional wind")) \
      .or. .not.ismissing(str_match(longname,"U at")) \
      .or. .not.ismissing(str_match(longname,"V at")) \
     ) then
     option_cmap = "purple_green"
  end if
  
  if (\
           .not.ismissing(str_match(longname,"precipitation rate")) \
      .or. .not.ismissing(str_match(longname,"Precipitation rate")) \
      .or. .not.ismissing(str_match(longname,"snow rate")) \
     )
    option_cmap = "dry_wet"
  end if

  if ( \
          .not.ismissing(str_match(longname,"water path")) \
     ) then
     option_cmap = "dry_wet"
  end if

  if ( \
          .not.ismissing(str_match(longname,"humidity")) \
     ) then
     option_cmap = "dry_wet"
  end if

;----------------
; program start
;----------------

  ;*** intialize opt_work ***
  opt_work = True
  opt_work@max_levels = 15  ; level for using function nice_mnmxintvl

  opt_work@cnlevels_predefine = False    ; True  - contour levels are given in res
  ;opt_work@cnlevels_predefine = True      False - contour levels will be computed in this procedure

  copy_VarAtts(opt_cncolors, opt_work)

  ;*** set colormap here ***
  if (option_cmap .eq. "dry_wet") then
    opt_work@colormap_positive = "MPL_YlGnBu"
    opt_work@colormap_positive_index_start = 2      ; the least positive color
    opt_work@colormap_positive_index_end = 113       ; the most  positive color

    opt_work@colormap_negative = "MPL_pink"
    opt_work@colormap_negative_index_start = 17      ; the most  negative color
    opt_work@colormap_negative_index_end = 129       ; the least negative color
  end if

  if (option_cmap .eq. "purple_green") then
    opt_work@colormap_positive = "MPL_Purples"       ; colormap for plotting positive colors
    opt_work@colormap_positive_index_start = 2       ; the least positive color
    opt_work@colormap_positive_index_end = 999       ; the most  positive color

    opt_work@colormap_negative = "MPL_Greens"        ; colormap for plotting negative colors
    opt_work@colormap_negative_index_start = 120     ; the most  negative color
    opt_work@colormap_negative_index_end = 2         ; the least negative color
  end if

  if (option_cmap .eq. "red_blue") then
    opt_work@colormap_positive = "NCV_blu_red"         ; colormap for plotting positive colors
    opt_work@colormap_positive_index_start = 123       ; the least positive color
    opt_work@colormap_positive_index_end = 2           ; the most  positive color

    opt_work@colormap_negative = "NCV_blu_red"         ; colormap for plotting negative colors
    opt_work@colormap_negative_index_start = 255       ; the most  negative color
    opt_work@colormap_negative_index_end = 137         ; the least negative color
  end if

  if (option_cmap .eq. "default") then
    opt_work@colormap_positive = "WhiteYellowOrangeRed"  ; colormap for plotting positive colors
    opt_work@colormap_positive_index_start = 2           ; the least positive color
    opt_work@colormap_positive_index_end = 999           ; the most  positive color

    opt_work@colormap_negative = "MPL_GnBu"              ; colormap for plotting negative colors
    opt_work@colormap_negative_index_start = 127         ; the most  negative color
    opt_work@colormap_negative_index_end = 2             ; the least negative color
  end if

  ;*********************
  ; get contour levels
  ;*********************

  ; given res@cnLevelSelectionMode = "ExplicitLevels"
  if ( opt_work@cnlevels_predefine .and. isatt(res,"cnLevelSelectionMode") .and. res@cnLevelSelectionMode .eq. "ExplicitLevels" ) then
    if (isatt(res,"cnLevels")) then
      cnlevels = res@cnLevels
      option_cnlevels = True
    else
      option_cnlevels = False
    end if

  ; given res@cnLevelSelectionMode = "ManualLevels"
  else if ( opt_work@cnlevels_predefine .and. isatt(res,"cnLevelSelectionMode") .and. res@cnLevelSelectionMode .eq. "ManualLevels" ) then
    cnmin = res@cnMinLevelValF
    cnmax = res@cnMaxLevelValF
    cnint = res@cnLevelSpacingF

    if ( cnint .ne. 0. ) then
      n1 = toint((cnmax-cnmin)/cnint) + 1
      ;print(n1)
      cnlevels = fspan(cnmin,cnmax,n1)
      option_cnlevels = True
    else
      option_cnlevels = False
    end if

  ; compute cnlevels in this procedure
  else
    cnmin = min(varvar)    
    cnmax = max(varvar)    

    max_levels = opt_work@max_levels

    mnmxint = nice_mnmxintvl(min(varvar), max(varvar), max_levels, False)
    ;print(mnmxint)
    cnmin = mnmxint(0)
    cnmax = mnmxint(1)
    cnint = mnmxint(2)

    if ( cnint .ne. 0. ) then
      n1 = toint((cnmax-cnmin)/cnint) + 1
      ;print(n1)
      cnlevels = fspan(cnmin,cnmax,n1)
      option_cnlevels = True
    else
      option_cnlevels = False
    end if

  end if 
  end if 
  ;print(cnlevels)

if (option_cnlevels) then

  ;*****************************************************************
  ; find indexes of zero, smallest positive, and smallest negative
  ;*****************************************************************
  num_cnlevels = dimsizes(cnlevels)
  num_cncolors = num_cnlevels+1
  index_zero = closest_val(0., cnlevels)

  if ( cnlevels(index_zero) .eq. 0. ) then
    num_negative = index_zero+1
    num_positive = num_cncolors - num_negative

  else if ( cnlevels(index_zero) .gt. 0. ) then
    num_negative = index_zero+1
    num_positive = num_cncolors - num_negative

  else if ( cnlevels(index_zero) .lt. 0. ) then
    num_negative = index_zero+1
    num_positive = num_cncolors - num_negative
  end if
  end if
  end if
  ;print("num_negative: "+num_negative+", num_positive: "+num_positive)

  ;************
  ; set colors
  ;************ 
  cncolors = new( (/num_cncolors,4/) , "float" )
  cncolors = 1  ; initialize colors to white 
 
  cmap_positive = read_colormap_file(opt_work@colormap_positive)
  cmap_negative = read_colormap_file(opt_work@colormap_negative)

  num_cmap_positive = dimsizes(cmap_positive(:,0))
  num_cmap_negative = dimsizes(cmap_negative(:,0))

  ; set positive colors
  if (isatt(opt_work, "colormap_positive_index_start") .and. opt_work@colormap_positive_index_start .lt. num_cmap_positive) then
    cmap_index_pos_start = opt_work@colormap_positive_index_start
  else
    cmap_index_pos_start = 2
  end if

  if (isatt(opt_work, "colormap_positive_index_end") .and. opt_work@colormap_positive_index_end .lt. num_cmap_positive) then
    cmap_index_pos_end = opt_work@colormap_positive_index_end
  else
    cmap_index_pos_end = num_cmap_positive-1
  end if

  ; set negative colors
  if (isatt(opt_work, "colormap_negative_index_start") .and. opt_work@colormap_negative_index_start .lt. num_cmap_negative) then
    cmap_index_neg_start = opt_work@colormap_negative_index_start
  else
    cmap_index_neg_start = 2
  end if

  if (isatt(opt_work, "colormap_negative_index_end") .and. opt_work@colormap_negative_index_end .lt. num_cmap_negative) then
    cmap_index_neg_end = opt_work@colormap_negative_index_end
  else
    cmap_index_neg_end = num_cmap_negative-1
  end if

  ; get index of colormap
  if (num_positive .gt. 1) then
    cmap_index_positive = toint (fspan(tofloat(cmap_index_pos_start),tofloat(cmap_index_pos_end),num_positive))
  else
    cmap_index_positive = 1
  end if

  if (num_negative .gt. 1) then
    cmap_index_negative = toint (fspan(tofloat(cmap_index_neg_start),tofloat(cmap_index_neg_end),num_negative))
  else
    cmap_index_negative = 1
  end if

  ; assign colors
  do i=0,num_negative-1
    ii1 = cmap_index_negative(i)
    cncolors(i,:) = cmap_negative(ii1,:)
  end do

  ii1 = 0
  ii2 = 0
  do i=num_negative,num_cncolors-1
    ii2 = cmap_index_positive(ii1)
    cncolors(i,:) = cmap_positive(ii2,:)
    ii1 = ii1+1
  end do

  ;*****************
  ; return NCL res
  ;*****************
  if ( isatt(res, "cnFillColors")) then
    delete(res@cnFillColors)
  end if

  if ( opt_work@cnlevels_predefine .and. isatt(res,"cnLevelSelectionMode") .and. res@cnLevelSelectionMode .eq. "ExplicitLevels" ) then
    res@cnFillColors = cncolors

  else if ( opt_work@cnlevels_predefine .and. isatt(res,"cnLevelSelectionMode") .and. res@cnLevelSelectionMode .eq. "ManualLevels" ) then
    res@cnFillColors = cncolors
      
  else
    if ( isatt(res, "cnMinLevelValF")) then
      delete(res@cnMinLevelValF)
    end if
    if ( isatt(res, "cnMaxLevelValF")) then
      delete(res@cnMaxLevelValF)
    end if
    if ( isatt(res, "cnLevelSpacingF")) then
      delete(res@cnLevelSpacingF)
    end if
  
    res@cnLevelSelectionMode = "ManualLevels"
    res@cnMinLevelValF = cnmin
    res@cnMaxLevelValF = cnmax
    res@cnLevelSpacingF = cnint
    res@cnFillColors = cncolors
  end if
  end if

  ;**************
  ; return opt 
  ;**************
  if ( isatt(opt_cncolors,"colormap_positive") ) then
    delete(opt_cncolors@colormap_positive)
    opt_cncolors@colormap_positive = opt_work@colormap_positive
  else
    opt_cncolors@colormap_positive = opt_work@colormap_positive
  end if

  if ( isatt(opt_cncolors,"cmap_index_positive") ) then
    delete(opt_cncolors@cmap_index_positive)
    opt_cncolors@cmap_index_positive = cmap_index_positive
  else
    opt_cncolors@cmap_index_positive = cmap_index_positive
  end if

  if ( isatt(opt_cncolors,"colormap_negative") ) then
    delete(opt_cncolors@colormap_negative)
    opt_cncolors@colormap_negative = opt_work@colormap_negative
  else
    opt_cncolors@colormap_negative = opt_work@colormap_negative
  end if

  if ( isatt(opt_cncolors,"cmap_index_negative") ) then
    delete(opt_cncolors@cmap_index_negative)
    opt_cncolors@cmap_index_negative = cmap_index_negative
  else
    opt_cncolors@cmap_index_negative = cmap_index_negative
  end if

  delete(cncolors)
  delete(opt_work)
  delete(cmap_positive)
  delete(cmap_negative)
  delete(num_cmap_positive)
  delete(num_cmap_negative)

end if  ; end if of option_cnlevels


end

;###################################################
undef("binmap2")
function binmap2(xdata[*]:numeric, ydata:numeric, nbin:integer, opt:logical)
begin

;--------
; check
;--------
;

;--- check dimensions ---
  dimx = dimsizes(xdata)
  dimy1 = dimsizes(dimsizes(ydata))

  if (dimy1 .eq. 1) then
    dimy = dimsizes(ydata)
    dimy2 = 1
  else if (dimy1 .eq. 2) then
    dimy  = dimsizes(ydata(0,:))
    dimy2 = dimsizes(ydata(:,0))
  else 
    print("")
    print("ERROR: (binmap2) Y data must be 1 or 2 dimensional data")
    return(0)
    print("")
  end if
  end if

  if (dimx .ne. dimy) then
    print("")
    print("ERROR: (binmap2) dimension of X and Y must be the same! (dimx= "+dimx+", dimy= "+dimy+")")
    return(0)
    print("")
  end if

;--- set work data ---
  if (dimy1 .eq. 1) then
    ydata_work = new( (/1,dimy/), typeof(ydata))
    ydata_work(0,:) = ydata
  end if

  if (dimy1 .eq. 2) then
    ydata_work = ydata
  end if
  ;printVarSummary(ydata_work)

;--- set max and min values ---
  if ( isatt(opt,"x_max")) then
    x_max = opt@x_max
  else
    x_max = max(xdata)
  end if

  if ( isatt(opt,"x_min")) then
    x_min = opt@x_min
  else
    x_min = min(xdata)
  end if
  ;print(x_max+" , "+x_min)

;--- compute intervals ---
  mnmxint = nice_mnmxintvl(x_min,x_max,nbin,True)
  nlevels = toint(((mnmxint(1)-mnmxint(0))/mnmxint(2))) + 1
  levels  = fspan(mnmxint(0),mnmxint(1),nlevels)
  layers  = levels(0:nlevels-2)
  do i=0,nlevels-2
    layers(i) = levels(i) + mnmxint(2)/2.
  end do
  ;print(levels)

;--- set return variable ---
  ydata_binx = new( (/dimy2,5,nlevels-1/) , typeof(ydata))

  ydata_binx!0 = "variable"
  ydata_binx!1 = "index"
  ydata_binx!2 = "bin"

  ;ydata_binx@bin_edge = levels
  ;ydata_binx@bin_mid  = layers
  ;ydata_binx@bin_min = x_min
  ;ydata_binx@bin_max = x_max
  ;ydata_binx@bin_number_of_data = dimx
  ydata_binx@description = "Index 0: number in each bin; Index 1: Sum in each bin; Index 2: Average value in each bin; Index 3: standard deviation in each bin; Index 4: PDF of each bin"
  ydata_binx@bin_number_of_data = num(.not.ismissing(xdata))
  ydata_binx@bin_max = x_max
  ydata_binx@bin_min = x_min
  ydata_binx@bin_mid  = layers
  ydata_binx@bin_edge = levels
  ydata_binx@bin_interval = mnmxint(2)

  ydata_binx(:,0,:) = 0.  ; set the number in all bins to zero
  ydata_binx(:,4,:) = 0.  ; set the PDF to zero
  ;printVarSummary(ydata_binx)

;--- sort ydata based on x bin ---
  do i=0,nlevels-2
    ii = ind(xdata.ge.levels(i).and.xdata.lt.levels(i+1))
    if (.not.all(ismissing(ii))) then
      do j=0,dimy2-1
        ynum1 = dimsizes(ii)
        ysum1 = sum(ydata_work(j,ii))
        yavg1 = avg(ydata_work(j,ii))
        ystd1 = stddev(ydata_work(j,ii))
        ypdf1 = tofloat(ynum1)/tofloat(dimy)

        ydata_binx(j,0,i) = ynum1
        ydata_binx(j,1,i) = ysum1
        ydata_binx(j,2,i) = yavg1
        ydata_binx(j,3,i) = ystd1
        ydata_binx(j,4,i) = ypdf1
      end do
    end if
    delete(ii)
  end do

  return(ydata_binx)

end

;###################################################
undef("sort_for_xyplot")
function sort_for_xyplot(xdata[*]:numeric, ydata[*]:numeric, zdata[*]:numeric, nbin:integer)
begin

;-----------------
; check dimension
;-----------------
  dimx = dimsizes(xdata)
  dimy = dimsizes(ydata)
  dimz = dimsizes(zdata)
  if ( dimx .eq. dimy .and. dimy .eq. dimz ) then
    npts = dimx
  else
    print("")
    print("ERROR: (sort_for_xyplot) input dimensions are not the same! ("+dimx+" , "+dimy+" , "+dimz+")")
    print("")
    return(0)
  end if

;---------------------------------------------------
; sort zdata and create a array for XY plot
;   ref: scatter_4.ncl, http://www.ncl.ucar.edu/Applications/scatter.shtml
;---------------------------------------------------

;---Create roughly nbin equally spaced levels through the data.
  mnmxint = nice_mnmxintvl(min(zdata),max(zdata),nbin,True)
  nlevels = toint(((mnmxint(1)-mnmxint(0))/mnmxint(2))) + 1
  levels  = fspan(mnmxint(0),mnmxint(1),nlevels)
  layers  = levels(0:nlevels-2)
  do i=0,nlevels-2
    layers(i) = levels(i) + mnmxint(2)/2.
  end do

  ;print("min/max data = " + min(zdata) + "/" + max(zdata))
  ;print("# of groups  = " + (nlevels-1))
  ;print("levels start at = " + levels(0) + ", end at " + levels(nlevels-1) + \
  ;      ", with a spacing of " + mnmxint(2))
  ;print("")
  ;print("x= "+xdata+" , y= "+ydata+" , z= "+zdata)

;---Create new 2D array to hold groupings of values
  xplotvar = new((/nlevels-1,npts/),typeof(xdata))
  xplotvar = -999.99
  xplotvar@_FillValue = -999.99

  yplotvar = xplotvar

;---Group the values and put in 2D array.
  labels = new(nlevels-1,string)
  do i=0,nlevels-2
    ii = ind(zdata.ge.levels(i).and.zdata.lt.levels(i+1))
    if (.not.all(ismissing(ii))) then
      xplotvar(i,ii) = (/xdata(ii)/)
      yplotvar(i,ii) = (/ydata(ii)/)
    end if

    labels(i) = levels(i) + ":" + levels(i+1)
    delete(ii)
  end do

;---Set return variable
  plotvars = (/xplotvar,yplotvar/)
  plotvars!0 = "index"
  plotvars!1 = "nbin"
  if (.not.ismissing(getVarDimNames(xdata))) then
    plotvars!2 = getVarDimNames(xdata)
  else
    plotvars!2 = "size"
  end if
  plotvars@nbin_labels = labels
  plotvars@nbin_mid  = layers
  plotvars@nbin_edge = levels
  plotvars@nbin_mid  = layers
  plotvars@index = "0: x_plot_var, 1: y_plot_var"

  return(plotvars)

end

;###################################################
undef("unit_convert")
function unit_convert(var_from:numeric, unit_from:string, unit_to:string)
begin

;--------------
; check input
;--------------
  ;if units is an attribute in var_from, directly use it
  if ( unit_from .eq. "" .and. isatt(var_from,"units")) then
    unit_from = var_from@units
  end if

  if ( isatt(var_from,"units") .and. var_from@units .ne. unit_from) then
    print("")
    print("WARNING: (unit_convert) input unit ["+unit_from+"] differs from the unit attribute ["+var_from@units+"]")
    print("NOT DOING unit conversion")
    print("")
    return(var_from)
  end if

  unit_conversion = unit_from+"_TO_"+unit_to

;-------------------------
; set the return variable 
;-------------------------
  var_to = var_from
  var_to@units = unit_to
  
  flag_conversion = False
;---------------------------
; unit conversion variables 
;---------------------------
  m2mm = 1000.		; meter to milimeter
  m2cm = 100.		; meter to centimenter
  m2km = 0.001		; meter to kilometer
  mm2m = 1./m2mm	; milimeter  to meter
  cm2m = 1./m2cm	; centimeter to meter
  km2m = 1./m2km	; kilometer  to meter

  sec2day = 1./86400.	; seconds to days
  sec2hr  = 1./3600.	; seconds to hours
  hr2day  = 1./24.	; hours to days 
  day2sec = 1./sec2day	; days to seconds
  day2hr  = 1./hr2day	; hours to days
  hr2sec  = 1./sec2hr	; hours to seconds

  g2kg  = 0.001		; gram to kilogram
  kg2g  = 1./g2kg	; kilogram to gram
  mg2kg = 10.^-6	; milligram to kilogram
  kg2mg = 1./mg2kg	; kilogram to milligram

  fraction2percent = 100. ; fraction (0-1) to percent (%)

  hpa2pa  = 100.	; hPa to Pa
  pa2hpa  = 1./hpa2pa   ; Pa to hPa

  ms2knots = 1.94384    ; m/s to knots
  knots2ms = 1./ms2knots; knots to m/s

  rho_water = 1000.  ; water density, 1000 kg/m3
  latentheat_evaporation  =  2.5 * 10.^6	      ; latent heat of vaporization for water, J/kg  (http://glossary.ametsoc.org/wiki/Latent_heat)
  latentheat_condensation = 1./latentheat_evaporation ; latent heat of condensation for water, J/kg

;-----------------
; unit conversion
;-----------------
;
; *** ***
;  if (unit_conversion .eq. "") then
;    var_to = var_to
;  end if

; *** fractin ***
  if (unit_conversion .eq. "fraction_TO_percent") then
    var_to = var_to * fraction2percent
    flag_conversion = True
  end if

  ;*** longitude unit conversion ***
  if (unit_conversion .eq. "degrees_west_TO_degrees_east") then
    var_to = 360. - var_to
    flag_conversion = True
  end if
  
  ;*** length ***
  if (unit_conversion .eq. "cm_TO_m") then
    var_to = var_to * cm2m
    flag_conversion = True
  end if

  if (unit_conversion .eq. "km_TO_m") then
    var_to = var_to * km2m
    flag_conversion = True
  end if

  ;*** mixing ratio ***
  if (unit_conversion .eq. "g/kg_TO_kg/kg") then
    var_to = var_to * g2kg
    flag_conversion = True
  end if

  if (unit_conversion .eq. "kg/kg_TO_g/kg") then
    var_to = var_to * kg2g
    flag_conversion = True
  end if

  if (unit_conversion .eq. "kg/kg_TO_mg/kg") then
    var_to = var_to * kg2mg
    flag_conversion = True
  end if
  
  ;*** Pressure ***
  if (unit_conversion .eq. "mb_TO_Pa") then
    var_to = var_to * hpa2pa
    flag_conversion = True
  end if

  ;*** precipitation ***
  if (unit_conversion .eq. "m/s_TO_mm/day") then
    var_to = var_to * m2mm * day2sec
    flag_conversion = True
  end if

  if (unit_conversion .eq. "mm/day_TO_m/s") then
    var_to = var_to * mm2m * sec2day
    flag_conversion = True
  end if

  if (unit_conversion .eq. "mm/hour_TO_m/s") then
    var_to = var_to * mm2m * sec2hr
    flag_conversion = True
  end if

  if (unit_conversion .eq. "W/m2_TO_mm/day") then
    var_to = var_to * latentheat_condensation / rho_water * m2mm * day2sec
    flag_conversion = True
  end if
  
  ;*** tendency - heating rate ***
  if (unit_conversion .eq. "K/hour_TO_K/s") then
    var_to = var_to * sec2hr
    flag_conversion = True
  end if

  ;*** tendency - mass mixing ratio ***
  if (unit_conversion .eq. "g/kg/hour_TO_kg/kg/s") then
    var_to = var_to * g2kg * sec2hr
    flag_conversion = True
  end if

  ;*** temperature ***
  if (unit_conversion .eq. "C_TO_K") then
    var_to = var_to + 273.15
    flag_conversion = True
  end if

; *** wind velocity *** 
  if (unit_conversion .eq. "m/s_TO_knots") then
    var_to = var_to * ms2knots
    flag_conversion = True
  end if

; *** vertical velocity *** 
  if (unit_conversion .eq. "mb/hour_TO_Pa/s") then
    var_to = var_to * hpa2pa * sec2hr
    flag_conversion = True
  end if

  if (unit_conversion .eq. "Pa/s_TO_hPa/day") then
    var_to = var_to * pa2hpa / sec2day
    flag_conversion = True
  end if

; *** water path *** 
  if (unit_conversion .eq. "kg/m2_TO_g/m2") then
    var_to = var_to * kg2g 
    flag_conversion = True
  end if

  if (unit_conversion .eq. "kg/m2_TO_mm") then
    var_to = var_to / rho_water * m2mm
    flag_conversion = True
  end if

;-------------------------------------------
; check whether executing unit conversion
;-------------------------------------------
  if (.not.flag_conversion) then
    var_to = var_from
    print("")
    print("WARNING: (unit_convert) unit conversion ["+unit_conversion+"] is unavilable")
    print("NOT DOING unit conversion")
    print("")
  end if

  return(var_to)
  
  ;printVarSummary(var_from)
  ;printVarSummary(var_to)
  ;print(var_from)
  ;print(var_to)

end

;###################################################
undef("ignorant_test")
function ignorant_test(a:numeric, b:numeric)

;----------------
; test function
;----------------

begin

  c = a + b
  return(c)

  c = a*b
  return(c)

end

;###################################################
undef("taylor_diagram")
function taylor_diagram (wks:graphic ,RATIO[*][*]:numeric, CC[*][*]:numeric \
                                     ,rOpts:logical)
;--------------------------------------------------------------------
; This version of taylor_diagram supports "paneling"
; It requires NCL version 4.2.0.a034 because it uses "gsn_create_legend"
;--------------------------------------------------------------------

;
; Generate a Taylor Diagram:
; Generate Multiple Aspects of Model Performance in a Single Diagram
; Taylor, K. E., J. Geophys. Res., 106, D7, 7183-7192, 2001
;
; An example:
; http://www.grida.no/climate/ipcc_tar/wg1/fig8-4.htm
;
; This expects one or more datasets. The left dimension 
; is the number of datasets. The rightmost is the number of pts.
;
; Markers are at: 
; http://www.ncl.ucar.edu/Document/Graphics/Resources/gs.shtml
;
; By default, the function can handle up to 10 variable comparisons..
; To expand ...  modify the 'Colors' and 'Markers' attributes.
; The user can change / add some default settings.
;
; The defaults that the user can modify:
;
; rOpts                 = True 
;                                  ; 'made-up' resources
; rOpts@Colors          =  (/ "blue" , "red", "green", "cyan", "black" \
;                           , "turquoise", "brown", "yellow"/)
; rOpts@Markers         =  (/ 2, 3, 6, 14, 9, 12, 7, 4/) ; Marker Indices
; rOpts@markerTxOffset  = 0.0175   ; offset for text above marker
; rOpts@stnRad          = (/ 1. /) ;  (/ 0.50, 0.75, 1.5 /) 
; rOpts@centerDiffRMS   = False    ;  True mean draw additional radii from REF 
; rOpts@caseLabelsFontHeightF = 0.05
; rOpts@varLabelsFontHeightF  = 0.013
; rOpts@varLabelsYloc         = 0.65
; rOpts@legendWidth           = 0.015
; rOpts@legendHeight          = 0.030*nCase
; rOpts@taylorDraw            = True
; rOpts@taylorFrame           = True
;
;                                  ; standard NCL resources
; rOpts@tiMainString    = "Taylor" ; not using title makes plot bigger
; rOpts@gsMarkerSizeF   = 0.0085   ; marker size   
; rOpts@gsMarkerThicknessF = 1.0
; rOpts@txFontHeightF   = 0.0125   ; text size 
; rOpts@tiMainFontHeightF = 0.0225 ; tiMainString size
;
; It returns to the user a graphic object containing the 
; Taylor background and plotted x/y pts.
; This graphic object contains a simple Taylor background appropriate
; for standardized data and the markers for the datasets.
; ==================================================================
; This version allows paneling:
;      The 'cumbersome' "dum" variables were added by 
;      Adam Phillips to allow paneling via "gsn_add_?".
; ==================================================================
begin
  dimR                  = dimsizes(RATIO)
  nCase                 = dimR(0)    ; # of cases [models] 
  nVar                  = dimR(1)    ; # of variables

                                     ; x/y coordinates for plotting
  X    = new ( (/nCase,nVar/) , typeof(RATIO) )
  Y    = new ( (/nCase,nVar/) , typeof(RATIO) )

  do nc=0,nCase-1
     angle      = acos( CC(nc,:) )   ; array operation                                    
     X(nc,:)    = RATIO(nc,:)*cos( angle )     
     Y(nc,:)    = RATIO(nc,:)*sin( angle )    
  end do

  xyMin                 = 0.  
  xyOne                 = 1.00
  xyMax                 = 1.65
  xyMax_Panel           = xyMax+ 0.10            ; paneling purposes
 
  if (rOpts .and. isatt(rOpts,"txFontHeightF"))  then 
      FontHeightF       = rOpts@txFontHeightF    ; user wants to specify size
  else
      FontHeightF       = 0.0175
  end if
 
; ----------------------------------------------------------------
; Part 1:
; base plot: Based upon request of Mark Stevens
; basic x-y and draw the 1.0 observed and the outer curve at 1.65
; ----------------------------------------------------------------
  
  rxy                   = True       
  rxy@gsnDraw           = False
  rxy@gsnFrame          = False
  rxy@vpHeightF         = 0.65
  rxy@vpWidthF          = 0.65
  rxy@tmYLBorderOn      = False
  rxy@tmXBBorderOn      = False

  rxy@tiYAxisString     = "Standardized Deviations (Normalized)"
  rxy@tiYAxisFontHeightF= FontHeightF                        ; default=0.025 
  
  rxy@tmXBMode          = "Explicit" 
  rxy@tmXBValues        = (/0.0,0.25,0.50,0.75,1.00,1.25,1.5/)    ; major tm
                                                                  ; default  "OBS" or "REF"
 ;rxy@tmXBLabels        = (/"0.00","0.25","0.50","0.75","REF" ,"1.25","1.50"/)
  rxy@tmXBLabels        = (/"    ","0.25","0.50","0.75","REF" ,"1.25","1.50"/)
  if (rOpts .and. isatt(rOpts,"OneX") )  then                     ; eg: rOpts@OneX="1.00" 
     ;rxy@tmXBLabels        = (/"0.00","0.25","0.50","0.75",rOpts@OneX,"1.25","1.50"/)
      rxy@tmXBLabels        = (/"    ","0.25","0.50","0.75",rOpts@OneX,"1.25","1.50"/)
  end if

  rxy@tmXBMajorLengthF  = 0.015      ; default=0.02 for a vpHeightF=0.6
  rxy@tmXBLabelFontHeightF = FontHeightF
  rxy@tmXBMinorOn       = False
  rxy@trXMaxF           = xyMax_Panel

  rxy@tmYLMode          = "Manual"
  rxy@tmYLMinorOn       = False
  rxy@tmYLMajorLengthF  = rxy@tmXBMajorLengthF
  rxy@tmYLLabelFontHeightF = FontHeightF
  rxy@tmYLMode          = "Explicit" 
  rxy@tmYLValues        = (/0.0, .25,0.50, 0.75, 1.00, 1.25, 1.5/) ; major tm
  rxy@tmYLLabels        = (/"0.00","0.25","0.50","0.75","1.00","1.25","1.50"/)
 ;rxy@tmYLLabels        = (/"    ","0.25","0.50","0.75","1.00","1.25","1.50"/)
  rxy@trYMaxF           = xyMax_Panel

  rxy@tmYRBorderOn      = False
  rxy@tmYROn            = False      ; Turn off right tick marks.

  rxy@tmXTBorderOn      = False
  rxy@tmXTOn            = False      ; Turn off right tick marks.

  rxy@xyDashPatterns    = (/ 0 /)    ; line characteristics (dash,solid)
  rxy@xyLineThicknesses = (/ 2./)    ; choose line thickness

  rxy@gsnFrame          = False      ; Don't advance the frame.

                                            ; create outer 'correlation axis'
  npts    = 200                        ; arbitrary
  xx      = fspan(xyMin,xyMax,npts) 
  yy      = sqrt(xyMax^2 - xx^2    )   ; outer correlation line (xyMax)

  sLabels = (/"0.0","0.1","0.2","0.3","0.4","0.5","0.6" \ ; correlation labels
             ,"0.7","0.8","0.9","0.95","0.99","1.0"     /); also, major tm
  cLabels = stringtofloat(sLabels)
  rad     = 4.*atan(1.0)/180.
  angC    = acos(cLabels)/rad                     ; angles: correlation labels
                                                                       
  if (rOpts .and. isatt(rOpts,"tiMainString")) then
      rxy@tiMainString      = rOpts@tiMainString
     ;rxy@tiMainOffsetYF    = 0.015               ; default  0.0
      if (isatt(rOpts,"tiMainFontHeightF")) then
           rxy@tiMainFontHeightF = rOpts@tiMainFontHeightF
      else
           rxy@tiMainFontHeightF = 0.0225         ; default  0.025              
      end if
  end if
;;if (rOpts .and. isatt(rOpts,"gsnCenterString")) then
;;    rxy@gsnCenterString  = rOpts@gsnCenterString      ; only gsn_csm_xy
;;end if

  taylor  = gsn_xy(wks,xx,yy,rxy)                 ; Create and draw XY plot.

  rsrRes  = True
  rsrRes@gsLineThicknessF  = rxy@xyLineThicknesses(0)  ; line thickness
  rsrRes@gsLineDashPattern = 0                    ; solid line pattern
                                                  ; draw x and y to xyMax
  dum0 = gsn_add_polyline(wks,taylor,(/0.,  0. /),(/0.,xyMax/), rsrRes)
  dum1 = gsn_add_polyline(wks,taylor,(/0.,xyMax/),(/0.,  0. /), rsrRes)

  xx   = fspan(xyMin, xyOne ,npts)                ; draw 1.0 standard radius
  yy   = sqrt(xyOne - xx^2)   
  rsrRes@gsLineDashPattern = 1                    ; dashed line pattern
  rsrRes@gsLineThicknessF  = rxy@xyLineThicknesses(0)  ; line thickness
  dum2 = gsn_add_polyline(wks,taylor,xx,yy, rsrRes)
  delete(xx)
  delete(yy)
                                                  
  if (rOpts .and. isatt(rOpts,"stnRad") ) then
      rsrRes@gsLineThicknessF  = 1   ; rxy@xyLineThicknesses(0)  
      nStnRad = dimsizes(rOpts@stnRad)

      dum3  = new(nStnRad,graphic)
      do n=0,nStnRad-1
         rr = rOpts@stnRad(n)
         xx = fspan(xyMin, rr ,npts) 
         yy = sqrt(rr^2   - xx^2)   
         dum3(n) = gsn_add_polyline(wks,taylor,xx,yy, rsrRes)
      end do
      taylor@$unique_string("dum")$ = dum3

      delete(xx)
      delete(yy)
  end if

  getvalues taylor                                ; get style info from taylor
    "tmYLLabelFont"        : tmYLLabelFont        ; use for correlation axis
    "tmYLLabelFontHeightF" : tmYLLabelFontHeightF
  end getvalues

; ----------------------------------------------------------------
; Part 2:
; Correlation labels
; ----------------------------------------------------------------
  radC    = xyMax                                  ; for correlation labels
  xC      = radC*cos(angC*rad)
  yC      = radC*sin(angC*rad)
; added to get some separation
  xC      = xC + 0.020*cos(rad*angC)
  yC      = yC + 0.060*sin(rad*angC)

  txRes               = True                      ; text mods desired
  txRes@txFontHeightF = FontHeightF               ; match YL 
  txRes@tmYLLabelFont = tmYLLabelFont             ; match YL
  txRes@txAngleF      = -45.
  if (.not.isatt(rOpts,"drawCorLabel") .or. rOpts@drawCorLabel) then 
      dum4 = gsn_add_text(wks,taylor,"Correlation",1.30,1.30,txRes)
	 taylor@$unique_string("dum")$ = dum4
  end if
  txRes@txAngleF      = 0.0 
  txRes@txFontHeightF = FontHeightF*0.50          ; bit smaller

;;dum0 = gsn_add_text(wks,taylor,"OBSERVED",1.00,0.075,txRes)

  plRes               = True
  plRes@gsLineThicknessF = 2.
  
  txRes@txJust        = "CenterLeft"              ; Default="CenterCenter".
  txRes@txFontHeightF = FontHeightF               ; match YL 
 ;txRes@txBackgroundFillColor = "white"

  tmEnd = 0.975
  radTM = xyMax*tmEnd                             ; radius end: major TM 
  xTM   = new( 2 , "float")
  yTM   = new( 2 , "float")

  dum5 = new(dimsizes(sLabels),graphic)
  dum6 = dum5

  do i=0,dimsizes(sLabels)-1                      ; Loop to draw strings
    txRes@txAngleF = angC(i)
    dum5(i) = gsn_add_text(wks, taylor, sLabels(i),xC(i),yC(i),txRes) ; cor label
    xTM(0)   = xyMax*cos(angC(i)*rad)             ; major tickmarks at
    yTM(0)   = xyMax*sin(angC(i)*rad)             ; correlation labels
    xTM(1)   = radTM*cos(angC(i)*rad)             
    yTM(1)   = radTM*sin(angC(i)*rad)
    dum6(i) = gsn_add_polyline(wks,taylor,xTM,yTM,plRes)
  end do
                                                  ; minor tm locations
  mTM     = (/0.05,0.15,0.25,0.35,0.45,0.55,0.65 \ 
             ,0.75,0.85,0.91,0.92,0.93,0.94,0.96,0.97,0.98  /)
  angmTM  = acos(mTM)/rad                         ; angles: correlation labels
  radmTM  = xyMax*(1.-(1.-tmEnd)*0.5)             ; radius end: minor TM 

  dum7 = new(dimsizes(mTM),graphic)

  do i=0,dimsizes(mTM)-1                          ; manually add tm
    xTM(0)   = xyMax*cos(angmTM(i)*rad)           ; minor tickmarks
    yTM(0)   = xyMax*sin(angmTM(i)*rad)
    xTM(1)   = radmTM*cos(angmTM(i)*rad)          
    yTM(1)   = radmTM*sin(angmTM(i)*rad)
    dum7(i)  = gsn_add_polyline(wks,taylor,xTM,yTM,plRes)
  end do
                                                  ; added for Wanli
  if (rOpts .and. isatt(rOpts,"ccRays") ) then
      angRL = acos(rOpts@ccRays)/rad             ; angles: radial lines

      rlRes = True
      rlRes@gsLineDashPattern= 2  ; line pattern
      rlRes@gsLineThicknessF = 1  ; choose line thickness
      if (isatt(rOpts,"ccRays_color")) then
          rlRes@gsLineColor    =  "LightGray"
      end if

      dum8 = new(dimsizes(angRL),graphic)
      do i=0,dimsizes(angRL)-1
         xRL     = xyMax*cos(angRL(i)*rad)
         yRL     = xyMax*sin(angRL(i)*rad)
         dum8(i) = gsn_add_polyline(wks,taylor,(/0, xRL /),(/0,  yRL  /),rlRes)
      end do
      taylor@$unique_string("dum")$ = dum8
  end if
  
; ----------------------------------------------------------------
; Part 3:
; Concentric about 1.0 on XB axis
; ----------------------------------------------------------------
  if (rOpts .and. isatt(rOpts,"centerDiffRMS") \
            .and. rOpts@centerDiffRMS) then
      respl                    = True                ; polyline mods desired
      respl@xyLineThicknessF   = 1.0                 ; line thickness
      respl@xyLineDashPattern  = 2                   ; short dash lines
      respl@gsLineColor        = "Black"             ; line color     
      if (isatt(rOpts,"centerDiffRMS_color")) then
          respl@gsLineColor    =  "LightGray"
      end if
      
      dx   = 0.25
      ncon = 4                                       ; 0.75, 0.50, 0.25, 0.0
      npts = 100                                     ; arbitrary
      ang  = fspan(180,360,npts)*rad

      dum9 = new(ncon,graphic)

      do n=1,ncon 
         rr  = n*dx            ; radius from 1.0 [OBS] abscissa
         xx  = 1. + rr*cos(ang)
         yy  = fabs( rr*sin(ang) )
         if (n.le.2) then
             dum9(n-1) = gsn_add_polyline(wks,taylor,xx,yy,respl)
         end if
         if (n.eq.3) then
             n3 = floattointeger( 0.77*npts ) 
             dum9(n-1) = gsn_add_polyline(wks,taylor,xx(0:n3),yy(0:n3),respl)
         end if
         if (n.eq.4) then
             n4 = floattointeger( 0.61*npts ) 
             dum9(n-1) = gsn_add_polyline(wks,taylor,xx(0:n4),yy(0:n4),respl)
         end if
      end do
      delete(ang)
      delete(xx)
      delete(yy)
      taylor@$unique_string("dum")$ = dum9

  end if
; ---------------------------------------------------------------
; Part 4:
; generic resources that will be applied to all users data points
; of course, these can be changed 
; http://www.ncl.ucar.edu/Document/Graphics/Resources/gs.shtml
; ---------------------------------------------------------------
  if (rOpts .and. isatt(rOpts,"Markers")) then
      Markers = rOpts@Markers
  else
      Markers = (/ 4, 6, 8,  0, 9, 12, 7, 2, 11, 16/) ; Marker Indices
  end if

  if (rOpts .and. isatt(rOpts,"Colors")) then
      Colors  = rOpts@Colors
  else
      Colors  = (/ "red", "blue", "green", "cyan", "orange" \
                 , "turquoise", "brown", "yellow", "purple", "black"/)
  end if

  if (rOpts .and. isatt(rOpts,"gsMarkerThicknessF")) then
      gsMarkerThicknessF = rOpts@gsMarkerThicknessF
  else
      gsMarkerThicknessF = 1.0
  end if

  if (rOpts .and. isatt(rOpts,"gsMarkerSizeF")) then
      gsMarkerSizeF      = rOpts@gsMarkerSizeF
  else
      gsMarkerSizeF      = 0.0085                  ; Default: 0.007
  end if

  gsRes = True
  gsRes@gsMarkerThicknessF = gsMarkerThicknessF      ; default=1.0
  gsRes@gsMarkerSizeF      = gsMarkerSizeF           ; Default: 0.007 

  ptRes = True                        ; text options for points
  ptRes@txJust             = "BottomCenter"; Default="CenterCenter".
  ptRes@txFontThicknessF   = 1.2      ; default=1.00
  ptRes@txFontHeightF      = 0.0125   ; default=0.05
  if (rOpts .and. isatt(rOpts,"txFontHeightF")) then
      ptRes@txFontHeightF  = rOpts@txFontHeightF  
  end if

  markerTxYOffset          = 0.0175   ; default
  if (rOpts .and. isatt(rOpts,"markerTxYOffset")) then
      markerTxYOffset = rOpts@markerTxYOffset             ; user defined offset
  end if

  dum10 = new((nCase*nVar),graphic)
  dum11 = dum10

  do n=0,nCase-1
     gsRes@gsMarkerIndex   = Markers(n)             ; marker style (+)
     gsRes@gsMarkerColor   = Colors(n)              ; marker color
     ptRes@txFontColor     = gsRes@gsMarkerColor
    do i=0,nVar-1
       dum10(n*nVar+i) = gsn_add_polymarker(wks,taylor,X(n,i),Y(n,i),gsRes) 
       dum11(n*nVar+i) = gsn_add_text(wks,taylor,(i+1),X(n,i),Y(n,i)+markerTxYOffset,ptRes)
    end do
  end do

; ---------------------------------------------------------------
; Part 5: ; add case legend and variable labels
; ---------------------------------------------------------------

  if (rOpts .and. isatt(rOpts,"caseLabels")) then 

      if (isatt(rOpts,"caseLabelsFontHeightF")) then
          caseLabelsFontHeightF = rOpts@caseLabelsFontHeightF
      else
          caseLabelsFontHeightF = 0.05  
      end if

      lgres                    = True
      lgres@lgMarkerColors     = Colors        ; colors of markers
      lgres@lgMarkerIndexes    = Markers       ; Markers 
      lgres@lgMarkerSizeF      = gsMarkerSizeF ; Marker size
      lgres@lgItemType         = "Markers"     ; draw markers only
      lgres@lgLabelFontHeightF = caseLabelsFontHeightF  ; font height of legend case labels

      if (isatt(rOpts,"legendWidth")) then
          lgres@vpWidthF       = rOpts@legendWidth
      else
          lgres@vpWidthF       = 0.15           ; width of legend (NDC)
      end if

      if (isatt(rOpts,"legendHeight")) then
          lgres@vpHeightF      = rOpts@legendHeight
      else   
          lgres@vpHeightF      = 0.030*nCase   ; height of legend (NDC)
      end if

      lgres@lgPerimOn          = False         ; turn off perimeter
      nModel                   = dimsizes( rOpts@caseLabels )
      lbid = gsn_create_legend(wks,nModel,rOpts@caseLabels,lgres)
	 
      amres = True
      amres@amParallelPosF     =  0.35           
      amres@amOrthogonalPosF   = -0.35             
      annoid1 = gsn_add_annotation(taylor,lbid,amres)	; add legend to plot
  end if

  if (rOpts .and. isatt(rOpts,"varLabels")) then 
      nVar    = dimsizes(rOpts@varLabels)

      if (isatt(rOpts,"varLabelsFontHeightF")) then
          varLabelsFontHeightF = rOpts@varLabelsFontHeightF
      else
          varLabelsFontHeightF = 0.013
      end if

      txres = True
      txres@txFontHeightF = varLabelsFontHeightF
      txres@txJust = "CenterLeft"              ; justify to the center left

     ;delta_y = 0.02       
     delta_y = 0.06   
      if (rOpts .and. isatt(rOpts,"varLabelsYloc")) then
          ys  = rOpts@varLabelsYloc            ; user specified
      else
          ys  = max( (/nVar*delta_y , 0.30/) )
      end if

      
      do i = 1,nVar     
         if (i.eq.1) then
             dum12 = new(nVar,graphic)
	 end if

         dum12(i-1) = gsn_add_text(wks,taylor,i+" - "+rOpts@varLabels(i-1), .125,ys,txres)
         ys = ys- delta_y
      end do

      taylor@$unique_string("dum")$ = dum12
  end if

  taylor@$unique_string("dum")$ = dum0   ; x-axis
  taylor@$unique_string("dum")$ = dum1   ; y-axis
  taylor@$unique_string("dum")$ = dum2   ; 1.0 std curve
  taylor@$unique_string("dum")$ = dum5   ; labels [COR]
  taylor@$unique_string("dum")$ = dum6   ; major tm [COR]
  taylor@$unique_string("dum")$ = dum7   ; minor tm
  taylor@$unique_string("dum")$ = dum10  ; markers
  taylor@$unique_string("dum")$ = dum11  ; text
  
  if (.not.isatt(rOpts,"taylorDraw") .or. \
     (isatt(rOpts,"taylorDraw") .and. rOpts@taylorDraw)) then 
	draw(taylor)
  end if
  if (.not.isatt(rOpts,"taylorFrame") .or. \
     (isatt(rOpts,"taylorFrame") .and. rOpts@taylorFrame)) then 
	frame(wks)
  end if
  return(taylor)
end

;###################################################
undef("binmap")
function binmap(xx:numeric,yy:numeric,xx_min[1]:numeric,xx_max[1]:numeric,xx_int[1]:numeric)

begin 

;--------------
; check part
;--------------

; check dimension sizes
  xx_dim = dimsizes(xx)
  yy_dim = dimsizes(yy)

  dd = dimsizes(xx_dim)

  do i=0,dd-1
    if (xx_dim(i).ne.yy_dim(i)) then
      print("")
      print("ERROR: (func binmap) variable dimension index [" + i + "] does not match [" + xx_dim(i) + "," + yy_dim(i) + "]")
      print("function binmap stop")
      return(0)
    end if
  end do

; check types
  xx_type = typeof(xx)
  yy_type = typeof(yy)

  if (xx_type.ne.yy_type) then
    print("")
    print("ERROR: (func binmap) variable type [" + xx_type + "] does not match [" + yy_type + "]")
    print("function binmap stop")
    return(0)
  end if

; check min,max,interval
  if (xx_min.ge.xx_max) then
    print("")
    print("ERROR: (func binmap) MIN value [" + xx_min + "] is greater than MAX value [" + xx_max + "]")
    print("function binmap stop")
    return(0)
  else if (xx_int.gt.xx_max-xx_min) then
    print("")
    print("ERROR: (func binmap) INTERVAL value [" + xx_int + "] is greater than the value range [" + xx_min + "," + xx_max + "]")
    print("function binmap stop")
    return(0)
  end if
  end if

;----------------------
; calculate bin for xx
;----------------------

;*** merge multidiemsional input array into 1-D temp array ***
  length = 1
  do i=0,dd-1
    length = length * xx_dim(i)  ; new 1-D array length
  end do

  xtemp  = new(length, xx_type)
  ytemp  = new(length, yy_type)
  ytemp0 = new(length, yy_type)

  if (isatt(xx,"_FillValue")) then
    xtemp@_FillValue  = xx@_FillValue
    ytemp@_FillValue  = xx@_FillValue
    ytemp0@_FillValue = xx@_FillValue
  else
    xtemp@_FillValue  = -999.99
    ytemp@_FillValue  = -999.99
    ytemp0@_FillValue = -999.99
  end if

  n=0

  if (dd.eq.1) then
    do i=0,xx_dim(0)-1
      if (.not.ismissing(xx(i))) then
        xtemp(n) = xx(i)
        ytemp0(n) = yy(i)
        n=n+1
      end if
    end do
  else if (dd.eq.2) then
    do i=0,xx_dim(0)-1
    do j=0,xx_dim(1)-1
      if (.not.ismissing(xx(i,j)) .and. .not.ismissing(yy(i,j))) then
        xtemp(n) = xx(i,j)
        ytemp0(n) = yy(i,j)
        n=n+1
      end if
    end do
    end do

  else if (dd.eq.3) then
    do i=0,xx_dim(0)-1
    do j=0,xx_dim(1)-1
    do k=0,xx_dim(2)-1
      if (.not.ismissing(xx(i,j,k)) .and. .not.ismissing(yy(i,j,k))) then
        xtemp(n) = xx(i,j,k)
        ytemp0(n) = yy(i,j,k)
        n=n+1
      end if
    end do
    end do
    end do

  else if (dd.eq.4) then
    do i=0,xx_dim(0)-1
    do j=0,xx_dim(1)-1
    do k=0,xx_dim(2)-1
    do t=0,xx_dim(3)-1
      if (.not.ismissing(xx(i,j,k,t)) .and. .not.ismissing(yy(i,j,k,t))) then
        xtemp(n) = xx(i,j,k,t)
        ytemp0(n) = yy(i,j,k,t)
        n=n+1
      end if
    end do
    end do
    end do
    end do

  end if
  end if
  end if
  end if

;--------------
; bin input xx
;--------------

; output arrays
  num_z = toint((xx_max-xx_min)/xx_int)

  zz_bin_int  = new(num_z+1,xx_type)  ; bin value at interface

  zz_bin_mid   = new(num_z,xx_type)   ; bin value at midpoint
  if (isatt(xx,"_FillValue")) then
    zz_bin_mid@_FillValue = xx@_FillValue
  else
    zz_bin_mid@_FillValue = -999.99
  end if

  zz_mean = zz_bin_mid  ; bin average value
  zz_std  = zz_bin_mid  ; bin standard deviation

  zz_count = new(num_z,"integer")  ; bin number count
  zz_count = 0

; set bin values at interface
  zz_bin_int(0)       = xx_min
  zz_bin_int(num_z) = xx_max

  do i=1,num_z-1
    zz_bin_int(i) = zz_bin_int(i-1) + xx_int
  end do

; set bin midpoint value
  do i=0,num_z-1
    zz_bin_mid(i) = (zz_bin_int(i)+zz_bin_int(i+1))/2.
  end do

; sort x and get indexes
  kflg = 2
  idx = dim_pqsort(xtemp(0:n-1),kflg)  ; sort xx in increasing order and get indexes

; count number in each bin 
  do i=0,n-1
    if (.not.ismissing(xtemp(i)) .and. xtemp(i) .le. zz_bin_int(num_z)) then
      j = closest_val(xtemp(i),zz_bin_mid)
      if (j .ge. num_z) then
        j=num_z-1
      end if
      zz_count(j) = zz_count(j)+1
    end if
    print(i + "/" + n + " bin processing...")
  end do

; assign yy into each bin
  do i=0,n-1
    ytemp(i) = ytemp0(idx(i))
  end do

; compute mean and standrad deviation of the first bin
  i=0
  if (zz_count(i).eq.0) then
    zz_mean(i) = zz_bin_mid@_FillValue
    zz_std (i) = zz_bin_mid@_FillValue
  else
    i1=0
    i2=0+zz_count(i)-1
    zz_mean(i) = sum(ytemp(i1:i2))/zz_count(i)
    zz_std (i) = stddev(ytemp(i1:i2))
  end if

; compute mean and standard deviation at each bin
  do i=1,num_z-1

    ; if no value in the bin
    if (zz_count(i).eq.0) then
      zz_mean(i) = zz_bin_mid@_FillValue
      zz_std (i) = zz_bin_mid@_FillValue

    else
      i1 = sum(zz_count(0:i-1))
      i2 = i1 + zz_count(i) - 1
      zz_mean(i) = sum(ytemp(i1:i2))/zz_count(i)
      zz_std (i) = stddev(ytemp(i1:i2))
    end if

  end do

;----------------
; return arrays
;----------------

  zz_all = new((/3,num_z/),xx_type) 

;  zz_all(0,:) = zz_bin_mid(:)

  zz_all(0,:) = zz_count  (:)
  zz_all(1,:) = zz_mean   (:)
  zz_all(2,:) = zz_std    (:)

  ; coordinate variable
  zz_all!0 = "index"
  zz_all!1 = "bin"
  zz_all&bin = zz_bin_mid

  ; attributes 
  zz_all@lingname = "Sort input data to bin"
  zz_all@description = "Index 0: number in each bin; Index 1: Average value in each bin; Index 2: standard deviation in each bin"
  zz_all@bin_max      = xx_max
  zz_all@bin_min      = xx_min
  zz_all@bin_interval = xx_int
  zz_all@bin_total_number = sum(zz_count(:))
  zz_all@bin_interface = zz_bin_int
  zz_all@pdf = tofloat(zz_count)/tofloat(zz_all@bin_total_number)

  return(zz_all)
 
end

;###################################################
undef("lat_wgt_avg")
function lat_wgt_avg(data_in:numeric)

begin

  pi = acos(-1.)
  lat = data_in&lat  ; latitude must in -90 to 90

  coslat = abs(cos(lat/180.*pi))
  ; function wgt_areaaveq (q[(...),y,x]:numeric, wgty [*]:numeric, wgtx[*]:numeric, 
  ;                        opt:integer, 0: calculate only non-missing data; 1:return missing value if there is any missing data)
  data_out = wgt_areaave_Wrap(data_in,coslat,1.0,1)

  if (isatt(data_in,"long_name")) then
    data_out@long_name = data_in@long_name ;+ " (latitude-weighted area mean)"
  ;else
  ;  data_out@long_name = "latitude-weighted area mean variable"
  end if

  return(data_out)

end

;###################################################
undef("plus")
function plus(a:numeric, b:numeric)

begin

  a_plus_b = a
  a_plus_b = a + b
  return(a_plus_b)

end

;###################################################
undef("divide")
function divide(a:numeric, b:numeric)

begin

  a_divide_b = a
  a_divide_b = a / b
  return(a_divide_b)

end

;###################################################
undef("multiply")
function multiply(a:numeric, b[1]:numeric)

begin

  a_multi_b = a
  a_multi_b = a * b
  return(a_multi_b)

end

;###################################################
undef("diff")
function diff(a:numeric, b:numeric)

begin

  a_minus_b = a
  a_minus_b = a - b
  return(a_minus_b)

end

;###################################################
undef("gsn_csm_contour_map_avg")
function gsn_csm_contour_map_avg(wks[1]:graphic, data_input[*][*]:numeric, \
                                 nclres[1]:logical, opt[1]:logical)

begin

;-------------
; initialize
;-------------
mean_format = "%7.3f"
mean_prefix = "mean: "
LatWeightCheck = True
pi = acos(-1.)

;-------------------
; set mean format
;-------------------
if ( isatt(opt, "MeanFormat")) then
  mean_format = opt@MeanFormat
end if

if ( isatt(opt, "MeanPrefix")) then
  mean_prefix = opt@MeanPrefix
end if

if ( isatt(opt, "LatWeighted")) then 
  LatWeightCheck = opt@LatWeighted
;else
;  LatWeightCheck = LatWeightCheck_default
end if

;--------------------------------------
; check input data has right lat coordinate
;--------------------------------------

if ( LatWeightCheck ) then
  if ( iscoord(data_input, "lat") .and. data_input&lat@units .eq. "degrees_north" ) then
      LatWeightCheck = True
      num_lat = dimsizes(data_input&lat)
      lat_temp = data_input&lat

  else if ( iscoord(data_input, "latitude") .and. data_input&latitude@units .eq. "degrees_north" ) then
      LatWeightCheck = True
      num_lat = dimsizes(data_input&latitude)
      lat_temp = data_input&latitude

  else
    LatWeightCheck = False

    print("")
    print("WARNING: (gsn_csm_contour_map_avg) Invalid coordinate variable's setup. Its name MUST be [lat OR latitude] and units be [degrees_north]")
    print("")

  end if    
  end if  
end if

;--------------------------
; compute global average
;   1. DO     latitude correction
;   2. NOT DO latitude correction
;--------------------------

;
;*** DO latitude correction ***
;
if (LatWeightCheck) then

  coslat = abs(cos(lat_temp/180.*pi))
  ; function wgt_areaaveq (q[(...),y,x]:numeric, wgty [*]:numeric, wgtx[*]:numeric, 
  ;                        opt:integer, 0: calculate only non-missing data; 1:return missing value if there is any missing data)
  data_input_avg = wgt_areaave(data_input,coslat,1.0,1)
;
;*** NOT DO latitude correction ***
;
else

  data_input_avg = wgt_areaave(data_input,1.0,1.0,1)

end if

;------------------------
; set ncl res and plot
;------------------------
if ( isatt(nclres,"gsnCenterString") ) then
  wres = nclres

else
  wres = nclres
  wres@gsnCenterString = (mean_prefix + sprintf(mean_format,data_input_avg))

end if

plot = gsn_csm_contour_map(wks,data_input,wres)

;--------
; return 
;--------
return(plot)

end

;######################################################
undef("ignorant_combine_colormaps")
function ignorant_combine_colormaps( cmap1[*][3]:numeric, cmap2[*][3]:numeric, \
                                     choice:integer  )

begin

;---------------------------
; save input cmap dimension
;----------------------------
  cmap1_dim = dimsizes(cmap1(:,0))
  cmap2_dim = dimsizes(cmap2(:,0))

;-----------------------------
; determine output color map
;-----------------------------

;*** combine total cmap1 & cmap2 *** 

  if (choice .eq. 1) then

    ;---------------------
    ; create output cmap
    ;---------------------
      ocamp_dim = cmap1_dim + cmap2_dim
      ocmap = new((/ocamp_dim,3/),"float")

    ;---------------------------
    ; determine out cmap value
    ;---------------------------
      c1 = cmap1_dim-1  ; cmap1 stop index in ocmap
      c2 = ocamp_dim-1  ; cmap2 stop index in ocmap

      ocmap (0:c1,:)    = cmap1
      ocmap (c1+1:c2,:) = cmap2

;*** don't combine cmap2 foreground/background color ***

  else

    ;---------------------
    ; create output cmap
    ;---------------------
      ocamp_dim = cmap1_dim + cmap2_dim - 2
      ocmap = new((/ocamp_dim,3/),"float")

    ;---------------------------
    ; determine out cmap value
    ;---------------------------
      c1 = cmap1_dim-1  ; cmap1 stop index in ocmap
      c2 = ocamp_dim-1  ; cmap2 stop index in ocmap

      ocmap (0:c1,:)    = cmap1
      ocmap (c1+1:c2,:) = cmap2(2:,:)

  end if

;----------
; return
;----------
  return(ocmap)

end

;###############################################
undef("ignorant_xy")
function ignorant_xy(wks:graphic, xx:numeric, yy:numeric , \
                     labels:string, nclres:logical, opt:logical)

begin

;----------------
; set labels
;----------------
  if (isatt(opt,"legend_mean") .and. opt@legend_mean.eq.False) then
    labels_work = "  "+labels
  else
    yy_avg = dim_avg_Wrap(yy)

    if ( isatt(opt, "MeanFormat")) then
      mean_format = opt@MeanFormat
      labels_work = "  "+labels + ", mean: "+sprintf(mean_format,yy_avg)
    else
      labels_work = "  "+labels + ", mean: "+tostring(yy_avg)
    end if
  end if

;------------------------
; set default resources
;------------------------

  res_default = True
  res_default@gsnMaximize = True

;*******************
; set XY resources
;*******************

;*** set XY line mode ***
  res_default@xyMonoMarkLineMode = True
  res_default@xyMarkLineMode     = "Lines"

;*** set XY line color ***
  xy_colors1 = (/ "red", \               
                  "blue",  \
                  "orange", \
;                  "yellow",  \
                  "green", \
                  "purple",  \
                  "pink",  \
                  "brown",  \
                  "gray",  \
                  "purple",  \
                  "black"  \
                /)

  res_default@xyMonoLineColor    = False
  res_default@xyLineColors       = xy_colors1

;*** set XY line dash ***
  res_default@xyMonoDashPattern  = True
  res_default@xyDashPattern      = 0

;*** set XY line thickness ***
  res_default@xyMonoLineThickness = True
  res_default@xyLineThicknessF    = 2.5

;***********************
; set Legend resources
;***********************

;*** set Legend size
  res_default@pmLegendDisplayMode    = "Always"
  res_default@pmLegendWidthF      = 0.2
  res_default@pmLegendHeightF     = 0.1

;*** set Legend font ***
  res_default@lgAutoManage = False
  res_default@lgLabelFontHeightF = 0.02
  res_default@xyExplicitLegendLabels = labels_work

;*** set Legend perim
  res_default@lgPerimOn              = True

;**********************
; set Title resources
;**********************

;*** XY title default value ***
  x_title = " "
  y_title = " "

;*** set X-Axis title ***
  if ( isatt(xx,"long_name") ) then
    x_title = xx@long_name
  end if

  if ( isatt(xx,"units") ) then
    x_title = x_title + " (" + xx@units + ")"
  end if

;*** set Y-Axis title ***
  if ( isatt(yy,"long_name") ) then
    y_title = yy@long_name
  end if

  if ( isatt(yy,"units") ) then
    y_title = y_title + " (" + yy@units + ")"
  end if

;*** call title resources ***
  res_default@tiXAxisString = x_title
  res_default@tiYAxisString = y_title


;-------------------------------------------
; set work resource,
; replace default by input NCL resources
;-------------------------------------------

  res_work = res_default
  copyatt(res_work,nclres)

  plot = gsn_csm_xy(wks,xx,yy,res_work)

  return(plot)
  
end


;###################################################
undef("ignorant_merge_colormaps")
function ignorant_merge_colormaps(icmap)

begin

;------------------------------
; define work color map array
;------------------------------
  wcmap_size = 10000
  wcmap_val = new( (/wcmap_size,3/), "float" )
  wcmap_val = -999.99

;*********************************
; merge NCL predefined color map
;*********************************

    if ( isstring(icmap) ) then

      icamp_number = dimsizes(icmap)

      do j = 0, icamp_number-1

;--------------------------------
; open wks & get colormap value
;--------------------------------
        wks = gsn_open_wks("ncgm","owrsfnmks")
        gsn_define_colormap( wks,icmap(j) )

        getvalues wks
          "wkColorMap" : icmap_j_val
        end getvalues

        icmap_j_size = dimsizes(icmap_j_val(:,0))

;---------------------------------
; decide work colormap position
; to put read-in color map value
;---------------------------------
        wcmap_j_index_start = 0
        do k = 0, wcmap_size-1
          if ( wcmap_val(k,0) .ne. -999.99 ) then
            wcmap_j_index_start = k+1
          end if
        end do

        wcmap_j_index_end = wcmap_j_index_start + icmap_j_size - 1

        wstart = wcmap_j_index_start
        wend   = wcmap_j_index_end

        wcmap_val(wstart:wend ,:) = icmap_j_val

;----------------------
; delete used varbles
;----------------------
        delete(icmap_j_val)
        delete(icmap_j_size)
        delete(wcmap_j_index_start)
        delete(wcmap_j_index_end)
        delete(wstart)
        delete(wend)       

      end do

;----------------------------
; return new color map value
;----------------------------

; remove temporary wks
      system("rm -f owrsfnmks.ncgm*")

; find output colormap size
      do k = 0, wcmap_size-1
        if ( wcmap_val(k,0) .ne. -999.99 ) then
          ocmap_size = k+1
        end if
      end do

; determine output colormap value
      ocmap_val = new( (/ocmap_size,3/),"float" )

      do k = 0, ocmap_size-1
        ocmap_val(k,:) = wcmap_val(k,:)
      end do

      return(ocmap_val)

  end if

end



;#####################################################3
undef("ignorant_select_colormap")
function ignorant_select_colormap(icmap_name:string, ocmap_index[*]:numeric, \
                                  option_foreback:logical)

begin

;-------------------------------
; get input colormap RGB array
;-------------------------------

  icmap_val0 = read_colormap_file(icmap_name) ; not read fore/back ground colors

  ndim = dimsizes(icmap_val0(:,0))   ; add fore/background colors
  ndim2 = ndim+2
  ndim1 = ndim2-1
  icmap_val = new( (/ndim+2,3/),typeof(icmap_val0)) ; only keep RGB values

  icmap_val(0,:) = 1.
  icmap_val(1,:) = 0.
  icmap_val(2:ndim1,:) = icmap_val0(:,0:2)

; if user select all colors in given colormap
  if ( dimsizes(ocmap_index) .eq. 1 .and. ocmap_index .eq. 999 ) then

    if (option_foreback) then
      ocmap_val = icmap_val
    else
      ocmap_val = icmap_val(2:,:)
    end if

    return(ocmap_val)
  end if

  icmap_size = dimsizes(icmap_val)

;---------------------------
; write out output colormap
;---------------------------

; add fore and background colors (white & black)
  if (option_foreback) then

    ocmap_size = dimsizes(ocmap_index) + 2
    ocmap_val = new((/ocmap_size,3/),"float")

    ocmap_val(0,:) = (/1., 1., 1./) ; set foreground color to black
    ocmap_val(1,:) = (/0., 0., 0./) ; set background color to white

    do i=2,ocmap_size-1
      j = ocmap_index(i-2)
      ocmap_val(i,:) = icmap_val(j,0:2)
    end do

; not add fore and background colors
  else

    ocmap_size = dimsizes(ocmap_index)
    ocmap_val = new((/ocmap_size,3/),"float")

    do i=0,ocmap_size-1
      j = ocmap_index(i)
      ocmap_val(i,:) = icmap_val(j,0:2)
    end do

  end if

;-----------------------------------
; return output colormap RGB array
;----------------------------------
  return(ocmap_val)

end
;
;
;------------------------------
undef("ignorant_write_nc")
procedure ignorant_write_nc(filename:string,outvarname:string,invar,opt:integer)

begin

  outfile = addfile(filename,"w")

;*******************************************************
;  outfile have unlimited time, overwrite from max time
;*******************************************************
  if (opt.eq.-1) then

    outvar = outfile->$outvarname$
    dim_outvar = dimsizes(outvar)
    dimT_out = dim_outvar(0)
    nout = dimT_out

    outfile->$outvarname$(nout,:,:) = (/invar/)

  end if

;*********************************************************
;  outfile have unlimited time, overwrite from first time
;*********************************************************
  if (opt.eq.-2) then

    nout = 0
    outfile->$outvarname$(nout,:,:) = (/invar/)

  end if

;**************************************************************
;  outfile don't have [outvarname] varible, 
;  automatically create [outvarname] varible and dimensions
;**************************************************************
  if (opt.eq.-3) then

    outfile->$outvarname$ = (/invar/)

  end if

;*********************************************************
;  Don't write in unlimited time varible
;*********************************************************
  if (opt.ge.0) then

    outfile->$outvarname$(opt,:,:) = (/invar/)

  end if

 
end

;---------------------------
undef("ignorant_LWP")
function ignorant_LWP(qc[*][*][*],p[*][*][*])

begin

;*** know input data dimesion ***
;
  dimqc = dimsizes(qc)
  dimz = dimqc(0)
  dimy = dimqc(1)
  dimx = dimqc(2)
;
;*** calculate liquid water path ***
;
  qcmean = new((/dimz-1,dimy,dimx/),float)
  dp = new((/dimz-1,dimy,dimx/),float)

  LWP = new((/dimy,dimx/),float)
  LWP!0 = qc!1
  LWP!1 = qc!2
  LWP@description = "liquid water path"
  LWP@units = "kg m-2"

  do k=0,dimz-1-1
    qcmean(k,:,:) = (qc(k+1,:,:) + qc(k,:,:))/2.
     dp(k,:,:) = p(k,:,:) - p(k+1,:,:)
  end do


  do i=0,dimx-1
    do j=0,dimy-1

      LWP(j,i) =  prcwater_dp(qcmean(:,j,i),dp(:,j,i))

    end do
  end do

  return(LWP)

end

;--------------------------------
undef("ignorant_HMLcloud")
function ignorant_HMLcloud(P[*][*][*]:numeric,Tk[*][*][*]:numeric,qv[*][*][*]:numeric,qall[*][*][*]:numeric)

begin

  dimP = dimsizes(P)
  nz = dimP(0)
  ny = dimP(1)
  nx = dimP(2)

  HML = new((/3,ny,nx/),float)
  HML = -100.
  HML@description = "High,Middle,Low cloud amount"
  HML@HighCloud = "index 2, 300-550 mb"
  HML@MiddleCloud = "index 1, 550-800 mb"
  HML@LowCloud = "index 0, 800-1000 mb"

  Pqs = 6.11*exp(19.83-5417./Tk)  ; Compute saturation water vapor pressure in millibar
  qsat = (Pqs/P)*18./28.56        ; Change Pqs to saturation water vapor mixing ratio

  do j=0,ny-1
    do i=0,nx-1
      do k=0,nz-1

        if (P(k,j,i).gt.800..and.P(k,j,i).le.1000.) then       ; Calaulate High cloud amount
          ratio0 = qall(k,j,i)/(0.01*qsat(k,j,i))
          ratio0 = ratio0 < 1.

          if (ratio0.gt.HML(0,j,i)) then
            HML(0,j,i) = ratio0
          end if
        end if

        if (P(k,j,i).gt.550..and.P(k,j,i).le.800.) then        ; Calculate Middle cloud amount
          ratio1 = qall(k,j,i)/(0.01*qsat(k,j,i))
          ratio1 = ratio1 < 1.

          if (ratio1.gt.HML(1,j,i)) then
            HML(1,j,i) = ratio1
          end if
        end if

        if (P(k,j,i).gt.300..and.P(k,j,i).le.550.) then        ; Calculate Low cloud amount
          ratio2 = qall(k,j,i)/(0.01*qsat(k,j,i))
          ratio2 = ratio2 < 1.

          if (ratio2.gt.HML(2,j,i)) then
            HML(2,j,i) = ratio2
          end if
        end if

      end do
    end do
  end do

  return(HML)

end

;-------------------------------------------------
undef("ignorant_contour")
function ignorant_contour(wks[1]:graphic,data[*][*]:numeric,opt[1]:logical)

begin

  res = opt

  res@gsnMaximize = True
  
;***********************
;  Contour Attribute
;***********************

  cn_logical = (/"cnFillOn", "cnMonoFillPattern", "cnMonoFillColor", \
                 "cnInfoLabelOn","cnLineLabelsOn"/)

  cnatt_logical =(/True,True,False,False,True/)

  cndim = dimsizes(cn_logical)

  do i=0,cndim-1
    cc =isatt(opt,cn_logical(i))
    if (cc.eq.False) then
      res@$cn_logical(i)$ =  cnatt_logical(i)
    end if
  end do

;************************************************************
;  Label Bar Attribute  (only valid when cnFillOn = True)
;************************************************************

if (isatt(opt,"cnFillOn").eq.False.or.opt@cnFillOn.eq.True) then

   res@cnLineLabelsOn = False
   res@cnLinesOn = False

   lb_string = (/"pmLabelBarDisplayMode","pmLabelBarSide","lbOrientation"/)
   lbatt_string = (/"Always","Bottom","Horizontal"/)

   lb_num = (/"pmLabelBarWidthF","pmLabelBarHeightF"/)
   lbatt_num = (/0.8,0.1/)

   lb_logical = (/"lbPerimOn"/)
   lbatt_logical = (/False/)

   dimlbstr = dimsizes(lb_string)
   dimlbnum = dimsizes(lb_num)
   dimlblog = dimsizes(lb_logical)

  do i=0,dimlbstr-1
    cc =isatt(opt,lb_string(i))
    if (cc.eq.False) then
      res@$lb_string(i)$ =  lbatt_string(i)
    end if
  end do

  do i=0,dimlbnum-1
    cc =isatt(opt,lb_num(i))
    if (cc.eq.False) then
      res@$lb_num(i)$ =  lbatt_num(i)
    end if
  end do

  do i=0,dimlblog-1
    cc =isatt(opt,lb_logical(i))
    if (cc.eq.False) then
      res@$lb_logical(i)$ =  lbatt_logical(i)
    end if
  end do

end if


  plot = gsn_contour(wks,data,res)

  return(plot)

end
;-------------------------------------------------------

undef("ignorant_nc_contour")
function ignorant_nc_contour( wks[1]:graphic, nc_file:file, data[*][*]:numeric, opt:logical, nclres:logical )

begin

  res = nclres
  res@tfDoNDCOverlay = True

  if (isatt(res,"gsnMaximize").eq.False) then
    res@gsnMaximize = True
  end if

;***********************************************
;  Determine X,Y range , default is full range
;***********************************************

  pos = (/"XLeft","XRight","YBottom","YTop"/)
  dim = dimsizes(data)
  pos_val = (/0,dim(1)-1,0,dim(0)-1/)
  do i=0,3
    if (isatt(opt,pos(i)).eq.True) then
      pos_val(i) = opt@$pos(i)$
    end if
  end do

;********************************************************
;  Determine Map Corners Latitude and Longitude value
;********************************************************

    proj_att = (/"MAP_PROJ", "TRUELAT1", "TRUELAT2", "STAND_LON",  "REF_LON", \
                 "REF_LAT" ,   "KNOWNI",   "KNOWNJ",  "POLE_LON", "POLE_LAT", \
                       "DX",       "DY",   "LATINC",    "LONINC" /)

    dim_projatt = dimsizes(proj_att)
 
    projres = True

    do i=0,dim_projatt-1

      if ( isatt(opt,proj_att(i)).eq.True) then
        projres@$proj_att(i)$ = opt@$proj_att(i)$

      else

        if (isatt(nc_file,proj_att(i)).eq.True) then
          projres@$proj_att(i)$ = nc_file@$proj_att(i)$
        end if

      end if

    end do


    if (isatt(opt,"WRFfile").eq.True.and.opt@WRFfile.eq.True) then

       LeftLon  = nc_file->XLONG(0,pos_val(2),pos_val(0))
       LeftLat  = nc_file->XLAT(0,pos_val(2),pos_val(0))
       RightLon = nc_file->XLONG(0,pos_val(3),pos_val(1))
       RightLat = nc_file->XLAT(0,pos_val(3),pos_val(1))

    else

       llLeft  = wrf_ij_to_ll(pos_val(0),pos_val(2),projres)
       llRight = wrf_ij_to_ll(pos_val(1),pos_val(3),projres)

       LeftLon  = llLeft(0)
       LeftLat  = llLeft(1)
       RightLon = llRight(0)
       RightLat = llRight(1) 
  
    end if

;************************************
;  Map Attribute
;************************************

   if (projres@MAP_PROJ.eq.1) then

     res@mpProjection = "LambertConformal"
     res@mpLambertMeridianF = projres@STAND_LON
     res@mpLambertParallel1F = projres@TRUELAT1
     res@mpLambertParallel2F = projres@TRUELAT2

   end if

   res@mpLimitMode = "Corners"
   res@mpLeftCornerLonF =  LeftLon
   res@mpLeftCornerLatF =  LeftLat
   res@mpRightCornerLonF = RightLon
   res@mpRightCornerLatF = RightLat


   if (isatt(res,"mpPerimOn").eq.False) then
     res@mpPerimOn = True
   end if

   if (isatt(res,"mpGridAndLimbOn").eq.False) then
     res@mpGridAndLimbOn = False
   end if

   if (isatt(res,"mpDataBaseVersion").eq.False) then
     res@mpDataBaseVersion = "Ncarg4_1"
   end if

   if (isatt(res,"mpDataSetName").eq.False) then
     res@mpDataSetName = "Earth..4" 
   end if

;***********************
;  Contour Attribute
;***********************

  cn_logical = (/"cnFillOn", "cnMonoFillPattern", "cnMonoFillColor", \
                 "cnInfoLabelOn","cnLineLabelsOn"/)

  cnatt_logical =(/True,True,False,False,True/)

  cndim = dimsizes(cn_logical)

  do i=0,cndim-1
    cc =isatt(nclres,cn_logical(i))
    if (cc.eq.False) then
      res@$cn_logical(i)$ =  cnatt_logical(i)
    end if
  end do

;************************************************************
;  Label Bar Attribute  (only valid when cnFillOn = True)
;************************************************************

if (isatt(nclres,"cnFillOn").eq.False.or.nclres@cnFillOn.eq.True) then

   if (isatt(nclres,"cnLineLabelsOn").eq.False) then
     res@cnLineLabelsOn = False
   end if

   if (isatt(nclres,"cnLinesOn").eq.False) then
     res@cnLinesOn = False
   end if

   lb_string = (/"pmLabelBarDisplayMode","pmLabelBarSide","lbOrientation"/)
   lbatt_string = (/"Always","Bottom","Horizontal"/)

   lb_num = (/"pmLabelBarWidthF","pmLabelBarHeightF"/)
   lbatt_num = (/0.8,0.1/)

   lb_logical = (/"lbPerimOn"/)
   lbatt_logical = (/False/)

   dimlbstr = dimsizes(lb_string)
   dimlbnum = dimsizes(lb_num)
   dimlblog = dimsizes(lb_logical)

  do i=0,dimlbstr-1
    cc =isatt(nclres,lb_string(i))
    if (cc.eq.False) then
      res@$lb_string(i)$ =  lbatt_string(i)
    end if
  end do

  do i=0,dimlbnum-1
    cc =isatt(nclres,lb_num(i))
    if (cc.eq.False) then
      res@$lb_num(i)$ =  lbatt_num(i)
    end if
  end do

  do i=0,dimlblog-1
    cc =isatt(nclres,lb_logical(i))
    if (cc.eq.False) then
      res@$lb_logical(i)$ =  lbatt_logical(i)
    end if
  end do

end if

; Label Bar Title

  name = " "
  unit = " "

  if (isatt(res,"lbTitleString").eq.False) then
    
    if (isatt(data,"description").eq.True) then
      name = data@description
    end if

    if (isatt(data,"units").eq.True) then
      unit = data@units
    end if

    res@lbTitleString = (name + "    (" + unit + ")")

  end if

;*********************************
;  Tick Mark Attribute
;*********************************

   if (isatt(res,"pmTickMarkDisplayMode").eq.False) then  
     res@pmTickMarkDisplayMode = "Conditional"
   end if

   if (isatt(res,"tmXTLabelsOn").eq.False) then
     res@tmXTLabelsOn = False
   end if

   if (isatt(res,"tmXTOn").eq.False) then
     res@tmXTOn = False
   end if

   if (isatt(res,"tmYROn").eq.False) then
     res@tmYROn = False
   end if

   if (isatt(res,"tmXBLabelFontHeightF").eq.False) then
     res@tmXBLabelFontHeightF = 0.005
   end if

;********************************
;  Plot and return
;********************************

   plot = gsn_contour_map(wks,data,res)

   return(plot)

end

;#############################################
undef("ignorant_get_var_files")
function ignorant_get_var_files(infile_names:string, read_var_name[1]:string)

begin

;----------------------------------------------
; determinie input files $ varible properties
;----------------------------------------------

; number of read-in files/varible
  file_num = dimsizes(infile_names)
  var_num  = dimsizes(read_var_name)

; properties of read-in varibles
  tmp_infile   = addfile( infile_names(0),"r" )
  var          = tmp_infile->$read_var_name$
  var_type     = getfilevartypes(tmp_infile, read_var_name)
  var_dimnames = getfilevardims(tmp_infile, read_var_name)
  var_dimsizes = getfilevardimsizes( tmp_infile, read_var_name )
  var_atts     = getfilevaratts(tmp_infile, read_var_name)

  var_dimnames_sizes = dimsizes(var_dimnames)
  var_atts_sizes     = dimsizes(var_atts) 

;------------------------------------
; determine output array properties
;------------------------------------

; define outpuy array size
  aa = 1 + var_dimnames_sizes
  work_dimsizes = new( aa, "integer" )

  work_dimsizes(0) = file_num
  do i = 1, aa-1
    work_dimsizes(i) = var_dimsizes(i-1)
  end do

  work_var = new( work_dimsizes, var_type )

; define output array named dimensions
  work_var!0 = "file"
  do i = 1, var_dimnames_sizes
    work_var!i = var_dimnames(i-1)

    ; copy coordinate
    if (iscoord(var,var_dimnames(i-1))) then
      work_var&$var_dimnames(i-1)$ = var&$var_dimnames(i-1)$
    end if
  end do

; define output array attributes
  file_att = new(1, "string")
  file_att = " "
  do i = 0, file_num-1
    aptr = tostring(i)
    file_att = file_att + aptr + ":[" + infile_names(i) + "], "
  end do

  work_var@file = file_att

  do i = 0, var_atts_sizes-1
    work_var@$var_atts(i)$ = var@$var_atts(i)$
  end do

;------------------------------------------------------
; read-in varibles from files & store into output array
;------------------------------------------------------ 

  do ff = 0, file_num-1

    tmp_infile_name = infile_names(ff)
    tmp_infile  = addfile( tmp_infile_name, "r" )   
    tmp_var     = tmp_infile->$read_var_name$

    if ( var_dimnames_sizes .eq. 1 ) then
      work_var(ff,:) = (/tmp_var/)
    end if
    if ( var_dimnames_sizes .eq. 2 ) then
      work_var(ff,:,:) = (/tmp_var/)
    end if

    if ( var_dimnames_sizes .eq. 3 ) then
      work_var(ff,:,:,:) = (/tmp_var/)
    end if

    if ( var_dimnames_sizes .eq. 4 ) then
      work_var(ff,:,:,:,:) = (/tmp_var/)
    end if 

  end do

;------------------------
; return to main program
;------------------------

  return(work_var)

end

;#############################################
undef("ignorant_get_vars_file")
function ignorant_get_vars_file (infilenames[1]:string, varvars[*]:string)
begin

;----------------------------------------------
; determinie input files $ varible properties
;----------------------------------------------

; number of read-in files/varible
  nfile = dimsizes(infilenames)
  nvar  = dimsizes(varvars)

; properties of read-in varibles
  file1 = infilenames(0)
  var1  = varvars(0)
  tmp_infile   = addfile( file1,"r" )
  var          = tmp_infile->$var1$
  var_type     = getfilevartypes(tmp_infile, var1)
  var_dimnames = getfilevardims(tmp_infile, var1)
  var_dimsizes = getfilevardimsizes( tmp_infile, var1 )
  var_atts     = getfilevaratts(tmp_infile, var1)

  var_dimnames_sizes = dimsizes(var_dimnames)
  var_atts_sizes     = dimsizes(var_atts)

;------------------------------------
; determine output array properties
;------------------------------------
;
; define outpuy array size
  aa = 1 + var_dimnames_sizes
  work_dimsizes = new( aa, "integer" )
;
  work_dimsizes(0) = nvar
  do i = 1, aa-1
    work_dimsizes(i) = var_dimsizes(i-1)
  end do
;
  work_var = new( work_dimsizes, var_type )
;
; define output array named dimensions
;
  work_var!0 = "variable"
  do i = 1, var_dimnames_sizes
    work_var!i = var_dimnames(i-1)
;
    ; copy coordinate
    if (iscoord(var,var_dimnames(i-1))) then
      work_var&$var_dimnames(i-1)$ = var&$var_dimnames(i-1)$
    end if
  end do
;
; define output array attributes
;
  file_att = new(nvar, "string")
  name_att = (/"var_name","long_name","units","Sampling_Sequence","cell_methods"/)
  natt     = dimsizes(name_att)

  do i=0,natt-1
    att1 = name_att(i)
    work_var@$att1$ = file_att
  end do

  do vv = 0,nvar-1
    work_var@var_name(vv) = varvars(vv)
  end do

  ;printVarSummary(work_var)

;---------------------------------------------------------------------
; read-in all varibles from the input file & store into output array
;---------------------------------------------------------------------
  ff = 0

  do vv = 0, nvar-1
    tmp_infile_name = infilenames(ff)
    tmp_infile  = addfile( tmp_infile_name, "r" )
    varname     = varvars(vv)
    tmp_var     = tmp_infile->$varname$

    ;
    ; read and save variable values
    ;
    if ( var_dimnames_sizes .eq. 1 ) then
      work_var(vv,:) = (/tmp_var/)
    end if
    if ( var_dimnames_sizes .eq. 2 ) then
      work_var(vv,:,:) = (/tmp_var/)
    end if

    if ( var_dimnames_sizes .eq. 3 ) then
      work_var(vv,:,:,:) = (/tmp_var/)
    end if

    if ( var_dimnames_sizes .eq. 4 ) then
      work_var(vv,:,:,:,:) = (/tmp_var/)
    end if

    ;
    ; read and save variable attributes
    ;
    do aa=0,natt-1
      att1 = name_att(aa)
      if ( isatt(tmp_var,att1) ) then
        work_var@$att1$(vv) = tmp_var@$att1$
      end if
    end do

    delete(tmp_var)
  end do

;----------
; return
;----------
  return(work_var)
  ;printVarSummary(work_var)

end

;#############################################
undef("ignorant_convert_2Ddata")
function ignorant_convert_2Ddata (infilename[1]:string, varvar[1]:string, opt:logical)
begin

;---------------
; default setup
;---------------
  option_execute = False

  case_names = (/ \
                "tlji2pt_avgji", \
                "tlji2tji_intp", \
                "tlji2pn_crossji", \
                "tlji2pj_avgti", \
                "tji2jt_avgi" \
                /)

  plev_default = (/1., 2., 3., 5., 7., 10., 20., 30., 50., 70., 100., 125., 150., 175., 200., 225., \
      250., 300., 350., 400., 450., 500., 550., 600., 650., 700., 750., 775., 800., 825., \
      850., 875., 900., 925., 950., 975., 1000./)

  if (isatt(opt, "return_type")) then
    if (opt@return_type .eq. "pressure_time-latlon_avg") then
      opt@return_type = "tlji2pt_avgji"
    end if

    if (opt@return_type .eq. "pressure_lat-timelon_avg") then
      opt@return_type = "tji2jt_avgi"
    end if

    if (opt@return_type .eq. "lat_time-lon_avg") then
      opt@return_type = "tji2jt_avgi"
    end if

    if (opt@return_type .eq. "time_lat_lon-plev") then
      opt@return_type = "tlji2tji_intp"
    end if
  end if

;--------------------------------------------
; convert (time,lev/ilev,lat,lon) to (plev,time)
;   1. interpolate CESM hybrid level to the given pressure levels
;   2. do lat & lon average
;--------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "tlji2pt_avgji") then

    ;*** read the variable ***
    filetemp = addfile(infilename, "r")

    hyai_tmp   = filetemp->hyai      ; hybrid A coefficient at layer interface
    hybi_tmp   = filetemp->hybi      ; hybrid B coefficient at layer interface
    hyam_tmp   = filetemp->hyam      ; hybrid A coefficient at layer midpoints
    hybm_tmp   = filetemp->hybm      ; hybrid B coefficient at layer midpoints
    PS_tmp     = filetemp->PS        ; surface pressures  in Pa
    P0_tmp     = filetemp->P0        ; reference pressure in Pa
    varvar_tmp = filetemp->$varvar$

    if (iscoord(varvar_tmp,"lev")) then
      hyaw_tmp  = hyam_tmp
      hybw_tmp  = hybm_tmp
    else if (iscoord(varvar_tmp,"ilev")) then
      hyaw_tmp  = hyai_tmp
      hybw_tmp  = hybi_tmp
    end if
    end if

    ;*** set pressure levels for interpolation ***
    if ( isatt(opt, "plev") ) then
      plev_work = opt@plev
    else
      plev_work = plev_default
    end if

    ; do interpolation
    varvar_tmp_plev = vinth2p(varvar_tmp, hyaw_tmp, hybw_tmp, plev_work, PS_tmp, 1, P0_tmp/100., 1, False)
    copy_VarAtts(varvar_tmp, varvar_tmp_plev)

    varvar_tmp_plev_ijavg = lat_wgt_avg(varvar_tmp_plev)

    ;*** return variable ***
    varvar_return = varvar_tmp_plev_ijavg(lev_p|:,time|:)

    varvar_return&lev_p@long_name = "Pressure"
    varvar_return&lev_p@units     = "hPa"

    ntime = dimsizes(varvar_return&time)
    time_coord = fspan(0.,tofloat(ntime-1),ntime)
    ;time_coord = ispan(0,ntime-1,1)
    delete(varvar_return&time)
    varvar_return&time = time_coord

    ;printVarSummary(varvar_tmp_plev)
    ;printVarSummary(varvar_tmp_plev_ijavg)
    ;printVarSummary(varvar_return)

    option_execute = True
  end if  ; end if of "tlji2pt_avgji"

;--------------------------------------------
; convert (time,lat,lon) to (lat,time)
;   1. do lon average
;--------------------------------------------

  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "tji2jt_avgi") then

    ;*** read the variable ***
    filetemp = addfile(infilename, "r")

    varvar_tmp = filetemp->$varvar$
    ;printVarSummary(varvar_tmp)

    varvar_tmp_lonavg = dim_avg_n_Wrap(varvar_tmp,2)
    ;printVarSummary(varvar_tmp_lonavg)
  
    latname = varvar_tmp!1    ; get the name of latitude dimension
    timename = varvar_tmp!0   ; get the name of time     dimension

    varvar_return = varvar_tmp_lonavg($latname$|:,$timename$|:) 

    ntime = dimsizes(varvar_return&$timename$)
    time_coord = fspan(0.,tofloat(ntime-1),ntime)
    delete(varvar_return&time)
    varvar_return&time = time_coord    

    option_execute = True
    ;printVarSummary(varvar_return)
  end if  ; end if of "tji2jt_avgi"

;--------------------------------------------
; OUTDATED: convert (time,lat,lon1) to (time,lat,lon2), lon1 is not monotonic while lon2 is. e.g. lon1 = (/350,0,10/) and lon2 is (/-10,0,10/).
;--------------------------------------------
;  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "time_lat_lon-lon_mono") then
;
;    ;*** read the variable ***
;    filetemp = addfile(infilename, "r")
;      varvar_tmp = filetemp->$varvar$
;
;    ;if ( isatt(opt, "time_step") .and. isint(opt@time_step)) then
;    ;  tt = opt@time_step
;    ;  varvar_tmp = filetemp->$varvar$(tt,:,:)
;    ;else
;    ;  varvar_tmp = filetemp->$varvar$
;    ;end if
;
;    ;*** get lon ***
;    if ( iscoord(varvar_tmp,"lon")) then
;      lonname = "lon"
;      lon = filetemp&$lonname$
;      option_execute = True
;
;    else if ( iscoord(varvar_tmp,"longitude")) then
;      lonname = "longitude"
;      lon = filetemp&$lonname$
;      option_execute = True
;    else
;      print("ERROR: (ignorant_convert_2Ddata) coordinate lon/longitude is not found in return type: [time_lat_lon-lon_mono]")
;      print("")
;      option_execute = False
;    end if
;    end if
;
;    ;*** make lon monotonic ***
;    if (option_execute) then
;
;      nlon = dimsizes(lon)
;      lon1 = lon
;      do i=0,nlon-1
;        if (lon(i).gt.180.) then
;          lon1(i) = lon(i) - 360.
;        end if
;      end do
;
;    ;*** return variable ***
;    varvar_return = varvar_tmp
;    varvar_return&$lonname$ = (/lon1/)
;
;    end if
;  end if ; end if of "time_lat_lon-lon_mono"

;--------------------------------------------
; convert (time,lev/ilev,lat,lon) to (time,lat,lon) at a pressure level
;   1. interpolate CESM hybrid level to the given pressure level
;--------------------------------------------

  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "tlji2tji_intp") then

    ;*** read the variable ***
    filetemp = addfile(infilename, "r")

    hyai_tmp   = filetemp->hyai      ; hybrid A coefficient at layer interface
    hybi_tmp   = filetemp->hybi      ; hybrid B coefficient at layer interface
    hyam_tmp   = filetemp->hyam      ; hybrid A coefficient at layer midpoints
    hybm_tmp   = filetemp->hybm      ; hybrid B coefficient at layer midpoints
    PS_tmp     = filetemp->PS        ; surface pressures  in Pa
    P0_tmp     = filetemp->P0        ; reference pressure in Pa
    varvar_tmp = filetemp->$varvar$

    if (iscoord(varvar_tmp,"lev")) then
      hyaw_tmp  = hyam_tmp
      hybw_tmp  = hybm_tmp
    else if (iscoord(varvar_tmp,"ilev")) then
      hyaw_tmp  = hyai_tmp
      hybw_tmp  = hybi_tmp
    end if
    end if

    ;*** set pressure levels for interpolation ***
    if ( isatt(opt, "plev") .and. dimsizes(opt@plev) .eq. 1) then
      plev_work = opt@plev
    else
      print("WARNING: (ignorant_convert_2Ddata) case [tlji2tji_intp] only allow a pressure level or opt@plev is not given. Set to 500 hPa")
      plev_work = 500.
    end if

    ; do interpolation
    varvar_tmp_plev = vinth2p(varvar_tmp, hyaw_tmp, hybw_tmp, plev_work, PS_tmp, 1, P0_tmp/100., 1, False)
    copy_VarAtts(varvar_tmp, varvar_tmp_plev)

    varvar_return = varvar_tmp_plev(:,0,:,:)  ; return (time,lat,lon)
    delete(varvar_return@lev_p)
    varvar_return@pressure_level = plev_work+" hPa"
    varvar_return@long_name = varvar_return@long_name+" at "+varvar_return@pressure_level

    option_execute = True
  end if  ; end if of "tlji2tji_intp"

;--------------------------------------------
; convert (time,lev/ilev,lat,lon) to (plev,lat)
;   1. interpolate CESM hybrid level to the given pressure levels
;   2. do time & lon average
;--------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "tlji2pj_avgti") then

    ;*** read the variable ***
    filetemp = addfile(infilename, "r")

    hyai_tmp   = filetemp->hyai      ; hybrid A coefficient at layer interface
    hybi_tmp   = filetemp->hybi      ; hybrid B coefficient at layer interface
    hyam_tmp   = filetemp->hyam      ; hybrid A coefficient at layer midpoints
    hybm_tmp   = filetemp->hybm      ; hybrid B coefficient at layer midpoints
    PS_tmp     = filetemp->PS        ; surface pressures  in Pa
    P0_tmp     = filetemp->P0        ; reference pressure in Pa
    varvar_tmp = filetemp->$varvar$

    if (iscoord(varvar_tmp,"lev")) then
      hyaw_tmp  = hyam_tmp
      hybw_tmp  = hybm_tmp
    else if (iscoord(varvar_tmp,"ilev")) then
      hyaw_tmp  = hyai_tmp
      hybw_tmp  = hybi_tmp
    end if
    end if

    ;*** set pressure levels for interpolation ***
    if ( isatt(opt, "plev") ) then
      plev_work = opt@plev
    else
      plev_work = plev_default
    end if

    ; do interpolation
    varvar_tmp_plev = vinth2p(varvar_tmp, hyaw_tmp, hybw_tmp, plev_work, PS_tmp, 1, P0_tmp/100., 1, False)
    copy_VarAtts(varvar_tmp, varvar_tmp_plev)
    ;printVarSummary(varvar_tmp_plev)

    varvar_tmp_plev_tavg = dim_avg_n_Wrap(varvar_tmp_plev,0)
    varvar_tmp_plev_itavg = dim_avg_n_Wrap(varvar_tmp_plev_tavg,2)
    ;printVarSummary(varvar_tmp_plev_itavg)

    varvar_return = varvar_tmp_plev_itavg
    option_execute = True

  end if ; end if of "tlji2pj_avgti"

;--------------------------------------------
; convert (time,lev/ilev,lat,lon) to (time,plev,points)
;   1. interpolate CESM hybrid level to the given pressure levels
;   2. do vertical cross section between two given lat/lon points
;--------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "tlji2pn_crossji") then

    ;*** set parameters ***
    leftlat   = opt@leftlat
    leftlon   = opt@leftlon 

    rightlat  = opt@rightlat 
    rightlon  = opt@rightlon 

    if (isatt(opt,"npts")) then
      npts = opt@npts
    else
      npts = 100 
    end if

    if (isatt(opt,"time_step") .and. isint(opt@time_step)) then
      tt = opt@time_step
    else if (isatt(opt,"time_step") .and. opt@time_step .eq. "avg") then
      tt = -999
    else
      tt=0
    end if
    end if

    if ( isatt(opt, "plev") ) then
      plev_work = opt@plev
    else
      plev_work = plev_default
    end if

    ;*** read the variable ***
    filetemp = addfile(infilename, "r")

    hyai_tmp   = filetemp->hyai      ; hybrid A coefficient at layer interface
    hybi_tmp   = filetemp->hybi      ; hybrid B coefficient at layer interface
    hyam_tmp   = filetemp->hyam      ; hybrid A coefficient at layer midpoints
    hybm_tmp   = filetemp->hybm      ; hybrid B coefficient at layer midpoints
    PS_tmp     = filetemp->PS        ; surface pressures  in Pa
    P0_tmp     = filetemp->P0        ; reference pressure in Pa
    varvar_tmp = filetemp->$varvar$

    if (iscoord(varvar_tmp,"lev")) then
      hyaw_tmp  = hyam_tmp
      hybw_tmp  = hybm_tmp
    else if (iscoord(varvar_tmp,"ilev")) then
      hyaw_tmp  = hyai_tmp
      hybw_tmp  = hybi_tmp
    end if
    end if

    ;*** set pressure levels for interpolation ***
    if ( isatt(opt, "plev") ) then
      plev_work = opt@plev
    else
      plev_work = plev_default
    end if

    ;*** do interpolation ***
    varvar_tmp_plev = vinth2p(varvar_tmp, hyaw_tmp, hybw_tmp, plev_work, PS_tmp, 1, P0_tmp/100., 1, False)
    copy_VarAtts(varvar_tmp, varvar_tmp_plev)
    ;printVarSummary(varvar_tmp_plev)

    ;*** get lon/lat coordinate ***
    if (isdim(varvar_tmp_plev,"lat")) then
      lat_var = varvar_tmp_plev&lat
    end if

    if (isdim(varvar_tmp_plev,"lon")) then
      lon_var = varvar_tmp_plev&lon

      ;*** make lon monotonic ***
      if (isMonotonic(lon_var) .eq. 0) then ; lon is not monotonic
        nlon = dimsizes(lon_var)
        do i=0,nlon-1
          if (lon_var(i).gt.180.) then
            lon_var(i) = lon_var(i) - 360.
          end if
        end do
        varvar_tmp_plev&lon = lon_var
      end if
    end if

    ;*** get points coordinate ***
    points   = fspan(0.,npts-1,npts)

    ;*** use NCL function "gc_latlon" to finds the great circle distance (true surface distance) between two given points ***
    ;*** gc_latlon, https://www.ncl.ucar.edu/Document/Functions/Built-in/gc_latlon.shtml ***
    dist  = gc_latlon(leftlat,leftlon,rightlat,rightlon,npts,2) ; iu=2, return the distance in degrees

    ;*** get all lat/lon between two given points ***
    lon_cross = dist@gclon
    lat_cross = dist@gclat

      ;*** make lon monotonic ***
      if (isMonotonic(lon_cross) .eq. 0) then ; lon is not monotonic
        nlon = dimsizes(lon_cross)
        do i=0,nlon-1
          if (lon_cross(i).gt.180.) then
            lon_cross(i) = lon_cross(i) - 360.
          end if
        end do
      end if

    ;*** use NCL function "linint2_points" to interpolate data between two given points ***
    ;*** linint2_points, https://www.ncl.ucar.edu/Document/Functions/Built-in/linint2_points.shtml ***
    varvar_cross = linint2_points(lon_var, lat_var, varvar_tmp_plev, opt@linint2_points_cyclic, lon_cross, lat_cross, 2) ; 2:Reserved for future use
    copy_VarAtts(varvar_tmp_plev,varvar_cross)

    ;*** set dimensions ***
    varvar_cross!0 = "time"
    varvar_cross&time = varvar_tmp_plev&time

    varvar_cross!1 = "lev_p"
    varvar_cross&lev_p = varvar_tmp_plev&lev_p

    varvar_cross!2 = "points"
    varvar_cross&points = points

    varvar_cross@lat_points = lat_cross
    varvar_cross@lon_points = lon_cross
    ;printVarSummary(varvar_cross)

    if ( tt .eq. -999 ) then
      varvar_return = dim_avg_n_Wrap(varvar_cross,0)
    else
      varvar_return = varvar_cross(tt,:,:)
    end if

    ;print(varvar_return)
    option_execute = True

  end if ; end if of "tlji2pn_crossji"

;---------
; default
;---------
  if (.not.option_execute) then

    if (isatt(opt, "return_type")) then 
      print("WARNING: (ignorant_convert_2Ddata) fail to execute the case ["+opt@return_type+"]")
      print("  avail case: "+ case_names)
      print("return default variable")
    end if

    filetemp = addfile(infilename, "r")
    varvar_return = filetemp->$varvar$

  end if

;------------------------
; option: change monotonically lon
;------------------------

  option_execute1 = False

  ;*** get longitude array ***
  if ( iscoord(varvar_return,"lon")) then
    lonname = "lon"
    lon = filetemp&$lonname$

    if (isMonotonic(lon) .eq. 0) then ; lon is not monotonic
      option_execute1 = True
    end if

  else if ( iscoord(varvar_return,"longitude")) then
    lonname = "longitude"
    lon = filetemp&$lonname$

    if (isMonotonic(lon) .eq. 0) then ; lon is not monotonic
      option_execute1 = True
    end if

  end if
  end if

  ;*** make lon monotonic ***
  if (option_execute1) then
    nlon = dimsizes(lon)
    lon1 = lon
    do i=0,nlon-1
      if (lon(i).gt.180.) then
        lon1(i) = lon(i) - 360.
      end if
    end do
    varvar_return&$lonname$ = (/lon1/)
  end if

;----------------------------------------
; option: select the returned time step
;----------------------------------------

  if ( isatt(opt, "time_step") .and. isint(opt@time_step)) then
    if ( iscoord(varvar_return,"time")) then
      tt = opt@time_step
      varvar_return1 = varvar_return(tt,:,:)
      delete(varvar_return)
      varvar_return = varvar_return1
    else
      print("ERROR: (ignorant_convert_2Ddata) time coordinate is not found so it cannot be processed")
      print("")
    end if
  end if

;----------------------
; return the variable
;----------------------
  return(varvar_return)
end

;#############################################
undef("gsn_csm_contour_map_siglvl")
function gsn_csm_contour_map_siglvl(wks:graphic, varvar_any[*][*][*]:numeric, varvar_base[*][*][*]:numeric, res_var[1]:logical, res_sig[1]:logical, opt[1]:logical)
begin

;*************
; initialize
;*************

; take difference
  varvar_anyMbase = diff(varvar_any,varvar_base)
  varvar_anyMbase_avg = dim_avg_n_Wrap(varvar_anyMbase,0)

; get lat/lon dimemsion
  nlat = -1
  nlon = -1

  ntime = dimsizes(varvar_base(:,0,0))

  if ( isdim(varvar_base,"lat") ) then
    nlat = dimsizes(varvar_base&lat)
  end if

  if ( isdim(varvar_base,"lon") ) then
    nlon = dimsizes(varvar_base&lon)
  end if

; get significance level
  if ( opt .and. isatt(opt,"siglvl")  ) then
    siglvl = opt@siglvl
  else
    siglvl = 0.05
  end if

; parameters for ttest
  siglev = 100.*(1.-siglvl)
  iflag= False

; res for plot
  res_var_work = True
    if (isatt(opt,"cnColors_PosNeg") .and. .not.opt@cnColors_PosNeg ) then
      opt1 = False
    else
      opt1 = True
      cnColors_PosNeg(varvar_anyMbase_avg,res_var_work,opt1)
    end if
  copy_VarAtts(res_var,res_var_work)


; res for siglvl
  res_sig_work = True
  res_sig_work@cnLevelSelectionMode = "ExplicitLevels"
  res_sig_work@cnLevels = (/siglev/)

  res_sig_work@cnFillOn = True
  res_sig_work@cnLinesOn = False
  res_sig_work@cnLineLabelsOn = False
  res_sig_work@cnInfoLabelOn = False

  res_sig_work@cnLineThicknessF = 3.

  res_sig_work@cnMonoFillColor = False
  res_sig_work@cnFillColors = (/-1,1/)

  res_sig_work@cnMonoFillPattern = False
  res_sig_work@cnFillPatterns = (/0,4/)   ; slash
  ;res_sig_work@cnFillPatterns = (/0,17/)  ; dotted

  copy_VarAtts(res_sig,res_sig_work)

  ;if (isatt(res_var,"gsnAddCyclic")) then
  ;  res_sig_work@gsnAddCyclic = res_var@gsnAddCyclic
  ;end if

  if ( .not.isatt(res_var_work, "tiMainString")) then
    res_var_work@tiMainString = "Significance level at "+siglev+"% for "+ntime+" steps"
  end if

;************ 
; do t-test
;************
  prob = varvar_any(0,:,:)
  prob@long_name = "significance level"
  prob@units = "NONE"

  do j=0,nlat-1
  do i=0,nlon-1
    xtmp = varvar_any(:,j,i)
    ytmp = varvar_base(:,j,i)

  ;*** compute time-average and variance ***
    xAve = dim_avg_Wrap (xtmp)              ; calculate means at each grid point 
    yAve = dim_avg_Wrap (ytmp)
    xVar = dim_variance_Wrap (xtmp)         ; calculate variances
    yVar = dim_variance_Wrap (ytmp)

  ;*** get size ***
    xN = dimsizes(xtmp)
    yN = dimsizes(ytmp)

    prob(j,i) = ttest(xAve,xVar,xN, yAve,yVar,yN, iflag, False)
  end do
  end do

  alpha = prob
  alpha = 100.*(1.-prob) ; A significance of 0.05 returned by ttest would yield 95% for alpha. This is often done for plotting.
                         ; ref: http://www.ncl.ucar.edu/Document/Functions/Built-in/ttest.shtml, Example 2

  ;**********************
  ; set plot variables
  ;**********************

  varvar_anyMbase_siglev = alpha

  ;********
  ; plot
  ;********

  plot_anyMbase_siglev = gsn_csm_contour_map_overlay(wks, varvar_anyMbase_avg, varvar_anyMbase_siglev, res_var_work, res_sig_work)

  return(plot_anyMbase_siglev)
end

;#############################################
undef("change_var_atts")
procedure change_var_atts (varname[1]:string, varin, res[1]:logical, opt_chgatt[1]:logical)
begin

if (opt_chgatt) then

;*************
; initialize
;*************

  if ( isatt(varin,"long_name") ) then
    longname = varin@long_name
  else
    longname = "slkdjflksjdfljasldfjlaks"
  end if

;*****************************************
; change fill colors, for absolute value
;*****************************************

  if (isatt(opt_chgatt,"chg_cncolors") .and. opt_chgatt@chg_cncolors) then

    opt_chgcolors = False
  
    if ( \
             .not.ismissing(str_match(longname,"Vertical velocity")) \
        .or. .not.ismissing(str_match(longname,"OMEGA")) \
       ) then
       option_cmap = "red_blue"
       opt_chgcolors = True
    end if
  
    if ( \
             .not.ismissing(str_match(longname,"Zonal wind")) \
        .or. .not.ismissing(str_match(longname,"zonal wind")) \
        .or. .not.ismissing(str_match(longname,"Meridional wind")) \
        .or. .not.ismissing(str_match(longname,"meridional wind")) \
        .or. .not.ismissing(str_match(longname,"U at")) \
        .or. .not.ismissing(str_match(longname,"V at")) \
       ) then
       option_cmap = "purple_green"
       opt_chgcolors = True
    end if
  
    if (opt_chgcolors) then
  
      opt_work = True
      opt_work@max_levels = 15  ; level for using function nice_mnmxintvl
  
      opt_work@cnlevels_predefine = False    ; True  - contour levels are given in res
      ;opt_work@cnlevels_predefine = True      False - contour levels will be computed in this procedure
  
      if (option_cmap .eq. "dry_wet") then
        opt_work@colormap_positive = "MPL_YlGnBu"
        ;opt_work@colormap_positive_index_start = 2       ; the least positive color
        ;opt_work@colormap_positive_index_end = 999       ; the most  positive color
  
        ;opt_work@colormap_negative = "MPL_YlOrBr"
        ;opt_work@colormap_negative_index_start = 120     ; the most  negative color
        ;opt_work@colormap_negative_index_end = 2         ; the least negative color
        opt_work@colormap_negative = "MPL_YlGnBu"
        opt_work@colormap_negative_index_start = 2     ; the most  negative color
        opt_work@colormap_negative_index_end = 129         ; the least negative color
      end if
  
      if (option_cmap .eq. "purple_green") then
        opt_work@colormap_positive = "MPL_Purples"       ; colormap for plotting positive colors
        opt_work@colormap_positive_index_start = 2       ; the least positive color
        opt_work@colormap_positive_index_end = 999       ; the most  positive color
    
        opt_work@colormap_negative = "MPL_Greens"        ; colormap for plotting negative colors
        opt_work@colormap_negative_index_start = 120     ; the most  negative color
        opt_work@colormap_negative_index_end = 2         ; the least negative color
      end if
    
      if (option_cmap .eq. "red_blue") then
        opt_work@colormap_positive = "NCV_blu_red"         ; colormap for plotting positive colors
        opt_work@colormap_positive_index_start = 123       ; the least positive color
        opt_work@colormap_positive_index_end = 2           ; the most  positive color
    
        opt_work@colormap_negative = "NCV_blu_red"         ; colormap for plotting negative colors
        opt_work@colormap_negative_index_start = 255       ; the most  negative color
        opt_work@colormap_negative_index_end = 137         ; the least negative color
      end if
    
      if (option_cmap .eq. "default") then
        opt_work@colormap_positive = "WhiteYellowOrangeRed"  ; colormap for plotting positive colors
        opt_work@colormap_positive_index_start = 2           ; the least positive color
        opt_work@colormap_positive_index_end = 999           ; the most  positive color
    
        opt_work@colormap_negative = "MPL_GnBu"              ; colormap for plotting negative colors
        opt_work@colormap_negative_index_start = 127         ; the most  negative color
        opt_work@colormap_negative_index_end = 2             ; the least negative color
      end if
    
      copy_VarAtts(opt_chgatt, opt_work)
  
      ndim = dimsizes(dimsizes(varin))
      if (ndim.eq.2) then
        varin_temp1 = varin
      else if (ndim.eq.3) then
        varin_temp1 = dim_avg_n_Wrap(varin,0)
      else if (ndim.eq.4) then
        varin_temp0 = dim_avg_n_Wrap(varin,0)
        varin_temp1 = dim_avg_n_Wrap(varin_temp0,0)
      end if
      end if
      end if
  
      if (varname.eq."OMEGA") then
         if ( isatt(varin, "units") .and. varin@units.eq."Pa/s") then
           varin_temp1 = unit_convert(varin_temp1,"Pa/s","hPa/day")
         end if
      end if
      cnColors_PosNeg(varin_temp1, res, opt_work)
  
    end if ; end if of opt_chgcolors
  
  end if ; end if of opt_chgatt@chg_cncolors

;***************
; change units
;***************
  if (varname.eq."PRECC" .or. varname.eq."PRECL" .or. varname.eq."PRECT") then
    if ( isatt(varin, "units") .and. varin@units.eq."m/s") then
      varin  = unit_convert(varin,"m/s","mm/day")
    end if
  end if

  if (varname.eq."Q") then
    if ( isatt(varin, "units") .and. varin@units.eq."kg/kg") then
      varin  = unit_convert(varin,"kg/kg","g/kg")
    end if
  end if

  if (varname.eq."CLDLIQ" .or. varname.eq."CLDICE") then
    if ( isatt(varin, "units") .and. varin@units.eq."kg/kg") then
      varin  = unit_convert(varin,"kg/kg","mg/kg")
    end if
  end if

  if (varname.eq."OMEGA") then
    if ( isatt(varin, "units") .and. varin@units.eq."Pa/s") then
      varin  = unit_convert(varin,"Pa/s","hPa/day")
    end if
  end if

;***********************
; change units string
;***********************
;
  if (isatt(varin,"units") .and. varin@units .eq. "W/m2") then
    varin@units = "W m~S~-2"
    res@gsnRightString = varin@units
  end if

  if (isatt(varin,"units") .and. varin@units .eq. "gram/m2") then
    varin@units = "g m~S~-2"
    res@gsnRightString = varin@units
  end if

;*************************
; change variables string
;*************************
;
  var_strings = (/  \
                  (/"CLDLOW"		, "Low cloud fraction"/),  \ 
                  (/"CLDMED"		, "Middle cloud fraction"/),  \ 
                  (/"CLDHGH"		, "High cloud fraction"/),  \ 
                  (/"CLDTOT"		, "Total cloud fraction"/),  \ 
                  (/"FLNS"		, "Net Up LW flux at the surface"/), \
                  (/"FLNSC"		, "clrsky Surf Net Up LW flux"/), \
                  (/"FLUT"		, "Outgoing LW radiation"/), \
                  (/"FLUTC"		, "clrsky Outgoing LW radiation"/), \
                  (/"FLDS"		, "Dn LW flux at the surf"/), \
                  (/"FSDS"		, "Dn SW flux at the surf"/), \
                  (/"FSDSC"		, "clr-sky Dn SW flux at the surf"/), \
                  (/"LHFLX"		, "Latent heat flux"/), \
                  (/"OMEGA500"		, "~F33~w~F~ at 500 hPa"/), \
                  (/"PRECC"		, "Conv Prec"/), \
                  (/"PRECL"		, "Stratiform Prec"/), \
                  (/"PRECT"		, "Total Prec"/), \
                  (/"RADCOOL_TOA_TO_SURF","Net Column Rad Cooling"/), \
                  (/"SHFLX"		, "Sensible heat flux"/), \
                  (/"TS"		, "Surf Rad temperature"/), \
                  (/"TREFHT"		, "Surface air temperature"/), \
                  (/"TGCLDIWP"		, "Grid-box ice water path"/), \
                  (/"TGCLDLWP"		, "Grid-box liquid water path"/) \
                  ;(/""	, /), \
                /)
  nstring = dimsizes(var_strings(:,0))
  do i=0,nstring-1
    if (varname.eq.var_strings(i,0)) then
      res@gsnLeftString = var_strings(i,1)
    end if
  end do

end if ; end if of opt_chgatt

end

;#############################################
undef("ignorant_convert_1Ddata")
procedure ignorant_convert_1Ddata (infilenames[*]:string, varvars[*]:string, res[1]:logical, opt[1]:logical)
begin

;---------------
; initialization
;---------------
  ;case_name = var(input dimension) + "2" + xx + "_" + yy + process , e.g. ftji2f_t_avgji
  case_names = (/ \
                "vtji2t_vt_avgji", \
                "vtlji2vp_p_avgtji", \
                "ftlji2fp_p_avgtji", \
                "ftji2t_ft_avgji" \
                /)

  ;*** set res_work ***
  res_work = True
  res_work@xyMonoMarkLineMode = True
  res_work@xyMarkLineMode     = "Lines"
  xy_colors1 = (/ "red", \
                  "blue",  \
                  "orange", \
;                  "yellow",  \
                  "green", \
                  "purple",  \
                  "pink",  \
                  "brown",  \
                  "gray",  \
                  "purple",  \
                  "black"  \
                /)
  res_work@xyMonoLineColor    = False
  res_work@xyLineColors       = xy_colors1

  res_work@xyMonoDashPattern  = True
  res_work@xyDashPattern      = 0

  res_work@xyMonoLineThickness = True
  res_work@xyLineThicknessF    = 2.5

  ;*** set opt for change_var_atts ***
  opt_change = True

  ;*** set opt@update_res ***
  if ( isatt(opt, "update_res") .and. .not.opt@update_res ) then
    opt@update_res = False
  else
    opt@update_res = True
  end if

  ;*** set default pressure levels for interpolation ***
  plev_default = (/1., 2., 3., 5., 7., 10., 20., 30., 50., 70., 100., 125., 150., 175., 200., 225., \
      250., 300., 350., 400., 450., 500., 550., 600., 650., 700., 750., 775., 800., 825., \
      850., 875., 900., 925., 950., 975., 1000./)

;------------
; check case
;------------
  ncase  = dimsizes(case_names)
  check1 = False

  do i=0,ncase-1
    if ( isatt(opt, "return_type") .and. opt@return_type .eq. case_names(i)) then
      check1 = True
    end if    
  end do

  if (.not.check1) then
    print("WARNING: (ignorant_convert_1Ddata) fail to execute the case ["+opt@return_type+"]")
    print("  avail case: "+ case_names)
    print("Not return var_xx & var_yy")
  end if

;------------------
; check time step
;------------------
  if (isatt(opt,"time_step_start") .and. isatt(opt,"time_step_end") ) then
    tt1 = opt@time_step_start
    tt2 = opt@time_step_end
  else
    ftemp_name = infilenames(0)
    ftemp1     = addfile(ftemp_name, "r")
    ;printVarSummary(ftemp1)
    if ( isdim(ftemp1,"time") ) then
      ntime = dimsizes(ftemp1&time)
      tt1 = 0
      tt2 = ntime-1
    end if
  end if
  ;print(tt1+" , "+tt2)

;------------------------------------------------------------------------------------
; convert (file,time,lat,lon) to xx=time, yy=(flile,time), averaged over lat and lon
;------------------------------------------------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "ftji2t_ft_avgji") then
    varvar_all0 = ignorant_get_var_files(infilenames,varvars)
    ;printVarSummary(varvar_all0)

    varvar_all1 = lat_wgt_avg(varvar_all0) ; lat-weighted averafe, vars (files,time)
    ;printVarSummary(varvar_all1)
 
    var_yy = varvar_all1(:,tt1:tt2)

    if ( isdim(varvar_all1,"time")) then
      var_xx = varvar_all1&time(tt1:tt2)
    else
      ntime = dimsizes(varvar_all1(0,tt1:tt2))
      var_xx = fspan(0.,tofloat(ntime-1),ntime)
    end if
  end if  ; end if of "ftji2t_ft_avgji"

;------------------------------------------------------------------------------------
; convert (var,time,lat,lon) to xx=time, yy=(var,time), averaged over lat and lon
;------------------------------------------------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "vtji2t_vt_avgji") then
    varvar_all0 = ignorant_get_vars_file(infilenames,varvars)
    ;printVarSummary(varvar_all0)

    varvar_all1 = lat_wgt_avg(varvar_all0) ; lat-weighted averafe, vars (variables,time)
    ;printVarSummary(varvar_all1)
 
    var_yy = varvar_all1(:,tt1:tt2)
    delete(var_yy@long_name)  ; b.c. both @long_name and @units are a multidimensional assry. gsn_csm_xy would fail.

    unit1 = var_yy@units(0)
    delete(var_yy@units)
    var_yy@units = unit1
 
    if ( isdim(varvar_all1,"time")) then
      var_xx = varvar_all1&time(tt1:tt2)
    else
      ntime = dimsizes(varvar_all1(0,tt1:tt2))
      var_xx = fspan(0.,tofloat(ntime-1),ntime)
    end if
  end if  ; end if of "vtji2t_vt_avgji

;------------------------------------------------------------------------------------
; convert (file,lev,time,lat,lon) to xx=(file,pres), yy=(pres), averaged over lat and lon
;------------------------------------------------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "ftlji2fp_p_avgtji") then
    hyai_all   = ignorant_get_var_files(infilenames,"hyai")
    hybi_all   = ignorant_get_var_files(infilenames,"hybi")
    hyam_all   = ignorant_get_var_files(infilenames,"hyam")
    hybm_all   = ignorant_get_var_files(infilenames,"hybm")
    PS_all     = ignorant_get_var_files(infilenames,"PS")
    P0_all     = ignorant_get_var_files(infilenames,"P0")
    varvar_all = ignorant_get_var_files(infilenames,varvars)

    if (iscoord(varvar_all,"lev")) then
      hyaw_all = hyam_all(0,:)
      hybw_all = hybm_all(0,:)
      P0       = P0_all(0,0)
    else if (iscoord(varvar_all,"ilev")) then
      hyaw_all = hyai_all(0,:)
      hybw_all = hybi_all(0,:)
      P0       = P0_all(0,0)
    end if
    end if

    if (isatt(opt,"plev")) then
      plev_work = opt@plev
    else
      plev_work = plev_default
    end if

    ; do interpolation
    varvar_all_plev = vinth2p(varvar_all, hyaw_all, hybw_all, plev_work, PS_all, 1, P0/100., 1, False)
    copy_VarAtts(varvar_all, varvar_all_plev)

    ; do time & lat & lon average
    varvar_all_plev_tavg = dim_avg_n_Wrap(varvar_all_plev(:,tt1:tt2,:,:,:),1)
    varvar_all_plev_tavg_ijavg = lat_wgt_avg(varvar_all_plev_tavg)

    var_xx = varvar_all_plev_tavg_ijavg
    var_yy = plev_work

  end if ; end if of "ftlji2fp_p_avgtji"    

;------------------------------------------------------------------------------------
; convert (var,lev,time,lat,lon) to xx=(var,pres), yy=(pres), averaged over lat and lon
;------------------------------------------------------------------------------------
  if ( isatt(opt, "return_type") .and. opt@return_type .eq. "vtlji2vp_p_avgtji") then
    ftemp1 = addfile(infilenames,"r")
    hyai_all   = ftemp1->hyai
    hybi_all   = ftemp1->hybi
    hyam_all   = ftemp1->hyam
    hybm_all   = ftemp1->hybm
    P0_all     = ftemp1->P0
    varvar_all = ignorant_get_vars_file(infilenames,varvars)

    PS_name = varvars
    PS_name = "PS"
    PS_all = ignorant_get_vars_file(infilenames,PS_name)

    if (iscoord(varvar_all,"lev")) then
      hyaw_all = hyam_all
      hybw_all = hybm_all
      P0       = P0_all
    else if (iscoord(varvar_all,"ilev")) then
      hyaw_all = hyai_all
      hybw_all = hybi_all
      P0       = P0_all
    end if
    end if

    if (isatt(opt,"plev")) then
      plev_work = opt@plev
    else
      plev_work = plev_default
    end if

    ; do interpolation
    varvar_all_plev = vinth2p(varvar_all, hyaw_all, hybw_all, plev_work, PS_all, 1, P0/100., 1, False)
    copy_VarAtts(varvar_all, varvar_all_plev)

    ; do time & lat & lon average
    varvar_all_plev_tavg = dim_avg_n_Wrap(varvar_all_plev,1)
    varvar_all_plev_tavg_ijavg = lat_wgt_avg(varvar_all_plev_tavg)

    var_xx = varvar_all_plev_tavg_ijavg
    var_yy = plev_work

    delete(var_xx@long_name)  ; b.c. both @long_name and @units are a multidimensional assry. gsn_csm_xy would fail.
    unit1 = var_xx@units(0)
    delete(var_xx@units)
    var_xx@units = unit1

  end if ; end if of "ftlji2fp_p_avgtji"    

  ;******************
  ; if var_xx = number
  ;******************
  if ( isatt(opt, "var_xx_number") .and. opt@var_xx_number) then
    nxx = dimsizes(var_xx)
    delete(var_xx)
    var_xx = fspan(0.,tofloat(nxx-1),nxx)
  end if

  ;******************
  ; if var_yy = number
  ;******************
  if ( isatt(opt, "var_yy_number") .and. opt@var_yy_number) then
    nyy = dimsizes(var_yy)
    delete(var_yy)
    var_yy = fspan(0.,tofloat(nyy-1),nyy)
  end if

;-------------
; modify res
;-------------

  if ( isatt(opt, "update_res") .and. opt@update_res ) then

    ;*************************
    ; variable name & unit
    ;*************************
    if ( isatt(opt, "return_type") .and. opt@return_type .eq. "ftji2t_ft_avgji") then
      change_var_atts(varvars,var_yy,res_work,opt_change)
      if (isatt(var_xx,"long_name") .and. isatt(var_xx,"units")) then
        res_work@tiXAxisString = var_xx@long_name+" ("+var_xx@units+")"
      end if
      res_work@gsnRightString = var_yy@units
      res_work@tiYAxisString  = var_yy@units
    end if

    if ( isatt(opt, "return_type") .and. opt@return_type .eq. "ftlji2fp_p_avgtji") then
      res_work@trYReverse = True
      res_work@tiYAxisString = "Pressure (hPa)"

      change_var_atts(varvars,var_xx,res_work,opt_change)
      if (isatt(var_xx,"long_name") .and. isatt(var_xx,"units")) then
        res_work@tiXAxisString = var_xx@long_name+" ("+var_xx@units+")"
      end if
    end if

    if ( isatt(opt, "return_type") .and. opt@return_type .eq. "vtji2t_vt_avgji") then
      if (isatt(var_xx,"long_name") .and. isatt(var_xx,"units")) then
        res_work@tiXAxisString = var_xx@long_name+" ("+var_xx@units+")"
      end if

      change_var_atts("ksjdfkjsdkfjksd",var_yy,res_work,opt_change)  ; just change units so ser varname to a strange string
      if (isatt(var_yy,"units")) then
        res_work@tiYAxisString = var_yy@units
      end if

      if (isatt(var_yy,"var_name")) then
        string1 = str_join(var_yy@var_name,", ")
        res_work@gsnLeftString = string1
      end if
    end if

    if ( isatt(opt, "return_type") .and. opt@return_type .eq. "vtlji2vp_p_avgtji") then
      res_work@trYReverse = True
      res_work@tiYAxisString = "Pressure (hPa)"
      change_var_atts(varvars(0),var_xx,res_work,opt_change)

      if (isatt(var_xx,"units")) then
        res_work@tiXAxisString = var_xx@units
      end if

      if (isatt(var_xx,"var_name")) then
        string1 = str_join(var_xx@var_name,", ")
        res_work@gsnLeftString = string1
      end if
    end if

    ;**********************************
    ; x_coordinate = "12months_number"
    ;**********************************
    if ( isatt(opt, "x_coordinate") .and. opt@x_coordinate .eq. "12months_number") then
      delete(var_xx)
      var_xx = fspan(0.,11.,12)
      res_work@tmXBMode = "Explicit"
      res_work@tmXBValues = var_xx
      res_work@trXMinF = 0.
      res_work@trXMaxF = 11.
  
      res_work@xyMarkLineMode  = "MarkLines"  ; "Lines" "Markers"
      res_work@xyMonoMarker = True
      res_work@xyMarker = 4                   ; empty circle

      res_work@tiXAxisString = "Month"

      ;res_work@tmXBLabels = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
      res_work@tmXBLabels = (/"1","2","3","4","5","6","7","8","9","10","11","12"/)
    end if

    ;**********************************
    ; x_coordinate = "12months_1characters"
    ;**********************************
    if ( isatt(opt, "x_coordinate") .and. opt@x_coordinate .eq. "12months_1characters") then
      delete(var_xx)
      var_xx = fspan(0.,11.,12)
      res_work@tmXBMode = "Explicit"
      res_work@tmXBValues = var_xx
      res_work@trXMinF = 0.
      res_work@trXMaxF = 11.
  
      res_work@xyMarkLineMode  = "MarkLines"  ; "Lines" "Markers"
      res_work@xyMonoMarker = True
      res_work@xyMarker = 4                   ; empty circle

      res_work@tiXAxisString = "Month"

      res_work@tmXBLabels = (/"J","F","M","A","M","J","J","A","S","O","N","D"/)
    end if

    ;**********************************
    ; x_coordinate = "12months_3characters"
    ;**********************************
    if ( isatt(opt, "x_coordinate") .and. opt@x_coordinate .eq. "12months_3characters") then
      delete(var_xx)
      var_xx = fspan(0.,11.,12)
      res_work@tmXBMode = "Explicit"
      res_work@tmXBValues = var_xx
      res_work@trXMinF = 0.
      res_work@trXMaxF = 11.
  
      res_work@xyMarkLineMode  = "MarkLines"  ; "Lines" "Markers"
      res_work@xyMonoMarker = True
      res_work@xyMarker = 4                   ; empty circle

      res_work@tiXAxisString = "Month"

      res_work@tmXBLabels = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
    end if

    ;*************
    ; return res
    ;*************
    copy_VarAtts(res_work,res)

  end if  ; end if of update_res

;-----------------
; return variable
;-----------------
  opt@var_xx = var_xx
  opt@var_yy = var_yy

end
