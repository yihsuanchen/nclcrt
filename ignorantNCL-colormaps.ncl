;*****************************************************
;  NCL program developed by ignorant
;  
;  History:
;    2014/04/03  ADD function "ignorant_define_colormap"
;
;  Yi-Hsuan Chen, RCEC, Taiwan
;*****************************************************

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;  Content of ignorant NCL program
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
; load "/home/chishin/ncarg/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "./ignorantNCL.ncl"  ; MUST CONTAIN "ignorant_select_colormap"
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
; function ignorant_define_colormap(cmap_in:string, cmap_index[*]:integer)
;          select colors in user-defiend colormap
;          cmap_in    : user-defined colormap name, ex: "testcmap"
;          cmap_index : selected color indexes in cmap_in, ex: (/1,2,3,7,8/)
;                       .eq. 999 - means select all colors
;
;          valid colormaps (2014/09/15 update): 
;            "testcmap" 	: test of colormap
;            "rainbow_white" 	: white + rainbow colors
;            "blue_red"		: blue + red colors
;            "blue_orange"	: blue + orange + red colors
;            "rainbow"	 	: rainbow colors
;            "cold_colors"      : light  green + blue + purple colors 
;            "cold_colors2"     : strong green + blue + purple colors
;            "green_blue"       : light green + strong blue
;            "warm_colors"      : yellow + orange + red colors
;            "cold_warm"        : light blue + green + yellow + orange + red colors
;            "GrYlRd"           : green + yellow + red
;            "WhBlGrYlRd"       : white + blue + green + yellow + red
;            "MiYlOrRdPk"       : milk + yellow + orange + red + pink
;
;!!!!!!!!!!!!!!!!!!!!!!
;  Program Code
;!!!!!!!!!!!!!!!!!!!!!!

;######################################################
undef("ignorant_define_colormap")
function ignorant_define_colormap(cmap_in:string, cmap_index[*]:integer)

begin

;----------------------
; check input argument
;----------------------

;*** check input colormap name ***
  if ( isstring(cmap_in) .and. dimsizes(cmap_in) .eq. 1 ) then
    cmap_work = cmap_in
  else
    print("")
    print("WARNING: invalid input colormap name [" + cmap_in + "]")
    print("Set to default color map")
    print("")
  end if

;*** check input colormap index ***
  if ( isint(cmap_index) .and. dimsizes(dimsizes(cmap_index)) .eq. 1) then
    index_work = cmap_index
  else
    print("")
    print("WARNING: invalid input colormap included indexes [" + cmap_index + "]")
    print("Set to default value")
    print("")
    index_work = 999
  end if

;--------------------------
; set predifined colormap
;--------------------------

  if ( cmap_work .eq. "testcmap" ) then
    cmap1 = ignorant_select_colormap("amwg",999,False)
    cmap2 = ignorant_select_colormap("default",(/3,5,6,7,8/),False)

    cmap_all = ignorant_combine_colormaps(cmap1,cmap2,1)

  else if ( cmap_work .eq. "rainbow_white" ) then
    cmap1 = ignorant_select_colormap("amwg",(/0,2,3,5,6,7,8,9,12,13,15/),False)
    cmap_all = cmap1

  else if ( cmap_work .eq. "blue_red" ) then
    cmap1 = ignorant_select_colormap("BlRe",(/2/),False)
    cmap2 = ignorant_select_colormap("BlueRed",(/2,31,69,95,111,0,128,145,193,225/),False)
    cmap3 = ignorant_select_colormap("BlRe",(/90,97/),False)

    cmap12  = ignorant_combine_colormaps(cmap1,cmap2,1)
    cmap123 = ignorant_combine_colormaps(cmap12,cmap3,1)
    cmap_all = cmap123

  else if ( cmap_work .eq. "cold_colors" ) then
    cmap1 = ignorant_select_colormap("precip_diff_12lev",(/8,9,10,11/),False)
    cmap2 = ignorant_select_colormap("temp_diff_18lev",(/9/),False)
    cmap3 = ignorant_select_colormap("precip_diff_12lev",(/12,13/),False)
    cmap4 = ignorant_select_colormap("BlueGreen14",(/6/),False)
    cmap5 = ignorant_select_colormap("StepSeq25",(/25,24,23/),False)

    cmap12 = ignorant_combine_colormaps(cmap1,cmap2,1)
    cmap123 = ignorant_combine_colormaps(cmap12,cmap3,1)
    cmap1234 = ignorant_combine_colormaps(cmap123,cmap4,1)
    cmap12345 = ignorant_combine_colormaps(cmap1234,cmap5,1)

    cmap_all = cmap12345

  else if ( cmap_work .eq. "cold_colors2" ) then
    cmap1 = ignorant_select_colormap("MPL_Greens",(/0,33,75,97/),False)
    cmap2 = ignorant_select_colormap("MPL_Blues",(/50,81,113/),False)
    cmap3 = ignorant_select_colormap("MPL_Purples",(/50,81,113/),False)

    cmap12  = ignorant_combine_colormaps(cmap1,cmap2,1)
    cmap123 = ignorant_combine_colormaps(cmap12,cmap3,1)

    cmap_all = cmap123

  else if ( cmap_work .eq. "green_blue" ) then
    cmap1 = ignorant_select_colormap("CBR_wet",(/2,4,5,6,7,8,9,10,11,12/),False)

    cmap_all = cmap1

  else if ( cmap_work .eq. "warm_colors" ) then
    cmap1 = ignorant_select_colormap("sunshine_9lev",(/2,3,4,5,6,7,8,11/),False)
    cmap2 = ignorant_select_colormap("MPL_gist_heat",(/45,18/),False)

    cmap12  = ignorant_combine_colormaps(cmap1,cmap2,1)

    cmap_all = cmap12

  else if ( cmap_work .eq. "cold_warm" ) then
    cmap1 = ignorant_select_colormap("WhiteBlueGreenYellowRed",                           (/17,33,49,65,81,97,113,129,145,161,177,209,255/),False)

    cmap_all = cmap1

  else if ( cmap_work .eq. "brown_green" ) then

    cmap1 = ignorant_select_colormap("OceanLakeLandSnow",(/225,208,191,174,157,140,123,0, \
                                                         81,66,29,18,4/),False)
    cmap2 = ignorant_select_colormap("BlueDarkOrange18",(/2/),False)
    cmap3 = ignorant_select_colormap("precip_diff_12lev",(/14/),False)

    cmap12 = ignorant_combine_colormaps(cmap1,cmap2,1)
    cmap123 = ignorant_combine_colormaps(cmap12,cmap3,1)

    cmap_all = cmap123

  else if ( cmap_work .eq. "blue_orange" ) then
    cmap1 = ignorant_select_colormap("BlueDarkRed18",(/4,5,6,7,9,10, 0, \
                                                      12,13,14,15,16,19/),False)
    cmap_all = cmap1

  else if ( cmap_work .eq. "BlWhOrRd" ) then
    cmap1 = ignorant_select_colormap("posneg_2",(/5,6,8,9,10, \
                                                  2, \
                                                  14,15,16,18,21 /),False)
    cmap_all = cmap1

  else if ( cmap_work .eq. "rainbow" ) then
    cmap1 = ignorant_select_colormap("rainbow",(/2,17,49,65,97, \
                                                 136,145,161,168,189/),False)

    cmap_all = cmap1

  else if ( cmap_work .eq. "GrYlRd" ) then
    cmap1 = ignorant_select_colormap("MPL_RdYlGn",(/0,129,119,109,99,89,79,69,59,49,39,29,19,10,2/),False)

    cmap_all = cmap1

  else if ( cmap_work .eq. "WhBlGrYlRd" ) then
    cmap1 = ignorant_select_colormap("WhiteBlueGreenYellowRed",(/2,10,17,33,49,65,81, \
                                                                 97,113,129, \
                                                                 145,161,177, \
                                                                 193,225,255/) ,False)
    cmap_all = cmap1
  
  else if ( cmap_work .eq. "MiYlOrRdPk" ) then

    cmap1 = ignorant_select_colormap("amwg256",(/  0,129,137,145,150, \
                                                 156,161,169,178,185, \
                                                 193,202,210,218,226, \
                                                 234,242,249,255/), False)

    cmap_all = cmap1

  else if ( cmap_work .eq. "BrWhGr" ) then
    cmap1 = ignorant_select_colormap("CBR_drywet",(/2,3,4,5,6, \
                                                    0,0, \
                                                    8,9,10,11,12/),False)

    cmap_all = cmap1

  else if ( cmap_work .eq. "BlRd_light" ) then
    cmap1 = ignorant_select_colormap("BlueRed",(/2,20,38,56,74,92,108,126, \
                                                 0, \
                                                 128,146,164,182,200,218,236,253/),False)

    cmap_all = cmap1

  else
    print("")
    print("WARNING: Not find input colormap name [" + cmap_in + "]")
    print("Set to NCL default color map")
    print("")

    cmap_all = ignorant_select_colormap("default",999,False)

  end if  ; end BlRd_light 
  end if  ; end BrWhGr 
  end if  ; end MiYlOrRdPk  
  end if  ; end WhBlGrYlRd
  end if  ; end GrYlRd
  end if  ; end rainbow
  end if  ; end BlWhOrRd
  end if  ; end blue_orange
  end if  ; end brown_green
  end if  ; end cold_warm
  end if  ; end warm_colors
  end if  ; end green_blue
  end if  ; end cold_colors2
  end if  ; end cold_colors
  end if  ; end blue_red
  end if  ; end rainbow_white
  end if  ; end testcmap

;-----------------------
; set return color map
;-----------------------

; dimension of cmap_all
  dim_cmapall = dimsizes(cmap_all(:,0))

; dimension of index_work
  dim_out = dimsizes(index_work)

; use all colors
  if ( dim_out .eq. 1 .and. index_work .eq. 999) then
    cmap_out = cmap_all

; use several colors
  else

    cmap_out = new((/dim_out,3/),"float")

    do i=0,dim_out-1

      idx = index_work(i)

      if ( idx .ge. 0 .and. idx .lt. dim_cmapall ) then
        cmap_out(i,:) = cmap_all(idx,:)

      else
        cmap_out(:,:) = 0.
        print("")
        print("WARNING: input colormap index is too large  [ in:" + idx + " > max:" + dim_cmapall + " ]")
        print("Could not create return colormap")
        print("")
        return (cmap_out)
      end if

    end do

  end if

  return(cmap_out)

end

