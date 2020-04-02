#' @title  Make the required INList for VIC
#' @description Make the required list according to the format of the model VIC input list.
#' @import HMtools EDHM
#' @importFrom purrr map
#' @param infoStarDay chr, the start day, e.g. "1989-1-1"
#' @param infoEndDay chr, the end day, e.g. "1993-12-31"
#' @param ClsName chr, chr vector, all the methonds of hydrology modul. e.g.ClsNa <- c("VIC", "PenmanMonteith", "GreenAmpt", "Gash", "ARNO", "G2RES")
#' @param MetData list, num, list of metrol data. For VIC Model should contain at least the following 8 data:
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item Tmean
#'     \item Tmax
#'     \item Tmin
#'     \item WindSpeed
#'     \item WindH
#'     \item SunHour
#'     \item RelativeHumidity
#'     \item PrecipitationHoch
#'     }
#'     Each data is a 2-array(periodN, gridN).
#' @param GeoData list, geological data,  For VIC Model should contain at least the following 4 data:
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item Evalution: 1-array(gridN). Evalution of all grids.
#'     \item Location: data.frame:	gridN obs. of  3 variables. ID,
#'     \item SoilParam: data.frame':	gridN obs. of  some variables.
#'     \item LanduseParam: data.frame':	gridN obs. of  some variables.
#'     }
#'     Location field names, for VIC Model should contain at least the following 2 data:
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item Latitude
#'     \item Longitude
#'     }
#'     SoilParam field names, for VIC Model should contain at least the following 12 data
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item T_Porosity_: top soil...
#'     \item T_FieldCapacity_
#'     \item T_WiltingPoint_
#'     \item T_WettingFrontSoilSuctionHead_mm
#'     \item T_SaturatedSoilSuctionHead_mm
#'     \item T_SaturatedHydraulicConductivity_mm_day
#'     \item S_Porosity_: sub soil...
#'     \item S_FieldCapacity_
#'     \item S_WiltingPoint_
#'     \item S_WettingFrontSoilSuctionHead_mm
#'     \item S_SaturatedSoilSuctionHead_mm
#'     \item S_SaturatedHydraulicConductivity_mm_day
#'     }
#'     LanduseParam field names, for VIC Model should contain at least the following 28 data
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item SL_mm
#'     \item root_depth_mm
#'     \item rarc
#'     \item rmin
#'     \item ROU.JAN - ROU.DEC (*12)
#'     \item DIS.JAN - DIS.DEC (*12)
#'     }
#'
#' @param GridData 4-list of grid-data used to calculate confluence(route)
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item TypeGridID: 4-list. Type of each grid point.
#'     \item GridID: matrix:	ID in grid-data, be sure to mark: attr(GridID, "Null"), what the Null-Wert in grid-data.
#'     \item FlowDirection: matrix':	FlowDirection in grid-data, be sure to mark: attr(GridID, "Null"), what the Null-Wert in grid-data.
#'     \item GridDEM: matrix':	DEM in grid-data, be sure to mark: attr(GridID, "Null"), what the Null-Wert in grid-data.
#'     }
#'     TypeGridID: 4-list
#'     \itemize{
#'     \item The field names in these lists must be consistent and will be the only index.
#'     \item GridGridID    : all grid ID in 1-array(gridN)
#'     \item RiverGridID   : river grid ID in 1-array, Estuary and HydroStation belong to river
#'     \item EstuaryID     : Estuary ID in 1-array, HydroStation belong to Estuary
#'     \item HydroStationID: HydroStation ID in 1-array
#'     }
#' @param UPMethondList four methonds Name for IUH, e.g. c("Shipeng", "Shipeng", "Shipeng", "Shipeng")
#' @param UHPeriodN howmany Period have in Discrete unit hydrograph
#' @param UHUnitTranslate Transformation parameters from mm to m^3/s
#' @return InList for VIC
#' @examples
#' ClsNa <- c("VIC", "PenmanMonteith", "GreenAmpt", "Gash", "ARNO", "G2RES")
#' UPMethondList = list("Shipeng", "Shipeng", "Shipeng", "Shipeng")
#' VICInList <- InListMake_VIC("1989-1-1",
#'                             "1993-12-31",
#'                             ClsNa,
#'                             MetroList,
#'                             GeoList,
#'                             GridList,
#'                             UPMethondList,
#'                             180,
#'                             35)
#' @export
InListMake_VIC <- function(infoStarDay, ##*##
                           infoEndDay, ##*##
                           ClsName,
                           MetData,
                           GeoData,
                           GridData,
                           UPMethondList,
                           UHPeriodN = 180,
                           UHUnitTranslate){
  GridN = length(GridData$TypeGridID$GridGridID)
  DateDay = seq(as.Date(infoStarDay),as.Date(infoEndDay),1)
  PeriodN = length(DateDay)
  NDay = toNDayofYear(DateDay)
  MON = toMON(DateDay)
  message("The number of grid points(caculate from GridGridID) is: ", GridN, "\nThe number of periods is: ", PeriodN)
  testML <- as.data.frame(map(MetData, dim))
  if(any(testML[1,] != PeriodN)) stop("Make sure that the data of each field in the metrol data is a two-dimensional array or matrix of periodN * gridN.", "\n  ***now: ",
                                                            "\n", "PeriodN of ", names(MetData)[which(testML[1,] != PeriodN)]," is: ", testML[1,which(testML[1,] != PeriodN)])
  if(any(testML[2,] != GridN)) stop("Make sure that the data of each field in the metrol data is a two-dimensional array or matrix of periodN * gridN.", "\n  ***now: ",
                                                            "\n", "GridN of ", names(MetData)[which(testML[2,] != GridN)]," is: ", testML[2,which(testML[2,] != GridN)])
  testEL <- length(GeoData$Evalution)
  if(testEL != GridN) stop("Make sure that the data of each field in Evalution(in GeoData) is a one-dimensional array of gridN.", "\n  ***now is: ", testEL)
  testLC <- dim(GeoData$Location)[1]
  if(testLC != GridN) stop("Make sure that the data of each field in Location(in GeoData) is a data.frame:	gridN obs. (of  3 variables).", "\n  ***now is: ", testLC)
  testSP <- dim(GeoData$SoilParam)[1]
  if(testSP != GridN) stop("Make sure that the data of each field in SoilParam(in GeoData) is a data.frame:	gridN obs. (of  n variables).", "\n  ***now is: ", testSP)
  testLP <- dim(GeoData$LanduseParam)[1]
  if(testLP != GridN) stop("Make sure that the data of each field in LanduseParam(in GeoData) is a data.frame:	gridN obs. (of  n variables).", "\n  ***now is: ", testLP)
  testUM <- length(UPMethondList)
  if(testUM != 4) stop("In Vic at least 4 methonds to implement the UH, which can be the same 4 methonds", "\n  ***now is: ", testUM)

  testNL <- names(attributes(GridData$FlowDirection))
  if(!("Null" %in% testNL)) stop("Please add the \"Null\" attribute to the FlowDirection data, whose value is the representation value of the empty value of the FlowDirection data.")




  RETInList <- list(JDay = matrix(rep(NDay, GridN), PeriodN, GridN),
                    Elevation = matrix(rep(GeoData$Evalution, PeriodN), PeriodN, GridN, byrow = F) / 1000.0,
                    Latitude = matrix(rep(GeoData$Location$Latitude , PeriodN), PeriodN, GridN, byrow = F),
                    Tmean = MetData$Tmean,
                    Tmax = MetData$Tmax,
                    Tmin = MetData$Tmin,
                    WindSpeed = MetData$WindSpeed,
                    WindH = MetData$WindH,
                    SunHour = MetData$SunHour,
                    RelativeHumidity = MetData$RelativeHumidity)
  class(RETInList) <- ClsName
  RET <- ReferenceET(RETInList)

  LandP1Mat <- matrix(0.0,PeriodN, GridN)
  LandP2Mat <- matrix(0.0,PeriodN, GridN)
  ROU = paste("ROU",MON, sep = ".")
  DIS = paste("DIS", MON, sep = ".")
  for (i in 1:PeriodN) {
    LandP1Mat[i,] = GeoData$LanduseParam[[DIS[i]]]
    LandP2Mat[i,] = GeoData$LanduseParam[[ROU[i]]]
  }
  AerodynamicResistance <- fctAerodynamicResistance(LandP1Mat,
                                                    LandP2Mat,
                                                    MetData$WindSpeed)


  G2AimGAll <- fctG2AimGAll(GridData$TypeGridID,
                            GridData$GridID,
                            GridData$FlowDirection,
                            GridData$GridDEM)
  TransAll <- fctGTransMatAll(G2AimGAll, GridData$TypeGridID)

  return(list(GridN = GridN,
              PeriodN = PeriodN,
              ClsName = ClsName,
              RET = RET,
              PrecipitationHoch = MetData$PrecipitationHoch,
              T_Porosity_ = GeoData$SoilParam$T_Porosity_,
              AerodynamicResistance = AerodynamicResistance,
              ArchitecturalResistance = GeoData$LanduseParam$rarc,
              StomatalResistance = GeoData$LanduseParam$rmin,
              S_Porosity_ = GeoData$SoilParam$S_Porosity_,
              T_SaturatedHydraulicConductivity_mm_day = GeoData$SoilParam$T_SaturatedHydraulicConductivity_mm_day,
              T_WettingFrontSoilSuctionHead_mm = GeoData$SoilParam$T_WettingFrontSoilSuctionHead_mm,
              FieldCapacity = GeoData$SoilParam$T_FieldCapacity_,
              WiltingPoint = GeoData$SoilParam$T_WiltingPoint_,
              SaturatedSoilSuctionHead = GeoData$SoilParam$T_SaturatedSoilSuctionHead_mm,
              SaturatedHydraulicConductivity = GeoData$SoilParam$T_SaturatedHydraulicConductivity_mm_day,
              TypeGridID = GridData$TypeGridID,
              G2AimGAll = G2AimGAll,
              TransAll = TransAll,
              UPMethondList = UPMethondList,
              UHPeriodN = UHPeriodN,
              UHUnitTranslate = UHUnitTranslate))


}

#' @title Make the required PaList for VIC
#' @description  Make the required list according to the format of the model VIC paramters list.
#' @param ParamterModell vector of all to be calibrated paramters for VIC.
#' \itemize{
#'     \item ZoneDepth = ParamterModell[1:4],
#'     \item paCoefficientFreeThroughfall = ParamterModell[5],
#'     \item paExponentARNOBase = ParamterModell[6],
#'     \item paSoilMoistureVolumeARNOBaseThresholdRadio = ParamterModell[7],
#'     \item paDrainageLossMax = ParamterModell[8],##*## #mm
#'     \item paDrainageLossMin = ParamterModell[9], ##*## #mm
#'     \item paSoilMoistureCapacityB = ParamterModell[10],
#'     \item paInfiltrationRateB = ParamterModell[11],
#'     \item paClappHornbergerB = ParamterModell[12],
#'     \item UPPaList = ParamterModell[13:19]
#' }
#' @examples
#' ParamterMax = c(15, 300, 600, 900, 0.9, 2.7, 0.7, 30, 9, 1,1,15,7,7,7,10,10,10,15)
#' ParamterMin = c(5, 50, 100, 150, 0.1, 1.3, 0.1,9, 0.1, 0.1, 0.1, 3,0.5, 0.5, 0.5 , 0.5,0.5,0.5,0.5)
#' ParamterModell = 0.5 * (ParamterMax + ParamterMin)
#' VICPaList <- PaListMake_VIC(ParamterModell)
#' @return PaList for VIC
#' @export
PaListMake_VIC <- function(ParamterModell){
  return(list(ZoneDepth = ParamterModell[1:4],
              ### 1.1 interception #########
              paCoefficientFreeThroughfall = ParamterModell[5],
              ### 1.2 base flow ########
              paExponentARNOBase = ParamterModell[6],
              paSoilMoistureVolumeARNOBaseThresholdRadio = ParamterModell[7],
              paDrainageLossMax = ParamterModell[8],##*## #mm
              paDrainageLossMin = ParamterModell[9], ##*## #mm
              ### 1.3 Runoff ###########
              paSoilMoistureCapacityB = ParamterModell[10], ##*## ##b is the soil moisture capacity shape paeter, which is a measure of the spatial variability of the soil moisture capacity
              paInfiltrationRateB = ParamterModell[11],  ##*## ##b is the soil infiltrtionrate shape paeter,
              ### 1.4 ground water #######
              paClappHornbergerB = ParamterModell[12],
              UPPaList = ParamterModell[13:19]))
}
#' modell VIC
#' @import EDHM
#' @importFrom VectorTools maxSVector
#' @param VorIn please checke output of EDHMvic::InListMake_VIC()
#' @param VorPa please checke output of EDHMvic::PaListMake_VIC()
#' @return Q(flow) of hydrostation, m^3/s
#' @examples
#' ClsNa <- c("VIC", "PenmanMonteith", "GreenAmpt", "Gash", "ARNO", "G2RES")
#' UPMethondList = list("Shipeng", "Shipeng", "Shipeng", "Shipeng")
#' VICInList <- InListMake_VIC("1989-1-1",
#'                             "1993-12-31",
#'                             ClsNa,
#'                             MetroList,
#'                             GeoList,
#'                             GridList,
#'                             UPMethondList,
#'                             180,
#'                             35)
#' ParamterMax = c(15, 300, 600, 900, 0.9, 2.7, 0.7, 30, 9, 1,1,15,7,7,7,10,10,10,15)
#' ParamterMin = c(5, 50, 100, 150, 0.1, 1.3, 0.1,9, 0.1, 0.1, 0.1, 3,0.5, 0.5, 0.5 , 0.5,0.5,0.5,0.5)
#' ParamterModell = 0.5 * (ParamterMax + ParamterMin)
#' VICPaList <- PaListMake_VIC(ParamterModell)
#' Q <- MODELL_VIC(VICInList, VICPaList)
#' @export
MODELL_VIC <- function(VorIn, VorPa){
  ClsName = VorIn$ClsName
  GridN = VorIn$GridN
  PeriodN = VorIn$PeriodN
  UHPeriodN = VorIn$UHPeriodN
  UHUnitTranslate = VorIn$UHUnitTranslate
  ############### make a VIC M_VIC #############
  ###### 0.0 ZoneDepth Initialization #########
  VICZoneDepth <- list(Depth0 = rep(VorPa$ZoneDepth[1], GridN),
                       Depth1 = rep(VorPa$ZoneDepth[2], GridN),
                       Depth2 = rep(VorPa$ZoneDepth[3], GridN),
                       Depth3 = rep(VorPa$ZoneDepth[4], GridN))
  SoilPaGW <- list(Porosity = VorIn$T_Porosity_,
                   FieldCapacity = VorIn$T_FieldCapacity_,
                   WiltingPoint = VorIn$T_WiltingPoint_,
                   SaturatedSoilSuctionHead = VorIn$T_SaturatedSoilSuctionHead_mm,
                   SaturatedHydraulicConductivity = VorIn$T_SaturatedHydraulicConductivity_mm_day)
  VICSoilParam4GroundWater <- c(VICZoneDepth, SoilPaGW)
  ### 1.5 route #######
  UPIn <- list(list(VorIn$G2AimGAll[[1]][,3],
                    rep(VorPa$UPPaList[1],GridN),
                    rep(VorPa$UPPaList[4],GridN)),
               list(VorIn$G2AimGAll[[1]][,3],
                    rep(VorPa$UPPaList[2],GridN),
                    rep(VorPa$UPPaList[5],GridN)),
               list(VorIn$G2AimGAll[[2]][,3],
                    rep(VorPa$UPPaList[3],GridN),
                    rep(VorPa$UPPaList[6],GridN)),
               list(VorIn$G2AimGAll[[3]][,3],
                    rep(VorPa$UPPaList[3],GridN),
                    rep(VorPa$UPPaList[7],GridN)))
  UPMethondList <- VorIn$UPMethondList
  ###### 2. data need ###########
  ### 2.2 MoistureVolume #####
  M_VICMoistureVolume <- array(0.0, c(PeriodN, GridN, 4))
  M_VICMoistureVolume[1,,] <- 0.5 * matrix(rep(VorPa$ZoneDepth[1:4], GridN), GridN, byrow = T)
  ### 2.3 data for ROUTE ###########
  M_VICRoutSurFlow = M_VICRoutBasFlow <- array(0.0, c(PeriodN, GridN))
  ###### 3. run ###########
  message("VIC star\n")

  for (M_VIC_i in 2:PeriodN){

    ### 3.1 evaptranspiration ############
    ETIn <- list(RET = VorIn$RET[M_VIC_i], #1
                 PrecipitationHoch = VorIn$PrecipitationHoch[M_VIC_i,], #2
                 MoistureVolume = M_VICMoistureVolume[(M_VIC_i-1),,1],
                 MoistureCapacityMax = VICZoneDepth$Depth0,
                 MoistureVolume1 = M_VICMoistureVolume[(M_VIC_i-1),,2] + M_VICMoistureVolume[(M_VIC_i-1),,3],
                 MoistureCapacityMax1 = (VICZoneDepth$Depth1 + VICZoneDepth$Depth2) * VorIn$T_Porosity_,
                 AerodynamicResistance = VorIn$AerodynamicResistance[M_VIC_i], #4
                 ArchitecturalResistance = VorIn$ArchitecturalResistance, #5
                 StomatalResistance = VorIn$StomatalResistance) #6
    class(ETIn) <- ClsName
    ETOut <- ET(ETIn, VorPa)

    ### 3.2 interception ###############
    ICIn <- list(Volum = M_VICMoistureVolume[(M_VIC_i-1),,1],
                 CanopyStorageCapacity = VICZoneDepth$Depth0,
                 RainfallDuringSaturation = VorIn$PrecipitationHoch[M_VIC_i,],
                 Evaporation = ETOut$EvapC)
    class(ICIn) <- ClsName
    InterceptionOut <- INTERCEPTION(ICIn, VorPa)

    ### 3.3 baseflow #########
    BFIn <- list(SoilMoistureVolume = M_VICMoistureVolume[(M_VIC_i-1),,4],
                 SoilMoistureVolumeMax = VICZoneDepth$Depth3 * VorIn$S_Porosity_)
    class(BFIn) <- ClsName
    BaseflowOut = BASEFLOW(BFIn, VorPa)

    ### 3.4 runoff ############
    IFIn <- list(HydraulicConductivity = VorIn$T_SaturatedHydraulicConductivity_mm_day,
                 WettingFrontSoilSuction = VorIn$T_WettingFrontSoilSuctionHead_mm,
                 SoilMoistureContent = M_VICMoistureVolume[(M_VIC_i-1),,2] / VICZoneDepth$Depth1,
                 EffectivePorosity = VorIn$T_Porosity_,
                 SoilMoistureVolume = M_VICMoistureVolume[(M_VIC_i-1),,2])
    class(IFIn) <- ClsName
    InfiltrationRateMax = fctInfiltration(IFIn)

    ROIn <- list(PrecipitationHoch = VorIn$PrecipitationHoch[M_VIC_i,] - InterceptionOut,
                 SoilMoistureCapacityMax = (VorPa$paSoilMoistureCapacityB + 1) *
                   (VICZoneDepth$Depth1 + VICZoneDepth$Depth2) * VorIn$T_Porosity_,
                 SoilMoistureVolume = maxSVector(0.0,M_VICMoistureVolume[(M_VIC_i-1),,2] + M_VICMoistureVolume[(M_VIC_i-1),,3]),
                 InfiltrationRateMax = InfiltrationRateMax)
    class(ROIn) <- ClsName
    RunoffOut <- RUNOFF(ROIn, VorPa)

    ### 3.5 ground water replan / interflow ##########
    GroundWaterIn <- M_VICMoistureVolume[M_VIC_i - 1,,]

    GWIn <- list(Evapotranspiration = ETOut,
                 Interception = InterceptionOut,
                 Infiltration = RunoffOut$Infiltration,
                 BaseFlow = BaseflowOut,
                 GroundWaterIn = GroundWaterIn,
                 GridSoilParame = VICSoilParam4GroundWater)
    class(GWIn) <- ClsName
    GroundwaterOut <- GROUNDWATER(GWIn, VorPa)

    M_VICMoistureVolume[M_VIC_i,,] <- GroundwaterOut
    ### 3.6 for route ###############
    M_VICRoutSurFlow[M_VIC_i,] <- RunoffOut$Runoff
    M_VICRoutBasFlow[M_VIC_i,] <- BaseflowOut
  }

  ### 3.7 IUH ############
  UParamAll <- fctMakeUHParamAll(UPIn, UPMethondList)

  M_VICUHAll <- fctUHALLMake(UParamAll, UHPeriodN)
  class(M_VICUHAll) <- ClsName
  ### 3.8 CONFLUENCE #######
  M_VICRoutSurFlow <- M_VICRoutSurFlow * UHUnitTranslate
  M_VICRoutBasFlow <- M_VICRoutBasFlow * UHUnitTranslate
  M_VICStationFlowQ <- CONFLUENCE(list(M_VICRoutSurFlow, M_VICRoutBasFlow), M_VICUHAll, VorIn$TypeGridID, VorIn$TransAll)
  ###### 4. return ############
  return(M_VICStationFlowQ)
}

