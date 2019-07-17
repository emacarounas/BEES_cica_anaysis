qair_to_vpd <- function(qair, tair, press) {
  #
  #   Convert Qair to VPD
  #
  #   Args:
  #   -----
  #   Qair : float
  #     specific humidity [kg kg-1]
  #   tair : float
  #     air temperature [deg C]
  #   press : float
  #     air pressure [Pa]
  #
  #   Returns:
  #   --------
  #     vpd : float
  #       vapour pressure deficit [kPa]
  #
  
  PA_TO_KPA = 0.001
  
  # saturation vapor pressure
  es = 100.0 * 6.112 * exp((17.67 * tair) / (243.5 + tair))
  
  # vapor pressure
  ea = (qair * press) / (0.622 + (1.0 - 0.622) * qair)
  
  vpd = (es - ea) * PA_TO_KPA
  
  return (vpd) 

}