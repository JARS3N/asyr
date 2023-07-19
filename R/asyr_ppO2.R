partial_pressure_ox <- Vectorize(function(TC, atm = 760) {
#  i <- (TC >= 30) + 1
#  coef <- c(0.678, 0.827)[i]
#  adj <- c(35, 49)[i]
#  0.005625 * (-0.0000058333 * TC ^ 3 + 0.0001821 * TC ^ 2 + 0.072405 * TC + 2.5443) * atm * 
#    (atm - (0.0456 * TC ^ 2 - 0.8559 * TC + 16.509)) * coef * (adj + TC)^-1
  message("Replace partial_pressure_ox with pp::O2")
  pp::O2(TC,atm)
})
