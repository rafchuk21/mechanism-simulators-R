motors <- data.frame("Redline"=c(18730,.71, 134,.7, 347,  .8,  433.75),
                     "BAG"=c(13180,.43, 53, 1.8,149,  .71, 209.86),
                     "CIM"=c(5330, 2.41,131,2.7,337.81,2.82,119.79),
                     "MiniCIM"=c(5840, 1.41,89, 3,  215,   2.16,99.54),
                     row.names = c("Free Speed (RPM)","Stall Torque (Nm)","Stall Current (Amp)","Free Current (Amp)","Power (W)","Weight (lb)", "Ratio (W/lb)")
)

nmToInLb = 8.85074579

#All units are inches, pounds, and seconds
modelThreeStageContinuous <- function(motor, numMotors, gearing, pulleyDiameter, voltage, velEfficiency, torqueEfficiency, 
                                      staticFriction, carriageWeight, carriageRun, smallStageWeight, smallStageRun, mediumStageWeight,
                                      mediumStageRun, deltaTime=0.0001, startHeight = 0) {
  motorForce = motors["Stall Torque (Nm)", motor]*nmToInLb*numMotors*gearing*torqueEfficiency/(pulleyDiameter/2) #pound force
  motorResistance = 12/motors["Stall Current (Amp)", motor] #ohms
  twelveVoltSpeed = motors["Free Speed (RPM)", motor]/60/gearing*pi*pulleyDiameter*velEfficiency #Inches/sec
  kV = 12/twelveVoltSpeed #volts/(inch/sec)
  weights = c(carriageWeight, carriageWeight + smallStageWeight, carriageWeight + smallStageWeight + mediumStageWeight) #pound mass
  constantForces = weights+staticFriction #pound force
  constantVoltages = 12*(constantForces/motorForce) #Volts
  maxAccels = motorForce/weights #Inches/sec^2, F = ma -> a = F/m
  kAs = 12/maxAccels # volts(inch/(sec^2))
  heights = c(carriageRun, carriageRun+smallStageRun, carriageRun+smallStageRun+mediumStageRun) #feet
}

modelThreeStageContinuous("Redline", 1, 30, 2.2560, 10.5, 0.9,0.9,10,11.5,23.25,3.484,25.375,4.168,26.875)