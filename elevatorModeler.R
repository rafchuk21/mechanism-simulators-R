motors <- data.frame("Redline"=c(18730,.71, 134,.7, 347,  .8,  433.75),
                     "BAG"=c(13180,.43, 53, 1.8,149,  .71, 209.86),
                     "CIM"=c(5330, 2.41,131,2.7,337.81,2.82,119.79),
                     "MiniCIM"=c(5840, 1.41,89, 3,  215,   2.16,99.54),
                     row.names = c("Free Speed (RPM)","Stall Torque (Nm)","Stall Current (Amp)","Free Current (Amp)","Power (W)","Weight (lb)", "Ratio (W/lb)")
)

nmToInLb <- 8.85074579

#All units are inches, pounds, and seconds
modelThreeStageContinuous <- function(motor, numMotors, gearing, pulleyDiameter, voltage, velEfficiency, torqueEfficiency, 
                                      staticFriction, carriageWeight, carriageRun, smallStageWeight, smallStageRun, mediumStageWeight,
                                      mediumStageRun, deltaTime=0.001, startHeight = 0) {
  motorForce <- motors["Stall Torque (Nm)", motor]*nmToInLb*numMotors*gearing*torqueEfficiency/(pulleyDiameter/2) #pound force
  motorResistance <- 12/motors["Stall Current (Amp)", motor] #ohms
  twelveVoltSpeed <- motors["Free Speed (RPM)", motor]/60/gearing*pi*pulleyDiameter*velEfficiency #Inches/sec
  kV <- 12/twelveVoltSpeed #volts/(inch/sec)
  weights <- c(carriageWeight, carriageWeight + smallStageWeight, carriageWeight + smallStageWeight + mediumStageWeight) #pound mass
  constantForces <- weights+staticFriction #pound force
  constantVoltages <- 12*(constantForces/motorForce) #Volts
  maxAccels <- motorForce/weights #Inches/sec^2, F = ma -> a = F/m
  kAs <- 12/maxAccels # volts(inch/(sec^2))
  heights <- c(carriageRun, carriageRun+smallStageRun, carriageRun+smallStageRun+mediumStageRun) #feet
  
  #Lookup functions
  lookupKa <- function(pos){
    if (pos < heights[1]){
      return(kAs[1])
    } else{
      if (pos < heights[2]){
        return(kAs[2])
      } else {
        return(kAs[3])
      }
    }
  }
  lookupConstantVoltage <- function(pos){
    if (pos < heights[1]){
      return(constantVoltages[1])
    } else{
      if (pos < heights[2]){
        return(constantVoltages[2])
      } else {
        return(constantVoltages[3])
      }
    }
  }
  
  #Load in starting values
  output <- data.frame("time"=0,"pos"=startHeight,"vel"=0, "constantVoltage"=lookupConstantVoltage(startHeight), "velVoltage"=0,
                       "accelVoltage"=(voltage-lookupConstantVoltage(startHeight)), "accel"=(voltage-lookupConstantVoltage(startHeight))/lookupKa(startHeight))
  count <- 1
  
  #Exit if we're at the top, if we're going down, or if it's been a minute
  while(output$pos[count] < heights[3] & output$pos[count] >= 0 & output$time[count] < 60){
    newTime <- output$time[count] + deltaTime
    newPos <- output$pos[count] + deltaTime*output$vel[count]
    newVel <- output$vel[count] + deltaTime*output$accel[count]
    newConstantVoltage <- lookupConstantVoltage(newPos)
    newVelVoltage <- kV*newVel
    newAccelVoltage <- voltage - newConstantVoltage - newVelVoltage
    newAccel <- newAccelVoltage/lookupKa(newPos)
    count <- count+1
    output[count,] <- c(newTime, newPos, newVel, newConstantVoltage, newVelVoltage, newAccelVoltage, newAccel)
  }
  
  plot(output$time, output$accel, t="l")
  print(paste("Took",round(output$time[length(output$time)], 1-log(deltaTime, 10)),"seconds to go to the top."))
  return(output)
}

output <- modelThreeStageContinuous("Redline", 1, 30, 2.2560, 10.5, 0.9,0.9,10,11.5,23.25,3.484,25.375,4.168,26.875)