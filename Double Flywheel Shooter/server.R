#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
source("multiplot.R")

shinyServer(function(input, output) {
  
  results <- reactive({
    returnedTable <- model(motor=input$motor, numMotors=input$numMotors, sourceVoltage=input$sourceVoltage, gearing=input$gearing,
                           wheelDiameter=input$wheelDiameter, robotResistance=input$robotResistance, velEfficiency=input$velEfficiency/100,
                           torqueEfficiency=input$torqueEfficiency/100, resistiveTorque=input$resistiveTorque, deltaTime=input$deltaTime,
                           setpoint=input$setpoint, projMass=input$projMass, wheelMOI=input$wheelMOI)
    return(returnedTable)
  })

  output$eulerTable <- renderTable({
    return(results())
  })
  
  output$outputUI <- renderUI({
    returnedTable <- results()
    
    vel <- renderPlot({plot(returnedTable$time, returnedTable$vel, xlab="Time (seconds)", ylab="Angular Velocity (RPM)", main="Angular Velocity over time", t="l")})
    accel <- renderPlot({plot(returnedTable$time, returnedTable$accel, xlab="Time (seconds)", ylab="Acceleration (RPM/sec)", main="Acceleration over time", ylim=c(min(0,returnedTable$accel),max(returnedTable$accel)), t="l")})
    appVoltage <- renderPlot({plot(returnedTable$time, returnedTable$appVoltage, xlab="time (seconds)", ylab="Applied Voltage (volts)", main="Applied Voltage over time", ylim=c(0,max(returnedTable$appVoltage)), t="l")})
    current <- renderPlot({plot(returnedTable$time, returnedTable$current, xlab="Time (seconds)", ylab="Current (amps)", main="Current draw per motor over time", ylim=c(0,max(returnedTable$current)), t="l")})
    velVoltage <- renderPlot({plot(returnedTable$time, returnedTable$velVoltage, xlab="Time (seconds)", ylab="Back-EMF voltage (volts)", main="Velocity voltage over time", ylim=c(min(0,returnedTable$velVoltage),max(returnedTable$velVoltage)), t="l")})
    accelVoltage <- renderPlot({plot(returnedTable$time, returnedTable$accelVoltage, xlab="Time (seconds)", ylab="Acceleration voltage (volts)", main="Acceleration voltage over time", ylim=c(min(0,returnedTable$accelVoltage),max(returnedTable$accelVoltage)), t="l")})
    sysVoltage <- renderPlot({plot(returnedTable$time, returnedTable$sysVoltage, xlab="Time (seconds)", ylab="System Voltage (volts)", main="System voltage over time", ylim=c(min(0,returnedTable$sysVoltage),max(returnedTable$sysVoltage)), t="l")})
    
    return(list(current, vel, accel, appVoltage, velVoltage, accelVoltage, sysVoltage))
  })
  
  ggplots <- function() {
    returnedTable <- results()
    
    theme_update(plot.title = element_text(size = (14), face = "bold", hjust = 0.5), axis.title = element_text(size = (12)))
    
    current <- ggplot(returnedTable, aes(x=time, y=current)) + geom_line() + xlab("Time (s)") + ylab("Current (A)") + ggtitle("Current (A) per Motor vs Time (s)")
    vel <- ggplot(returnedTable, aes(x=time, y=vel)) + geom_line() + xlab("Time (s)") + ylab("Flywheel Speed (RPM)") + ggtitle("Flywheel Speed (RPM) vs Time (s)")
    accel <- ggplot(returnedTable, aes(x=time, y=accel)) + geom_line() + xlab("Time (s)") + ylab("Flywheel Acceleration (RPM/s)") + ggtitle("Flywheel Acceleration (RPM/s) vs Time (s)")
    volYAxis <- scale_y_continuous(limits = c(0,12), breaks = c(0,3,6,9,12))
    appVoltage <- ggplot(returnedTable, aes(x=time, y=appVoltage)) + geom_line() + xlab("Time (s)") + ylab("Applied Voltage (V)") + ggtitle("Applied Voltage (V) vs Time (s)") + volYAxis
    velVoltage <- ggplot(returnedTable, aes(x=time, y=velVoltage)) + geom_line() + xlab("Time (s)") + ylab("Velocity Voltage/Back EMF (V)") + ggtitle("Velocity Voltage (V) vs Time (s)") + volYAxis
    accelVoltage <- ggplot(returnedTable, aes(x=time, y=accelVoltage)) + geom_line() + xlab("Time (s)") + ylab("Acceleration Voltage (V)") + ggtitle("Acceleration Voltage (V) vs Time (s)") + volYAxis
    sysVoltage <- ggplot(returnedTable, aes(x=time, y=sysVoltage)) + geom_line() + xlab("Time (s)") + ylab("System Voltage (V)") + ggtitle("System Voltage (V) vs Time (s)") + volYAxis
    p <- list("current" = current, 'vel' = vel, 'accel' = accel, 'appVoltage' = appVoltage, 'velVoltage' = velVoltage, 'accelVoltage' = accelVoltage, 'sysVoltage' = sysVoltage)
    return(p)
  }
  
  output$current <- renderPlot({
    return(ggplots()$current)
  })
  
  output$vel <- renderPlot({
    return(ggplots()$vel)
  })
  
  output$accel <- renderPlot({
    return(ggplots()$accel)
  })
  
  output$appVoltage <- renderPlot({
    return(ggplots()$appVoltage)
  })
  
  output$velVoltage <- renderPlot({
    return(ggplots()$velVoltage)
  })
  
  output$accelVoltage <- renderPlot({
    return(ggplots()$accelVoltage)
  })
  
  output$sysVoltage <- renderPlot({
    return(ggplots()$sysVoltage)
  })
  
})

motors <- data.frame("Redline"=c(18730,.71, 134,.7, 347,  .8,  433.75),
                     "BAG"=c(13180,.43, 53, 1.8,149,  .71, 209.86),
                     "CIM"=c(5330, 2.41,131,2.7,337.81,2.82,119.79),
                     "MiniCIM"=c(5840, 1.41,89, 3,  215,   2.16,99.54),
                     "NEO" = c(5880, 3.36, 166, 1.3, 516, 0.94, 548.9),
                     "Falcon" = c(6380, 4.69, 257, 1.5, 783, 1.1, 711.8),
                     row.names = c("Free Speed (RPM)","Stall Torque (Nm)","Stall Current (Amp)","Free Current (Amp)","Power (W)","Weight (lb)", "Ratio (W/lb)")
)

nmToInLb <- 8.85074579
gsToInchPerSecSquared <- 386.09
RPMToRadPerSec <- 2*pi/60
lbmInSqToKgMSq <- 0.0002926
ftToM <- 0.3048
lbToKg <- 0.453592
timeLimit <- 10

model <- function(motor, numMotors, sourceVoltage, gearing, wheelDiameter, robotResistance, velEfficiency, 
                       torqueEfficiency, resistiveTorque, deltaTime, setpoint, projMass, wheelMOI) {

  # Convert wheel diameter from inches to meters
  wheelDiameter <- wheelDiameter / 12 * ftToM #m
  # Convert projectile mass from lbm to kg
  projMass <- projMass * lbToKg #kg
  # Convert wheel MOI from lbm*in^2 to kg*m^2
  wheelMOI <- wheelMOI * lbmInSqToKgMSq # kg*m^2
  
  motorTorque <- motors["Stall Torque (Nm)", motor]*numMotors*gearing*torqueEfficiency #N*m
  motorResistance <- 12/motors["Stall Current (Amp)", motor] #ohms
  sysVoltage = sourceVoltage/(1+numMotors*(robotResistance/motorResistance))
  twelveVoltSpeed <- motors["Free Speed (RPM)", motor]/gearing #RPM
  kV <- 12/twelveVoltSpeed #volts/RPM
  constantVoltage <- 12*resistiveTorque/motorTorque #Volts
  maxAngularAccel <- motorTorque / wheelMOI / RPMToRadPerSec #RPM/s^2
  kA <- 12/maxAngularAccel
  
  print(kA)
  
  targetVel <- setpoint
  
  startVel <- 0
  
  startV <- currentLimit(0, 60, startVel, kV, motorResistance)
  
  #Load in starting values
  output <- data.frame("time"=0,"vel"=startVel, "velVoltage"=kV*startVel,
                       "accelVoltage"=(startV-constantVoltage-kV*startVel), "accel"=(startV-constantVoltage-kV*startVel)/kA,
                       "current"=startV/motorResistance, "sysVoltage"=sysVoltage, "appVoltage" = startV)
  count <- 1
  
  # Exit if stabilized at target vel or timeLimit is reached
  while(!(output$accel[count] < 0.1 && abs(output$vel[count]-targetVel) < 10) && output$time[count] < timeLimit){
    err <- targetVel - output$vel[count]
    Vdesired <- currentLimit(err * 0.005 + constantVoltage + kV*output$vel[count], 60, output$vel[count], kV, motorResistance)
    newData <- step(Vdesired, output, numMotors, constantVoltage, kV, kA, motorResistance, robotResistance, sourceVoltage, deltaTime)
    count <- count+1
    output[count,] <- newData
  }
  
  # Fire one ball
  
  newSysVoltage <- sourceVoltage - output$current[count]*numMotors*robotResistance
  newAppVoltage <- min(12, newSysVoltage)
  newTime <- output$time[count] + deltaTime
  
  newVel <- output$vel[count]*sqrt(wheelMOI/(projMass*(wheelDiameter/2)^2+wheelMOI))
  
  newVelVoltage <- kV*newVel
  newAccelVoltage <- newAppVoltage - constantVoltage - newVelVoltage
  newAccel <- newAccelVoltage/kA
  newCurrent <- abs(constantVoltage+newAccelVoltage)/motorResistance
  
  count <- count + 1
  output[count,] <- c(newTime, newVel, newVelVoltage, newAccelVoltage, newAccel, newCurrent, newSysVoltage, newAppVoltage)
  
  # Exit if stabilized at target vel or timeLimit is reached
  while(!(output$accel[count] < 0.1 && abs(output$vel[count]-targetVel) < 10) && output$time[count] < timeLimit){
    err <- targetVel - output$vel[count]
    Vdesired = currentLimit(err * 0.005 + constantVoltage + kV*output$vel[count], 60, output$vel[count], kV, motorResistance)
    newData <- step(Vdesired, output, numMotors, constantVoltage, kV, kA, motorResistance, robotResistance, sourceVoltage, deltaTime)
    count <- count+1
    output[count,] <- newData
  }
  
  return(output)
}

step <- function(Vin, currVals, numMotors, constantVoltage, kV, kA, motorResistance, robotResistance, sourceVoltage, deltaTime) {
  newSysVoltage <- sourceVoltage - tail(currVals$current, n=1)*numMotors*robotResistance
  newAppVoltage <- min(Vin, newSysVoltage)
  newTime <- tail(currVals$time, n=1) + deltaTime
  newVel <- tail(currVals$vel, n=1) + deltaTime*tail(currVals$accel, n=1)
  newVelVoltage <- kV*newVel
  newAccelVoltage <- newAppVoltage - constantVoltage - newVelVoltage
  newAccel <- newAccelVoltage/kA
  newCurrent <- abs(constantVoltage+newAccelVoltage)/motorResistance
  newVals <- c(newTime, newVel, newVelVoltage, newAccelVoltage, newAccel, newCurrent, newSysVoltage, newAppVoltage)
  
  return(newVals)
}

currentLimit <- function(desiredVoltage, currentLimit, velocity, kV, motorResistance) {
  # I = (kC + Va) / Rm
  # I = (kC + Vapp - kC - Vv) / Rm
  # I = (Vapp - kV*v) / Rm
  # Vapp = I*Rm + kV*v
  
  v <- min(desiredVoltage, currentLimit*motorResistance + kV*velocity)
  return(v)
}