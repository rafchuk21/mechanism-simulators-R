#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {

  output$eulerTable <- renderTable({
    returnedTable <- modelDrive(motor=input$motor, numMotors=input$numMotors, gearing=input$gearing, wheelDiameter=input$wheelDiameter,
                              robotResistance=input$robotResistance, velEfficiency=input$velEfficiency/100, 
                              torqueEfficiency=input$torqueEfficiency/100, resistiveTorque=input$resistiveTorque, 
                              deltaTime=input$deltaTime, distance = input$distance, startVel = input$startVel,
                              sourceVoltage = input$sourceVoltage, robotWeight = input$robotWeight)
    return(returnedTable)
  })
  
  output$outputUI <- renderUI({
    returnedTable <- modelDrive(motor=input$motor, numMotors=input$numMotors, gearing=input$gearing, wheelDiameter=input$wheelDiameter,
                                               robotResistance=input$robotResistance, velEfficiency=input$velEfficiency/100, 
                                               torqueEfficiency=input$torqueEfficiency/100, resistiveTorque=input$resistiveTorque, 
                                               deltaTime=input$deltaTime, distance = input$distance,startVel = input$startVel,
                                               sourceVoltage = input$sourceVoltage, robotWeight= input$robotWeight)
    if (returnedTable$time[length(returnedTable$time)] >= 60){
      text <- "Took over a minute, timed out."
    } else {
      text <- paste("Took",round(returnedTable$time[length(returnedTable$time)], 1-log(input$deltaTime, 10)),"seconds to go to the requested distance.")
    }
    pos <- renderPlot({plot(returnedTable$time, returnedTable$pos, xlab="Time (seconds)", ylab="Position (inches)", main="Position over time", t="l")})
    vel <- renderPlot({plot(returnedTable$time, returnedTable$vel, xlab="Time (seconds)", ylab="Velocity (inches/sec)", main="Velocity over time", t="l")})
    accel <- renderPlot({plot(returnedTable$time, returnedTable$accel, xlab="Time (seconds)", ylab="Acceleration (inches/sec^2)", main="Acceleration over time", ylim=c(min(0,returnedTable$accel),max(returnedTable$accel)), t="l")})
    voltage <- renderPlot({plot(returnedTable$time, returnedTable$voltage, xlab="Time (seconds)", ylab="Voltage (volts)", main="Total voltage over time", ylim=c(min(0,returnedTable$voltage),max(returnedTable$voltage)), t="l")})
    current <- renderPlot({plot(returnedTable$time, returnedTable$current, xlab="Time (seconds)", ylab="Current (amps)", main="Current draw per motor over time", ylim=c(0,max(returnedTable$current)), t="l")})
    velVoltage <- renderPlot({plot(returnedTable$time, returnedTable$velVoltage, xlab="Time (seconds)", ylab="Back-EMF voltage (volts)", main="Velocity voltage over time", ylim=c(min(0,returnedTable$velVoltage),max(returnedTable$velVoltage)), t="l")})
    accelVoltage <- renderPlot({plot(returnedTable$time, returnedTable$accelVoltage, xlab="Time (seconds)", ylab="Acceleration voltage (volts)", main="Acceleration voltage over time", ylim=c(min(0,returnedTable$accelVoltage),max(returnedTable$accelVoltage)), t="l")})
    return(list(text, current, pos, vel, accel, voltage, velVoltage, accelVoltage))
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

#All units are inches, pounds, and seconds
modelDrive <- function(motor, numMotors, gearing, wheelDiameter, robotResistance, velEfficiency, torqueEfficiency, 
                                      robotWeight, resistiveTorque, deltaTime, startVel, distance, sourceVoltage) {
  motorForce <- motors["Stall Torque (Nm)", motor]*nmToInLb*numMotors*gearing*torqueEfficiency/(wheelDiameter/2) #pound force
  motorResistance <- 12/motors["Stall Current (Amp)", motor] #ohms
  voltage = sourceVoltage/(1+numMotors*(robotResistance/motorResistance))
  twelveVoltSpeed <- motors["Free Speed (RPM)", motor]/60/gearing*pi*wheelDiameter*velEfficiency #Inches/sec
  kV <- 12/twelveVoltSpeed #volts/(inch/sec)
  constantVoltage <- 12*(resistiveTorque*(wheelDiameter/2)/motorForce) #Volts
  maxAccel <- motorForce/robotWeight*gsToInchPerSecSquared #Inches/sec^2, F = ma -> a = F/m
  kA <- 12/maxAccel # volts(inch/(sec^2))
  
  #Load in starting values
  output <- data.frame("time"=0,"pos"=0,"vel"=startVel, "velVoltage"=kV*startVel,
                       "accelVoltage"=(voltage-constantVoltage-kV*startVel), "accel"=(voltage-constantVoltage-kV*startVel)/kA,
                       "current"=voltage/motorResistance, "voltage"=voltage)
  count <- 1
  
  #Exit if we're at the destination or if it's been a minute
  while(output$pos[count] < distance & output$time[count] < 60){
    newVoltage <- sourceVoltage - output$current[count]*numMotors*robotResistance
    newTime <- output$time[count] + deltaTime
    newPos <- output$pos[count] + deltaTime*output$vel[count]
    newVel <- output$vel[count] + deltaTime*output$accel[count]
    newVelVoltage <- kV*newVel
    newAccelVoltage <- newVoltage - constantVoltage - newVelVoltage
    newAccel <- newAccelVoltage/kA
    newCurrent <- abs(constantVoltage+newAccelVoltage)/motorResistance
    count <- count+1
    output[count,] <- c(newTime, newPos, newVel, newVelVoltage, newAccelVoltage, newAccel, newCurrent, newVoltage)
  }
  
  return(output)
}