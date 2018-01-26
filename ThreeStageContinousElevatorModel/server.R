#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$eulerTable <- renderTable({
    returnedTable <- modelThreeStageContinuous(motor=input$motor, numMotors=input$numMotors, gearing=input$gearing, pulleyDiameter=input$pulleyDiameter,
                              robotResistance=input$robotResistance, velEfficiency=input$velEfficiency, 
                              torqueEfficiency=input$torqueEfficiency, staticFriction=input$staticFriction, 
                              carriageWeight=input$carriageWeight, carriageRun=input$carriageRun, smallStageWeight=input$smallStageWeight,
                              smallStageRun=input$smallStageRun, mediumStageWeight=input$mediumStageWeight,
                              mediumStageRun=input$mediumStageRun, deltaTime=input$deltaTime, startHeight = input$startHeight,
                              sourceVoltage = input$sourceVoltage)
    return(returnedTable)
  })
  
  # output$timeToTop <- renderText({
  #   returnedTable <- modelThreeStageContinuous(motor=input$motor, numMotors=input$numMotors, gearing=input$gearing, pulleyDiameter=input$pulleyDiameter,
  #                                       robotResistance=input$robotResistance, velEfficiency=input$velEfficiency,
  #                                       torqueEfficiency=input$torqueEfficiency, staticFriction=input$staticFriction,
  #                                       carriageWeight=input$carriageWeight, carriageRun=input$carriageRun, smallStageWeight=input$smallStageWeight,
  #                                       smallStageRun=input$smallStageRun, mediumStageWeight=input$mediumStageWeight,
  #                                       mediumStageRun=input$mediumStageRun, deltaTime=input$deltaTime, startHeight = input$startHeight,
  #                                       sourceVoltage = input$sourceVoltage)
  #   if(returnedTable$pos[length(returnedTable$pos)] < 0){
  #     return("Not enough torque to move!")
  #   } else {
  #     if (returnedTable$time[length(returnedTable$time)] >= 60){
  #       return("Took over a minute, timed out.")
  #     } else {
  #       return(paste("Took",round(returnedTable$time[length(returnedTable$time)], 1-log(input$deltaTime, 10)),"seconds to go to the top."))
  #     }
  #   }
  # })
  output$outputUI <- renderUI({
    returnedTable <- modelThreeStageContinuous(motor=input$motor, numMotors=input$numMotors, gearing=input$gearing, pulleyDiameter=input$pulleyDiameter,
                                               robotResistance=input$robotResistance, velEfficiency=input$velEfficiency, 
                                               torqueEfficiency=input$torqueEfficiency, staticFriction=input$staticFriction, 
                                               carriageWeight=input$carriageWeight, carriageRun=input$carriageRun, smallStageWeight=input$smallStageWeight,
                                               smallStageRun=input$smallStageRun, mediumStageWeight=input$mediumStageWeight,
                                               mediumStageRun=input$mediumStageRun, deltaTime=input$deltaTime, startHeight = input$startHeight,
                                               sourceVoltage = input$sourceVoltage)
    if(returnedTable$pos[length(returnedTable$pos)] < 0){
      text <- "Not enough torque to move!"
    } else {
      if (returnedTable$time[length(returnedTable$time)] >= 60){
        text <- "Took over a minute, timed out."
      } else {
        text <- paste("Took",round(returnedTable$time[length(returnedTable$time)], 1-log(input$deltaTime, 10)),"seconds to go to the top.")
      }
    }
    pos <- renderPlot({plot(returnedTable$time, returnedTable$pos, xlab="Time (seconds)", ylab="Position (inches)", main="Position over time", t="l")})
    vel <- renderPlot({plot(returnedTable$time, returnedTable$vel, xlab="Time (seconds)", ylab="Velocity (inches/sec)", main="Velocity over time", t="l")})
    accel <- renderPlot({plot(returnedTable$time, returnedTable$accel, xlab="Time (seconds)", ylab="Acceleration (inches/sec^2)", main="Acceleration over time", ylim=c(min(0,returnedTable$accel),max(returnedTable$accel)), t="l")})
    voltage <- renderPlot({plot(returnedTable$time, returnedTable$voltage, xlab="Time (seconds)", ylab="Voltage (volts)", main="Total voltage over time", ylim=c(min(0,returnedTable$voltage),max(returnedTable$voltage)), t="l")})
    current <- renderPlot({plot(returnedTable$time, returnedTable$current, xlab="Time (seconds)", ylab="Current (amps)", main="Current draw per motor over time", ylim=c(0,max(returnedTable$current)), t="l")})
    constantVoltage <- renderPlot({plot(returnedTable$time, returnedTable$constantVoltage, xlab="Time (seconds)", ylab="Voltage counteracting gravity (volts)", main="Constant voltage over time", ylim=c(0,max(returnedTable$constantVoltage)), t="l")})
    velVoltage <- renderPlot({plot(returnedTable$time, returnedTable$velVoltage, xlab="Time (seconds)", ylab="Back-EMF voltage (volts)", main="Velocity voltage over time", ylim=c(min(0,returnedTable$velVoltage),max(returnedTable$velVoltage)), t="l")})
    accelVoltage <- renderPlot({plot(returnedTable$time, returnedTable$accelVoltage, xlab="Time (seconds)", ylab="Acceleration voltage (volts)", main="Acceleration voltage over time", ylim=c(min(0,returnedTable$accelVoltage),max(returnedTable$accelVoltage)), t="l")})
    return(list(text, current, pos, vel, accel, voltage, constantVoltage, velVoltage, accelVoltage))
  })
  
})

motors <- data.frame("Redline"=c(18730,.71, 134,.7, 347,  .8,  433.75),
                     "BAG"=c(13180,.43, 53, 1.8,149,  .71, 209.86),
                     "CIM"=c(5330, 2.41,131,2.7,337.81,2.82,119.79),
                     "MiniCIM"=c(5840, 1.41,89, 3,  215,   2.16,99.54),
                     row.names = c("Free Speed (RPM)","Stall Torque (Nm)","Stall Current (Amp)","Free Current (Amp)","Power (W)","Weight (lb)", "Ratio (W/lb)")
)

nmToInLb <- 8.85074579
gsToInchPerSecSquared <- 386.09

#All units are inches, pounds, and seconds
modelThreeStageContinuous <- function(motor, numMotors, gearing, pulleyDiameter, robotResistance, velEfficiency, torqueEfficiency, 
                                      staticFriction, carriageWeight, carriageRun, smallStageWeight, smallStageRun, mediumStageWeight,
                                      mediumStageRun, deltaTime, startHeight, sourceVoltage) {
  motorForce <- motors["Stall Torque (Nm)", motor]*nmToInLb*numMotors*gearing*torqueEfficiency/(pulleyDiameter/2) #pound force
  motorResistance <- 12/motors["Stall Current (Amp)", motor] #ohms
  voltage = sourceVoltage/(1+numMotors*(robotResistance/motorResistance))
  twelveVoltSpeed <- motors["Free Speed (RPM)", motor]/60/gearing*pi*pulleyDiameter*velEfficiency #Inches/sec
  kV <- 12/twelveVoltSpeed #volts/(inch/sec)
  weights <- c(carriageWeight, carriageWeight + smallStageWeight, carriageWeight + smallStageWeight + mediumStageWeight) #pound mass
  constantForces <- weights+staticFriction #pound force
  constantVoltages <- 12*(constantForces/motorForce) #Volts
  maxAccels <- motorForce/weights*gsToInchPerSecSquared #Inches/sec^2, F = ma -> a = F/m
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
                       "accelVoltage"=(voltage-lookupConstantVoltage(startHeight)), "accel"=(voltage-lookupConstantVoltage(startHeight))/lookupKa(startHeight),
                       "current"=voltage/motorResistance, "voltage"=voltage)
  count <- 1
  
  #Exit if we're at the top, if we're going down, or if it's been a minute
  while(output$pos[count] < heights[3] & output$pos[count] >= 0 & output$time[count] < 60){
    newVoltage <- sourceVoltage - output$current[count]*numMotors*robotResistance
    newTime <- output$time[count] + deltaTime
    newPos <- output$pos[count] + deltaTime*output$vel[count]
    newVel <- output$vel[count] + deltaTime*output$accel[count]
    newConstantVoltage <- lookupConstantVoltage(newPos)
    newVelVoltage <- kV*newVel
    newAccelVoltage <- newVoltage - newConstantVoltage - newVelVoltage
    newAccel <- newAccelVoltage/lookupKa(newPos)
    newCurrent <- abs(newConstantVoltage+newAccelVoltage)/motorResistance
    count <- count+1
    output[count,] <- c(newTime, newPos, newVel, newConstantVoltage, newVelVoltage, newAccelVoltage, newAccel, newCurrent ,newVoltage)
  }
  
  # if(output$pos[count] < 0){
  #   print("Not enough torque to move!")
  # } else {
  #   if (output$time[count] >= 60){
  #     print("Took over a minute, timed out.")
  #   } else {
  #     print(paste("Took",round(output$time[length(output$time)], 1-log(deltaTime, 10)),"seconds to go to the top."))
  #   }
  # }
  return(output)
}