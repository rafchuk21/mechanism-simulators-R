#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Single-Speed Drive Simulator"),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title="Input",
          fluidRow(column(width=11, p(""), offset=1)), 
          fluidRow(
            column(4, selectInput(inputId="motor", label="Motor", choices = c("Redline","BAG","CIM","MiniCIM"), selected = "MiniCIM")),
            column(4, numericInput(inputId="numMotors", label = "Number of motors", value=6, min=1)),
            column(4, numericInput(inputId="sourceVoltage", label="Source voltage (volts)", value=12, min=0, step=0.5))
          ), 
          fluidRow(
            column(4, numericInput(inputId="gearing", label = "Gearing (X:1)", value=9, min=1/100)),
            column(4, numericInput(inputId="wheelDiameter", label="Wheel diameter (inches)", value=6, min=1/100)),
            column(4, numericInput(inputId="robotResistance", label="Robot resistance (ohms)", value=0.02, min=0, step=0.01))
          ),
          fluidRow(
            column(4, numericInput(inputId="velEfficiency", label="Velocity efficiency (%)", value=90, min=0, max=100, step=5)),
            column(4, numericInput(inputId="torqueEfficiency", label="Torque efficiency (%)", value=90, min=0, max=100, step=5)),
            column(4, numericInput(inputId="resistiveTorque", label="Resistive torque at wheel (inch pounds)", value=7, min=0))
          ),
          fluidRow(
            column(4, numericInput(inputId="deltaTime", label="Delta time (seconds)", value=0.001, min=0.0001, step=0.001)),
            column(4, numericInput(inputId="distance", label="Distance to travel (inches)", value=12, min=0)),
            column(4, numericInput(inputId="startVel", label="Initial velocity (feet/second)", value=0, min=0))
          ),
          fluidRow(
            column(4, numericInput(inputId="robotWeight", label="Robot weight (pounds)", value=150, min=0.0, step=5))
          )
        ),
        tabPanel("Output", uiOutput("outputUI")),
        tabPanel("Euler", tableOutput("eulerTable"))
      )
    )
  )
)
