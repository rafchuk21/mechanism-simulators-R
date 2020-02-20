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
  titlePanel("Dual Flywheel Simulator"),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title="Input",
          fluidRow(column(width=11, p(""), offset=1)), 
          fluidRow(
            column(4, selectInput(inputId="motor", label="Motor", choices = c("Redline","BAG","CIM","MiniCIM","NEO","Falcon"), selected = "NEO")),
            column(4, numericInput(inputId="numMotors", label = "Number of motors", value=2, min=1)),
            column(4, numericInput(inputId="sourceVoltage", label="Source voltage (volts)", value=12, min=0, step=0.5))
          ), 
          fluidRow(
            column(4, numericInput(inputId="gearing", label = "Gearing (X:1)", value=1, min=1/100)),
            column(4, numericInput(inputId="wheelDiameter", label="Wheel diameter (inches)", value=4, min=1/100)),
            column(4, numericInput(inputId="robotResistance", label="Robot resistance (ohms)", value=0.02, min=0, step=0.01))
          ),
          fluidRow(
            column(4, numericInput(inputId="velEfficiency", label="Velocity efficiency (%)", value=90, min=0, max=100, step=5)),
            column(4, numericInput(inputId="torqueEfficiency", label="Torque efficiency (%)", value=90, min=0, max=100, step=5)),
            column(4, numericInput(inputId="resistiveTorque", label="Resistive torque at wheel (inch pounds)", value=0, min=0))
          ),
          fluidRow(
            column(4, numericInput(inputId="deltaTime", label="Delta time (seconds)", value=0.01, min=0.0001, step=0.001)),
            column(4, numericInput(inputId="setpoint", label="Flywheel Setpoint (RPM)", value=5000, min=0)),
            column(4, numericInput(inputId="projMass", label="Projectile Mass (pound mass)", value=0.3, min=0.01))
          ),
          fluidRow(
            column(4, numericInput(inputId="wheelMOI", label="Total Wheel Moment of Inertia (pound mass * inch squared)", value=2.5, min=0.01))
          )
        ),
        tabPanel(title="Output", 
          fluidRow(plotOutput("current")),
          fluidRow(plotOutput("vel")),
          fluidRow(plotOutput("accel")),
          fluidRow(plotOutput("appVoltage")),
          fluidRow(plotOutput("velVoltage")),
          fluidRow(plotOutput("accelVoltage")),
          fluidRow(plotOutput("sysVoltage"))),
        tabPanel("Euler", tableOutput("eulerTable"))
      )
    )
  )
)
