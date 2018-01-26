#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Three Stage Continous Elevator Simulator"),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Input", selectInput(inputId="motor", label="Motor", choices = c("Redline","BAG","CIM","MiniCIM"), selected = "Redline"), 
                 numericInput(inputId="numMotors", label = "Number of motors", value=1, min=1),
                 numericInput(inputId="gearing", label = "Gearing (X:1)", value=30, min=1/100),
                 numericInput(inputId="pulleyDiameter", label="Pulley diameter (inches)", value=2.256, min=1/100),
                 numericInput(inputId="robotResistance", label="Robot resistance (ohms)", value=0.02, min=0, step=0.01),
                 numericInput(inputId="velEfficiency", label="Velocity efficiency (%)", value=0.9, min=0, max=1, step=0.05),
                 numericInput(inputId="torqueEfficiency", label="Torque efficiency (%)", value=0.9, min=0, max=1, step=0.05),
                 numericInput(inputId="staticFriction", label="Static friction (force pounds)", value=2, min=0),
                 numericInput(inputId="carriageWeight", label="Carriage weight (mass pounds)", value=10, min=0),
                 numericInput(inputId="carriageRun", label="Carriage run (inches)", value=20, min=0),
                 numericInput(inputId="smallStageWeight", label="Small stage weight (mass pounds)", value=10, min=0),
                 numericInput(inputId="smallStageRun", label="Small stage run (inches)", value=20, min=0),
                 numericInput(inputId="mediumStageWeight", label="Medium stage weight (mass pounds)", value=10, min=0),
                 numericInput(inputId="mediumStageRun", label="Medium stage run (inches)", value=20, min=0),
                 numericInput(inputId="deltaTime", label="Delta time (seconds)", value=0.01, min=0.001, step=0.01),
                 numericInput(inputId="startHeight", label="Start height (inches)", value=0, min=0),
                 numericInput(inputId="sourceVoltage", label="Source voltage (volts)", value=12, min=0, step=0.5)), 
        # tabPanel("Output", textOutput("timeToTop"), plotOutput("posOverTime"), plotOutput("velOverTime"), plotOutput("accelOverTime"),
        #          plotOutput("currentOverTime"), plotOutput("voltageOverTime"), plotOutput("velVoltageOverTime"),
        #          plotOutput("accelVoltageOverTime"), plotOutput("constantVoltageOverTime")), 
        tabPanel("Output", uiOutput("outputUI")),
        tabPanel("Euler", tableOutput("eulerTable"))
      )
    )
  )
)
