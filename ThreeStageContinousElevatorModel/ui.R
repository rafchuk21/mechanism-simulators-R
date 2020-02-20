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
        tabPanel(title="Input",
          fluidRow(column(width=11, p(""), offset=1)), 
          fluidRow(
            column(4, selectInput(inputId="motor", label="Motor", choices = c("Redline","BAG","CIM","MiniCIM", "NEO", "Falcon"), selected = "Redline")),
            column(4, numericInput(inputId="numMotors", label = "Number of motors", value=1, min=1)),
            column(4, numericInput(inputId="sourceVoltage", label="Source voltage (volts)", value=12, min=0, step=0.5))
          ), 
          fluidRow(
            column(4, numericInput(inputId="gearing", label = "Gearing (X:1)", value=30, min=1/100)),
            column(4, numericInput(inputId="pulleyDiameter", label="Pulley diameter (inches)", value=2.256, min=1/100)),
            column(4, numericInput(inputId="robotResistance", label="Robot resistance (ohms)", value=0.02, min=0, step=0.01))
          ),
          fluidRow(
            column(4, numericInput(inputId="velEfficiency", label="Velocity efficiency (%)", value=90, min=0, max=100, step=5)),
            column(4, numericInput(inputId="torqueEfficiency", label="Torque efficiency (%)", value=90, min=0, max=100, step=5)),
            column(4, numericInput(inputId="staticFriction", label="Static friction (force pounds)", value=2, min=0))
          ),
          fluidRow(
            column(4, numericInput(inputId="carriageWeight", label="Carriage weight (mass pounds)", value=10, min=0)),
            column(4, numericInput(inputId="smallStageWeight", label="Small stage weight (mass pounds)", value=10, min=0)),
            column(4, numericInput(inputId="mediumStageWeight", label="Medium stage weight (mass pounds)", value=10, min=0))
          ),
          fluidRow(
            column(4, numericInput(inputId="carriageRun", label="Carriage run (inches)", value=20, min=0)),
            column(4, numericInput(inputId="smallStageRun", label="Small stage run (inches)", value=20, min=0)),
            column(4, numericInput(inputId="mediumStageRun", label="Medium stage run (inches)", value=20, min=0))
          ),
          fluidRow(
            column(4, numericInput(inputId="deltaTime", label="Delta time (seconds)", value=0.001, min=0.0001, step=0.001)),
            column(4, numericInput(inputId="startHeight", label="Start height (inches)", value=0, min=0))
          )
        ),
        tabPanel("Output", uiOutput("outputUI")),
        tabPanel("Euler", tableOutput("eulerTable"))
      )
    )
  )
)
