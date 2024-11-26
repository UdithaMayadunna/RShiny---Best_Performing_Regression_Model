shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Uditha Mayadunna"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput("vis_miss_plot"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table"),
             plotOutput("missingnessPattern")
             
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             HTML("<br><br>Best Five Models: <span style='color: #00FF00;'>Green Colour</span>"),
             HTML("<br>Best Five Transparent Models: <span style='color: #0000FF;'>Blue Colour</span>"),
             HTML("<br>Models Failed to train: <span style='color: red;'>Red Colour</span>"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel(title = span("GLMnet Model", style = "color: #0000FF;"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         tabPanel("Random Forest Model",
                                  verbatimTextOutput(outputId = "rf_ModelSummary0"),  # Output for model summary
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(
                                             inputId = "rf_Preprocess",
                                             label = "Pre-processing",
                                             choices = unique(c(rf_initial, ppchoices)),  
                                             multiple = TRUE,
                                             selected = rf_initial
                                           ),
                                           bsTooltip(id = "rf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")),  # Button to train the model
                                           bsTooltip(id = "rf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-arrow-up")),  # Button to load a saved model
                                           bsTooltip(id = "rf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-can")),  # Button to delete the model
                                           bsTooltip(id = "rf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),  
                                  tableOutput(outputId = "rf_Metrics"),  
                                  hr(),
                                  plotOutput(outputId = "rf_ModelPlots"),  
                                  verbatimTextOutput(outputId = "rf_Recipe"),  
                                  verbatimTextOutput(outputId = "rf_ModelSummary2"), 
                                  wellPanel(
                                    h3("Feature Importance"),
                                    tableOutput(outputId = "rf_Coef")  
                                  )
                         ),
    #------------------------------------------------------------------------------------------------------------------------------------------------------
      tabPanel("GBM Model",
               verbatimTextOutput(outputId = "gbm_ModelSummary0"),  
               fluidRow(
                 column(width = 4,
                        
                        selectizeInput(
                          inputId = "gbm_Preprocess",
                          label = "Pre-processing",
                          choices = unique(c(gbm_initial, ppchoices)),  
                          multiple = TRUE,
                          selected = gbm_initial 
                        ),
                        bsTooltip(id = "gbm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                 ),
                 column(width = 1,
                        actionButton(inputId = "gbm_Go", label = "Train", icon = icon("play")),  
                        bsTooltip(id = "gbm_Go", title = "This will train or retrain your model (and save it)")
                 ),
                 column(width = 1,
                        actionButton(inputId = "gbm_Load", label = "Load", icon = icon("file-arrow-up")), 
                        bsTooltip(id = "gbm_Load", title = "This will reload your saved model")
                 ),
                 column(width = 1,
                        actionButton(inputId = "gbm_Delete", label = "Forget", icon = icon("trash-can")),  
                        bsTooltip(id = "gbm_Delete", title = "This will remove your model from memory")
                 )
               ),
               hr(),
               h3("Resampled performance:"),  
               tableOutput(outputId = "gbm_Metrics"),  
               hr(),
               plotOutput(outputId = "gbm_ModelPlots"),  
               verbatimTextOutput(outputId = "gbm_Recipe"),  
               verbatimTextOutput(outputId = "gbm_ModelSummary2"),  
               wellPanel(
                 h3("Feature Importance"),  
                 tableOutput(outputId = "gbm_Coef")  
               )
      ),
 #---------------------------------------------------------------------------------------------------------------------------------------------------
 
 tabPanel("glmboost Model",
          verbatimTextOutput(outputId = "glmboost_ModelSummary0"),  
          fluidRow(
            column(width = 4,
                   
                   selectizeInput(
                     inputId = "glmboost_Preprocess",
                     label = "Pre-processing",
                     choices = unique(c(glmboost_initial, ppchoices)),  
                     multiple = TRUE ,
                     selected = glmboost_initial  
                   ),
                   bsTooltip(id = "glmboost_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
            ),
            column(width = 1,
                   actionButton(inputId = "glmboost_Go", label = "Train", icon = icon("play")),  
                   bsTooltip(id = "glmboost_Go", title = "This will train or retrain your model (and save it)")
            ),
            column(width = 1,
                   actionButton(inputId = "glmboost_Load", label = "Load", icon = icon("file-arrow-up")), 
                   bsTooltip(id = "glmboost_Load", title = "This will reload your saved model")
            ),
            column(width = 1,
                   actionButton(inputId = "glmboost_Delete", label = "Forget", icon = icon("trash-can")), 
                   bsTooltip(id = "glmboost_Delete", title = "This will remove your model from memory")
            )
          ),
          hr(),
          h3("Resampled performance:"),  
          tableOutput(outputId = "glmboost_Metrics"), 
          hr(),
          plotOutput(outputId = "glmboost_ModelPlots"),  
          verbatimTextOutput(outputId = "glmboost_Recipe"),  
          verbatimTextOutput(outputId = "glmboost_ModelSummary2"),  
          wellPanel(
            h3("Coefficients"),  
            tableOutput(outputId = "glmboost_Coef")  
          )
 ),
  #---------------------------------------------------------------------------------------------------------------------------------------------
 
 tabPanel("NNLS Model",
          verbatimTextOutput(outputId = "nnls_ModelSummary0"),  # Display model summary
          fluidRow(
            column(width = 4,
                   # The id of the recipe preprocessing steps control MUST be: "<method>_Preprocess"
                   selectizeInput(
                     inputId = "nnls_Preprocess",
                     label = "Pre-processing",
                     choices = unique(c(nnls_initial, ppchoices)),  # Preprocessing choices
                     multiple = TRUE,
                     selected = nnls_initial  # Default preprocessing
                   ),
                   bsTooltip(id = "nnls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
            ),
            column(width = 1,
                   actionButton(inputId = "nnls_Go", label = "Train", icon = icon("play")),  # Train the NNLS model
                   bsTooltip(id = "nnls_Go", title = "This will train or retrain your model (and save it)")
            ),
            column(width = 1,
                   actionButton(inputId = "nnls_Load", label = "Load", icon = icon("file-arrow-up")),  # Load a saved model
                   bsTooltip(id = "nnls_Load", title = "This will reload your saved model")
            ),
            column(width = 1,
                   actionButton(inputId = "nnls_Delete", label = "Forget", icon = icon("trash-can")),  # Delete the NNLS model
                   bsTooltip(id = "nnls_Delete", title = "This will remove your model from memory")
            )
          ),
          hr(),
          h3("Resampled performance:"),  # Section for NNLS performance metrics
          tableOutput(outputId = "nnls_Metrics"),  # Output for model metrics like RMSE
          hr(),
          plotOutput(outputId = "nnls_ModelPlots"),  # Output for model plots
          verbatimTextOutput(outputId = "nnls_Recipe"),  # Output for the model's recipe
          verbatimTextOutput(outputId = "nnls_ModelSummary2"),  # Detailed model summary
          wellPanel(
            h3("Coefficients"),  # Display coefficients from the NNLS model
            tableOutput(outputId = "nnls_Coef")  # Output for coefficients
          )
 ),
#-----------------------------------------------------------------------------------------------------------
tabPanel(title = span("SVM Radial Model", style = "color: #00FF00;"),
         verbatimTextOutput(outputId = "svmRadial_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "<method>_Preprocess"
                  selectizeInput(
                    inputId = "svmRadial_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(svmRadial_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = svmRadial_initial  # Recommended default
                  ),
                  bsTooltip(id = "svmRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "svmRadial_Go", label = "Train", icon = icon("play")),  # Train the `svmRadial` model
                  bsTooltip(id = "svmRadial_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "svmRadial_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "svmRadial_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "svmRadial_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "svmRadial_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `svmRadial` performance metrics
         tableOutput(outputId = "svmRadial_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "svmRadial_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "svmRadial_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "svmRadial_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `svmRadial`
           tableOutput(outputId = "svmRadial_Coef")  # Output for coefficients
         )
),

#---------------------------------------------------------------------------------------------

tabPanel(title = span("Robust Linear Model", style = "color: #0000FF;"),
         verbatimTextOutput(outputId = "rlm_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "<method>_Preprocess"
                  selectizeInput(
                    inputId = "rlm_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(rlm_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = rlm_initial  # Recommended default preprocessing
                  ),
                  bsTooltip(id = "rlm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "rlm_Go", label = "Train", icon = icon("play")),  # Train the `rlm` model
                  bsTooltip(id = "rlm_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "rlm_Load", label = "Load", icon = icon("file-arrow-up")),  # Load a saved model
                  bsTooltip(id = "rlm_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "rlm_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "rlm_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `rlm` performance metrics
         tableOutput(outputId = "rlm_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "rlm_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "rlm_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "rlm_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `rlm`
           tableOutput(outputId = "rlm_Coef")  # Output for coefficients
         )
),
#---------------------------------------------------------------------------

tabPanel("Rpart SE Model",
         verbatimTextOutput(outputId = "rpartSe_ModelSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "rpartSe_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(rpart_initial, ppchoices)),  # Assuming rpart_initial and ppchoices are defined for this model as well
                                 multiple = TRUE,
                                 selected = rpart_initial),  # Adjust these starting values as necessary for Rpart SE
                  bsTooltip(id = "rpartSe_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "rpartSe_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "rpartSe_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "rpartSe_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "rpartSe_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "rpartSe_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "rpartSe_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "rpartSe_Metrics"),
         hr(),
         plotOutput(outputId = "rpartSe_ModelPlots"),
         verbatimTextOutput(outputId = "rpartSe_Recipe"),
         plotOutput(outputId = "rpartSe_ModelTree")
),
#-------------------------------------------------------------------------------------------------------

tabPanel("Rpart 2 Model",
         verbatimTextOutput(outputId = "rpart2_ModelSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "rpart2_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(rpart_initial, ppchoices)),  # Ensure these choices are relevant for rpart2
                                 multiple = TRUE,
                                 selected = rpart_initial),  # Adjust these starting values as necessary for rpart2
                  bsTooltip(id = "rpart2_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "rpart2_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "rpart2_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "rpart2_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "rpart2_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "rpart2_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "rpart2_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "rpart2_Metrics"),
         hr(),
         plotOutput(outputId = "rpart2_ModelPlots"),
         verbatimTextOutput(outputId = "rpart2_Recipe"),
         plotOutput(outputId = "rpart2_ModelTree")
),


#-----------------------------------------------------------------------------------------------------

tabPanel("Conditional Inference Tree Model",
         verbatimTextOutput(outputId = "ctree_ModelSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "ctree_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(rpart_initial, ppchoices)),  # Ensure these are appropriate for ctree
                                 multiple = TRUE,
                                 selected = rpart_initial),  #yes Adjust these starting values as necessary for ctree
                  bsTooltip(id = "ctree_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "ctree_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "ctree_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "ctree_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "ctree_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "ctree_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "ctree_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "ctree_Metrics"),
         hr(),
         plotOutput(outputId = "ctree_ModelPlots"),
         verbatimTextOutput(outputId = "ctree_Recipe"),
         plotOutput(outputId = "ctree_ModelTree")
),
#--------------------------------------------------------------------------------------------------------

tabPanel("Neural Network Model",
         verbatimTextOutput(outputId = "neuralnet_ModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "nnet_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(neuralnet_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = neuralnet_initial),  # Adjust these starting values based on typical pre-processing for neural networks
                  bsTooltip(id = "nnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "nnet_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "nnet_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "nnet_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "nnet_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "nnet_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "nnet_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Model Overview:"),
         verbatimTextOutput(outputId = "nnet_ModelSummary2"),
         hr(),
         h3("Performance:"),
         tableOutput(outputId = "nnet_Metrics"),
         hr(),
         plotOutput(outputId = "nnet_ModelPlots"),
         verbatimTextOutput(outputId = "nnet_Recipe"),
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "nnet_Coef")  
         )
         
),
#-----------------------------------------------------------------------------------
tabPanel("pcaNNet Model",
           verbatimTextOutput(outputId = "pcaNNet_ModelSummary0"),
           fluidRow(
             column(width = 4,
                    selectizeInput(inputId = "pcaNNet_Preprocess",
                                   label = "Pre-processing",
                                   choices = unique(c(neuralnet_initial, ppchoices)),
                                   multiple = TRUE,
                                   selected = neuralnet_initial),  # Adjust these starting values based on typical pre-processing for neural networks
                    bsTooltip(id = "pcaNNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
             ),
             column(width = 1,
                    actionButton(inputId = "pcaNNet_Go", label = "Train", icon = icon("play")),
                    bsTooltip(id = "pcaNNet_Go", title = "This will train or retrain your model (and save it)")
             ),
             column(width = 1,
                    actionButton(inputId = "pcaNNet_Load", label = "Load", icon = icon("file-arrow-up")),
                    bsTooltip(id = "pcaNNet_Load", title = "This will reload your saved model")
             ),
             column(width = 1,
                    actionButton(inputId = "pcaNNet_Delete", label = "Forget", icon = icon("trash-can")),
                    bsTooltip(id = "pcaNNet_Delete", title = "This will remove your model from memory")
             )
           ),
           hr(),
           h3("Model Overview:"),
           verbatimTextOutput(outputId = "pcaNNet_ModelSummary2"),
           hr(),
           h3("Performance:"),
           tableOutput(outputId = "pcaNNet_Metrics"),
           hr(),
           plotOutput(outputId = "pcaNNet_ModelPlots"),
           verbatimTextOutput(outputId = "pcaNNet_Recipe"),
           wellPanel(
             h3("Coefficients"),  
             tableOutput(outputId = "pcaNNet_Coef")  
           )
           
),
#-------------------------------------------------------------------------------------------------------------
tabPanel("avNNet Model",
         verbatimTextOutput(outputId = "avNNet_ModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "avNNet_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(neuralnet_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = neuralnet_initial),  # Adjust these starting values based on typical pre-processing for neural networks
                  bsTooltip(id = "avNNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "avNNet_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "avNNet_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "avNNet_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "avNNet_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "avNNet_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "avNNet_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Model Overview:"),
         verbatimTextOutput(outputId = "avNNet_ModelSummary2"),
         hr(),
         h3("Performance:"),
         tableOutput(outputId = "avNNet_Metrics"),
         hr(),
         plotOutput(outputId = "avNNet_ModelPlots"),
         verbatimTextOutput(outputId = "pcaNNet_Recipe"),
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "avNNet_Coef")  
         )
         
),

                         
#----------------------------------------------------------------------------------------------------------------                        
tabPanel(title = span("monmlp Model", style = "color: #00FF00;"),
         verbatimTextOutput(outputId = "monmlp_ModelSummary0"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "monmlp_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(neuralnet_initial, ppchoices)),
                                 multiple = TRUE,
                                 selected = monmlp_initial),  # Adjust these starting values based on typical pre-processing for neural networks
                  bsTooltip(id = "monmlp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "monmlp_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "monmlp_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "monmlp_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "monmlp_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "monmlp_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "monmlp_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Model Overview:"),
         verbatimTextOutput(outputId = "monmlp_ModelSummary2"),
         hr(),
         h3("Performance:"),
         tableOutput(outputId = "monmlp_Metrics"),
         hr(),
         plotOutput(outputId = "monmlp_ModelPlots"),
         verbatimTextOutput(outputId = "monmlp_Recipe"),
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "monmlp_Coef")  
         )
         
),   

#------------------------------------------------------------------------------------

tabPanel("Black Boost Model",
         verbatimTextOutput(outputId = "blackboost_ModelSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "blackboost_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(rpart_initial, ppchoices)),  # Example preprocessing choices
                                 multiple = TRUE,
                                 selected = rpart_initial),  # Default selections, adjust as necessary for blackboost
                  bsTooltip(id = "blackboost_Preprocess", title = "Select preprocessing steps to apply before model training.", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "blackboost_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "blackboost_Go", title = "Train or retrain the model using gradient boosting.")
           ),
           column(width = 1,
                  actionButton(inputId = "blackboost_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "blackboost_Load", title = "Reload your previously saved model.")
           ),
           column(width = 1,
                  actionButton(inputId = "blackboost_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "blackboost_Delete", title = "Remove the model from memory.")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "blackboost_Metrics"),
         hr(),
         plotOutput(outputId = "blackboost_ModelPlots"),
         verbatimTextOutput(outputId = "blackboost_Recipe"),
         plotOutput(outputId = "blackboost_LearnerPlot")
),

#-------------------------------------------------------------------------------------------------------------

tabPanel(title = span("Boosted Trees Model", style = "color: #00FF00;"),
         verbatimTextOutput(outputId = "bstTree_ModelSummary"),
         fluidRow(
           column(width = 4,
                  selectizeInput(inputId = "bstTree_Preprocess",
                                 label = "Pre-processing",
                                 choices = unique(c(rpart_initial, ppchoices)),  # Example preprocessing choices
                                 multiple = TRUE,
                                 selected = bstTree_initial),  # Default selections, adjust as necessary for bstTree
                  bsTooltip(id = "bstTree_Preprocess", title = "Select preprocessing steps to apply before model training.", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "bstTree_Go", label = "Train", icon = icon("play")),
                  bsTooltip(id = "bstTree_Go", title = "Train or retrain the model using boosted trees.")
           ),
           column(width = 1,
                  actionButton(inputId = "bstTree_Load", label = "Load", icon = icon("file-arrow-up")),
                  bsTooltip(id = "bstTree_Load", title = "Reload your previously saved model.")
           ),
           column(width = 1,
                  actionButton(inputId = "bstTree_Delete", label = "Forget", icon = icon("trash-can")),
                  bsTooltip(id = "bstTree_Delete", title = "Remove the model from memory.")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "bstTree_Metrics"),
         hr(),
         plotOutput(outputId = "bstTree_ModelPlots"),
         verbatimTextOutput(outputId = "bstTree_Recipe"),
         plotOutput(outputId = "bstTree_StructurePlot")
),
#-------------------------------------------------------------------------------------------------------------------------
tabPanel("rvm Radial Model",
         verbatimTextOutput(outputId = "rvmRadial_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "<method>_Preprocess"
                  selectizeInput(
                    inputId = "rvmRadial_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(rvmRadial_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = rvmRadial_initial  # Recommended default
                  ),
                  bsTooltip(id = "rvmRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "rvmRadial_Go", label = "Train", icon = icon("play")),  # Train the `rvmRadial` model
                  bsTooltip(id = "rvmRadial_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "rvmRadial_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "rvmRadial_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "rvmRadial_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "rvmRadial_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `rvmRadial` performance metrics
         tableOutput(outputId = "rvmRadial_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "rvmRadial_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "rvmRadial_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "rvmRadial_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `rvmRadial`
           tableOutput(outputId = "rvmRadial_Coef")  # Output for coefficients
         )
),

#-------------------------------------------------------------------------------------------------------------------------

tabPanel(title = span("SVM Radial Cost Model", style = "color: #00FF00;"),
         verbatimTextOutput(outputId = "svmRadialCost_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "<method>_Preprocess"
                  selectizeInput(
                    inputId = "svmRadialCost_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(svmRadialCost_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = svmRadialCost_initial  # Recommended default
                  ),
                  bsTooltip(id = "svmRadialCost_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "svmRadialCost_Go", label = "Train", icon = icon("play")),  # Train the `svmRadialCost` model
                  bsTooltip(id = "svmRadialCost_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "svmRadialCost_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "svmRadialCost_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "svmRadialCost_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "svmRadialCost_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `svmRadialCost` performance metrics
         tableOutput(outputId = "svmRadialCost_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "svmRadialCost_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "svmRadialCost_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "svmRadialCost_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `svmRadialCost`
           tableOutput(outputId = "svmRadialCost_Coef")  # Output for coefficients
         )
),

#--------------------------------------------------------------------------------------------------------------
tabPanel("kNN Model",
         verbatimTextOutput(outputId = "knn_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "<method>_Preprocess"
                  selectizeInput(
                    inputId = "knn_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(knn_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = knn_initial  # Recommended default
                  ),
                  bsTooltip(id = "knn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "knn_Go", label = "Train", icon = icon("play")),  # Train the `knn` model
                  bsTooltip(id = "knn_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "knn_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "knn_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "knn_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "knn_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `knn` performance metrics
         tableOutput(outputId = "knn_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "knn_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "knn_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "knn_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `knn`
           tableOutput(outputId = "knn_Coef")  # Output for coefficients
         )
),

#---------------------------------------------------------------------------------------------------------------------------

tabPanel(title = span("Gausspr Radial Model", style = "color: #00FF00;"),
         verbatimTextOutput(outputId = "gaussprRadial_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "gaussprRadial_Preprocess"
                  selectizeInput(
                    inputId = "gaussprRadial_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(gaussprRadial_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = gaussprRadial_initial  # Recommended default
                  ),
                  bsTooltip(id = "gaussprRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "gaussprRadial_Go", label = "Train", icon = icon("play")),  # Train the `gaussprRadial` model
                  bsTooltip(id = "gaussprRadial_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "gaussprRadial_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "gaussprRadial_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "gaussprRadial_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "gaussprRadial_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `gaussprRadial` performance metrics
         tableOutput(outputId = "gaussprRadial_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "gaussprRadial_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "gaussprRadial_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "gaussprRadial_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `gaussprRadial`
           tableOutput(outputId = "gaussprRadial_Coef")  # Output for coefficients
         )
),
#-----------------------------------------------------------------------------------------------

tabPanel(title = span("QRNN Model", style = "color: #FF0000;"),
         verbatimTextOutput(outputId = "qrnn_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "qrnn_Preprocess"
                  selectizeInput(
                    inputId = "qrnn_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(qrnn_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = qrnn_initial  # Recommended default
                  ),
                  bsTooltip(id = "qrnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "qrnn_Go", label = "Train", icon = icon("play")),  # Train the `qrnn` model
                  bsTooltip(id = "qrnn_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "qrnn_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "qrnn_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "qrnn_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "qrnn_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `qrnn` performance metrics
         tableOutput(outputId = "qrnn_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "qrnn_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "qrnn_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "qrnn_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `qrnn`
           tableOutput(outputId = "qrnn_Coef")  # Output for coefficients
         )
),
#--------------------------------------------------------------------------


tabPanel(title = span("M5 Model", style = "color: #0000FF;"),
         verbatimTextOutput(outputId = "M5_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "M5_Preprocess"
                  selectizeInput(
                    inputId = "M5_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(M5_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = M5_initial  # Recommended default
                  ),
                  bsTooltip(id = "M5_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "M5_Go", label = "Train", icon = icon("play")),  # Train the `M5` model
                  bsTooltip(id = "M5_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "M5_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "M5_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "M5_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "M5_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `M5` performance metrics
         tableOutput(outputId = "M5_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "M5_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "M5_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "M5_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `M5`
           tableOutput(outputId = "M5_Coef")  # Output for coefficients
         )
),

#--------------------------------------------------------------

tabPanel(title = span("M5Rules Model", style = "color: #0000FF;"),
         verbatimTextOutput(outputId = "M5Rules_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "M5Rules_Preprocess"
                  selectizeInput(
                    inputId = "M5Rules_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(M5Rules_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = M5Rules_initial  # Recommended default
                  ),
                  bsTooltip(id = "M5Rules_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "M5Rules_Go", label = "Train", icon = icon("play")),  # Train the `M5Rules` model
                  bsTooltip(id = "M5Rules_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "M5Rules_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "M5Rules_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "M5Rules_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "M5Rules_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `M5Rules` performance metrics
         tableOutput(outputId = "M5Rules_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "M5Rules_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "M5Rules_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "M5Rules_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `M5Rules`
           tableOutput(outputId = "M5Rules_Coef")  # Output for coefficients
         )
),


#------------------------------------------------------------------------

tabPanel(title = span("Gaussian Process with Polynomial Kernel Model", style = "color: #FF0000;"),
         verbatimTextOutput(outputId = "gaussprPoly_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "gaussprPoly_Preprocess"
                  selectizeInput(
                    inputId = "gaussprPoly_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(gaussprPoly_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = gaussprPoly_initial  # Recommended default
                  ),
                  bsTooltip(id = "gaussprPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "gaussprPoly_Go", label = "Train", icon = icon("play")),  # Train the `gaussprPoly` model
                  bsTooltip(id = "gaussprPoly_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "gaussprPoly_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "gaussprPoly_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "gaussprPoly_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "gaussprPoly_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `gaussprPoly` performance metrics
         tableOutput(outputId = "gaussprPoly_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "gaussprPoly_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "gaussprPoly_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "gaussprPoly_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `gaussprPoly`
           tableOutput(outputId = "gaussprPoly_Coef")  # Output for coefficients
         )
),
#----------------------------------------------------------------------------------------

tabPanel("Bagged Trees Model",
         verbatimTextOutput(outputId = "treebag_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "treebag_Preprocess"
                  selectizeInput(
                    inputId = "treebag_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(treebag_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = treebag_initial  # Recommended default
                  ),
                  bsTooltip(id = "treebag_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "treebag_Go", label = "Train", icon = icon("play")),  # Train the `treebag` model
                  bsTooltip(id = "treebag_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "treebag_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "treebag_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "treebag_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "treebag_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `treebag` performance metrics
         tableOutput(outputId = "treebag_Metrics"),  # Output for model metrics like RMSE
         hr(),
         #plotOutput(outputId = "treebag_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "treebag_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "treebag_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `treebag`
           tableOutput(outputId = "treebag_Coef")  # Output for coefficients
         )
),

#-------------------------------------------------------------------------------

tabPanel(title = span("Bayesian Regularized Neural Networks", style = "color: #FF0000;"),
         verbatimTextOutput(outputId = "brnn_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "brnn_Preprocess"
                  selectizeInput(
                    inputId = "brnn_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(brnn_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = brnn_initial  # Recommended default
                  ),
                  bsTooltip(id = "brnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "brnn_Go", label = "Train", icon = icon("play")),  # Train the `brnn` model
                  bsTooltip(id = "brnn_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "brnn_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "brnn_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "brnn_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "brnn_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `brnn` performance metrics
         tableOutput(outputId = "brnn_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "brnn_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "brnn_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "brnn_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `brnn`
           tableOutput(outputId = "brnn_Coef")  # Output for coefficients
         )
),

#-------------------------------------------

tabPanel("Kernel Regularized Least Squares (Polynomial)",
         verbatimTextOutput(outputId = "krlsPoly_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "krlsPoly_Preprocess"
                  selectizeInput(
                    inputId = "krlsPoly_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(krlsPoly_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = krlsPoly_initial  # Recommended default
                  ),
                  bsTooltip(id = "krlsPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "krlsPoly_Go", label = "Train", icon = icon("play")),  # Train the `krlsPoly` model
                  bsTooltip(id = "krlsPoly_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "krlsPoly_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "krlsPoly_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "krlsPoly_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "krlsPoly_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `krlsPoly` performance metrics
         tableOutput(outputId = "krlsPoly_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "krlsPoly_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "krlsPoly_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "krlsPoly_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `krlsPoly`
           tableOutput(outputId = "krlsPoly_Coef")  # Output for coefficients
         )
),

#--------------------------------------------------------------------------------------------------

tabPanel("Multi-Layer Perceptron",
         verbatimTextOutput(outputId = "mlp_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "mlp_Preprocess"
                  selectizeInput(
                    inputId = "mlp_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(mlp_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = mlp_initial  # Recommended default
                  ),
                  bsTooltip(id = "mlp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "mlp_Go", label = "Train", icon = icon("play")),  # Train the `mlp` model
                  bsTooltip(id = "mlp_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "mlp_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "mlp_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "mlp_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "mlp_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `mlp` performance metrics
         tableOutput(outputId = "mlp_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "mlp_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "mlp_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "mlp_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `mlp`
           tableOutput(outputId = "mlp_Coef")  # Output for coefficients
         )
),

#----------------------------------------

tabPanel(title = span("SVM with Polynomial Kernel", style = "color: #FF0000;"),
         verbatimTextOutput(outputId = "svmPoly_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "svmPoly_Preprocess"
                  selectizeInput(
                    inputId = "svmPoly_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(svmPoly_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = svmPoly_initial  # Recommended default
                  ),
                  bsTooltip(id = "svmPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "svmPoly_Go", label = "Train", icon = icon("play")),  # Train the `svmPoly` model
                  bsTooltip(id = "svmPoly_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "svmPoly_Load", label = "Load", icon = icon("file-arrow-up")),  
                  bsTooltip(id = "svmPoly_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "svmPoly_Delete", label = "Forget", icon = icon("trash-can")),  
                  bsTooltip(id = "svmPoly_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  
         tableOutput(outputId = "svmPoly_Metrics"),  
         hr(),
         plotOutput(outputId = "svmPoly_ModelPlots"),  
         verbatimTextOutput(outputId = "svmPoly_Recipe"),  
         verbatimTextOutput(outputId = "svmPoly_ModelSummary2"),  
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "svmPoly_Coef")  
         )
),






#-------------------------------------------------------------------------------------------------------------


tabPanel(title = span("Extreme Learning Machine (ELM)", style = "color: #FF0000;"),
         verbatimTextOutput(outputId = "elm_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  selectizeInput(
                    inputId = "elm_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(elm_initial, ppchoices)),  #
                    multiple = TRUE,
                    selected = elm_initial  
                  ),
                  bsTooltip(id = "elm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "elm_Go", label = "Train", icon = icon("play")),  
                  bsTooltip(id = "elm_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "elm_Load", label = "Load", icon = icon("file-arrow-up")),  
                  bsTooltip(id = "elm_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "elm_Delete", label = "Forget", icon = icon("trash-can")),  
                  bsTooltip(id = "elm_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  
         tableOutput(outputId = "elm_Metrics"),  
         hr(),
         plotOutput(outputId = "elm_ModelPlots"),  
         verbatimTextOutput(outputId = "elm_Recipe"),  
         verbatimTextOutput(outputId = "elm_ModelSummary2"),  
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "elm_Coef")  
         )
),
#---------------------------------------------------------

tabPanel("Linear Regression with Stepwise Selection (AIC)",
         verbatimTextOutput(outputId = "lmStepAIC_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  selectizeInput(
                    inputId = "lmStepAIC_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(lmStepAIC_initial, ppchoices)),  
                    multiple = TRUE,
                    selected = lmStepAIC_initial  
                  ),
                  bsTooltip(id = "lmStepAIC_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "lmStepAIC_Go", label = "Train", icon = icon("play")),  
                  bsTooltip(id = "lmStepAIC_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "lmStepAIC_Load", label = "Load", icon = icon("file-arrow-up")),  
                  bsTooltip(id = "lmStepAIC_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "lmStepAIC_Delete", label = "Forget", icon = icon("trash-can")),  
                  bsTooltip(id = "lmStepAIC_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  
         tableOutput(outputId = "lmStepAIC_Metrics"),  
         hr(),
         plotOutput(outputId = "lmStepAIC_ModelPlots"),  
         verbatimTextOutput(outputId = "lmStepAIC_Recipe"),  
         verbatimTextOutput(outputId = "lmStepAIC_ModelSummary2"),  
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "lmStepAIC_Coef")  
         )
),

#----------------------------------------------------------------------------------------

tabPanel(title = span("Deep Neural Network (MXNet)", style = "color: #FF0000;"),
         verbatimTextOutput(outputId = "mxnet_ModelSummary0"),  
         fluidRow(
           column(width = 4,
                  selectizeInput(
                    inputId = "mxnet_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(mxnet_initial, ppchoices)),  
                    multiple = TRUE,
                    selected = mxnet_initial 
                  ),
                  bsTooltip(id = "mxnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "mxnet_Go", label = "Train", icon = icon("play")),  
                  bsTooltip(id = "mxnet_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "mxnet_Load", label = "Load", icon = icon("file-arrow-up")),  
                  bsTooltip(id = "mxnet_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "mxnet_Delete", label = "Forget", icon = icon("trash-can")), 
                  bsTooltip(id = "mxnet_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  
         tableOutput(outputId = "mxnet_Metrics"),  
         hr(),
         plotOutput(outputId = "mxnet_ModelPlots"),  
         verbatimTextOutput(outputId = "mxnet_Recipe"),  
         verbatimTextOutput(outputId = "mxnet_ModelSummary2"),  
         wellPanel(
           h3("Coefficients"),  
           tableOutput(outputId = "mxnet_Coef")  
         )
),

#-----------------------------------------------------------

tabPanel(title = span("Linear Regression with Sequential Feature Selection", style = "color: #0000FF;"),
         verbatimTextOutput(outputId = "leapSeq_ModelSummary0"),  # Display model summary
         fluidRow(
           column(width = 4,
                  # The id of the recipe preprocessing steps control MUST be: "leapSeq_Preprocess"
                  selectizeInput(
                    inputId = "leapSeq_Preprocess",
                    label = "Pre-processing",
                    choices = unique(c(leapSeq_initial, ppchoices)),  # Example preprocessing choices
                    multiple = TRUE,
                    selected = leapSeq_initial  # Recommended default
                  ),
                  bsTooltip(id = "leapSeq_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
           ),
           column(width = 1,
                  actionButton(inputId = "leapSeq_Go", label = "Train", icon = icon("play")),  # Train the `leapSeq` model
                  bsTooltip(id = "leapSeq_Go", title = "This will train or retrain your model (and save it)")
           ),
           column(width = 1,
                  actionButton(inputId = "leapSeq_Load", label = "Load", icon = icon("file-arrow-up")),  # Load the saved model
                  bsTooltip(id = "leapSeq_Load", title = "This will reload your saved model")
           ),
           column(width = 1,
                  actionButton(inputId = "leapSeq_Delete", label = "Forget", icon = icon("trash-can")),  # Delete a model from memory
                  bsTooltip(id = "leapSeq_Delete", title = "This will remove your model from memory")
           )
         ),
         hr(),
         h3("Resampled performance:"),  # Section for `leapSeq` performance metrics
         tableOutput(outputId = "leapSeq_Metrics"),  # Output for model metrics like RMSE
         hr(),
         plotOutput(outputId = "leapSeq_ModelPlots"),  # Output for model plots
         verbatimTextOutput(outputId = "leapSeq_Recipe"),  # Output for the model's recipe
         verbatimTextOutput(outputId = "leapSeq_ModelSummary2"),  # Detailed model summary
         wellPanel(
           h3("Coefficients"),  # Display coefficients from `leapSeq`
           tableOutput(outputId = "leapSeq_Coef")  # Output for coefficients
         )
)














#-------------------------------------------------------------------------------------------------------------------










#-----------------------------------------------------------------------------------------------------------------------



         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
