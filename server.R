shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  misdata <- reactive({
    e <- read.csv(file = "Ass3Data.csv",row.names = "Patient", header = TRUE, na.strings = c("","NA", "na", "N/A", "-1", "-99","--"),stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    e$ObservationDate <- as.Date(e$ObservationDate, "%Y-%m-%d")
    e
 })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  output$vis_miss_plot <- renderPlot({
    d <- getData()
    vis_miss(d)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  output$missingnessPattern <- renderPlot({
    
  data_missing <-misdata()
  data_missing$missingness <- apply(X = is.na(data_missing), MARGIN = 1, FUN = sum)
  tree <- caret::train(missingness ~ ., 
                         data = data_missing, 
                         method = "rpart", 
                         na.action = na.rpart)
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               sub = "Check whether the outcome variable is an important variable",
               roundint = TRUE, 
               clip.facs = TRUE)
  })
  
  
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "")  ## change the value parameter to your best method
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.null(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # METHOD * randomForest ---------------------------------------------------------------------------------------------------------------------------
  library(randomForest)
  
  getRFRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rf_Preprocess) %>%           
      step_rm(has_type("date"))   
  })
  
  # Observe events for model building, loading, and deleting
  observeEvent(
    input$rf_Go,
    {
      method <- "rf"  
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel) 
      tryCatch({
        model <- caret::train(getRFRecipe(), data = getTrainData(), method = "rf", metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  
      })
    }
  )
  
  # Handle loading a stored model
  observeEvent(
    input$rf_Load,
    {
      method  <- "rf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  # Handle deleting the RF model
  observeEvent(
    input$rf_Delete,
    {
      models[["rf"]] <- NULL
      gc()  # Garbage collection
    }
  )
  
  # Output for RF Model Summary
  output$rf_ModelSummary0 <- renderText({
    description("randomForest")  
  })
  
  # Output for RF Metrics
  output$rf_Metrics <- renderTable({
    req(models$rf)  
    models$rf$results[which.min(models$rf$results[, "RMSE"]), ]  
  })
  
  # Output for RF Model Plots
  output$rf_ModelPlots <- renderPlot({
    req(models$rf)  # Require the RF model
    plot(models$rf)  # Plot the model
  })
  
  # Output for RF Recipe
  output$rf_Recipe <- renderPrint({
    req(models$rf)  # Require the RF model
    models$rf$recipe  # Print the recipe used for the RF model
  })
  
  # Output for RF Model Summary
  output$rf_ModelSummary2 <- renderPrint({
    req(models$rf)  # Require the RF model
    print(models$rf)  # Print details of the RF model
  })
  
  # Output for RF Coefficients
  output$rf_Coef <- renderTable({
    req(models$rf)  # Require the RF model
    rf_co <- as.matrix(randomForest::importance(models$rf$finalModel))  # Get feature importances for Random Forest
    df <- as.data.frame(rf_co, row.names = rownames(rf_co))  # Convert to a data frame
    df[df[,1] != 0.000, , drop = FALSE]  # Filter non-zero importance features
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * gbm ---------------------------------------------------------------------------------------------------------------------------
  library(gbm)
  library(xgboost)
  
  # Reactive GBM Recipe
  getGbmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gbm_Preprocess) %>%
      step_rm(has_type("date"))  # Remove original date variables
  })
  
  # Reactive XGBoost Recipe (optional)
  getXgbTreeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$xgbTree_Preprocess) %>%
      step_rm(has_type("date"))  # Remove original date variables
  })
  
  # Observe events for GBM model training
  observeEvent(
    input$gbm_Go,
    {
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(
          getGbmRecipe(), 
          data = getTrainData(), 
          method = "gbm", 
          metric = "RMSE", 
          trControl = getTrControl(),
          distribution = "gaussian", 
          tuneLength = 5,
          verbose = FALSE  # Suppress verbosity
        )
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  # Stop parallel processing
      })
    }
  )
  
  
  
  
  # Loading GBM model
  observeEvent(
    input$gbm_Load,
    {
      method  <- "gbm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  

  
  # Deleting the GBM model
  observeEvent(
    input$gbm_Delete,
    {
      models[["gbm"]] <- NULL
      gc()  # Garbage collection
    }
  )
  

  
  # Output for GBM Model Summary
  output$gbm_ModelSummary0 <- renderText({
    description("gbm")  # Description of the GBM model
  })
  
  # Output for GBM Metrics
  output$gbm_Metrics <- renderTable({
    req(models$gbm)  # Require the GBM model to be present
    models$gbm$results[which.min(models$gbm$results[, "RMSE"]), ]  # Find the best RMSE result
  })
  
  # Output for GBM Model Plots
  output$gbm_ModelPlots <- renderPlot({
    req(models$gbm)  # Require the GBM model
    plot(models$gbm)  # Plot the GBM model
  })
  
  # Output for GBM Recipe
  output$gbm_Recipe <- renderPrint({
    req(models$gbm)  # Require the GBM model
    models$gbm$recipe  # Print the recipe used for the GBM model
  })
  
  # Output for GBM Model Summary (detailed)
  output$gbm_ModelSummary2 <- renderPrint({
    req(models$gbm)  # Require the GBM model
    print(models$gbm)  # Print details of the GBM model
  })
  
  # Output for GBM Feature Importance
  output$gbm_Coef <- renderTable({
    req(models$gbm)  # Require the GBM model
    gbm_imp <- caret::varImp(models$gbm)  # Get feature importances for GBM
    df <- as.data.frame(gbm_imp, row.names = rownames(gbm_imp))  # Convert to a data frame
    df[df$Overall != 0.000, , drop = FALSE]  # Show only features with importance
  }, rownames = TRUE, colnames = FALSE)
  
  #elasticnet-----------------------------------------------------------------------------------------------------------------------
  
  library(mboost)  # Ensure you have the 'mboost' package installed
  
  # Reactive `glmboost` Recipe
  getGlmboostRecipe <- reactive({
    form <- formula(Response ~ .)  # Define the formula
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmboost_Preprocess) %>%
      step_rm(has_type("date"))  # Remove original date variables
  })
  
  # Event handling for `glmboost` model training
  observeEvent(
    input$glmboost_Go,
    {
      method <- "glmboost"  # Method name for 'glmboost'
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)  # Start parallel processing
      tryCatch({
        model <- caret::train(getGlmboostRecipe(), data = getTrainData(), method = "glmboost", metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.omit)  # Model training and tuning
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model  # Store the trained model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  # Stop parallel processing
      })
    }
  )
  
  # Event handling for `glmboost` model loading
  observeEvent(
    input$glmboost_Load,
    {
      method  <- "glmboost"
      model <- loadRds(method, session)  # Load the model
      if (!is.null(model)) {
        models[[method]] <- model  # Store the loaded model
      }
    }
  )
  
  # Event handling for `glmboost` model deletion
  observeEvent(
    input$glmboost_Delete,
    {
      models[["glmboost"]] <- NULL
      gc()  # Perform garbage collection
    }
  )
  
  # Output for `glmboost` Model Summary
  output$glmboost_ModelSummary0 <- renderText({
    description("glmboost")  # Use the caret method name here
  })
  
  # Output for `glmboost` Metrics
  output$glmboost_Metrics <- renderTable({
    req(models$glmboost)  # Ensure the model is present
    models$glmboost$results[which.min(models$glmboost$results[, "RMSE"]), ]  # Find the best RMSE result
  })
  
  # Output for `glmboost` Model Plots
  output$glmboost_ModelPlots <- renderPlot({
    req(models$glmboost)  # Ensure the `glmboost` model is available
    plot(models$glmboost)  # Plot the `glmboost` model
  })
  
  # Output for `glmboost` Recipe
  output$glmboost_Recipe <- renderPrint({
    req(models$glmboost)  # Ensure the `glmboost` model is present
    models$glmboost$recipe  # Print the recipe used for `glmboost`
  })
  
  # Output for `glmboost` Model Summary (detailed)
  output$glmboost_ModelSummary2 <- renderPrint({
    req(models$glmboost)  # Ensure the `glmboost` model is available
    summary(models$glmboost$finalModel)  # Detailed summary of the `glmboost` model
  })
  
  # Output for `glmboost` Coefficients
  output$glmboost_Coef <- renderTable({
    req(models$glmboost)  # Ensure the `glmboost` model is present
    co <- coef(models$glmboost$finalModel)  # Get coefficients for `glmboost`
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
  #-------------------------------------------------------------------------------------------------------------------------------------------------------
  library(nnls)  # Ensure you have the NNLS package installed
  
  # Reactive NNLS Recipe
  getNnlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$nnls_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for NNLS model training
  observeEvent(
    input$nnls_Go,
    {
      method <- "nnls"  # Method name for Non-Negative Least Squares
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)  # Start parallel processing
      tryCatch({
        model <- caret::train(getNnlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.omit)  # Tune length and action on missing values
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  # Stop parallel processing
      })
    }
  )
  
  # Event handling for NNLS model loading
  observeEvent(
    input$nnls_Load,
    {
      method  <- "nnls"
      model <- loadRds(method, session)  # Load the model
      if (!is.null(model)) {
        models[[method]] <- model  # Assign the loaded model to the reactive list
      }
    }
  )
  
  # Event handling for NNLS model deletion
  observeEvent(
    input$nnls_Delete,
    {
      models[["nnls"]] <- NULL
      gc()  # Perform garbage collection
    }
  )
  
  # Output for NNLS Model Summary
  output$nnls_ModelSummary0 <- renderText({
    description("nnls")  # Use the caret method name here
  })
  
  # Output for NNLS Metrics
  output$nnls_Metrics <- renderTable({
    req(models$nnls)  # Ensure the model is available
    models$nnls$results[which.min(models$nnls$results[, "RMSE"]), ]  # Find the best RMSE result
  })
  
  # Output for NNLS Model Plots
  output$nnls_ModelPlots <- renderPlot({
    req(models$nnls)  # Require the NNLS model
    plot(models$nnls)  # Plot the NNLS model
  })
  
  # Output for NNLS Recipe
  output$nnls_Recipe <- renderPrint({
    req(models$nnls)  # Require the NNLS model
    models$nnls$recipe  # Print the recipe used for NNLS
  })
  
  # Output for NNLS Model Summary (detailed)
  output$nnls_ModelSummary2 <- renderPrint({
    req(models$nnls)  # Require the NNLS model
    summary(models$nnls$finalModel)  # Summary of the NNLS final model
  })
  
  # Output for NNLS Coefficients
  output$nnls_Coef <- renderTable({
    req(models$nnls)  # Require the NNLS model
    co <- coef(models$nnls$finalModel)  # Get coefficients for NNLS
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------

  library(kernlab)  # Required for 'svmRadial'
  
  # Reactive `svmRadial` Recipe
  getSvmRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadial_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `svmRadial` model training
  observeEvent(
    input$svmRadial_Go,
    {
      method <- "svmRadial"  # Method name for SVM with radial basis function
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)  # Start parallel processing
      tryCatch({
        model <- caret::train(getSvmRadialRecipe(), data = getTrainData(), method = "svmRadial", metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.omit)  # Training and tuning
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model  # Store the trained model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  # Stop parallel processing
      })
    }
  )
  
  # Event handling for `svmRadial` model loading
  observeEvent(
    input$svmRadial_Load,
    {
      method  <- "svmRadial"
      model <- loadRds(method, session)  # Load the saved model
      if (!is.null(model)) {
        models[[method]] <- model  # Store the loaded model in the reactive list
      }
    }
  )
  
  # Event handling for `svmRadial` model deletion
  observeEvent(
    input$svmRadial_Delete,
    {
      models[["svmRadial"]] <- NULL
      gc()  # Garbage collection
    }
  )
  
  # Output for `svmRadial` Model Summary
  output$svmRadial_ModelSummary0 <- renderText({
    description("svmRadial")  # Use the caret method name
  })
  
  # Output for `svmRadial` Metrics
  output$svmRadial_Metrics <- renderTable({
    req(models$svmRadial)  # Ensure the model is present
    models$svmRadial$results[which.min(models$svmRadial$results[, "RMSE"]), ]  # Best RMSE result
  })
  
  # Output for `svmRadial` Model Plots
  output$svmRadial_ModelPlots <- renderPlot({
    req(models$svmRadial)  # Ensure the `svmRadial` model is available
    plot(models$svmRadial)  # Plot the `svmRadial` model
  })
  
  # Output for `svmRadial` Recipe
  output$svmRadial_Recipe <- renderPrint({
    req(models$svmRadial)  # Require the `svmRadial` model
    models$svmRadial$recipe  # Print the recipe used for `svmRadial`
  })
  
  # Output for `svmRadial` Model Summary (detailed)
  output$svmRadial_ModelSummary2 <- renderPrint({
    req(models$svmRadial)  # Require the `svmRadial` model
    summary(models$svmRadial$finalModel)  # Detailed summary of the `svmRadial` model
  })
  
  # Output for `svmRadial` Coefficients
  output$svmRadial_Coef <- renderTable({
    req(models$svmRadial)  # Require the `svmRadial` model
    co <- coef(models$svmRadial$finalModel)  # Get coefficients for `svmRadial`
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
  
 #-------------------------------------------------------------- 

  library(MASS)  # Required for 'rlm'
  
  # Reactive `rlm` Recipe
  getRlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rlm_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `rlm` model training
  observeEvent(
    input$rlm_Go,
    {
      method <- "rlm"  # Method name for Robust Linear Model
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)  # Start parallel processing
      tryCatch({
        model <- caret::train(getRlmRecipe(), data = getTrainData(), method = "rlm", metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.omit)  # Training and tuning
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model  # Store the trained model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  # Stop parallel processing
      })
    }
  )
  
  # Event handling for `rlm` model loading
  observeEvent(
    input$rlm_Load,
    {
      method  <- "rlm"
      model <- loadRds(method, session)  # Load the saved model
      if (!is.null(model)) {
        models[[method]] <- model  # Store the loaded model in the reactive list
      }
    }
  )
  
  # Event handling for `rlm` model deletion
  observeEvent(
    input$rlm_Delete,
    {
      models[["rlm"]] <- NULL
      gc()  # Perform garbage collection
    }
  )
  
  # Output for `rlm` Model Summary
  output$rlm_ModelSummary0 <- renderText({
    description("rlm")  # Use the caret method name
  })
  
  # Output for `rlm` Metrics
  output$rlm_Metrics <- renderTable({
    req(models$rlm)  # Ensure the model is present
    models$rlm$results[which.min(models$rlm$results[, "RMSE"]), ]  # Best RMSE result
  })
  
  # Output for `rlm` Model Plots
  output$rlm_ModelPlots <- renderPlot({
    req(models$rlm)  # Ensure the `rlm` model is available
    plot(models$rlm)  # Plot the `rlm` model
  })
  
  # Output for `rlm` Recipe
  output$rlm_Recipe <- renderPrint({
    req(models$rlm)  # Require the `rlm` model
    models$rlm$recipe  # Print the recipe used for `rlm`
  })
  
  # Output for `rlm` Model Summary (detailed)
  output$rlm_ModelSummary2 <- renderPrint({
    req(models$rlm)  # Require the `rlm` model
    summary(models$rlm$finalModel)  # Detailed summary of the `rlm` model
  })
  
  # Output for `rlm` Coefficients
  output$rlm_Coef <- renderTable({
    req(models$rlm)  # Require the `rlm` model
    co <- coef(models$rlm$finalModel)  # Get coefficients for `rlm`
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
#----------------------------------------------------------------------------------------------------------------  
  # reactive getRpartSeRecipe ----
  getRpartSeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpartSe_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent rpartSe_Go ----
  observeEvent(
    input$rpartSe_Go,
    {
      method <- "rpart1SE"  # Assuming 'rpart2' is your method identifier for Rpart SE
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartSeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  # Use the appropriate na.action for Rpart
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent rpartSe_Load ----
  observeEvent(
    input$rpartSe_Load,
    {
      method  <- "rpart1SE"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  # observeEvent rpartSe_Delete ----
  observeEvent(
    input$rpartSe_Delete,
    {
      models[["rpart1SE"]] <- NULL
      gc()
    }
  )
  
  # output rpartSe_ModelSummary (print) ----
  output$rpartSe_ModelSummary <- renderText({
    description("rpart1SE")  # Use the caret method name here
  })
  
  # output rpartSe_Metrics (table) ----
  output$rpartSe_Metrics <- renderTable({
    req(models$rpart1SE)
    models$rpart1SE$results[which.min(models$rpart1SE$results[, "RMSE"]), ]
  })
  
  # output rpartSe_Recipe (print) ----
  output$rpartSe_Recipe <- renderPrint({
    req(models$rpart1SE)
    models$rpart1SE$recipe
  })
  
  # output rpartSe_ModelPlots (plot) ----
  output$rpartSe_ModelPlots <- renderPlot({
    req(models$rpart1SE)
    plot(models$rpart1SE)
  })
  
  # output rpartSe_ModelTree (plot) ----
  output$rpartSe_ModelTree <- renderPlot({
    req(models$rpart1SE)
    rpart.plot::rpart.plot(models$rpart1SE$finalModel, roundint = FALSE)
  })
#----------------------------------------------------------------------------- 
  # reactive getRpart2Recipe ----
  getRpart2Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart2_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart2_Go ----
  observeEvent(
    input$rpart2_Go,
    {
      method <- "rpart2"  # Updated method identifier for Rpart2
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpart2Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  # Adjust na.action if necessary for Rpart2
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent rpart2_Load ----
  observeEvent(
    input$rpart2_Load,
    {
      method  <- "rpart2"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  # observeEvent rpart2_Delete ----
  observeEvent(
    input$rpart2_Delete,
    {
      models[["rpart2"]] <- NULL
      gc()
    }
  )
  
  # output rpart2_ModelSummary (print) ----
  output$rpart2_ModelSummary <- renderText({
    description("rpart2")  # Use the caret method name here
  })
  
  # output rpart2_Metrics (table) ----
  output$rpart2_Metrics <- renderTable({
    req(models$rpart2)
    models$rpart2$results[which.min(models$rpart2$results[, "RMSE"]), ]
  })
  
  # output rpart2_Recipe (print) ----
  output$rpart2_Recipe <- renderPrint({
    req(models$rpart2)
    models$rpart2$recipe
  })
  
  # output rpart2_ModelPlots (plot) ----
  output$rpart2_ModelPlots <- renderPlot({
    req(models$rpart2)
    plot(models$rpart2)
  })
  
  # output rpart2_ModelTree (plot) ----
  output$rpart2_ModelTree <- renderPlot({
    req(models$rpart2)
    rpart.plot::rpart.plot(models$rpart2$finalModel, roundint = FALSE)
  })
  
  
#------------------------------------------------------------------------------  
  # reactive getCtreeRecipe ----
  getCtreeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ctree_Preprocess) %>%
      step_rm(has_type("date"))
  })
  
  # observeEvent ctree_Go ----
  observeEvent(
    input$ctree_Go,
    {
      method <- "ctree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getCtreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)  # ctree does not typically use a specific na.action unlike rpart
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent ctree_Load ----
  observeEvent(
    input$ctree_Load,
    {
      method  <- "ctree"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  # observeEvent ctree_Delete ----
  observeEvent(
    input$ctree_Delete,
    {
      models[["ctree"]] <- NULL
      gc()
    }
  )
  
  # output ctree_ModelSummary (print) ----
  output$ctree_ModelSummary <- renderText({
    description("ctree")  # Use the caret method name here
  })
  
  # output ctree_Metrics (table) ----
  output$ctree_Metrics <- renderTable({
    req(models$ctree)
    models$ctree$results[which.min(models$ctree$results[, "RMSE"]), ]
  })
  
  # output ctree_Recipe (print) ----
  output$ctree_Recipe <- renderPrint({
    req(models$ctree)
    models$ctree$recipe
  })
  
  # output ctree_ModelPlots (plot) ----
  output$ctree_ModelPlots <- renderPlot({
    req(models$ctree)
    plot(models$ctree)
  })
  
  output$ctree_ModelTree <- renderPlot({
    req(models$ctree)
    # Check if the final model is of class 'party'
    if (inherits(models$ctree$finalModel, "party")) {
      partykit::plot.party(models$ctree$finalModel)
    } else {
      # If not, handle the error or try a different plot method
      print("Not a 'party' class model")
    }
  })
  
#------------------------------------------------------------------------------------------------  
  getNnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$nnet_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `nnet` model training
  observeEvent(
    input$nnet_Go,
    {
      method <- "nnet"  # Method name for Neural Network
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)  # Start parallel processing
      tryCatch({
        model <- caret::train(getNnetRecipe(), data = getTrainData(), method = "nnet", metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 3, na.action = na.omit, linout = TRUE)  # Training and tuning
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model  # Store the trained model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)  # Stop parallel processing
      })
    }
  )
  
  # Event handling for `nnet` model loading
  observeEvent(
    input$nnet_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)  # Load the saved model
      if (!is.null(model)) {
        models[[method]] <- model  # Store the loaded model in the reactive list
      }
    }
  )
  
  # Event handling for `nnet` model deletion
  observeEvent(
    input$nnet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()  # Perform garbage collection
    }
  )
  
  # Output for `nnet` Model Summary
  output$nnet_ModelSummary0 <- renderText({
    description("nnet")  # Use the caret method name
  })
  
  # Output for `nnet` Metrics
  output$nnet_Metrics <- renderTable({
    req(models$nnet)  # Ensure the model is present
    models$nnet$results[which.min(models$nnet$results[, "RMSE"]), ]  # Best RMSE result
  })
  
  # Output for `nnet` Model Plots
  output$nnet_ModelPlots <- renderPlot({
    req(models$nnet)  # Ensure the `nnet` model is available
    plot(models$nnet)  # Plot the `nnet` model
  })
  
  # Output for `nnet` Recipe
  output$nnet_Recipe <- renderPrint({
    req(models$nnet)  # Require the `nnet` model
    models$nnet$recipe  # Print the recipe used for `nnet`
  })
  
  # Output for `nnet` Model Summary (detailed)
  output$nnet_ModelSummary2 <- renderPrint({
    req(models$nnet)  # Require the `nnet` model
    summary(models$nnet$finalModel)  # Detailed summary of the `nnet` model
  })
  
  # Output for `nnet` Coefficients
  output$nnet_Coef <- renderTable({
    req(models$nnet)  # Require the `nnet` model
    co <- coef(models$nnet$finalModel)  # Get coefficients for `nnet`
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
#-----------------------------------------------------------------------------------------------------------------
  
  
  getPcaNNetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pcaNNet_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Training event for pcaNNet
  observeEvent(input$pcaNNet_Go, {
    method <- "pcaNNet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing, adjust function accordingly
    tryCatch({
      model <- caret::train(getPcaNNetRecipe(), data = getTrainData(), method = "pcaNNet", metric = "RMSE", trControl = getTrControl(),
                            preProcess = "pca", tuneLength = 3, na.action = na.omit, linout = TRUE)
      deleteRds(method)  # Function to delete previous model file, if needed
      saveToRds(model, method)  # Function to save the model to an RDS file
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method, session = session)
      stopMode(clus)  # Stop parallel processing, adjust function accordingly
    })
  })
  
  # Event handling for `nnet` model loading
  observeEvent(
    input$pcaNNet_Load,
    {
      method  <- "pcaNNet"
      model <- loadRds(method, session)  # Load the saved model
      if (!is.null(model)) {
        models[[method]] <- model  # Store the loaded model in the reactive list
      }
    }
  )
  
  # Event handling for `nnet` model deletion
  observeEvent(
    input$pcaNNet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()  # Perform garbage collection
    }
  )
  

  
  # Output: pcaNNet Model Summary (Short)
  output$pcaNNet_ModelSummary0 <- renderText({
    req(models$pcaNNet)
    paste("PCA Neural Network model trained with", input$pcaNNet_Preprocess, "preprocessing.")
  })
  
  # Output: pcaNNet Model Metrics
  output$pcaNNet_Metrics <- renderTable({
    req(models$pcaNNet)
    models$pcaNNet$results[which.min(models$pcaNNet$results[, "RMSE"]), ]
  })
  
  # Output: pcaNNet Model Plots
  output$pcaNNet_ModelPlots <- renderPlot({
    req(models$pcaNNet)
    plot(models$pcaNNet)
  })
  
  # Output: pcaNNet Recipe
  output$pcaNNet_Recipe <- renderPrint({
    req(models$pcaNNet)
    summary(getPcaNNetRecipe())
  })
  
  # Output: pcaNNet Model Summary (Detailed)
  output$pcaNNet_ModelSummary2 <- renderPrint({
    req(models$pcaNNet)
    summary(models$pcaNNet$finalModel)
  })
  
  # Output: pcaNNet Coefficients
  output$pcaNNet_Coef <- renderTable({
    req(models$pcaNNet)
    co <- coef(models$pcaNNet$finalModel)  # Get coefficients of the final model
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
 
#---------------------------------------------------------------------------------------------------------------
  getAvNNetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$avNNet_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for avNNet model training
  observeEvent(input$avNNet_Go, {
    method <- "avNNet"  # Method name for Averaged Neural Network
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getAvNNetRecipe(), data = getTrainData(), method = "avNNet", metric = "RMSE", trControl = getTrControl(),
                            tuneLength = 3,na.action = na.omit,  linout = TRUE, trace = FALSE, repeats = 5)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model  # Store the trained model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)  # Stop parallel processing
    })
  })
  
  # Event handling for avNNet model loading
  observeEvent(input$avNNet_Load, {
    method  <- "avNNet"
    model <- loadRds(method, session)  # Load the saved model
    if (!is.null(model)) {
      models[[method]] <- model  # Store the loaded model in the reactive list
    }
  })
  
  # Event handling for avNNet model deletion
  observeEvent(input$avNNet_Delete, {
    models[["avNNet"]] <- NULL
    gc()  # Perform garbage collection
  })
  
  # Outputs
  output$avNNet_ModelSummary0 <- renderText({
    description("avNNet")  # Use the caret method name
  })
  
  output$avNNet_Metrics <- renderTable({
    req(models$avNNet)  # Ensure the model is present
    models$avNNet$results[which.min(models$avNNet$results[, "RMSE"]), ]  # Best RMSE result
  })
  
  output$avNNet_ModelPlots <- renderPlot({
    req(models$avNNet)  # Ensure the avNNet model is available
    plot(models$avNNet)  # Plot the avNNet model
  })
  
  output$avNNet_Recipe <- renderPrint({
    req(models$avNNet)  # Require the avNNet model
    models$avNNet$recipe  # Print the recipe used for avNNet
  })
  
  output$avNNet_ModelSummary2 <- renderPrint({
    req(models$avNNet)  # Require the avNNet model
    summary(models$avNNet$finalModel)  # Detailed summary of the avNNet model
  })
  
  output$avNNet_Coef <- renderTable({
    req(models$avNNet)  # Require the avNNet model
    co <- coef(models$avNNet$finalModel)  # Get coefficients for avNNet
    as.data.frame(co, row.names = rownames(co))  # Convert to a data frame
  }, rownames = TRUE, colnames = FALSE)
  
#--------------------------------------------------------------------------------------
  library(monmlp)
  getMonMlpRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$monmlp_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for monmlp model training
  observeEvent(input$monmlp_Go, {
    method <- "monmlp"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(getMonMlpRecipe(), data = getTrainData(), method = "monmlp", metric = "RMSE", trControl = getTrControl(),
                            tuneLength = 3)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for monmlp model loading
  observeEvent(input$monmlp_Load, {
    method  <- "monmlp"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for monmlp model deletion
  observeEvent(input$monmlp_Delete, {
    models[["monmlp"]] <- NULL
    gc()
  })
  
  # Outputs
  output$monmlp_ModelSummary0 <- renderText({
    description("monmlp")
  })
  
  output$monmlp_Metrics <- renderTable({
    req(models$monmlp)
    models$monmlp$results[which.min(models$monmlp$results[, "RMSE"]), ]
  })
  
  output$monmlp_ModelPlots <- renderPlot({
    req(models$monmlp)
    plot(models$monmlp)
  })
  
  output$monmlp_Recipe <- renderPrint({
    req(models$monmlp)
    models$monmlp$recipe
  })
  
  output$monmlp_ModelSummary2 <- renderPrint({
    req(models$monmlp)
    summary(models$monmlp$finalModel)
  })
  
  output$monmlp_Coef <- renderTable({
    req(models$monmlp)
    co <- coef(models$monmlp$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
 #------------------------------------------------------------------------------------------------------------------
  getBlackboostRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$blackboost_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for blackboost model training
  observeEvent(input$blackboost_Go, {
    method <- "blackboost"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(getBlackboostRecipe(), data = getTrainData(), method = "blackboost", metric = "RMSE", trControl = getTrControl(),
                            tuneLength = 5)  # Configure specific blackboost parameters if necessary
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for blackboost model loading
  observeEvent(input$blackboost_Load, {
    method  <- "blackboost"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for blackboost model deletion
  observeEvent(input$blackboost_Delete, {
    models[["blackboost"]] <- NULL
    gc()
  })
  
  # Outputs
  output$blackboost_ModelSummary <- renderText({
    description("blackboost")  # Use the caret method name here
  })
  
  output$blackboost_Metrics <- renderTable({
    req(models$blackboost)
    models$blackboost$results[which.min(models$blackboost$results[, "RMSE"]), ]
  })
  
  output$blackboost_Recipe <- renderPrint({
    req(models$blackboost)
    models$blackboost$recipe
  })
  
  # output blackboost_ModelPlots (plot) ----
  output$blackboost_ModelPlots <- renderPlot({
    req(models$blackboost)
    plot(models$blackboost)
  })
  
  output$blackboost_LearnerPlot <- renderPlot({
    req(models$blackboost)
    if (inherits(models$blackboost$finalModel, "mboost")) {
      mboost::plot(models$blackboost$finalModel, which = 1)  # Adjust 'which' as necessary
    } else {
      print("Not a 'mboost' class model")
    }
  })
  
#-------------------------------------------------------------------------------------------------------------
  getBstTreeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bstTree_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for bstTree model training
  observeEvent(input$bstTree_Go, {
    method <- "bstTree"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)
    tryCatch({
      model <- caret::train(getBstTreeRecipe(), data = getTrainData(), method = "bstTree", metric = "RMSE", trControl = getTrControl(),
                            tuneLength = 5)  # Configure bst specific parameters
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    }, 
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for bstTree model loading
  observeEvent(input$bstTree_Load, {
    method  <- "bstTree"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for bstTree model deletion
  observeEvent(input$bstTree_Delete, {
    models[["bstTree"]] <- NULL
    gc()
  })
  
  # Outputs
  output$bstTree_ModelSummary <- renderText({
    description("bstTree")  # Use the caret method name here
  })
  
  output$bstTree_Metrics <- renderTable({
    req(models$bstTree)
    models$bstTree$results[which.min(models$bstTree$results[, "RMSE"]), ]
  })
  
  output$bstTree_Recipe <- renderPrint({
    req(models$bstTree)
    models$bstTree$recipe
  })
  
  # output bstTree_ModelPlots (plot)
  output$bstTree_ModelPlots <- renderPlot({
    req(models$bstTree)
    plot(models$bstTree)
  })
  
  output$bstTree_StructurePlot <- renderPlot({
    req(models$bstTree)
    if (inherits(models$bstTree$finalModel, "bst")) {
      plot(models$bstTree$finalModel, which = 1)  # Use the generic plot function
    } else {
      print("Not a 'bst' class model")
    }
  })
  
#-------------------------------------------------------------------------------------------------------------
  library(kernlab)  # Required for 'rvmRadial'
  
  getrvmRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rvmRadial_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `rvmRadial` model training
  observeEvent(
    input$rvmRadial_Go,
    {
      method <- "rvmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)  # Start parallel processing
      tryCatch({
        model <- caret::train(getrvmRadialRecipe(), data = getTrainData(), method = "rvmRadial", metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.omit)  # Training and tuning
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  observeEvent(input$rvmRadial_Load, {
    method  <- "rvmRadial"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  
  observeEvent(input$rvmRadial_Delete, {
    models[["rvmRadial"]] <- NULL
    gc()
  }) 
  
  
  # Output sections: adjust all references from svmRadial to rvmRadial
  output$rvmRadial_ModelSummary0 <- renderText({
    description("rvmRadial")
  })
  
  output$rvmRadial_Metrics <- renderTable({
    req(models$rvmRadial)
    models$rvmRadial$results[which.min(models$rvmRadial$results[, "RMSE"]), ]
  })
  
  output$rvmRadial_ModelPlots <- renderPlot({
    req(models$rvmRadial)
    plot(models$rvmRadial)
  })
  
  output$rvmRadial_Recipe <- renderPrint({
    req(models$rvmRadial)
    models$rvmRadial$recipe
  })
  
  output$rvmRadial_ModelSummary2 <- renderPrint({
    req(models$rvmRadial)
    summary(models$rvmRadial$finalModel)
  })
  
  output$rvmRadial_Coef <- renderTable({
    req(models$rvmRadial)
    co <- coef(models$rvmRadial$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  #---------------------------------------------------------------------------------------------
  
  getsvmRadialCostRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadialCost_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `svmRadialCost` model training
  observeEvent(input$svmRadialCost_Go, {
    method <- "svmRadialCost"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getsvmRadialCostRecipe(), data = getTrainData(), method = "svmRadialCost", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `svmRadialCost` model loading
  observeEvent(input$svmRadialCost_Load, {
    method  <- "svmRadialCost"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `svmRadialCost` model deletion
  observeEvent(input$svmRadialCost_Delete, {
    models[["svmRadialCost"]] <- NULL
    gc()
  })
  
  output$svmRadialCost_ModelSummary0 <- renderText({
    description("svmRadialCost")
  })
  
  output$svmRadialCost_Metrics <- renderTable({
    req(models$svmRadialCost)
    models$svmRadialCost$results[which.min(models$svmRadialCost$results[, "RMSE"]), ]
  })
  
  output$svmRadialCost_ModelPlots <- renderPlot({
    req(models$svmRadialCost)
    plot(models$svmRadialCost)
  })
  
  output$svmRadialCost_Recipe <- renderPrint({
    req(models$svmRadialCost)
    models$svmRadialCost$recipe
  })
  
  output$svmRadialCost_ModelSummary2 <- renderPrint({
    req(models$svmRadialCost)
    summary(models$svmRadialCost$finalModel)
  })
  
  output$svmRadialCost_Coef <- renderTable({
    req(models$svmRadialCost)
    co <- coef(models$svmRadialCost$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  #---------------------------------------------------------------------------------------------------------------
  
  library(LiblineaR)  # Assuming knn still requires SVM functionality
  
  # Reactive `knn` Recipe
  getknnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$knn_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `knn` model training
  observeEvent(input$knn_Go, {
    method <- "knn"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getknnRecipe(), data = getTrainData(), method = "knn", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `knn` model loading
  observeEvent(input$knn_Load, {
    method  <- "knn"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `knn` model deletion
  observeEvent(input$knn_Delete, {
    models[["knn"]] <- NULL
    gc()
  })
  
  output$knn_ModelSummary0 <- renderText({
    description("knn")
  })
  
  output$knn_Metrics <- renderTable({
    req(models$knn)
    models$knn$results[which.min(models$knn$results[, "RMSE"]), ]
  })
  
  output$knn_ModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })
  
  output$knn_Recipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })
  
  output$knn_ModelSummary2 <- renderPrint({
    req(models$knn)
    summary(models$knn$finalModel)
  })
  
  output$knn_Coef <- renderTable({
    req(models$knn)
    co <- coef(models$knn$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  #-------------------------------------------------------------------------------------------------------------------
  getGaussprRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprRadial_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `gaussprRadial` model training
  observeEvent(input$gaussprRadial_Go, {
    method <- "gaussprRadial"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getGaussprRadialRecipe(), data = getTrainData(), method = "gaussprRadial", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `gaussprRadial` model loading
  observeEvent(input$gaussprRadial_Load, {
    method  <- "gaussprRadial"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `gaussprRadial` model deletion
  observeEvent(input$gaussprRadial_Delete, {
    models[["gaussprRadial"]] <- NULL
    gc()
  })
  
  output$gaussprRadial_ModelSummary0 <- renderText({
    description("gaussprRadial")
  })
  
  output$gaussprRadial_Metrics <- renderTable({
    req(models$gaussprRadial)
    models$gaussprRadial$results[which.min(models$gaussprRadial$results[, "RMSE"]), ]
  })
  
  output$gaussprRadial_ModelPlots <- renderPlot({
    req(models$gaussprRadial)
    plot(models$gaussprRadial)
  })
  
  output$gaussprRadial_Recipe <- renderPrint({
    req(models$gaussprRadial)
    models$gaussprRadial$recipe
  })
  
  output$gaussprRadial_ModelSummary2 <- renderPrint({
    req(models$gaussprRadial)
    summary(models$gaussprRadial$finalModel)
  })
  
  output$gaussprRadial_Coef <- renderTable({
    req(models$gaussprRadial)
    co <- coef(models$gaussprRadial$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
#--------------------------------------------------------------------------------------------------------------------

  getQrnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$qrnn_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `qrnn` model training
  observeEvent(input$qrnn_Go, {
    method <- "qrnn"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getQrnnRecipe(), data = getTrainData(), method = "qrnn", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `qrnn` model loading
  observeEvent(input$qrnn_Load, {
    method  <- "qrnn"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `qrnn` model deletion
  observeEvent(input$qrnn_Delete, {
    models[["qrnn"]] <- NULL
    gc()
  })
  
  output$qrnn_ModelSummary0 <- renderText({
    description("qrnn")
  })
  
  output$qrnn_Metrics <- renderTable({
    req(models$qrnn)
    models$qrnn$results[which.min(models$qrnn$results[, "RMSE"]), ]
  })
  
  output$qrnn_ModelPlots <- renderPlot({
    req(models$qrnn)
    plot(models$qrnn)
  })
  
  output$qrnn_Recipe <- renderPrint({
    req(models$qrnn)
    models$qrnn$recipe
  })
  
  output$qrnn_ModelSummary2 <- renderPrint({
    req(models$qrnn)
    summary(models$qrnn$finalModel)
  })
  
  output$qrnn_Coef <- renderTable({
    req(models$qrnn)
    co <- coef(models$qrnn$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  #-----------------------------------------------------------------------
  
  getM5Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$M5_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `M5` model training
  observeEvent(input$M5_Go, {
    method <- "M5"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getM5Recipe(), data = getTrainData(), method = "M5", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `M5` model loading
  observeEvent(input$M5_Load, {
    method  <- "M5"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `M5` model deletion
  observeEvent(input$M5_Delete, {
    models[["M5"]] <- NULL
    gc()
  })
  
  output$M5_ModelSummary0 <- renderText({
    description("M5")
  })
  
  output$M5_Metrics <- renderTable({
    req(models$M5)
    models$M5$results[which.min(models$M5$results[, "RMSE"]), ]
  })
  
  output$M5_ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })
  
  output$M5_Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })
  
  output$M5_ModelSummary2 <- renderPrint({
    req(models$M5)
    summary(models$M5$finalModel)
  })
  
  output$M5_Coef <- renderTable({
    req(models$M5)
    co <- coef(models$M5$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
#----------------------------------------------------------------------------------------------
  
  getM5RulesRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$M5Rules_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `M5Rules` model training
  observeEvent(input$M5Rules_Go, {
    method <- "M5Rules"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getM5RulesRecipe(), data = getTrainData(), method = "M5Rules", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `M5Rules` model loading
  observeEvent(input$M5Rules_Load, {
    method  <- "M5Rules"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `M5Rules` model deletion
  observeEvent(input$M5Rules_Delete, {
    models[["M5Rules"]] <- NULL
    gc()
  })
  
  output$M5Rules_ModelSummary0 <- renderText({
    description("M5Rules")
  })
  
  output$M5Rules_Metrics <- renderTable({
    req(models$M5Rules)
    models$M5Rules$results[which.min(models$M5Rules$results[, "RMSE"]), ]
  })
  
  output$M5Rules_ModelPlots <- renderPlot({
    req(models$M5Rules)
    plot(models$M5Rules)
  })
  
  output$M5Rules_Recipe <- renderPrint({
    req(models$M5Rules)
    models$M5Rules$recipe
  })
  
  output$M5Rules_ModelSummary2 <- renderPrint({
    req(models$M5Rules)
    summary(models$M5Rules$finalModel)
  })
  
  output$M5Rules_Coef <- renderTable({
    req(models$M5Rules)
    co <- coef(models$M5Rules$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
#--------------------------------------------------------------------------------------------------
  
  getGaussprPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprPoly_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `gaussprPoly` model training
  observeEvent(input$gaussprPoly_Go, {
    method <- "gaussprPoly"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getGaussprPolyRecipe(), data = getTrainData(), method = "gaussprPoly", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `gaussprPoly` model loading
  observeEvent(input$gaussprPoly_Load, {
    method  <- "gaussprPoly"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `gaussprPoly` model deletion
  observeEvent(input$gaussprPoly_Delete, {
    models[["gaussprPoly"]] <- NULL
    gc()
  })
  
  output$gaussprPoly_ModelSummary0 <- renderText({
    description("gaussprPoly")
  })
  
  output$gaussprPoly_Metrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  output$gaussprPoly_ModelPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })
  
  output$gaussprPoly_Recipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })
  
  output$gaussprPoly_ModelSummary2 <- renderPrint({
    req(models$gaussprPoly)
    summary(models$gaussprPoly$finalModel)
  })
  
  output$gaussprPoly_Coef <- renderTable({
    req(models$gaussprPoly)
    co <- coef(models$gaussprPoly$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
#-----------------------------------------------------------------------------------------
  getTreeBagRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$treebag_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `treebag` model training
  observeEvent(input$treebag_Go, {
    method <- "treebag"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getTreeBagRecipe(), data = getTrainData(), method = "treebag", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `treebag` model loading
  observeEvent(input$treebag_Load, {
    method  <- "treebag"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `treebag` model deletion
  observeEvent(input$treebag_Delete, {
    models[["treebag"]] <- NULL
    gc()
  })
  
  output$treebag_ModelSummary0 <- renderText({
    description("treebag")
  })
  
  output$treebag_Metrics <- renderTable({
    req(models$treebag)
    models$treebag$results[which.min(models$treebag$results[, "RMSE"]), ]
  })
  
  #output$treebag_ModelPlots <- renderPlot({
  #  req(models$treebag)
  #  plot(models$treebag)
  #})
  
  output$treebag_Recipe <- renderPrint({
    req(models$treebag)
    models$treebag$recipe
  })
  
  output$treebag_ModelSummary2 <- renderPrint({
    req(models$treebag)
    summary(models$treebag$finalModel)
  })
  
  output$treebag_Coef <- renderTable({
    req(models$treebag)
    co <- coef(models$treebag$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
#-----------------------------------------------------------------------------------------
  
  getBrnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$brnn_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `brnn` model training
  observeEvent(input$brnn_Go, {
    method <- "brnn"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getBrnnRecipe(), data = getTrainData(), method = "brnn", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `brnn` model loading
  observeEvent(input$brnn_Load, {
    method  <- "brnn"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `brnn` model deletion
  observeEvent(input$brnn_Delete, {
    models[["brnn"]] <- NULL
    gc()
  })
  
  output$brnn_ModelSummary0 <- renderText({
    description("brnn")
  })
  
  output$brnn_Metrics <- renderTable({
    req(models$brnn)
    models$brnn$results[which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  output$brnn_ModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  output$brnn_Recipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })
  
  output$brnn_ModelSummary2 <- renderPrint({
    req(models$brnn)
    summary(models$brnn$finalModel)
  })
  
  output$brnn_Coef <- renderTable({
    req(models$brnn)
    co <- coef(models$brnn$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  #----------------------------------------------------
  
  
  getKrlsPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$krlsPoly_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `krlsPoly` model training
  observeEvent(input$krlsPoly_Go, {
    method <- "krlsPoly"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getKrlsPolyRecipe(), data = getTrainData(), method = "krlsPoly", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `krlsPoly` model loading
  observeEvent(input$krlsPoly_Load, {
    method  <- "krlsPoly"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `krlsPoly` model deletion
  observeEvent(input$krlsPoly_Delete, {
    models[["krlsPoly"]] <- NULL
    gc()
  })
  
  output$krlsPoly_ModelSummary0 <- renderText({
    description("krlsPoly")
  })
  
  output$krlsPoly_Metrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  output$krlsPoly_ModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })
  
  output$krlsPoly_Recipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })
  
  output$krlsPoly_ModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    summary(models$krlsPoly$finalModel)
  })
  
  output$krlsPoly_Coef <- renderTable({
    req(models$krlsPoly)
    co <- coef(models$krlsPoly$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
 #---------------------------------------------------------------------------------------------------------------------------
  
  getMlpRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$mlp_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `mlp` model training
  observeEvent(input$mlp_Go, {
    method <- "mlp"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getMlpRecipe(), data = getTrainData(), method = "mlp", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `mlp` model loading
  observeEvent(input$mlp_Load, {
    method  <- "mlp"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `mlp` model deletion
  observeEvent(input$mlp_Delete, {
    models[["mlp"]] <- NULL
    gc()
  })
  
  output$mlp_ModelSummary0 <- renderText({
    description("mlp")
  })
  
  output$mlp_Metrics <- renderTable({
    req(models$mlp)
    models$mlp$results[which.min(models$mlp$results[, "RMSE"]), ]
  })
  
  output$mlp_ModelPlots <- renderPlot({
    req(models$mlp)
    plot(models$mlp)
  })
  
  output$mlp_Recipe <- renderPrint({
    req(models$mlp)
    models$mlp$recipe
  })
  
  output$mlp_ModelSummary2 <- renderPrint({
    req(models$mlp)
    summary(models$mlp$finalModel)
  })
  
  output$mlp_Coef <- renderTable({
    req(models$mlp)
    co <- coef(models$mlp$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  #--------------------------------------------------
  
  getSvmPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmPoly_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  observeEvent(input$svmPoly_Go, {
    method <- "svmPoly"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      # Adjusting seeds appropriately
      set.seed(123)  # Set a seed for reproducibility
      numResamples <- getTrControl()$number
      seedList <- vector(mode = "list", length = numResamples + 1)
      for (i in 1:numResamples) {
        seedList[[i]] <- sample.int(2000, 1875, replace = TRUE)  # Increase population size or set `replace = TRUE`
      }
      seedList[[numResamples + 1]] <- sample.int(2000, 1)
      
      # Adjust trControl with proper seeds
      trControl <- getTrControl()
      trControl$seeds <- seedList
      
      model <- caret::train(
        getSvmPolyRecipe(), 
        data = getTrainData(), 
        method = "svmPoly", 
        metric = "RMSE", 
        trControl = trControl, 
        tuneLength = 25
      )  # Training and tuning
      
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  
  # Event handling for `svmPoly` model loading
  observeEvent(input$svmPoly_Load, {
    method  <- "svmPoly"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `svmPoly` model deletion
  observeEvent(input$svmPoly_Delete, {
    models[["svmPoly"]] <- NULL
    gc()
  })
  
  output$svmPoly_ModelSummary0 <- renderText({
    description("svmPoly")
  })
  
  output$svmPoly_Metrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  output$svmPoly_ModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  output$svmPoly_Recipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })
  
  output$svmPoly_ModelSummary2 <- renderPrint({
    req(models$svmPoly)
    summary(models$svmPoly$finalModel)
  })
  
  output$svmPoly_Coef <- renderTable({
    req(models$svmPoly)
    co <- coef(models$svmPoly$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  #--------------------------------------------
  
  getElmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$elm_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `elm` model training
  observeEvent(input$elm_Go, {
    method <- "elm"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getElmRecipe(), data = getTrainData(), method = "elm", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `elm` model loading
  observeEvent(input$elm_Load, {
    method  <- "elm"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `elm` model deletion
  observeEvent(input$elm_Delete, {
    models[["elm"]] <- NULL
    gc()
  })
  
  output$elm_ModelSummary0 <- renderText({
    description("elm")
  })
  
  output$elm_Metrics <- renderTable({
    req(models$elm)
    models$elm$results[which.min(models$elm$results[, "RMSE"]), ]
  })
  
  output$elm_ModelPlots <- renderPlot({
    req(models$elm)
    plot(models$elm)
  })
  
  output$elm_Recipe <- renderPrint({
    req(models$elm)
    models$elm$recipe
  })
  
  output$elm_ModelSummary2 <- renderPrint({
    req(models$elm)
    summary(models$elm$finalModel)
  })
  
  output$elm_Coef <- renderTable({
    req(models$elm)
    co <- coef(models$elm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
#----------------------------------------------------
  
  getLmStepAICRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lmStepAIC_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `lmStepAIC` model training
  observeEvent(input$lmStepAIC_Go, {
    method <- "lmStepAIC"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getLmStepAICRecipe(), data = getTrainData(), method = "lmStepAIC", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25, na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `lmStepAIC` model loading
  observeEvent(input$lmStepAIC_Load, {
    method  <- "lmStepAIC"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `lmStepAIC` model deletion
  observeEvent(input$lmStepAIC_Delete, {
    models[["lmStepAIC"]] <- NULL
    gc()
  })
  
  output$lmStepAIC_ModelSummary0 <- renderText({
    description("lmStepAIC")
  })
  
  output$lmStepAIC_Metrics <- renderTable({
    req(models$lmStepAIC)
    models$lmStepAIC$results[which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })
  
  output$lmStepAIC_ModelPlots <- renderPlot({
    req(models$lmStepAIC)
    plot(models$lmStepAIC)
  })
  
  output$lmStepAIC_Recipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })
  
  output$lmStepAIC_ModelSummary2 <- renderPrint({
    req(models$lmStepAIC)
    summary(models$lmStepAIC$finalModel)
  })
  
  output$lmStepAIC_Coef <- renderTable({
    req(models$lmStepAIC)
    co <- coef(models$lmStepAIC$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  #---------------------------------------------------------
  
  getMxnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$mxnet_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `mxnet` model training
  observeEvent(input$mxnet_Go, {
    method <- "mxnet"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      # Adjusting seeds appropriately
      set.seed(123)  # Set a seed for reproducibility
      numResamples <- getTrControl()$number
      seedList <- vector(mode = "list", length = numResamples + 1)
      for (i in 1:numResamples) {
        seedList[[i]] <- sample.int(2000, 1875, replace = TRUE)  # Increase population size or set `replace = TRUE`
      }
      seedList[[numResamples + 1]] <- sample.int(2000, 1)
      
      # Adjust trControl with proper seeds
      trControl <- getTrControl()
      trControl$seeds <- seedList
      model <- caret::train(getMxnetRecipe(), data = getTrainData(), method = "mxnet", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25,na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `mxnet` model loading
  observeEvent(input$mxnet_Load, {
    method  <- "mxnet"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `mxnet` model deletion
  observeEvent(input$mxnet_Delete, {
    models[["mxnet"]] <- NULL
    gc()
  })
  
  output$mxnet_ModelSummary0 <- renderText({
    description("mxnet")
  })
  
  output$mxnet_Metrics <- renderTable({
    req(models$mxnet)
    models$mxnet$results[which.min(models$mxnet$results[, "RMSE"]), ]
  })
  
  output$mxnet_ModelPlots <- renderPlot({
    req(models$mxnet)
    plot(models$mxnet)
  })
  
  output$mxnet_Recipe <- renderPrint({
    req(models$mxnet)
    models$mxnet$recipe
  })
  
  output$mxnet_ModelSummary2 <- renderPrint({
    req(models$mxnet)
    summary(models$mxnet$finalModel)
  })
  
  output$mxnet_Coef <- renderTable({
    req(models$mxnet)
    co <- coef(models$mxnet$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
#-----------------------------------------------------------------------------------------
  
  getLeapSeqRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$leapSeq_Preprocess) %>%
      step_rm(has_type("date"))  # Remove date-type variables
  })
  
  # Event handling for `leapSeq` model training
  observeEvent(input$leapSeq_Go, {
    method <- "leapSeq"
    models[[method]] <- NULL
    showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
    clus <- startMode(input$Parallel)  # Start parallel processing
    tryCatch({
      model <- caret::train(getLeapSeqRecipe(), data = getTrainData(), method = "leapSeq", metric = "RMSE", trControl = getTrControl(), 
                            tuneLength = 25,na.action = na.omit)  # Training and tuning
      deleteRds(method)
      saveToRds(model, method)
      models[[method]] <- model
    },
    finally = {
      removeNotification(id = method)
      stopMode(clus)
    })
  })
  
  # Event handling for `leapSeq` model loading
  observeEvent(input$leapSeq_Load, {
    method  <- "leapSeq"
    model <- loadRds(method, session)
    if (!is.null(model)) {
      models[[method]] <- model
    }
  })
  
  # Event handling for `leapSeq` model deletion
  observeEvent(input$leapSeq_Delete, {
    models[["leapSeq"]] <- NULL
    gc()
  })
  
  output$leapSeq_ModelSummary0 <- renderText({
    description("leapSeq")
  })
  
  output$leapSeq_Metrics <- renderTable({
    req(models$leapSeq)
    models$leapSeq$results[which.min(models$leapSeq$results[, "RMSE"]), ]
  })
  
  output$leapSeq_ModelPlots <- renderPlot({
    req(models$leapSeq)
    plot(models$leapSeq)
  })
  
  output$leapSeq_Recipe <- renderPrint({
    req(models$leapSeq)
    models$leapSeq$recipe
  })
  
  output$leapSeq_ModelSummary2 <- renderPrint({
    req(models$leapSeq)
    summary(models$leapSeq$finalModel)
  })
  
  output$leapSeq_Coef <- renderTable({
    req(models$leapSeq)
    co <- coef(models$leapSeq$finalModel)
    as.dataframe(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  
###########################################################################################################################  
  
  
  
  
  
  
  
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
