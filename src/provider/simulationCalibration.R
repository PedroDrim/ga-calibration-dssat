#===============================================#
simulationFunction = function(multiplyMatrix, templateId, inputList) {
  # Iniciando timer
  cat(sprintf("==> [%s] iniciando rodadas.\n", templateId))
  startTime = Sys.time() |> as.numeric()
  
  # Arredondando valores devido a precisao do dssat
  multiplyMatrix = round(multiplyMatrix, digits = 2)

  # Criando diretorios das simulacoes
  simulation.list = createSimulationDirectories(multiplyMatrix, templateId, inputList)

  # Iniciando paralelismo caso esteja ativado
  noCores = as.numeric(inputList$simulationCores)

  # Obtendo modelo e calibracao
  model = as.character(inputList$model)
  calibration = as.character(inputList$calibration)

  # Obtendo arquivo de execussao do dssat
  dssatFile = as.character(inputList$dssatFile)

  if(noCores > 1) {
    # Habilitando clusters
    cl = makeForkCluster(noCores)

    # Executando Dssat em paralelo
    run.list = parLapply(cl, simulation.list, runDssat, model, dssatFile, calibration)

    # Desabilitando clusters
    stopCluster(cl)
    stopImplicitCluster()
  } else {
    # Executando Dssat em serie
    run.list = lapply(simulation.list, runDssat, model, dssatFile, calibration)
  }
    
  # Tamanho da lista de respostas
  runSize = length(run.list)

  # Obtendo resposta SSE
  SSE.list = lapply(1:runSize, function(index, run.list, calibration) {
    # Separando rodada
    SSE = run.list[[index]]

    # Adiiconando identificador
    SSE$id = index

    # Retornando SSE
    return(SSE)
  }, run.list, calibration)

  # Encerrando timer
  endTime = Sys.time() |> as.numeric()
  cat(sprintf("==> [%s] %s rodadas concluidas em '%s' segundos.\n", templateId, runSize, round(endTime - startTime, 3)))  
  return(SSE.list)
}
#===============================================#

#===============================================#
runSimulationGA = function(input, correlationMatrix) {
  # Tratando parametros de entrada
  maxIteration = as.numeric(input$maxiter)
  maxPopulation = as.numeric(input$popSize)
  coefficients = as.character(input$coefficients)
  
  # Calculando GA
  runGeneration(input, correlationMatrix, simulationFunction,
    maxIteration = maxIteration, maxPopulation = maxPopulation,
    mutationRate = 0.2, minVariation = 0.8, maxVariation = 1.2)
    
  cat("\nFim da simulacao\n")
}
#===============================================#