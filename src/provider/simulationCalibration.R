#===============================================#
# Funcao responsavel por executar a simulacao
# @param (matrix) multiplyMatrix => Matrix com as multiplicacoes realizadas na simulacao
# @param (character) templateId => Identificador para os diretorios de rodada
# @param (list) inputList => Lista de parametros de execussao
# @retuns (list) Lista contendo os resultados da rodada
simulationFunction = function(multiplyMatrix, templateId, inputList) {
  # Iniciando timer
  cat(sprintf("==> [%s] iniciando rodadas.\n", templateId))
  startTime = Sys.time()
  
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
  SSE.list = lapply(1:runSize, function(index, run.list, calibration, filtrar) {
    # Separando rodada
    SSE = run.list[[index]]

    # Adiiconando identificador
    SSE$id = index

    # Retornando SSE
    return(SSE)
  }, run.list, calibration, filtrar)

  
  # Encerrando timer
  cat(sprintf("==> [%s] %s rodadas concluidas em %s.\n", templateId, runSize, round(Sys.time() - startTime, 3)))  
  return(SSE.list)
}
#===============================================#

#===============================================#
# Funcao responsavel por executar o algoritmo genetico da simulacao
# @param (list) input => Lista de parametros de execussao
# @param (list) correlationMatrix => Lista contendo a matrix de correlacao para cada tratamento
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