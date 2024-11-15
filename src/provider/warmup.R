#===============================================#
# Funcao responsavel por executar o aquecimento da simulacao
# @param (list) input => Lista de parametros de execussao
# @returns (list) Lista contendo a matrix de correlacao para cada tratamento 
runSimulationWarmup = function(input) {
  # Obtendo coeficientes
  coefficients = input$coefficients
  coefficientsSize = length(coefficients)

  # Obtendo a calibracao
  calibration = input$calibration

  # Calculando vetores de inicializacao
  baseSequence = seq(0.5, 1.5, length.out = 100)

  # Obtendo matrix de multiplicacao
  multiplyMatrix = matrix(1, ncol = coefficientsSize, nrow = coefficientsSize * 100)
  colnames(multiplyMatrix) = coefficients

  # Preenchendo matrix
  for(index in 1:coefficientsSize) {
    indexSubset = (((index - 1) * 100) + 1): (index * 100)
    multiplyMatrix[indexSubset, index] = baseSequence
  }

  # Executando felonogia
  warmup = simulationFunction(multiplyMatrix, "warmup", input)

  # Convertendo para data.table
  multiplyMatrix = as.data.table(multiplyMatrix)
  multiplyMatrix$id = 1:(coefficientsSize * 100)

  # Unificando resultados
  warmup = do.call(rbind, warmup)
  warmup = merge(warmup, multiplyMatrix, by = "id")

  # Verificando existencia de resultados NA
  calibrationRegex = paste0(calibration, collapse = "|")
  validation.index = grep(calibrationRegex, names(warmup))
  warmupValidation = warmup[, ..validation.index]

  # Verificando a existencia de colunas com todos os valores NA
  warmupValidation = apply(warmupValidation, 2, function(warmupCalibration) {
    validation = sum(is.na(warmupCalibration)) == length(warmupCalibration)
    return(!validation)
  }) 
  
  # Validando warmup
  if(sum(warmupValidation) == 0) {
    calibration.error = paste0(calibration, collapse = ", ")
    stop(sprintf("[ERROR] Nao foi possivel encontrar valores validos para as calibracoes selecionadas: %s.", calibration.error))
  }
  
  # Gerando matrix de correlacao
  treatment = unique(warmup$TN)
  correlationMatrix = lapply(treatment, createCorrelationMatrix, warmup, coefficients, calibration)

  # Diretorio de saida
  outputDir = input$outputDir

  # Gerando graficos
  status = sapply(treatment, plotWarmupCorrelation, warmup, correlationMatrix, outputDir)
  
  # Retornando matrix de correlacao
  return(correlationMatrix)
}
#===============================================#