#===============================================#
createCorrelationMatrix = function(treatment, warmup, coefficient, calibration) {
    # Extraindo subset do dado
    data.subset = warmup[TN == treatment]
    correlationMatrix = matrix(NA, nrow = length(coefficient), ncol = length(calibration))

    # Adicionando nomes as linhas e colunas
    rownames(correlationMatrix) = coefficient
    colnames(correlationMatrix) = calibration

    for(calibration.index in calibration) {
      for(coefficient.index in coefficient) {
        # Extraindo indices das colunas
        calibrationGrep = grep(calibration.index, names(data.subset))
        coefficientGrep = grep(coefficient.index, names(data.subset))

        # Extraindo indices das linhas
        line = which(coefficient.index == coefficient)
        subset.index = (((line - 1) * 100) + 1): (line * 100)
        
        # Gerando correlacao
        coef = data.subset[subset.index, ..coefficientGrep]
        cali = data.subset[subset.index, ..calibrationGrep]
        
        # Calculando correlacao
        suppressWarnings({
          correlation = cor(coef, cali)
          correlation = ifelse(is.na(correlation), 0, correlation)
        })

        # Adicionando na matrix
        correlationMatrix[coefficient.index, calibration.index] = round(correlation, digits = 3)
      }
    }

    # Retornando matrix de correlacao
    return(correlationMatrix)
}
#===============================================#

#===============================================#
evaluateDifference = function(evaluateData, calibration) {
  # Obtendo variaveis simuladas de calibracao
  variableSimulated = paste0(sprintf("%sS", calibration), collapse = "|")
  variableSimulated.index = grep(variableSimulated, names(evaluateData))

  # Obtendo variaveis observadas de calibracao
  variableObserved = paste0(sprintf("%sM", calibration), collapse = "|")
  variableObserved.index = grep(variableObserved, names(evaluateData))

  # Obtendo variaveis simuladas
  calibrationDataSimulated = evaluateData[, ..variableSimulated.index]
  calibrationDataSimulated[calibrationDataSimulated == -99] = NA

  # Obtendo variaveis observadas
  calibrationDataObserved = evaluateData[, ..variableObserved.index]
  calibrationDataObserved[calibrationDataObserved == -99] = NA

  # Calculando a diferenca entre Observado e Simulado
  response = (calibrationDataObserved - calibrationDataSimulated) / abs(calibrationDataObserved)

  # Adicionando tratamento
  response$TN = evaluateData$TN

  # Retornando resposta
  return(response)
}
#===============================================#

#===============================================#
plantgroDifference = function(plantgroData, tData, calibration) {
  # Gerando regex das variaveis de calibracao
  variableRegex = paste0(c("TRNO", calibration), collapse = "|")
  
  # Obtendo variaveis simuladas de calibracao
  variableSimulated.index = grep(variableRegex, names(plantgroData))

  # Obtendo variaveis observadas de calibracao
  variableObserved.index = grep(variableRegex, names(tData))

  # Obtendo variaveis simuladas
  calibrationDataSimulated = plantgroData[, ..variableSimulated.index]
  calibrationDataSimulated[calibrationDataSimulated == -99] = NA
  calibrationDataSimulated = calibrationDataSimulated[, lapply(.SD, mean, na.rm = TRUE), by = TRNO]

  # Obtendo variaveis observadas
  calibrationDataObserved = tData[, ..variableObserved.index]
  calibrationDataObserved[calibrationDataObserved == -99] = NA
  calibrationDataObserved = calibrationDataObserved[, lapply(.SD, mean, na.rm = TRUE), by = TRNO]

  # Unindo dados e ordenando por "Tratamento"
  calibrationData = merge(calibrationDataObserved, calibrationDataSimulated, by = "TRNO")
  calibrationData = calibrationData[order(TRNO)]

  # Atualizando variaveis simuladas de calibracao
  variableSimulated = paste0(sprintf("%s.y", calibration), collapse = "|")
  variableSimulated.index = grep(variableSimulated, names(calibrationData))

  # Atuallizando variaveis observadas de calibracao
  variableObserved = paste0(sprintf("%s.x", calibration), collapse = "|")
  variableObserved.index = grep(variableObserved, names(calibrationData))

  # Calculando a diferenca entre Observado e Simulado
  response = (calibrationData[, ..variableObserved.index] - calibrationData[, ..variableSimulated.index]) / abs(calibrationData[, ..variableObserved.index])

  # Atualizando nomes
  names(response) = gsub(".x|.y", "", names(response))

  # Adicionando tratamento
  response$TN = calibrationData$TRNO

  # Retornando resposta
  return(response)
}
#===============================================#

#===============================================#
rmse = function(SSERow) {
  RMSE = sqrt(mean(SSERow^2, na.rm = T))
  return(RMSE)
}
#===============================================#
