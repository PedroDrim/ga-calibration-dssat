#===============================================#
createInitialPopulation = function(popSize, coefficients, rangeValue = c(0.2, 1.8)) {
    # Gerando vetor inicial
    initialVector = seq(rangeValue[1], rangeValue[2], length.out = popSize)
    coefficientsSize = length(coefficients)

    # Gerando matrix da populacao inicial
    initialPopulation = matrix(rep(initialVector, coefficientsSize), ncol = coefficientsSize)
    colnames(initialPopulation) = coefficients

    # Retornando populacao inicial
    return(initialPopulation)
}
#===============================================#

#===============================================#
generateNextPopulation = function(survivors, popSize) {
    # Gerando indices da proxima populacao
    survivorsSize = nrow(survivors)
    nextPopulation.index = sample(1:survivorsSize, popSize, replace = TRUE)

    # Gerando novos individuos
    nextPopulation = survivors[nextPopulation.index,]
    
    # Obtendo indices para crossbreeding
    crossbreeding.index = sample(1:popSize)
    crossbreedingSize = floor(popSize / 2)
    
    # Obtendo tamanho do cromossomo
    cromossomeSize = ncol(nextPopulation)
    
    # Tratando caso seja cromossomo de tamanho 1
    if(length(cromossomeSize) == 0) {
        cromossomeSize = 1
        nextPopulation = matrix(nextPopulation, ncol = 1)
        return(nextPopulation)
    }

    # Iniciando crossbreeding
    for(pair.index in 1:crossbreedingSize) {
        # Separando os individuos a serem cruzados
        son1 = nextPopulation[pair.index,]
        son2 = nextPopulation[pair.index + 1,]

        # Gerando constante de cruzamento
        crossingFlag = runif(cromossomeSize, -1, 1) >= 0
        
        # Cruzando pares de genes
        nextPopulation[pair.index,] = (son1 * crossingFlag) + (son2 * !crossingFlag)
        nextPopulation[pair.index + 1,] = (son1 * !crossingFlag) + (son2 * crossingFlag)
    }

    # Retornando populacao
    return(nextPopulation)
}
#===============================================#

#===============================================#
mutatePopulation = function(population, mutationRate = 0.1, rangeVariation = c(0.2, 1.8)) {
    # Obtendo indice de mutacao
    populationSize = nrow(population)
    cromossomeSize = ncol(population)
    mutation.index = sample(1:populationSize, populationSize * mutationRate)

    # Gerando matrix de mutacao
    mutationMatrix = matrix(runif(populationSize * mutationRate * cromossomeSize, rangeVariation[1], rangeVariation[2]), ncol = cromossomeSize)

    # Aplicando variacoes (mutacao) nos valores
    population[mutation.index,] = population[mutation.index,] * mutationMatrix

    # Retornando populacao
    return(population)
}
#===============================================#

#===============================================#
fitness = function(SSE, correlationMatrix) {
    # Calculando media por calibracao
    correlationMean = sapply(correlationMatrix, function(page) {
        response = apply(page, 2, mean, na.rm = TRUE)
        return(response)
    })

    # Convertendo para data.table
    correlationMean = as.data.table(t(correlationMean))

    # Multiplicando por -1 devido a diferenca entre "Observado - Simulado"
    correlationMean = correlationMean * (-1)

    # Verificacao para ordenacao multivariavel de saida
    if(dim(correlationMean)[1] > 1) {
        # Ordenando os nomes
        sortedNames = sort(names(correlationMean))
        correlationMean = correlationMean[, ..sortedNames]

        # Ordenando os nomes
        calibrationNames = sapply(sortedNames, function(sortedName, SSE) {
            response.index = grep(sortedName, names(SSE))
            return(response.index)
        }, SSE)

        # Organizando colunas
        SSE = SSE[, ..calibrationNames]

        # Calculando rmse para tratamentos (saida multivariavel)
        rmseTreatment = apply(SSE * correlationMean, 2, rmse)
    } else {
        # Calculando rmse para tratamentos (saida univariavel)
        tn.index = -grep("TN", names(SSE))
        SSE = SSE[, ..tn.index] |> unlist()
        rmseTreatment = (SSE * correlationMean) |> sapply(rmse)
    }

    rmseTreatment[is.nan(rmseTreatment)] = NA

    # Calculando y
    y = mean(rmseTreatment, na.rm = TRUE) |> round(digits = 8)

    # Retornando valor
    return(-y)
}
#===============================================#

#===============================================#
filterPopulation = function(iterationData, cromossome, bestValue) {
    # Obtendo nomes das colunas necessarias
    iterarionData.index = c("y", cromossome)
    
    # Reduzindo dados e ordenando do melhor ao pior
    iteraitonSummary = unique(iterationData[, ..iterarionData.index])
    iteraitonSummary = iteraitonSummary[order(-y),]
    
    # Separando os melhores (20%)
    cutPoint = ceiling(nrow(iteraitonSummary) * 0.2)
    iteraitonSummary = iteraitonSummary[1:cutPoint, ]

    # Obtendo melhor valor da rodada
    best.y = iteraitonSummary$y[1]

    # Obtendo valor medio dos melhores (20%)
    mean.y = mean(iteraitonSummary$y) |> round(digits = 4)

    # Obtendo desvio padrao dos melhores (20%)
    sd.y = sd(iteraitonSummary$y) |> round(digits = 4)

    # Removendo y
    iteraitonSummary$y = NULL

    # Substituindo melhor valor
    bestValue = ifelse(best.y > bestValue, best.y, bestValue)

    # Retornando lista de resposta
    return(list(
        top20 = iteraitonSummary, 
        bestValue = bestValue, 
        meanValue = mean.y,
        sdValue = sd.y
    ))
}
#===============================================#

#===============================================#
runGeneration = function(inputList, correlationMatrix, validationFunction, maxIteration = 10, maxPopulation = 10, mutationRate = 0.1, minVariation = 0.2, maxVariation = 1.8) {
    # OBtendo diretorio de saida
    outputDir = inputList$outputDir

    # Limpando arquivo de saida
    gaRoundFile = sprintf("%s//ga_round.csv", outputDir)
    unlink(gaRoundFile)

    # Melhor valor
    bestValue = -9999

    # Extraindo cromossomos
    cromossome = inputList$coefficients
    
    # Gerando populacao inicial
    population = createInitialPopulation(maxPopulation, cromossome, c(minVariation, maxVariation))

    # Obtendo semente de geracao
    seedValue = as.numeric(inputList$seed)
    if(seedValue != 0) {
        cat(sprintf("[Configuration] Definindo semente de geracao para: '%s'\n", seedValue))
    }

    # Iterando as geracoes
    for(iteration in 1:maxIteration) {

        # Gerando populacao da geracao
        roundPopulation = generateNextPopulation(population, maxPopulation)

        # Aplicando semente de geracao da mutacao
        if(seedValue != 0) {
            set.seed(seedValue)
        }

        # Aplicando mutacao
        roundPopulation = mutatePopulation(roundPopulation, mutationRate = mutationRate, c(minVariation, maxVariation))

        # Arredondando valores
        roundPopulation = round(roundPopulation, digits = 3)

        # Executando SSE
        SSE.list = simulationFunction(roundPopulation, sprintf("iteration_%s", iteration), inputList)

        # Calculando ativacao
        sseData.list = lapply(SSE.list, function(SSE, correlationMatrix) {
            # Armazenando identificador
            id = SSE$id
            SSE$id = NULL

            # Calculando ativacao (-y) em porcentagem (%)
            y = fitness(SSE, correlationMatrix) * 100.0

            # Aplicando marcadores
            SSE[, ':='(y = y, populationId = id)]

            # Retornando SSE com ativacao
            return(SSE)
        }, correlationMatrix)

        # Unindo resultado SSE
        sseData = do.call(rbind, sseData.list)
        calibration = input$calibration

        calibration.index = sapply(calibration, grep, names(sseData))
        names(sseData)[calibration.index] = sprintf("%s_E", calibration)

        # Convertendo a populacao para data.table
        roundPopulation = as.data.table(roundPopulation)
        names(roundPopulation) = cromossome

        # Adicionando identificador da geracao
        roundPopulation[, ':='(generationId = iteration, populationId = 1:maxPopulation)]

        # Unindo com a populacao
        iterationData = merge(sseData, roundPopulation)

        # Exportando resposta da geracao
        fwrite(iterationData, gaRoundFile, append = file.exists(gaRoundFile))

        # Filtrando os melhores da rodada (20%)
        population.list = filterPopulation(iterationData, cromossome, bestValue)

        # Reiniciando ciclo
        population = population.list$top20

        # Mensagem de fim da rodada
        cat(sprintf("[Generation %s/%s] Melhor valor: %s (y%%), Media: %s (y%%), D.Padrao: %s (y%%)\n", iteration, maxIteration, population.list$bestValue, population.list$meanValue, population.list$sdValue), sep = "")
    }

    # Gerando grafico de GA
    gaRound = fread(gaRoundFile)
    plotGaHistory(gaRound, inputList$coefficients, outputDir)

    # Salvando melhores individuos
    populationOutput = sprintf("%s//top20.csv", outputDir)
    fwrite(population, populationOutput)
}
#===============================================#