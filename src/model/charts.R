#===============================================#
plotWarmupCorrelation = function(treatment, warmup, correlationMatrix, outputDir) {
    # Filtrando tratamento
    warmup.subset = warmup[TN == treatment]
    correlation.subset = correlationMatrix[[treatment]]

    # Obtendo coeficientes
    coefficients = rownames(correlation.subset)

    # Separando dados de calibracao
    calibration.index = setdiff(names(warmup.subset), c("id", "TN", coefficients))

    # Reestruturando dados
    chartData.list = lapply(1:length(coefficients), function(i, calibration.index) {
        subset.index = (((i - 1) * 100) + 1): (i * 100)
        cali = c(coefficients[i], calibration.index)
        chartData = warmup.subset[subset.index, ..cali]

        return(chartData)
    }, calibration.index)

    # Gerando grafico
    chart.list = lapply(chartData.list, function(chartData, correlation.subset, outputDir) {
        # Armazenando o nome do coeficiente em questao
        coefficient = names(chartData)[1]

        # Unindo a correlacao na legenda
        coef.m.index = which(rownames(correlation.subset) == coefficient)
        correlationData = correlation.subset[coef.m.index,]
        correlationData = data.table(cor = correlationData, Calibration = colnames(correlation.subset))
        correlationData$label = sprintf("%s (%s)", correlationData$Calibration, correlationData$cor)

        # Renomeando colunas
        names(chartData)[1] = "coef"
        for(index in 1:(dim(correlationData)[1])) {
            greep.index = grep(correlationData$Calibration[index], names(chartData))
            names(chartData)[greep.index] = correlationData$label[index]
        }

        # Organizando dados
        chartData.long = melt(chartData, "coef", variable.name = "Calibration")
        chartData.long = chartData.long[complete.cases(chartData.long),]
        chartData.long$Calibration = as.character(chartData.long$Calibration)
        chartData.long$value = (chartData.long$value * 100) |> round(digits = 2)

        # Gerando grafico
        p = ggplot(chartData.long, aes(x = coef, y = value, group = Calibration, color = Calibration)) +
            geom_smooth(formula = y ~ x, method = "loess") +
            geom_hline(yintercept = 0) +
            xlab(sprintf("%s multiplier", coefficient)) + ylab("Observed variation (%)")
        
        # Retornando grafico
        return(p)
    }, correlation.subset, outputDir)

    # Salvando graficos como .pdf
    pdf(sprintf("%s//warmup_treatment_%s.pdf", outputDir, treatment))
    for(p in chart.list) {
        plot(p)
    }
    dev.off()
}
#===============================================#

#===============================================#
plotGaHistory = function(gaData, coefficient, outputDir) {
    # Filtrando dados de geracao
    coefficient.index = c("y", "generationId", coefficient)
    gaDataResult = unique(gaData[, ..coefficient.index])
    gaDataResult[, ':='(bestY = max(y)), by = "generationId"]

    # Filtrando problema de ilha "IP"
    ip.index = c("IP", "generationId")
    ipData = unique(gaData[, ..ip.index])[IP == TRUE]
    ipData = ipData$generationId

    # Gerando grafico
    p = ggplot(gaDataResult, aes(x = generationId, y = y)) +
        geom_line(aes(y = bestY), color = "green", linewidth = 3) +
        geom_smooth(formula = y ~ x, method = "loess", color = "red") +
        geom_vline(xintercept = ipData, color = "orange", linetype = "dotted") +
        xlab("Generation") + ylab("Fitness")

    # Salvando grafico de geracoes
    png(sprintf("%s//ga_generation.png", outputDir))
    plot(p)
    dev.off()
}
#===============================================#

#===============================================#
plotEffectiveness = function(gaData, coefficient, outputDir) {
    # Padronizando nomes
    coefficient.index = sapply(coefficient, grep, names(gaData))
    names(gaData)[coefficient.index] = names(coefficient.index)

    treatment = unique(gaData$TN)[1]

    # Extraindo subset das simulacoes geradas
    gaData.subset = gaData[TN == treatment]

    # Obtendo nomes para subset
    extraName = c("y", "IP", "generationId")
    dataName = names(gaData.subset)

    # Agrupando indices
    y.index = c(coefficient, extraName)
    gaData.subset = gaData.subset[, ..y.index]

    # Verificando a existencia de colunas com todos os valores NA
    gaDataValidation = apply(gaData.subset, 2, function(gaCoefficient) {
        validation = sum(is.na(gaCoefficient)) == length(gaCoefficient)
        return(!validation)
    }) 

    # Removendo colunas vazias
    gaData.subset = unique(gaData.subset[, ..gaDataValidation])

    # Filtrando problema de ilha "IP"
    ipData = unique(gaData.subset[, ..extraName])[IP == TRUE]
    ipData = ipData[, .(yMean = mean(y)), by = "generationId"]$yMean
    ipData = unique(ipData)

    # Removendo coluna IP
    gaData.subset$IP = NULL
    gaData.subset$generationId = NULL

    # Organizando dados
    chartData.long = melt(gaData.subset, "y", variable.name = "Coefficient")
    chartData.long$Coefficient = as.character(chartData.long$Coefficient)
    
    # Gerando grafico
    p = ggplot(chartData.long, aes(x = y, y = value, group = Coefficient, color = Coefficient)) +
        geom_smooth(formula = y ~ x, method = "loess", se = FALSE) +
        geom_vline(xintercept = ipData, color = "orange", linetype = "dotted") +
        xlab("RMSE (y)") + ylab("Coefficient multiplier")

    # Salvando graficos como .png
    png(sprintf("%s//coefficient_effectiveness.png", outputDir))
    plot(p)
    dev.off()
}
#===============================================#
