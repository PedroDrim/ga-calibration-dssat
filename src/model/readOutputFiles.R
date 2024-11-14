#===============================================#
# Funcao responsavel por ler o identificador de tratamento no arquivo X
readTreatmentsId = function(x_file) {
    # Criando conexao com o arquivo X
    con = file(x_file, "r")
    xLines = readLines(con)

    # Encerrando conexao com o arquivo X
    close(con)

    # Obtendo linha dos grupos
    label.index = grep("\\*[A-Z]", xLines)
    treatLabel = grep("TREATMENTS", xLines[label.index])
    
    # Intervalo do grupo de tratamento
    rangeValue = label.index[treatLabel + (0:1)] + c(2, -2)

    # Obtendo grupo de tratamento
    xLines = xLines[rangeValue[1]:rangeValue[2]]

    # Extraindo identificador de tratamentos
    treatmentId = strsplit(xLines, " ")
    treatmentId = sapply(treatmentId, function(x) x[2])
    treatmentId = as.numeric(treatmentId)
    
    # Retornando resultado
    return(treatmentId)
}
#===============================================#

#===============================================#
# Funcao responsavel por ler a regiao do arquivo X
readRegion = function(x_file) {
    # Criando conexao com o arquivo X
    con = file(x_file, "r")
    xLines = readLines(con)

    # Encerrando conexao com o arquivo X
    close(con)

    # Obtendo linha dos grupos
    label.index = grep("@SITE", xLines) + 1
    xLines = xLines[label.index]

    # Retornando regiao  
    return(xLines)
}
#===============================================#

#===============================================#
readTfile = function(t_file) {
    # Criando conexao com o arquivo .T
    con = file(t_file, "r")
    bntLines = readLines(con)

    # Encerrando conexao com o arquivo .T
    close(con)

    # Removendo caracteres especiais de tabulacao
    bntLines = gsub("\t", "", bntLines)
    
    # Gerando data.table
    bntData = fread(text = bntLines[-1])
    names(bntData) = gsub("@", "", names(bntData))
    bntData$DOY = yday(bntData$DATE)

    # Retornando dados
    return(bntData)
}
#===============================================#

#===============================================#
# Funcao responsavel por ler os resultados do arquivo Evaluate.OUT
readEvaluate = function(simulationDirectory, region) {
    # Localizando arquivo
    evaluationFile = paste0(simulationDirectory, "//", 'Evaluate.OUT')

    # Extraindo identificador da rodada
    idRun = strsplit(simulationDirectory, "/")[[1]]
    idRun = idRun[length(idRun)]

    # Lendo as informacoes
    evaluate = fread(evaluationFile)
    
    # Adicionando as colunas de regiao e identificador
    evaluate$region = region
    evaluate$idRun = idRun

    # Retornando resultado
    return(evaluate)
}
#===============================================#

#===============================================#
# Funcao responsavel por ler os resultados do arquivo PlantGro.OUT
readPlantgrout = function(simulationDirectory, treatmentId) {
    # Localizando arquivo
    plantgroFIle = paste0(simulationDirectory, "//", 'PlantGro.OUT')

    # Identificador da rodada
    treatment = as.numeric(treatmentId)

    # Criando conexao com o arquivo PlantGro.OUT
    con = file(plantgroFIle, "r")
    plantgroLines = readLines(con)

    # Encerrando conexao com o arquivo PlantGro.OUT
    close(con)

    # Limpando o arquivo
    ast.index = grep("\\*", plantgroLines)
    exc.index = grep("!", plantgroLines)

    # Filtrando linhas
    plantgroLines = plantgroLines[-c(ast.index, exc.index)]
    plantgroLines = plantgroLines[plantgroLines != ""]

    # Obtendo index dos cabecalhos
    header.index = grep('@YEAR', plantgroLines)

    # Adicionando comprimento final +1
    header.index = c(header.index, length(plantgroLines) + 1)

    # Gerando data.table
    plantgro = lapply(1:(length(header.index) - 1), function(index, plantgroLines, treatment, header.index) {
        # Obtendo o intervalo da fatia
        rangeValue = header.index[(0:1) + index] - c(0, 1)
        rangeLines = plantgroLines[rangeValue[1]:rangeValue[2]]

        # Lendo os dados parciais
        subsetData = fread(text = rangeLines)
        subsetData$TRNO = treatment[index]

        return(subsetData)
    }, plantgroLines, treatment, header.index)

    # Unificando lista
    plantgro = as.data.table(do.call(rbind, plantgro))
    names(plantgro) = gsub("@", "", names(plantgro))

    # Retornando resultado
    return(plantgro)
}
#===============================================#
