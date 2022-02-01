arqProdutos = "produtos.txt"
arqVendas = "vendas.txt"

arquivoProdutos = read.csv(arqProdutos, header = FALSE, sep = ' ')
arquivoVendas = read.csv(arqVendas, header = FALSE, sep = ' ')

arquivoProdutos = array(arquivoProdutos)
arquivoVendas = array(arquivoVendas)

#Qtd operações
operacoes = function(arquivoVendas){
  dfVendas = data.frame(op = arquivoVendas[1])
  
  return(dfVendas)
}

# codigos unicos
codigosUnicos = function(arquivoVendas){
  
  dfVendas = data.frame(codigos = arquivoVendas[1])
  
  dfCodigos = unique(dfVendas$codigos)
  
  dfCodigos = sort(dfCodigos)
  
  return(dfCodigos)
  
}
#datas unicas
datasUnicas = function(arquivoVendas){
  
  dfVendas = data.frame(datas = arquivoVendas[2])
  
  dfDatas = unique(dfVendas$datas)
  
  return(dfDatas)
  
}
cdUnicos = codigosUnicos(arquivoVendas)

dtUnicas = datasUnicas(arquivoVendas)

qtdOperacoes = operacoes(arquivoVendas)

for(d in 1:length(dtUnicas)){
  dadosT = c()
  produtoT = c()
  valorpT = c()
  valorTT = c()
  qtdT = c()
  codigosT = c()
  
  dfD = subset(arquivoVendas, arquivoVendas[,2] == dtUnicas[d])
  for (c in 1:length(cdUnicos)){
    
    codigosT[length(codigosT) + 1] = toString(cdUnicos[c])
    
    dfCD = subset(dfD, dfD[,1] == cdUnicos[c])
    qtd = length(dfCD[,1])
    qtdT[length(qtdT) + 1] = qtd
    
    
    infoP = subset(arquivoProdutos, arquivoProdutos[,1] == cdUnicos[c])
    
    produto = array(infoP[2])
    produtoT[length(produtoT) + 1] = toString(produto)
    
    
    valorp = infoP[3]
    valorpT[length(valorpT) + 1] = toString(valorp)
    
    valorT =  qtd * valorp[1,1]
    valorTT[length(valorTT) + 1] = toString(valorT)
    
  }
  dados = c(codigosT, produtoT, valorpT, qtdT, valorTT)
  dfT = data.frame(Codigo = c(codigosT), Produto = c(produtoT), Valor=c(valorpT), Qtd=c(qtdT), ValorVendido=c(valorTT))
  print(dados)
  nomeArquivo = paste(dtUnicas[d],'.txt')
  write.table(dfT, file=nomeArquivo, sep=';', quote = FALSE)
}
