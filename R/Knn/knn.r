# Código para o algoritmo K Nearest Neighbours feito em sala de aula
# dataset - the data
# query - Elemento para ser avaliado
# k - numero de vizinhos a considerar
knn <- function(dataset, query, k=3) {

	classId = ncol(dataset) # Número para definir onde separar o dataset
	X = dataset[,1:(classId-1)] # Pega todas as colunas menos a ultima
	Y = dataset[,classId] # Pega apenas a última coluna

	# Aplica a distancia euclidiana do query com todos valores da entrada X
	Euclidean = apply(X, 1, function(x) {sqrt(sum((x-query)^2))})
	ids = sort.list(Euclidean, dec=F)[1:k] # Pega as posições das 3 menores distancias

	# Cria ret para ser retornado
	ret = list()
	ret$possible = unique(Y[ids]) # Pega os possiveis valores de Y(sem repetir)
	ret$votes = rep(0, length(ret$possible)) # Cria um vetor de 0 para representar cada possibilidade
	counter = 1

	# Conta quantas vezes cada valor possivel apareceu
	for (c in ret$possible) {
		ret$votes[counter] = sum(Y[ids] == c)
		counter = counter + 1
	}
	return(ret)
}