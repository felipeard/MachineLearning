# Código para o algoritmo K-means feito em sala de aula

# Funcao que realiza o algoritmo kmeans

kmeans <- function(dataset, centers=2, threshold=1e-5) {
	ids = sample(1:nrow(dataset), size=centers) # Pega "size" numeros aleatorios de um vetor de 1 a nrow(dataset)
	centroids = dataset[ids,] # pega os pontos que vao ser centroids
	error = 2*threshold # inicializa o erro maior
	iter = 1 # numero de iteracoes

	# executa enquanto tiver um erro maior
	while (error > threshold) {
		cat("Iteration: ", iter, "\n") # Imprime o numero de iteracoes
		iter = iter + 1
		distances = NULL # inicializa o vetor de distancias
		
		for (i in 1:centers) {
			# Ira calcular a distancia de todos os pontos para cada centroide
			distances = cbind(distances,apply(dataset,1,function(row) {sqrt(sum((row-centroids[i,])^2)) } ))
		}
		# Vetor de qual centroid cada elemento do conjunto esta mais perto
		ids = apply(distances,1,function(row) {which.min(row)})

		error = 0
		# Recalcula a posiçao de cada centroid para o centro de massa dos pontos que ele representa
		for (i in 1:centers) {
			rowIds = which(ids == i) # Pega os pontos que estao no grupo do centroid i
			error = error + sqrt(sum((centroids[i,] - colMeans(dataset[rowIds,]))^2)) # Calcula o erro
			centroids[i,] = colMeans(dataset[rowIds,]) # Move o centroid para o centro de massa
		}
	}

	# prepara o ret para retornar a posicao dos centroids e a qual centroid cada elemento pertence
	ret = list()
	ret$centroids = centroids
	ret$ids = ids

	return (ret)
}

create.data <- function() {
	data = cbind(rnorm(mean=-5, sd=1, n=500), rnorm(mean=-5, sd=1, n=500))
	data = rbind(data, cbind(rnorm(mean=5, sd=1, n=500), rnorm(mean=5, sd=1, n=500)))
	plot(data)

	return (data)
}