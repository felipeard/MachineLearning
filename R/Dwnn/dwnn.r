# Código para o algoritmo Distance-Weighted Nearest Neighbour feito em sala de aula

# Funcao para calcular os pesos de cada valor do conjunto de dados a partir de uma Gaussiana
# X - Conjunto de entrada
# x.query - valor novo
# sigma - abertura padrao
weights <- function(X, x.query, sigma) {
	# Vetor com as distancias euclidianas do ponto novo com todos do conjunto
	Euclidean = apply(X, 1, function(row) { sqrt(sum((row-x.query)^2)) })
	# retorna o vetor com os pesos de cada valor depois de aplicado a gaussiana
	return (exp(-Euclidean^2/(2*sigma^2)))
}

# Funcao que aplica o dwnn no conjunto de entrada X
# X - Conjunto de entrada
# Y - Respostas do conjunto de entrada
# x.query - valor novo
# sigma - abertura padrao
dwnn <- function(X, Y, x.query, sigma) {

	w = weights(X, x.query, sigma) # vetor de pesos
	y.query = sum(w * Y) / sum(w) # calcula o valor do ponto novo

	return (y.query) # retorna o resultado do valor novo
}
