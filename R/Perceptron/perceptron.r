#Codigo igual ao da sala de aula feito pelo professor

# Funçao de ativação dos neuronios (fnet)
# net - valores dos atribs de X vezes os pesos
f <- function(net, threshold=0.5) {
	if (net >= threshold)
		return (1)
	return (0)
}

# Funcao para testar os pesos obtidos do treinamento e ver a resposta
perceptron.test <- function(x, weights) {
	net = as.numeric(c(x,1)) %*% weights # Multiplicacao elemento a elemento dos atribs de X e os Pesos
	return(f(net))
}

# Function that trains the perceptron
# X - Train elements
# Y - Outputs
perceptron.train <- function(X, Y, eta, acceptable.error=10^-3, max.iter=200) {

	# Um peso para cada atributo de X e um a mais para o Theta
	weights = runif(min=-0.5,max=0.5,n=ncol(X)+1)

	counter = 0
	total.error = 2*acceptable.error
	# Realizar o treinamento até ter um erro aceitavel
	while(total.error > acceptable.error && counter < max.iter){
		# Calcular o output para cada entrada de X
		for (i in 1:nrow(X)) {
			x_i = X[i,] #Pega uma fila de X
			y_i = Y[i]	#Pega o valor resposta desejada daquela fila

			net_i = as.numeric(c(x_i,1)) %*% weights # realiza a multiplicaçao elemento a elemento e soma tudo
			y.hat_i = f(net_i) # Aplica a funcao de ativação no net obtido
			#y.hat_i é a resposta obtida com os pesos atuais

			error_i = y_i - y.hat_i # Calcula o erro dessa entrada. Resultado desejado - Resultado obtido
			total.error = total.error + error_i^2 # Soma ao erro total de todas entradas

			# Treinamento
			dE2_dweights = -2*error_i*as.numeric(c(x_i,1)) # Calcula a derivada do erro ao quadrado na direção dos pesos
			weights = weights-eta*dE2_dweights # Recalcula cada peso utilizando um eta(passo) e a derivada
		}

		total.error = total.error/nrow(X) # Erro médio do conjunto
		cat("Total error = ", total.error, "\n")
		counter = counter+1 # Contador
	}
	#terminado o while, os pesos ja estao com o minimo de erro aceitavel
	return(weights)
}

# Funcao para testar o perceptron na porta lógica AND
and.toy <- function(eta=0.1) {
	dataset = read.table("in.dat")
	X = dataset[,1:2]
	Y = dataset[,3]

	model = perceptron.train(X=X,Y=Y,eta=eta)

	for (i in 1:nrow(X)) {
		y_i = perceptron.test(X[i,],model)
		cat("Y",i," = ",y_i,"\n")
	}
}