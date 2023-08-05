package algos.neural

// will always return a value between 0 and 1
def sigmoid(x: Double): Double = 1 / (1 + Math.exp(-x))

// sigmoid'
def derivativeSigmoid(x: Double): Double = 
    val sig = sigmoid(x)
    sig * (1 - sig)