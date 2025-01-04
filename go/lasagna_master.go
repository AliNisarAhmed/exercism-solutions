package lasagna

func PreparationTime(layers []string, prepTime int) int {
	if prepTime == 0 {
	   prepTime = 2	
	}
	return len(layers) * prepTime
}

func Quantities(layers []string) (noodles int, sauce float64) {
	for _, layer := range layers {
		if layer == "noodles" {
			noodles += 50
		}
		if layer == "sauce" {
			sauce += 0.2
		}
	}
	return
}

func AddSecretIngredient(friendList []string, myList []string) {
	myList[len(myList)-1] = friendList[len(friendList)-1]
}

func ScaleRecipe(amounts []float64, numPortions int) []float64 {
	result := make([]float64, len(amounts))
	for i, amount := range amounts {
		result[i] = amount * (float64(numPortions) / 2)
	}
	return result
}
