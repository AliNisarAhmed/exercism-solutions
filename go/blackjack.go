package blackjack

// ParseCard returns the integer value of a card following blackjack ruleset.
func ParseCard(card string) int {
	switch card {
	case "ace":
		return 11
	case "two":
		return 2
	case "three":
		return 3
	case "four":
		return 4
	case "five":
		return 5
	case "six":
		return 6
	case "seven":
		return 7
	case "eight":
		return 8
	case "nine":
		return 9
	case "ten", "jack", "queen", "king":
		return 10
	default:
		return 0
	}
}

// FirstTurn returns the decision for the first turn, given two cards of the
// player and one card of the dealer.
func FirstTurn(card1, card2, dealerCard string) string {
	dv := ParseCard(dealerCard)
	v1 := ParseCard(card1)
	v2 := ParseCard(card2)
	total := v1 + v2
	switch {
	case total == 22:
		return "P"
	case total == 21 && dv < 10:
		return "W"
	case total == 21 && dv >= 10:
		return "S"
	case total >= 17 && total <= 21:
		return "S"
	case total >= 12 && total <= 16 && dv >= 7:
		return "H"
	case total >= 12 && total <= 16 && dv < 7:
		return "S"
	default:
		return "H"
	}
}
