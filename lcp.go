package suffixset

// Kasai's algorithm for building the LCP array in O(n) time.
func BuildLCPArray(suffixArray []int, text []byte) []int {
	rank := make([]int, len(suffixArray))
	for i := range suffixArray {
		rank[suffixArray[i]] = i
	}

	lcp := make([]int, len(suffixArray) - 1)
	l := 0
	for i := range suffixArray {
		if rank[i]+1 == len(suffixArray) {
			l = 0
			continue
		}
		j := int(suffixArray[rank[i]+1])
		for i+l < len(text) && j+l < len(text) && text[i+l] == text[j+l] {
			l++
		}
		lcp[rank[i]] = l
		if l > 0 {
			l--
		}
	}

	return lcp
}
