package suffixset

import (
	"bytes"
	"errors"
	"sort"
	"strings"
	"unicode/utf8"

	"github.com/viniciusth/rmq"
	"golang.org/x/text/unicode/norm"
)

var (
	ErrInvalidUTF8 = errors.New("suffixset: invalid UTF-8 encoding in input words")
)

const (
	// 0xFF is not a valid UTF-8 byte, so we use it as a separator between words.
	// It's useful that it's also the maximum value of a byte.
	wordSeparator = 0xFF
)

type SuffixSetBuilder struct {
	words         []string
	useLCP        bool
	useDocListing bool
	caseSensitive bool
	normalize     bool
}

func NewBuilder(words []string) *SuffixSetBuilder {
	return &SuffixSetBuilder{
		words:         words,
		useLCP:        true,
		useDocListing: true,
		caseSensitive: false,
		normalize:     true,
	}
}

// Skips the LCP array construction, this makes binary search O(|P| * log(|S|)) instead of O(|P| + log(|S|)).
// Saves O(|S|) memory: doesn't use 2*|S| extra memory.
// Trade-off: binary search is slower, but you spend less memory.
func (b *SuffixSetBuilder) SkipLCP() *SuffixSetBuilder {
	b.useLCP = false
	return b
}

// Skips the document listing structures construction.
// This makes the step of finding which words contain a pattern into a naive algorithm,
// which can take up to O(|S|) time in the worst case.
// Saves O(|S|) memory: doesn't use 2*|S| extra memory
// Most useful if the number of matched words is small or the number of wanted matched words is small.
// Trade-off: FindKMatches is slower, but you spend less memory.
func (b *SuffixSetBuilder) SkipDocListing() *SuffixSetBuilder {
	b.useDocListing = false
	return b
}

// Makes the search case sensitive.
func (b *SuffixSetBuilder) CaseSensitive() *SuffixSetBuilder {
	b.caseSensitive = true
	return b
}

// Skips the normalization of the words with NFC.
func (b *SuffixSetBuilder) SkipNormalization() *SuffixSetBuilder {
	b.normalize = false
	return b
}

func (b *SuffixSetBuilder) Build() (*SuffixSet, error) {
	for _, word := range b.words {
		if !utf8.Valid([]byte(word)) {
			return nil, ErrInvalidUTF8
		}
	}

	concatenatedWords := prepareWords(b.words, b.caseSensitive, b.normalize)
	suffixArray, err := BuildSuffixArray(concatenatedWords)
	if err != nil {
		return nil, err
	}

	var lcp []int
	var lcpRMQ *rmq.RMQHybridNaive[int]
	if b.useLCP {
		lcp = BuildLCPArray(suffixArray, concatenatedWords)
		lcpRMQ = rmq.NewRMQHybridNaive(lcp)
	}

	wordIndex := buildWordIndexArray(suffixArray, concatenatedWords)
	var prev []int
	var prevRMQ *rmq.RMQHybridNaive[int]
	if b.useDocListing {
		prev = buildPrevArray(suffixArray, wordIndex, b.words)
		prevRMQ = rmq.NewRMQHybridNaive(prev)
	}
	return &SuffixSet{
		suffixArray:       suffixArray,
		words:             b.words,
		wordIndex:         wordIndex,
		lcp:               lcp,
		lcpRMQ:            lcpRMQ,
		prev:              prev,
		prevRMQ:           prevRMQ,
		concatenatedWords: concatenatedWords,
		normalize:         b.normalize,
		caseSensitive:     b.caseSensitive,
	}, nil
}

type SuffixSet struct {
	suffixArray       []int
	words             []string
	wordIndex         []int
	concatenatedWords []byte
	lcp               []int
	lcpRMQ            *rmq.RMQHybridNaive[int]
	prev              []int
	prevRMQ           *rmq.RMQHybridNaive[int]
	normalize         bool
	caseSensitive     bool
}

// Builds a byte array that is the concatenation of the words in the order of the suffix array.
func prepareWords(words []string, caseSensitive bool, normalize bool) []byte {
	builder := strings.Builder{}
	for i, word := range words {
		if i > 0 {
			builder.WriteByte(wordSeparator)
		}
		w := applyTransforms(word, caseSensitive, normalize)
		builder.WriteString(w)
	}
	return []byte(builder.String())
}

func applyTransforms(word string, caseSensitive bool, normalize bool) string {
	if !caseSensitive {
		word = strings.ToLower(word)
	}
	if normalize {
		word = norm.NFC.String(word)
	}
	return word
}

func buildWordIndexArray(suffixArray []int, concatenatedWords []byte) []int {
	wordIndex := make([]int, len(suffixArray))
	word := 0
	for i := range concatenatedWords {
		if concatenatedWords[i] == wordSeparator {
			word++
		}
		wordIndex[i] = word
	}
	return wordIndex
}

// Builds the prev array and the wordIndex array for the doc listing problem.
// For each index i in the suffix array, prev[i] is the index of the previous index of the same word in the suffix array.
// If there is no previous index of the same word, prev[i] is -1.
func buildPrevArray(suffixArray, wordIndex []int, words []string) []int {
	prev := make([]int, len(suffixArray))
	wordPrev := make([]int, len(words))
	for i := range wordPrev {
		wordPrev[i] = -1
	}

	for i := range suffixArray {
		prev[i] = wordPrev[wordIndex[suffixArray[i]]]
		wordPrev[wordIndex[suffixArray[i]]] = i
	}

	return prev
}

func (s *SuffixSet) FindKMatches(pattern string, k int) []int {
	pattern = applyTransforms(pattern, s.caseSensitive, s.normalize)

	// Every element in [l, r] is a match for the pattern.
	l, r := findBoundaries([]byte(pattern), s.concatenatedWords, s.suffixArray, s.lcp, s.lcpRMQ)
	if l == -1 {
		return nil
	}

	// Find k distinct word matches for the pattern.
	matches := make([]int, 0, k)
	if s.prev != nil {
		return recursiveFindKMatches(l, l, r, k, s.wordIndex, matches, s.prev, s.prevRMQ)
	}

	usedWord := make(map[int]bool)
	for i := l; i <= r && len(matches) < k; i++ {
		if usedWord[s.wordIndex[i]] {
			continue
		}
		usedWord[s.wordIndex[i]] = true
		matches = append(matches, s.wordIndex[i])
	}
	return matches
}

func (s *SuffixSet) FindKMatchesString(pattern string, k int) []string {
	matchesIdx := s.FindKMatches(pattern, k)
	matches := make([]string, len(matchesIdx))
	for i := range matches {
		matches[i] = s.words[matchesIdx[i]]
	}
	return matches
}

func findBoundaries(pattern []byte, str []byte, suffixArray, lcp []int, lcpRMQ *rmq.RMQHybridNaive[int]) (int, int) {
	bestIdx, best, n := -1, -1, len(suffixArray)

	expandBest := func(i int) bool {
		for best < len(pattern) && i+best < n && pattern[best] == str[i+best] {
			best++
		}
		if best == len(pattern) {
			// p < str[i:]
			return true
		} else if i+best == n {
			// p > str[i:]
			return false
		} else {
			return pattern[best] < str[i+best]
		}
	}

	// find first index where pattern is a prefix
	l := sort.Search(n, func(i int) bool {
		if lcp != nil {
			if bestIdx == -1 {
				bestIdx = i
				best = 0
				return expandBest(i)
			}
			lcpLen := lcp[lcpRMQ.Query(min(bestIdx, i), max(bestIdx, i)-1)]
			if lcpLen < best {
				// not part of the matching group, you can just skip it
				// if i < bestIdx, then you want to shorten to [i+1, r].
				return i > bestIdx
			} else {
				return expandBest(i)
			}
		}

		// naive compare as we dont have lcp, find first l where p <= s[l:]
		return bytes.Compare(pattern, str[i:]) <= 0
	})

	// Check if L has pattern as a prefix, otherwise we have no matches
	if l == n || (lcp != nil && best < len(pattern)) || (lcp == nil && !bytes.HasPrefix(str[l:], pattern)) {
		return -1, -1
	}

	// last index where pattern is a prefix
	// we have T T T F F F, where pattern is a prefix now.
	// to use sort.Search we need F F F T T T, so just return the negation and find the first T => find first F, return that -1
	r := sort.Search(n-l, func(i int) bool {
		if lcp != nil {
			if i == 0 {
				return false // always true since we already know best == |pattern|
			}
			lcp := lcp[lcpRMQ.Query(l, l+i-1)]
			return !(lcp >= len(pattern))
		}
		// Naive check if we don't have LCP, still apply the negation to find the first T (first F)
		return !bytes.HasPrefix(str[l+i:], pattern)
	})

	return l, l + r - 1
}

func recursiveFindKMatches(baseL, l, r, k int, wordIndex, matches, prev []int, rmq *rmq.RMQHybridNaive[int]) []int {
	if k <= len(matches) || l > r {
		return matches
	}

	// prev[p] < l, since if prev[p] >= l, prev[p] ∈ [l, r] and we would have prev[prev[p]] < prev[p], a contradiction.
	p := rmq.Query(l, r)

	// nothing in [l, r] is outside of the original l anymore, no more new elements.
	if prev[p] >= baseL {
		return matches
	}
	matches = append(matches, wordIndex[p])
	matches = recursiveFindKMatches(baseL, l, p-1, k, wordIndex, matches, prev, rmq)
	return recursiveFindKMatches(baseL, p+1, r, k, wordIndex, matches, prev, rmq)
}
