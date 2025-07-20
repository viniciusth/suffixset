package suffixset

import (
	"bytes"
	"slices"
	"sort"
	"strings"
	"testing"
	"unicode/utf8"
)

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func naiveMatches(words []string, pattern string, caseSensitive, normalize bool) []int {
	var res []int
	p := applyTransforms(pattern, caseSensitive, normalize)
	for i, w := range words {
		ww := applyTransforms(w, caseSensitive, normalize)
		if strings.Contains(ww, p) {
			res = append(res, i)
		}
	}
	return res
}

func checkMatches(t *testing.T, got []int, expected []int, k int) {
	expLen := len(expected)
	wantLen := min(k, expLen)
	if len(got) != wantLen {
		t.Errorf("wrong number of matches: got %d, want %d", len(got), wantLen)
	}
	seen := make(map[int]bool)
	expSet := make(map[int]bool)
	for _, e := range expected {
		expSet[e] = true
	}
	for _, g := range got {
		if seen[g] {
			t.Errorf("duplicate in got: %d", g)
		}
		seen[g] = true
		if !expSet[g] {
			t.Errorf("invalid match in got: %d", g)
		}
	}
}

func TestFindKMatchesBasic(t *testing.T) {
	words := []string{"apple", "banana", "app", "pineapple", "bandana"}
	builder := NewBuilder(words)
	ss, err := builder.Build()
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		pattern string
		k       int
	}{
		{"app", 3},
		{"an", 2},
		{"pine", 1},
		{"xyz", 5},
		{"", 10},
		{"App", 3}, // case insensitive
	}

	for _, tc := range tests {
		t.Run(tc.pattern, func(t *testing.T) {
			got := ss.FindKMatches(tc.pattern, tc.k)
			expected := naiveMatches(words, tc.pattern, ss.caseSensitive, ss.normalize)
			checkMatches(t, got, expected, tc.k)
			if len(got) == 0 {
				return
			}

			gotStr := ss.FindKMatchesString(tc.pattern, tc.k)
			if len(gotStr) != len(got) {
				t.Errorf("string version length mismatch: %d vs %d", len(gotStr), len(got))
			}

			var expectedStr []string
			for _, idx := range got {
				expectedStr = append(expectedStr, words[idx])
			}
			sort.Strings(expectedStr)

			gotStrCopy := append([]string{}, gotStr...)
			sort.Strings(gotStrCopy)

			if !slices.Equal(gotStrCopy, expectedStr) {
				t.Errorf("string matches don't correspond to index matches: got %v, want %v", gotStr, expectedStr)
			}
		})
	}
}

func TestFindKMatchesOptions(t *testing.T) {
	words := []string{"Café", "cafe", "CAFE", "élite", "elite"}
	builder := NewBuilder(words).CaseSensitive().SkipNormalization().SkipLCP().SkipDocListing()
	ss, err := builder.Build()
	if err != nil {
		t.Fatal(err)
	}

	tests := []struct {
		pattern string
		k       int
	}{
		{"cafe", 1}, // only "cafe" since case sensitive
		{"Café", 1}, // "Café", with accent
		{"elite", 1},
	}

	for _, tc := range tests {
		t.Run(tc.pattern, func(t *testing.T) {
			got := ss.FindKMatches(tc.pattern, tc.k)
			expected := naiveMatches(words, tc.pattern, ss.caseSensitive, ss.normalize)
			checkMatches(t, got, expected, tc.k)
		})
	}

	// Test with default options (case insensitive, normalize)
	builderDefault := NewBuilder(words)
	ssDefault, err := builderDefault.Build()
	if err != nil {
		t.Fatal(err)
	}
	gotDefault := ssDefault.FindKMatches("cafe", 3)
	expectedDefault := naiveMatches(words, "cafe", false, true)
	checkMatches(t, gotDefault, expectedDefault, 3) // should match cafe and CAFE after lower (but not Café due to accent)
}

func FuzzFindKMatches(f *testing.F) {
	f.Add([]byte("apple\xffbanana\xffapp\xffpineapple\xffbandana"), []byte("app"), uint(3))
	f.Add([]byte("hello\xffworld\xffhell\xffloworld\xff😂🙈🙉🙊"), []byte("😂"), uint(2))

	f.Fuzz(func(t *testing.T, data []byte, pat []byte, kk uint) {
		if !utf8.Valid(pat) {
			return
		}
		wordsBytes := bytes.Split(data, []byte{wordSeparator})
		words := make([]string, 0, len(wordsBytes))
		totalLen := 0
		for _, wb := range wordsBytes {
			if len(wb) == 0 || !utf8.Valid(wb) {
				continue
			}
			words = append(words, string(wb))
			totalLen += len(wb)
		}
		if len(words) == 0 || len(words) > 50 || totalLen > 1000 || len(pat) > 100 {
			return
		}
		k := int(kk)

		builder := NewBuilder(words)
		ss, err := builder.Build()
		if err != nil {
			return
		}

		got := ss.FindKMatches(string(pat), k)
		expected := naiveMatches(words, string(pat), ss.caseSensitive, ss.normalize)
		checkMatches(t, got, expected, k)

		gotStr := ss.FindKMatchesString(string(pat), k)
		if len(gotStr) != len(got) {
			t.Errorf("string length mismatch")
		}

		var expectedStr []string
		for _, idx := range got {
			expectedStr = append(expectedStr, words[idx])
		}
		sort.Strings(expectedStr)

		gotStrCopy := append([]string{}, gotStr...)
		sort.Strings(gotStrCopy)

		if !slices.Equal(gotStrCopy, expectedStr) {
			t.Errorf("string matches don't correspond")
		}
	})
}
