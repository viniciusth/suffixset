package suffixset

import (
	"errors"
	"strings"

	"golang.org/x/text/unicode/norm"
)

var (
	ErrInvalidUTF8    = errors.New("suffixset: invalid UTF-8 encoding in input words")
	ErrFailedToWrite  = errors.New("suffixset: failed to write suffix array")
	ErrVarintOverflow = errors.New("suffixset: varint overflow")
)

type SuffixSet[K int32 | int64] struct {
	suffixArray []K
	words       []string
}

func joinWords(words []string) []byte {
  builder := strings.Builder{}
  for i, word := range words {
    if i > 0 {
      // 0xC0 is not a valid UTF-8 byte, so we use it as a separator between words.
      builder.WriteByte(0xC0)
    }
    builder.WriteString(norm.NFC.String(word))
  }
  return []byte(builder.String())
}

func New32(words []string) (*SuffixSet[int32], error) {
	return &SuffixSet[int32]{
		suffixArray: BuildSuffixArray32(joinWords(words)),
		words:       words,
	}, nil
}

func New64(words []string) (*SuffixSet[int64], error) {
  return &SuffixSet[int64]{
    suffixArray: BuildSuffixArray64(joinWords(words)),
    words:       words,
  }, nil
}




