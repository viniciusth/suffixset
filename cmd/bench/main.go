package main

import (
	"flag"
	"fmt"
	"math/rand"
	"os"
	"runtime"
	"runtime/pprof"
	"time"

	"github.com/viniciusth/suffixset"
)

type RMQSet interface {
	FindKMatches(pattern string, k int) []int
}

type variant struct {
	name   string
	config func(*suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder
}

var variants = map[string]variant{
	"full":            {name: "full", config: func(b *suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder { return b }},
	"no_lcp":          {name: "no_lcp", config: func(b *suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder { return b.SkipLCP() }},
	"no_doc":          {name: "no_doc", config: func(b *suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder { return b.SkipDocListing() }},
	"no_lcp_no_doc":   {name: "no_lcp_no_doc", config: func(b *suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder { return b.SkipLCP().SkipDocListing() }},
	"full_hybrid_log": {name: "full_hybrid_log", config: func(b *suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder { return b.UseHybridLogRMQ() }},
}

type densityType string

const (
	densityLow  densityType = "low"
	densityHigh densityType = "high"
)

type memMonitor struct {
	maxAlloc uint64
	stop     chan struct{}
}

func newMemMonitor() *memMonitor {
	mm := &memMonitor{stop: make(chan struct{})}
	go func() {
		for {
			var m runtime.MemStats
			runtime.ReadMemStats(&m)
			if m.Alloc > mm.maxAlloc {
				mm.maxAlloc = m.Alloc
			}
			select {
			case <-mm.stop:
				return
			default:
				time.Sleep(10 * time.Millisecond)
			}
		}
	}()
	return mm
}

func (mm *memMonitor) Stop() uint64 {
	close(mm.stop)
	return mm.maxAlloc
}

func getCurrentAlloc() uint64 {
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	return m.Alloc
}

func measureBuild(words []string, config func(*suffixset.SuffixSetBuilder) *suffixset.SuffixSetBuilder) (time.Duration, uint64, uint64, *suffixset.SuffixSet) {
	runtime.GC()
	mm := newMemMonitor()
	start := time.Now()
	builder := suffixset.NewBuilder(words)
	builder = config(builder)
	ss, err := builder.Build()
	if err != nil {
		panic(err)
	}
	dur := time.Since(start)
	peak := mm.Stop()
	runtime.GC()
	alloc := getCurrentAlloc()
	return dur, peak, alloc, ss
}

func measureQuery(ss *suffixset.SuffixSet, patterns []string, k int) (time.Duration, uint64, uint64) {
	runtime.GC()
	mm := newMemMonitor()
	start := time.Now()
	for _, p := range patterns {
		_ = ss.FindKMatches(p, k)
	}
	dur := time.Since(start)
	peak := mm.Stop()
	runtime.GC()
	alloc := getCurrentAlloc()
	return dur, peak, alloc
}

func runBenchmark(v variant, M, W, P, K, Q, runs int, density densityType) {
	for run := 0; run < runs; run++ {
		r := rand.New(rand.NewSource(int64(run)))
		var commonStr string
		words := make([]string, M)
		if density == densityHigh {
			common := make([]byte, P)
			for j := range common {
				common[j] = byte(r.Intn(26) + 'a')
			}
			commonStr = string(common)
			for i := range words {
				word := make([]byte, W)
				for j := range word {
					word[j] = byte(r.Intn(26) + 'a')
				}
				insertPos := r.Intn(W - P + 1)
				copy(word[insertPos:], common)
				words[i] = string(word)
			}
		} else {
			for i := range words {
				word := make([]byte, W)
				for j := range word {
					word[j] = byte(r.Intn(26) + 'a')
				}
				words[i] = string(word)
			}
		}
		bt, bp, ba, ss := measureBuild(words, v.config)
		patterns := make([]string, Q)
		for i := range patterns {
			if density == densityHigh {
				patterns[i] = commonStr // All queries use the common pattern
			} else {
				wordIdx := r.Intn(M)
				start := r.Intn(W - P + 1)
				pat := words[wordIdx][start : start+P]
				patterns[i] = pat
			}
		}
		qt, qp, qa := measureQuery(ss, patterns, K)
		fmt.Printf("%s,%d,%d,%d,%d,%d,%s,%.0f,%d,%d,%.0f,%d,%d\n",
			v.name, M, W, P, K, Q, density,
			float64(bt.Nanoseconds()), bp, ba,
			float64(qt.Nanoseconds()), qp, qa)
	}
}

func main() {
	variantName := flag.String("variant", "", "Variant to benchmark")
	m := flag.Int("m", 0, "Number of words M")
	w := flag.Int("w", 0, "Word length W")
	p := flag.Int("p", 0, "Pattern length P")
	k := flag.Int("k", 0, "Number of matches K")
	q := flag.Int("q", 0, "Number of queries Q")
	runs := flag.Int("runs", 3, "Number of runs for averaging")
	d := flag.String("d", "low", "Density: low or high")
	cpuprofile := flag.String("cpuprofile", "", "Write CPU profile to file")
	flag.Parse()

	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			fmt.Fprintf(os.Stderr, "could not create CPU profile: %v\n", err)
			os.Exit(1)
		}
		defer f.Close()
		if err := pprof.StartCPUProfile(f); err != nil {
			fmt.Fprintf(os.Stderr, "could not start CPU profile: %v\n", err)
			os.Exit(1)
		}
		defer pprof.StopCPUProfile()
	}

	if *variantName == "" || *m <= 0 || *w <= 0 || *p <= 0 || *k <= 0 || *q <= 0 || *p > *w {
		fmt.Println("Usage: go run main.go -variant=<variant> -m=<M> -w=<W> -p=<P> -k=<K> -q=<Q> -d=<density> [-runs=<runs>]")
		fmt.Println("Available variants:", variants)
		os.Exit(1)
	}

	v, ok := variants[*variantName]
	if !ok {
		fmt.Println("Invalid variant:", *variantName)
		os.Exit(1)
	}

	runBenchmark(v, *m, *w, *p, *k, *q, *runs, densityType(*d))
}
