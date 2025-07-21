all: plot

bench: bench.csv

bench.csv:
	@echo "variant,M,W,P,K,Q,density,build_time_ns,build_peak_bytes,build_alloc_bytes,query_time_ns,query_peak_bytes,query_alloc_bytes" > bench.csv
	@for variant in full no_lcp no_doc no_lcp_no_doc full_hybrid_log; do \
		for density in low high; do \
			k=1; \
			while [ $$k -le 50000 ]; do \
				echo "Benchmarking $$variant $$density with M=50000 W=20 P=5 K=$$k Q=10000"; \
				go run cmd/bench/main.go -variant=$$variant -m=50000 -w=20 -p=5 -k=$$k -q=10000 -d=$$density -runs=3 >> bench.csv; \
				k=$$((k * 3)); \
				if [ $$k -lt 3 ]; then k=3; fi \
			done; \
		done; \
	done

plot: bench.csv
	python3 plot.py bench.csv

clean:
	rm -f bench.csv
	rm -rf plots 
	rm -f cpu.prof flame.svg

fuzz:
	go test -fuzz=FuzzFindKMatches -fuzztime=120s

publish:
	@if [ -z "$(VERSION)" ]; then echo "Set VERSION=<tag> (e.g. make publish VERSION=v0.1.0)"; exit 1; fi
	git tag $(VERSION)
	git push origin $(VERSION) 

interactive: cpu.prof
	go tool pprof -http=:8080 cpu.prof

cpu.prof:
	go build -o bench cmd/bench/main.go
	./bench -variant=full -m=50000 -w=20 -p=5 -k=100 -q=100000 -d=high -runs=1 -cpuprofile=cpu.prof
	rm bench

flamegraph: cpu.prof
	go tool pprof -raw -output=cpu.txt cpu.prof
	if [ ! -d ~/FlameGraph ]; then git clone https://github.com/brendangregg/FlameGraph.git ~/FlameGraph; fi
	~/FlameGraph/stackcollapse-go.pl cpu.txt | ~/FlameGraph/flamegraph.pl > flame.svg
	rm cpu.txt 