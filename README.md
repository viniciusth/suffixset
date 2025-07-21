# suffixset

Efficient substring search over a set of words using suffix arrays.

This library implements a suffix array-based structure for fast substring matching in a collection of words. It concatenates all words with a special separator, builds a suffix array, and uses optional LCP arrays and document listing structures for optimized queries.

## Builder Options

The `SuffixSetBuilder` allows configuring trade-offs between preprocessing time, memory usage, and query performance:

1. **SkipLCP** (`b.SkipLCP()`):
   - **Description**: Skips building the Longest Common Prefix (LCP) array.
   - **Preprocessing**: Saves O(|S|) time and space (where |S| is the total length of concatenated words).
   - **Query Time**: Binary search becomes O(|P| * log(|S|)) instead of O(|P| + log(|S|)) (where |P| is pattern length).
   - **Use Case**: When memory is limited and patterns are short.

2. **SkipDocListing** (`b.SkipDocListing()`):
   - **Description**: Skips building document listing structures (prev array and RMQ).
   - **Preprocessing**: Saves O(|S|) time and space.
   - **Query Time**: Finding distinct matches can take O(occ) worst-case (where occ is number of occurrences), using a naive scan with a map.
   - **Use Case**: When the number of occurrences is small or K is small, trading query speed for lower memory.

3. **CaseSensitive** (`b.CaseSensitive()`):
   - **Description**: Makes the search case-sensitive (default is case-insensitive).
   - **Preprocessing**: No significant change.
   - **Query Time**: Unchanged.
   - **Use Case**: When case matters in matching.

4. **SkipNormalization** (`b.SkipNormalization()`):
   - **Description**: Skips NFC Unicode normalization.
   - **Preprocessing**: Slightly faster, no normalization step.
   - **Query Time**: Unchanged.
   - **Use Case**: When input is already normalized or normalization is not desired.

5. **UseHybridLogRMQ** (`b.UseHybridLogRMQ()`):
   - **Description**: Uses hybrid log RMQ for LCP and prev arrays instead of hybrid naive.
   - **Preprocessing**: O(n log log n) time and space.
   - **Query Time**: Faster RMQ queries in practice.
   - **Use Case**: For larger datasets where RMQ speed matters, at the cost of more memory.

## Benchmarks

The benchmarks focus on varying K (number of requested matches) with fixed M=50,000 words, W=20 word length, P=5 pattern length, Q=10,000 queries.

### Setting Up and Running Benchmarks

1. **Set up Python virtual environment** (if needed for plotting):
   ```bash
   python3 -m venv venv
   source venv/bin/activate
   pip install matplotlib pandas seaborn
   ```

2. **Run the benchmarks and generate plots**:
   ```bash
   make
   ```
   This will:
   - Run the Go benchmark program to generate `bench.csv`.
   - Use the Python script to generate plots in the `plots/` directory.

3. **Clean up**:
   ```bash
   make clean
   ```
   Removes generated CSV and plots.

## Benchmark Results

(Add your system specifications and run `make` to generate results)

### Query Time vs. K (M=50,000, W=20, P=5, Q=1,000,000)
![Query Time vs. K](plots/query_time_vs_k.png)