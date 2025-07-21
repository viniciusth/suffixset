import pandas as pd
import matplotlib.pyplot as plt
import sys
import os
import matplotlib.ticker as ticker
import seaborn as sns
import numpy as np

def format_time_ns(y, _):
    if y == 0:
        return '0'
    if y >= 1e9:
        return f'{y/1e9:g}s'
    if y >= 1e6:
        return f'{y/1e6:g}ms'
    if y >= 1e3:
        return f'{y/1e3:g}μs'
    return f'{y:g}ns'

def format_mem_bytes(y, _):
    if y == 0:
        return '0'
    if y >= 1024**3:
        return f'{y/1024**3:g}GB'
    if y >= 1024**2:
        return f'{y/1024**2:g}MB'
    if y >= 1024:
        return f'{y/1024:g}KB'
    return f'{y:g}B'

def main():
    if len(sys.argv) < 2:
        print("Usage: python plot.py data.csv")
        sys.exit(1)
    df = pd.read_csv(sys.argv[1])
    os.makedirs("plots", exist_ok=True)
    sns.set_theme(style="darkgrid")

    # Only vary K
    fixed_m = 50000
    fixed_w = 20
    fixed_p = 5
    fixed_q = 10000
    df_k = df[(df['M'] == fixed_m) & (df['W'] == fixed_w) & (df['P'] == fixed_p) & (df['Q'] == fixed_q)]

    power_of_10_formatter = ticker.FuncFormatter(lambda x, _: f'$10^{{{int(np.log10(x))}}}$' if x > 0 else '0')

    plt.figure(figsize=(10, 6))
    ax = sns.lineplot(data=df_k, x='K', y='query_time_ns', hue='variant', style='density', errorbar='sd')
    ax.set_xscale('log')
    ax.set_yscale('log')
    ax.yaxis.set_major_formatter(ticker.FuncFormatter(format_time_ns))
    ax.set_xlabel('Requested Matches (K)')
    ax.set_ylabel('Total Query Time for Q queries')
    ax.set_title(f'Query Time vs K (M={fixed_m:,}, W={fixed_w}, P={fixed_p}, Q={fixed_q:,})')
    ax.xaxis.set_major_formatter(power_of_10_formatter)
    plt.savefig('plots/query_time_vs_k.png')
    plt.close()

if __name__ == "__main__":
    main() 