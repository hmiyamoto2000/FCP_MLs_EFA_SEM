# Cross-species gut microbiome & metabolome response to compost treatment

Analysis code and data accompanying the manuscript *[Title — TODO]*. The study examines
how a compost treatment (Control vs. Compost) reshapes the **gut microbiome** and
**metabolome** across three host species — **Fish, Chicken, and Pig** — and asks whether
any responses are shared across all three hosts.

The analysis moves from the community level to the factor level:

1. **Community structure** (Fig. 1–2): ordination, PERMANOVA, and variation partitioning
   show that host species dominates community variation while the treatment effect is
   comparatively small at the whole-community level.
2. **Factor level** (Fig. 4): a species-first, bottom-up **Cliff's delta** analysis
   identifies the genera and metabolites that respond to compost **in the same direction
   across all three species**, and cross-classifies them against ELA state-defining factors.
3. **Confirmation** (Fig. 5): a Bayesian multivariate model tests whether the key
   universally responsive metabolites move **as a group** under compost.

---

## Study design

- **Hosts (`env`):** Fish, Chicken, Pig.
- **Treatment (`cnd`):** `con` = Control, `Tst` = Compost.
- **Two data scopes:**
  - **Whole 16S cohort** — 61 samples (unbalanced), microbiome only (phylum + genus). Used for Fig. 1.
  - **Paired microbiome–metabolome subset** — 24 samples, balanced (4 per species × treatment), with both genus-level microbiome and the metabolome. Used for Fig. 2, Fig. 4, Fig. 5 and Supplementary S8/S11.

---

## Repository structure

Each figure is distributed as a self-contained bundle. A figure's outer zip contains its
code/data zip plus its own README:

```
Fig<X>.zip
├── Fig<X>_Code.zip     # scripts + input CSVs for that figure
└── Fig<X>_README.md    # how to reproduce that figure
```

For example, `Fig5_S11.zip` contains `Fig5_S11_Code.zip` and `Fig5_S11_README.md`.
Download the bundle for the figure you want, unzip both levels, and follow the enclosed
README.

---

## Figures

| Bundle | Manuscript figure | Analysis | Key tools |
|--------|-------------------|----------|-----------|
| `Fig1e.zip` | **Fig. 1e** | NMDS ordination (Bray–Curtis) of the whole-community microbiome + PERMANOVA | R `vegan`; Python matplotlib |
| `Fig2.zip` | **Fig. 2** | dbRDA + variation partitioning (microbiome × metabolome) | R `vegan`; Python matplotlib, matplotlib-venn |
| `Fig4_S10.zip` | **Fig. 4 + S10** | Bottom-up Cliff's delta (species-first) + ELA cross-classification; S10 = all-factor version | R `effsize`, `compositions`; Python matplotlib-venn |
| `FigS8.zip` | **Fig. S8** | Variation partitioning at the metabolite-**category** level (supplement to Fig. 2) | R `vegan`; Python matplotlib-venn |
| `Fig5_S11.zip` | **Fig. 5 + S11** | Bayesian group-importance of four metabolites (blavaan/Stan); S11 = diagnostics | R `blavaan`+Stan; Python matplotlib |

Each README carries the exact commands, inputs, and outputs for that figure.

---

## Shared input data

Several CSVs recur across bundles. They share the same sample ordering within each cohort
(the first column of every abundance table is the sample ID, matched to the metadata by
row position).

| File | Meaning |
|------|---------|
| `List.csv` / `list_whole.csv` | Sample metadata (`sample`, `cnd`, `env`) for the 24-sample subset / the 61-sample whole cohort. |
| `Bacteria_whole_genus.csv`, `Bacteria_whole_phylum.csv` | Whole-cohort microbiome (388 genera / 17 phyla). |
| `B_Genus_bacdata.csv` | Subset microbiome (207 genera). |
| `Chemical_data.csv` | Metabolite table (339 compounds). |
| `Chemical_Category_data.csv` | Metabolites collapsed into 5 categories (phosphate, carbohydrate, amino acid, SCFAs, others). |
| `interactions_list_040625.csv` | ELA interaction edge list (used for ★ annotation in Fig. 4). |

---

## Software requirements

The bundles use a mix of R and Python. Install what a given figure needs (see its README);
the union across all figures is:

**R (≥ 4.0)**

```r
install.packages(c(
  "vegan",                              # ordination, PERMANOVA, varpart (Fig.1, 2, S8)
  "effsize", "compositions",            # Cliff's delta, CLR (Fig.4)
  "blavaan", "loo", "future",           # Bayesian model (Fig.5)  — needs a Stan backend
  "rstan", "posterior",                 # Stan + diagnostics (Fig.5)
  "DiagrammeR", "DiagrammeRsvg", "rsvg" # association network (Fig.5)
))
```

> `blavaan` compiles models via Stan, so a working C++ toolchain and `rstan`/CmdStan are
> required for Fig. 5 — the heaviest dependency in the repository.

**Python (≥ 3.8)**

```bash
pip install pandas numpy matplotlib seaborn scipy matplotlib-venn python-docx
```

(`matplotlib-venn` is needed for the Venn diagrams; `python-docx` is optional and only used
for the Word diagnostics table in Fig. 5.)

---

## Getting started

1. Download the outer zip for the figure you want (e.g. `Fig2.zip`).
2. Unzip it, then unzip the inner `Fig2_Code.zip`.
3. Open `Fig2_README.md` and run the commands there, in order.

Most figures follow the same two/three-stage pattern: an R script produces result CSVs,
then a Python script renders the figure. Figures involving the metabolome first run
`metabolite_transform.py` to produce the log→z-scored table.

---

## Notes

- **Analysis vs. manuscript numbering.** Some scripts carry legacy figure labels in their
  comments (e.g. the Cliff's delta script in `Fig4_S10` refers internally to "Fig.3");
  the manuscript numbering in the table above is authoritative.
- **Reproducibility.** Random seeds are set (`set.seed(123)` for the vegan analyses,
  `seed = 1` for the Bayesian sampler), so results are stable up to backend/version
  differences.
- **Per-bundle prerequisites.** A couple of bundles depend on a file that lives with
  another figure — e.g. the category-level Fig. S8 needs the same `List.csv` as Fig. 2, and
  the Fig. 4 analysis needs `Chemical_Category_data.csv`. Each README calls these out; make
  sure each `Code.zip` contains everything its README lists before running.

---

## Acknowledgement

Analysis scripts in this repository were developed with the assistance of Claude (Anthropic). All code was critically reviewed, tested, and validated by H. Miyamoto.
