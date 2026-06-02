# Search & Match

## How it works

QualX performs a multi-step search:

1. **Strongest peaks filter**: for each database card, only the _N_ strongest
   reference peaks are compared against the experimental peak list (fast pre-filter).
2. **FOM calculation**: a weighted Figure of Merit combines peak-position
   matching and intensity matching.
3. **Ranking**: cards are sorted by descending FOM; only cards above
   **Min. FOM** are shown.

## Figure of Merit

$$
\text{FOM} = w_{2\theta} \cdot \text{FOM}_{2\theta} + w_I \cdot \text{FOM}_I
$$

where $w_{2\theta}$ and $w_I$ are user-defined weights (0–1).

## Search Options

Open **Search → Search & Match Options**:

| Option | Description |
|--------|-------------|
| **Min. FOM** | Discard cards below this threshold |
| **2θ / d weight** | Contribution of peak-position agreement to FOM |
| **Intensity weight** | Contribution of intensity agreement to FOM |
| **Phases weight** | Bias towards single-phase vs. multi-phase solutions |
| **Δ2θ** | Peak matching tolerance in degrees (Auto = calculated from peak widths) |
| **Max. entries** | Maximum number of results to display |
| **Check strongest peaks** | Use only the N strongest peaks for the pre-filter |
| **Check deleted cards** | Include cards marked as deleted in the database |
| **Residual searching** | After accepting a phase, automatically re-search on residual peaks |

## Accepting a Phase

1. Select a card in the Results List.
2. Click **Accept** (✓). The card is moved to the **Quantitative** dock.
3. If **Residual searching** is enabled, QualX subtracts the accepted phase
   contribution and repeats the search on the remaining peaks.
