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

At the end of the search–match step a list of plausible database crystalline
phases are ranked according to the decreasing values of a FoM. The FoM
includes four contributions, three of them suitably weighted, according to
the following formula:

$$
\mathrm{FoM}=\sqrt{\frac{\mathrm{FoM}_{db}\cdot\left(w_{\theta}\,\mathrm{FoM}_{\theta}
+w_{I}\,\mathrm{FoM}_{I}+w_{ph}\,\mathrm{FoM}_{ph}\right)}{w_{\theta}+w_{I}+w_{ph}}}
$$

where FoM<sub>θ</sub> takes into account the average difference between the
2θ values of the observed and matched database peaks; FoM<sub>I</sub> is
related to the average difference between the intensity values of the
observed and matched database peaks; FoM<sub>ph</sub> depends on the
percentage of the matched experimental peaks and on their intensity; and
FoM<sub>db</sub> depends on the percentage of the matched database peaks and
on their intensity.

FoM<sub>θ</sub> is the contribution coming from the 2θ differences between
the experimental and the associated database peaks:

$$
\mathrm{FoM}_{\theta} = 1 - \dfrac{\displaystyle\sum_{i}^{N_{db}^{ass}}
\left|2\theta_i^{\mathrm{exp}} - 2\theta_i^{db}\right|}{N_{db}^{ass}\cdot\Delta}
$$

where the summation is over the associated database peaks (a database peak
is considered associated if its 2θ distance from the experimental peak is
less than Δ), 2θ<sup>exp</sup> and 2θ<sup>db</sup> are the positions of the
experimental and database peaks, respectively.

FoM<sub>I</sub> is the contribution due to the differences between the
intensities of the experimental and the associated database peaks:

$$
\mathrm{FoM}_{I} = 1 - \dfrac{\displaystyle\sum_{i}^{N_{\mathrm{exp}}^{ass}}
\left|I_i^{\mathrm{exp}} - I_i^{db}\right|}{N_{\mathrm{exp}}^{ass}}
$$

where I<sup>exp</sup> and I<sup>db</sup> are the experimental and database
intensity respectively, the summation is over the associated experimental
peaks.

FoM<sub>ph</sub> is the contribution due to the intensities of the
associated experimental peaks and their percentage:

$$
\mathrm{FoM}_{ph} = \sqrt{\dfrac{\displaystyle\sum_{i=1}^{N_{\mathrm{exp}}^{ass}}
I_i^{\mathrm{exp}}}{\displaystyle\sum_{i=1}^{N_{\mathrm{exp}}} I_i^{\mathrm{exp}}}
\cdot\dfrac{N_{\mathrm{exp}}^{ass}}{N_{\mathrm{exp}}}}
$$

where N<sub>exp</sub> is the total number of experimental peaks.

FoM<sub>db</sub> is the contribution due to the intensities of the
associated database peaks and their percentage:

$$
\mathrm{FoM}_{db} = \sqrt{\dfrac{\displaystyle\sum_{i=1}^{N_{db}^{ass}}
I_i^{db}}{\displaystyle\sum_{i=1}^{N_{db}} I_i^{db}}
\cdot\dfrac{N_{db}^{ass}}{N_{db}}}
$$

where I<sub>db</sub> is the database intensity and the summation at the
numerator is over the associated database peaks; N<sub>db</sub> is the total
number of the database peaks.

The weighting factors w<sub>θ</sub>, w<sub>I</sub>, w<sub>ph</sub> and Δ
(default heuristic values are set for them) can be adjusted directly by the
user via the graphical interface. w<sub>ph</sub> is related to the number of
expected phases.

The default value of the weights w<sub>θ</sub>, w<sub>I</sub>, and
w<sub>ph</sub> is 0.5, but it can be changed by the dialogue window in
Figure 1 (via the "2θ", "Intensity" and "Phases" trackbars).

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
