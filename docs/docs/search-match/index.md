# Search & Match

QualX identifies crystalline phases by comparing the experimental peak list
against a reference database and ranking candidates by a **Figure of Merit (FOM)**.

## Menu

| Action | Description |
|--------|-------------|
| **Search → Search & Match** | Run a full search using current peaks and options |
| **Search → Search & Match Options** | Configure FOM weights, Δ2θ, min. FOM |
| **Search → Restraints** | Restrict the search to specific elements, space groups, etc. |
| **Search → Manage Databases** | Add, remove or switch reference databases |

## Results List

After a search, the **Results List** dock shows ranked candidates with columns:

| Column | Description |
|--------|-------------|
| (colour) | Unique colour assigned to the card |
| QM | Quality mark of the reference card |
| ID | Card identifier |
| Chemical Name | Compound name |
| Chemical Formula | Chemical formula |
| Peakpos. | Peak-position component of FOM |
| Intensity | Intensity component of FOM |
| Scale | Intensity scale factor |
| FOM | Total Figure of Merit (higher = better match) |
| S-Quant. | Semi-quantitative percentage (RIR, if available) |

See [Search Match](search-match.md) and [Restraints](restraints.md) for details.
