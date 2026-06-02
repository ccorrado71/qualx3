# Restraints

Restraints narrow the database search by pre-filtering cards that do not match
specified chemical or crystallographic criteria.

Open **Search → Restraints**.

## Properties tab

| Field | Description |
|-------|-------------|
| **Chemical name** | Filter by compound name (partial match) |
| **Color** | Filter by crystal color |
| **Density (calc.)** | Filter by calculated density ± tolerance |
| **Density (meas.)** | Filter by measured density ± tolerance |

## Composition tab

| Field | Description |
|-------|-------------|
| **Elements** | Include/exclude chemical elements |
| **Mode** | `Contains` (AND/OR), `Only` (exact composition), `Just` (at least these) |

## Symmetry tab

| Field | Description |
|-------|-------------|
| **Crystal system** | Filter by crystal system (cubic, tetragonal, …) |
| **Space group** | Filter by space group symbol |
| **Cell parameters** | Filter by unit cell constants a, b, c, α, β, γ ± tolerance |

## Entries tab

Paste a list of card IDs (one per line) to restrict the search to those
specific entries.

## Subfiles tab

Filter by PDF-2 subfile codes (e.g., `I` = inorganic, `O` = organic).

## Buttons

| Button | Action |
|--------|--------|
| **Load cards** | Query the database with the current restraints (replaces results) |
| **Load & Merge** | Query and add results to the current list |
| **Search & Match** | Run a FOM-ranked search within the restrained subset |
