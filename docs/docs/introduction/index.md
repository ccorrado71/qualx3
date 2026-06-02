# Introduction

QualX is designed around a simple workflow:

```
Import pattern → Data Reduction → Search & Match → Accept phases
```

## Main Window

The main window is divided into several panels:

- **Graphic area** (centre): displays the diffraction pattern, background,
  peaks and accepted phase reflections
- **Results List** (bottom/right dock): ranked list of candidate phases
- **Peaks** (dock): list of experimental peaks
- **Peak Compare** (dock): overlay of selected card reflections vs. experimental peaks
- **Quantitative** (dock): accepted phases with RIR percentages

## Workflow Overview

1. **Import** a diffraction pattern (File → Import Diffraction Pattern)
2. **Background**: estimate and optionally subtract the background
3. **Peak Search**: detect peak positions and intensities
4. **Search/Match**: query the database and rank candidate phases by FOM
5. **Accept**: move identified phases to the Quantitative panel
6. **Save** the project (.qxp) to resume later

See the individual sections for detailed instructions.
