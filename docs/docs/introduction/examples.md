# Examples

QualX installs three test diffraction patterns together with the application,
in the `share/qualx/examples` folder inside the installation directory (for
example `<install_dir>/share/qualx/examples` on Linux). They can be opened
directly with **File → Import Diffraction Pattern**.

All three patterns are laboratory data collected with CuKα radiation, and
all are multi-phase mixtures with known ("true") weight fractions — they are
useful both for trying out the Search & Match workflow described in
[Getting Started](getting-started.md) and for checking the accuracy of the
Quantitative Phase Analysis (RIR) results against a reference composition.

## Example 1 — `example1.dat`

A three-phase mixture prepared in the laboratory.

| Phase                    | True weight fraction |
| ------------------------ | --------------------- |
| Corundum                 | 50.0% |
| Silicon                  | 30.0% |
| Lanthanum hexaboride     | 20.0% |

This is the simplest of the three mixtures and a good first test of the
default Search & Match settings (no restraints needed).

## Example 2 — `CPD2.dat`

A four-phase sample from a round-robin study on Quantitative Phase Analysis
(QPA) organized by the IUCr Commission on Powder Diffraction. The diffraction
data are affected by preferred orientation effects, which makes the
quantitative step less straightforward than Example 1.

| Phase     | True weight fraction |
| --------- | --------------------- |
| Corundum  | 21.27% |
| Zincite   | 19.94% |
| Fluorite  | 22.53% |
| Brucite   | 36.26% |

## Example 3 — `MIXT_5.dat`

A five-phase mixture prepared in the laboratory — the most demanding of the
three examples, useful for testing Search & Match restraints (e.g. chemical
composition or cell parameters) when the unrestrained result list becomes
too large to inspect by hand.

| Phase                 | True weight fraction |
| ---------------------- | --------------------- |
| Corundum               | 28.40% |
| Lanthanum hexaboride   | 18.70% |
| Zincite                | 13.10% |
| Calcite                | 30.90% |
| Silicon                | 8.90% |

## Suggested workflow

1. Import one of the example patterns (**File → Import Diffraction Pattern**).
2. Run background subtraction and peak search as described in
   [Getting Started](getting-started.md).
3. Run **Search → Search & Match** against a reference database containing
   the expected phases (e.g. corundum, silicon, zincite, fluorite, brucite,
   calcite, lanthanum hexaboride).
4. Accept the matching phases one by one and compare the RIR weight
   fractions shown in the **Quantitative** panel against the true values
   listed above.
