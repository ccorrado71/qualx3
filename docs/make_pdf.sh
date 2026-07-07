#!/bin/bash
# Generate QualX PDF manual from Markdown sources.
# Requires: pandoc, xelatex (texlive-xetex)
#
# Usage: bash make_pdf.sh [output.pdf]

set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-${SCRIPT_DIR}/qualx_manual.pdf}"

pandoc \
  "${SCRIPT_DIR}/docs/index.md" \
  "${SCRIPT_DIR}/docs/installation.md" \
  "${SCRIPT_DIR}/docs/database-installation.md" \
  "${SCRIPT_DIR}/docs/introduction/index.md" \
  "${SCRIPT_DIR}/docs/introduction/getting-started.md" \
  "${SCRIPT_DIR}/docs/introduction/examples.md" \
  "${SCRIPT_DIR}/docs/data-reduction/index.md" \
  "${SCRIPT_DIR}/docs/data-reduction/background.md" \
  "${SCRIPT_DIR}/docs/data-reduction/peak-search.md" \
  "${SCRIPT_DIR}/docs/data-reduction/smoothing.md" \
  "${SCRIPT_DIR}/docs/search-match/index.md" \
  "${SCRIPT_DIR}/docs/search-match/search-match.md" \
  "${SCRIPT_DIR}/docs/search-match/restraints.md" \
  "${SCRIPT_DIR}/docs/references.md" \
  -o "${OUT}" \
  --pdf-engine=xelatex \
  --toc \
  --toc-depth=2 \
  -V geometry:margin=2.5cm \
  -V fontsize=11pt \
  -V lang=en \
  -V mainfont="DejaVu Serif" \
  -V sansfont="DejaVu Sans" \
  -V monofont="DejaVu Sans Mono" \
  --metadata title="QualX Manual"

echo "PDF generated: ${OUT}"
