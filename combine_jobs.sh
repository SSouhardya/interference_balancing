#!/bin/bash
# ---------------------------------------------------------------------------
# combine_jobs.sh
#
# Loops over all experiment settings and calls combine_results_general.R on
# each, the same way run_codes.sh does, with the same conveniences as
# submit_jobs.sh:
#
#   1. It builds the setting list programmatically with a loop.
#   2. It checks whether each setting folder exists before combining. A
#      missing folder cannot be combined, so it is skipped and recorded as
#      pending.
#   3. Settings that fail to combine (or whose folder is not present yet) are
#      recorded as pending so you can see what still needs attention.
#
# How the combiner is called (based on run_codes.sh):
#     module load R
#     Rscript ~/combine_results_general.R <flag> <path>
#   where <flag> is 1 for CI settings and 0 for estimation (est) settings.
#   (combine_results_general.R lives in the home directory.)
#
# Setting layout (mirrors run_codes.sh / submit_jobs.sh):
#   - methods: additive, graph, stratified
#   - each method has: ci, ci_lopsided, ci_snr, est, est_lopsided, est_snr
#   - the "_adapt" variants exist ONLY for graph, and ONLY on the CI side:
#       graph_ci_adapt, graph_ci_lopsided_adapt, graph_ci_snr_adapt
# ---------------------------------------------------------------------------

# --- configuration ---------------------------------------------------------

# Base directory that holds all the experiment folders (matches submit_jobs.sh).
BASE=~/

# The combiner R script (lives in the home directory).
COMBINE_SCRIPT=~/combine_results_general.R

# File that records settings that could not be combined this run.
STATE_DIR=~/
PENDING_FILE="$STATE_DIR/pending_combine.txt"       # lines: "setting"

# --- build the list of settings --------------------------------------------

SETTINGS=()

for method in additive graph stratified; do
    for variant in ci ci_lopsided ci_snr est est_lopsided est_snr; do
        SETTINGS+=("${method}_${variant}")
    done
    # The adapt variants are only made for 'graph' (and only for CI).
    if [ "$method" = "graph" ]; then
        SETTINGS+=("graph_ci_adapt" "graph_ci_lopsided_adapt" "graph_ci_snr_adapt")
    fi
done

# --- helpers ----------------------------------------------------------------

# Flag passed to combine_results_general.R: 1 for CI, 0 for estimation.
ci_flag() {
    case "$1" in
        *_est*) echo 0 ;;
        *)      echo 1 ;;
    esac
}

# --- run the combiner -------------------------------------------------------

module load R

# Rebuild the pending list from scratch on every run.
: > "$PENDING_FILE"

for setting in "${SETTINGS[@]}"; do

    folder="$BASE/$setting"
    flag=$(ci_flag "$setting")

    # Folder must exist to combine its results.
    if [ ! -d "$folder" ]; then
        echo "[pending] $folder does not exist yet -> skipped"
        echo "$setting" >> "$PENDING_FILE"
        continue
    fi

    echo "[run]     Rscript $COMBINE_SCRIPT $flag $folder"
    if Rscript "$COMBINE_SCRIPT" "$flag" "$folder"; then
        echo "[ok]      $setting combined (flag=$flag)"
    else
        echo "$setting" >> "$PENDING_FILE"
        echo "[pending] $setting failed to combine"
    fi
done

# --- summary ----------------------------------------------------------------

echo
echo "==================== summary ===================="
if [ -s "$PENDING_FILE" ]; then
    echo "Could not combine : $(wc -l < "$PENDING_FILE") settings (in $PENDING_FILE)"
else
    echo "All settings combined successfully."
fi
echo "================================================="
