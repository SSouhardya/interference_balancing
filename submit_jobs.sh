#!/bin/bash
# ---------------------------------------------------------------------------
# submit_jobs.sh
#
# Loops over all experiment settings and submits the SLURM array jobs, the
# same way run_codes.sh does, but with three extra features:
#
#   1. It builds the folder list programmatically with a loop.
#   2. Before submitting, it checks whether each experiment folder exists.
#      If not, it creates it (mkdir -p).
#   3. It remembers what it has already submitted. Successful submissions
#      (with their SLURM job id) are stored in a state file. Settings that
#      could NOT be submitted (e.g. because a job-submission limit was hit)
#      are recorded as "pending". The NEXT time you run this script, it
#      skips the ones already submitted and only retries the pending ones.
#
# Setting layout (mirrors run_codes.sh):
#   - methods: additive, graph, stratified
#   - each method has: ci, ci_lopsided, ci_snr, est, est_lopsided, est_snr
#   - the "_adapt" variants exist ONLY for graph, and ONLY on the CI side:
#       graph_ci_adapt, graph_ci_lopsided_adapt, graph_ci_snr_adapt
# ---------------------------------------------------------------------------

# --- configuration ---------------------------------------------------------

# Base directory that holds all the experiment folders.
BASE=~/

# SLURM array range and the job script inside each folder.
ARRAY_RANGE="1-1500"
JOB_SCRIPT="run_spillover.sh"

# State files that let the script resume across runs.
STATE_DIR=~/
SUBMITTED_FILE="$STATE_DIR/submitted_jobs.txt"   # lines: "setting  jobid"
PENDING_FILE="$STATE_DIR/pending_settings.txt"   # lines: "setting"

mkdir -p "$STATE_DIR"
touch "$SUBMITTED_FILE"

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

# --- helper: has this setting already been submitted? -----------------------

already_submitted() {
    # Matches the first whitespace-separated column in the submitted file.
    grep -q "^$1[[:space:]]" "$SUBMITTED_FILE"
}

# --- main loop --------------------------------------------------------------

# Rebuild the pending list from scratch on every run: a setting is pending
# only if it is still not submitted AND fails again this time.
: > "$PENDING_FILE"

for setting in "${SETTINGS[@]}"; do

    # Skip anything we have already successfully submitted on a previous run.
    if already_submitted "$setting"; then
        echo "[skip]   $setting already submitted (see $SUBMITTED_FILE)"
        continue
    fi

    folder="$BASE/$setting"

    # Feature 2: create the folder if it does not exist.
    if [ ! -d "$folder" ]; then
        echo "[mkdir]  $folder did not exist -> creating"
        mkdir -p "$folder"
    fi

    cd "$folder" || { echo "[error]  cannot cd into $folder"; echo "$setting" >> "$PENDING_FILE"; continue; }

    # Clean previous outputs (safe if none exist).
    rm -f *.err *.out *.list

    # Make sure the job script is actually present before trying to submit.
    if [ ! -f "$JOB_SCRIPT" ]; then
        echo "[error]  $JOB_SCRIPT missing in $folder -> recorded as pending"
        echo "$setting" >> "$PENDING_FILE"
        continue
    fi

    # Attempt the submission and capture output + exit status.
    output=$(sbatch --array="$ARRAY_RANGE" "$JOB_SCRIPT" 2>&1)
    status=$?

    if [ $status -eq 0 ]; then
        # sbatch prints: "Submitted batch job 123456"
        jobid=$(echo "$output" | grep -oE '[0-9]+' | tail -1)
        echo "$setting $jobid" >> "$SUBMITTED_FILE"
        echo "[ok]     $setting -> job id $jobid"
    else
        # Could not submit (e.g. job-submission limit reached). Record it so
        # the next run picks it up.
        echo "$setting" >> "$PENDING_FILE"
        echo "[pending] $setting could NOT be submitted: $output"
    fi
done

# --- summary ----------------------------------------------------------------

echo
echo "==================== summary ===================="
echo "Submitted so far : $(wc -l < "$SUBMITTED_FILE") settings (ids in $SUBMITTED_FILE)"
if [ -s "$PENDING_FILE" ]; then
    echo "Still pending    : $(wc -l < "$PENDING_FILE") settings (in $PENDING_FILE)"
    echo "Re-run this script to retry them."
else
    echo "Still pending    : none — everything submitted."
fi
echo "================================================="
