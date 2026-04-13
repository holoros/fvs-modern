#!/usr/bin/env bash
# ==========================================================================
# check_upstream.sh
# Checks the USFS ForestVegetationSimulator GitHub repos for new commits
# since the last known sync point, and generates a change report.
#
# Usage:
#   ./check_upstream.sh [--sync-file PATH] [--output PATH] [--quiet]
#
# Requires: curl, python3 (or jq)
# ==========================================================================

set -euo pipefail

# Defaults
SYNC_FILE="${HOME}/.fvs-modern/upstream_sync.json"
OUTPUT=""
QUIET=false

# USFS repos to track
FVS_REPO="USDAForestService/ForestVegetationSimulator"
IFACE_REPO="USDAForestService/ForestVegetationSimulator-Interface"
GITHUB_API="https://api.github.com"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --sync-file) SYNC_FILE="$2"; shift 2 ;;
        --output)    OUTPUT="$2"; shift 2 ;;
        --quiet)     QUIET=true; shift ;;
        --help)
            echo "Usage: $0 [--sync-file PATH] [--output PATH] [--quiet]"
            exit 0 ;;
        *) echo "Unknown option: $1"; exit 1 ;;
    esac
done

# Ensure sync directory exists
mkdir -p "$(dirname "$SYNC_FILE")"

# Initialize sync file if it doesn't exist
if [ ! -f "$SYNC_FILE" ]; then
    cat > "$SYNC_FILE" << 'INIT'
{
    "fvs_last_sha": "66a5e14e",
    "fvs_last_date": "2026-03-06",
    "iface_last_sha": "",
    "iface_last_date": "2025-09-30",
    "last_check": "",
    "last_sync": "2026-04-13",
    "notes": "Baseline after PR #118 sync (SDI reporting update, format comma fixes, Reineke plumbing)"
}
INIT
    $QUIET || echo "Created initial sync file: $SYNC_FILE"
fi

# Function to get commits since a date from GitHub API
get_new_commits() {
    local repo="$1"
    local since_date="$2"
    local max_commits="${3:-50}"

    curl -sf "${GITHUB_API}/repos/${repo}/commits?since=${since_date}T00:00:00Z&per_page=${max_commits}" 2>/dev/null
}

# Function to get changed files for a specific commit
get_commit_files() {
    local repo="$1"
    local sha="$2"

    curl -sf "${GITHUB_API}/repos/${repo}/commits/${sha}" 2>/dev/null
}

# Read current sync state
LAST_FVS_DATE=$(python3 -c "import json; print(json.load(open('$SYNC_FILE'))['fvs_last_date'])" 2>/dev/null)
LAST_IFACE_DATE=$(python3 -c "import json; print(json.load(open('$SYNC_FILE'))['iface_last_date'])" 2>/dev/null)

$QUIET || echo "=== FVS Upstream Change Detection ==="
$QUIET || echo ""
$QUIET || echo "Checking since: FVS=$LAST_FVS_DATE  Interface=$LAST_IFACE_DATE"
$QUIET || echo ""

# Temp files for API responses
TMPFVS=$(mktemp)
TMPIFACE=$(mktemp)
trap "rm -f $TMPFVS $TMPIFACE" EXIT

# Check FVS main repo
get_new_commits "$FVS_REPO" "$LAST_FVS_DATE" 50 > "$TMPFVS" || echo "[]" > "$TMPFVS"
FVS_COUNT=$(python3 -c "import json; print(len(json.load(open('$TMPFVS'))))" 2>/dev/null || echo "0")

# Check Interface repo
get_new_commits "$IFACE_REPO" "$LAST_IFACE_DATE" 50 > "$TMPIFACE" || echo "[]" > "$TMPIFACE"
IFACE_COUNT=$(python3 -c "import json; print(len(json.load(open('$TMPIFACE'))))" 2>/dev/null || echo "0")

# Generate report
REPORT=$(python3 -c "
import json, sys
from datetime import datetime

fvs_data = json.load(open('$TMPFVS'))
iface_data = json.load(open('$TMPIFACE'))

report = []
report.append('=' * 68)
report.append('  FVS UPSTREAM CHANGE REPORT')
report.append('  Generated: ' + datetime.now().strftime('%Y-%m-%d %H:%M'))
report.append('=' * 68)
report.append('')

fvs_commits = []
for c in fvs_data:
    sha = c['sha'][:8]
    date = c['commit']['author']['date'][:10]
    msg = c['commit']['message'].split(chr(10))[0][:72]
    fvs_commits.append(f'  {sha}  {date}  {msg}')

report.append(f'FVS Core ({len(fvs_commits)} new commits since $LAST_FVS_DATE):')
if fvs_commits:
    report.extend(fvs_commits)
else:
    report.append('  (no new commits)')
report.append('')

iface_commits = []
for c in iface_data:
    sha = c['sha'][:8]
    date = c['commit']['author']['date'][:10]
    msg = c['commit']['message'].split(chr(10))[0][:72]
    iface_commits.append(f'  {sha}  {date}  {msg}')

report.append(f'FVS Interface ({len(iface_commits)} new commits since $LAST_IFACE_DATE):')
if iface_commits:
    report.extend(iface_commits)
else:
    report.append('  (no new commits)')
report.append('')

total = len(fvs_commits) + len(iface_commits)
if total == 0:
    report.append('STATUS: No upstream changes detected. fvs-modern is current.')
else:
    report.append(f'STATUS: {total} upstream commit(s) detected.')
    report.append('')
    report.append('RECOMMENDED ACTIONS:')
    if fvs_commits:
        report.append('  1. Review FVS core commits for bug fixes and new features')
        report.append('  2. Run: ./sync_upstream.sh --pull --convert --rebuild')
        report.append('  3. Run: ./run_regression_tests.sh (verify nothing broke)')
    if iface_commits:
        report.append('  4. Review Interface commits for rFVS/fvsOL changes')
        report.append('  5. Check if NAMESPACE files need updating')
        report.append('  6. Reinstall R packages and retest')

report.append('')
report.append('=' * 68)

print(chr(10).join(report))
" 2>&1)

if [ -n "$OUTPUT" ]; then
    echo "$REPORT" > "$OUTPUT"
    $QUIET || echo "Report saved to: $OUTPUT"
else
    echo "$REPORT"
fi

# Update last check timestamp
python3 << PYEOF
import json
from datetime import datetime
with open('$SYNC_FILE', 'r') as f:
    data = json.load(f)
data['last_check'] = datetime.now().strftime('%Y-%m-%d %H:%M')
with open('$SYNC_FILE', 'w') as f:
    json.dump(data, f, indent=2)
PYEOF

# Exit code: 0 if no changes, 1 if changes detected
if [ "$FVS_COUNT" -eq 0 ] && [ "$IFACE_COUNT" -eq 0 ]; then
    exit 0
else
    exit 2
fi
