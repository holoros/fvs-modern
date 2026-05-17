# fvs-modern integration test v2 (variant-specific decks)

Run: SLURM job $SLURM_JOB_ID
Date: 2026-05-17 10:40 UTC
Branch: acd-bridge-fix-2026-05-15
Commit: 4bd6e96 Autopilot round 7: comprehensive integration test

## Test scope: 12 variants

**Eastern** (7): ACD, NE, CS, LS, SN, KT, EM
**Western sample** (5): WC (Westside Cascades), OP (Olympic Peninsula), CA (Inland California), BM (Blue Mountains), CR (Central Rockies)

**Test deck:** each variant uses its own upstream tests/FVS<variant>/ deck.
ACD falls back to NE's net01 because no FVSacd test dir exists upstream.

## Refined rubric

- **PASS**: rc in {0, 10} and .sum >= 1KB
- **WARN**: rc=20 (input-variant mismatch or non-fatal error) but .sum complete
- **FAIL**: no .sum, .sum too small, marker missing, or rc >= 30

## Results

| Phase | Variant | Status | Detail |
| --- | --- | --- | --- |
| build | acd | PASS | 8156064B |
| build | ne | PASS | 8169728B |
| build | cs | PASS | 8129504B |
| build | ls | PASS | 8116920B |
| build | sn | PASS | 8157192B |
| build | kt | PASS | 9143240B |
| build | em | PASS | 9270872B |
| build | wc | PASS | 9329560B |
| build | op | PASS | 8949160B |
| build | ca | PASS | 8653744B |
| build | bm | PASS | 9278736B |
| build | cr | PASS | 9313736B |
| run | acd | PASS | rc=10 sum=8762B out=217766B  deck=net01.key |
| run | ne | PASS | rc=10 sum=8762B out=216966B  deck=net01.key |
| run | cs | FAIL | rc=2 sum=0B (too small) |
| run | ls | FAIL | rc=2 sum=0B (too small) |
| run | sn | FAIL | rc=2 sum=0B (too small) |
| run | kt | FAIL | rc=2 sum=0B (too small) |
| run | em | PASS | rc=10 sum=8762B out=188821B  deck=emt01.key |
| run | wc | FAIL | rc=2 sum=0B (too small) |
| run | op | PASS | rc=10 sum=9791B out=231583B  deck=opt01.key |
| run | ca | FAIL | rc=2 sum=0B (too small) |
| run | bm | FAIL | rc=2 sum=0B (too small) |
| run | cr | FAIL | rc=20 sum=547B (too small) |
| marker | acd | PASS | found AC |
| marker | ne | PASS | found NE |
| marker | cs | SKIP | no .sum |
| marker | ls | SKIP | no .sum |
| marker | sn | SKIP | no .sum |
| marker | kt | SKIP | no .sum |
| marker | em | PASS | found EM |
| marker | wc | SKIP | no .sum |
| marker | op | PASS | found OP |
| marker | ca | SKIP | no .sum |
| marker | bm | SKIP | no .sum |
| marker | cr | PASS | found CR |
| distinct | all | PASS | 5 variants, 5 unique md5s |
| smoke | postpass | PASS | stratified post-pass OK |

## Pass summary

- Total: 38
- PASS: 23
- WARN: 0
- FAIL: 8
- SKIP: 7

**Overall: PARTIAL** (8 FAIL among 38 checks)
