# FVS Parameter Extraction Summary

## Overview

A comprehensive Python script (`extract_parameters.py`) has been created to extract species-specific parameter DATA statements from FVS (Forest Vegetation Simulator) variant Fortran source files and output them as structured JSON.

## Extraction Scope

### Variants Processed: 25

| Code | Name | Species Count | Categories | Files Parsed |
|------|------|---------------|-----------|--------------|
| acd | Alaska Coastal | 108 | 5 | 6 |
| ak | Alaska | 23 | 4 | 5 |
| bc | British Columbia | 15 | 2 | 5 |
| bm | Blue Mountains | 18 | 3 | 6 |
| ca | California | 50 | 4 | 6 |
| ci | Central Interior | 19 | 4 | 5 |
| cr | Central Rockies | 38 | 3 | 5 |
| cs | Central States | 96 | 5 | 6 |
| ec | East Central | 32 | 5 | 6 |
| em | Eastern Mountain | 19 | 4 | 5 |
| ie | Inland Empire | 23 | 3 | 5 |
| kt | Klamath | 11 | 3 | 5 |
| ls | Lake States | 68 | 5 | 6 |
| nc | North Central | 12 | 4 | 6 |
| ne | Northeastern US | 108 | 6 | 6 |
| oc | Okanogan | 50 | 3 | 5 |
| on | Ontario | 72 | 5 | 6 |
| op | Operable | 39 | 3 | 5 |
| pn | Pacific Northwest | 39 | 3 | 5 |
| sn | Southern | 90 | 3 | 6 |
| so | Southern Oregon | 33 | 3 | 6 |
| tt | Tahoe | 18 | 4 | 5 |
| ut | Utah | 24 | 4 | 5 |
| wc | West Coast | 39 | 3 | 6 |
| ws | Western Sierra | 43 | 4 | 6 |

## Source Files Parsed

For each variant, the script attempts to parse the following files:

1. **blkdat.f90** - Species definitions, bark ratios, error terms, bounds
2. **crown.f90** - Crown ratio coefficients (BCR1, BCR2, BCR3, BCR4)
3. **dgf.f90** - Diameter growth coefficients (B1, B2, B3)
4. **htdbh.f90** - Height-diameter relationship parameters
5. **htgf.f90** - Height growth coefficients
6. **sitset.f90** - Site index parameters

## Output Structure

Each variant generates a JSON file at `/home/aweiskittel/Documents/Claude/fvs-modern/config/{variant_code}.json` with the following schema:

```json
{
  "variant": "ne",
  "variant_name": "Northeastern US",
  "maxsp": 108,
  "files_parsed": ["blkdat.f90", "crown.f90", "dgf.f90", "htdbh.f90", "htgf.f90", "sitset.f90"],
  "categories": {
    "bark_ratio": {
      "BKRAT": [0.9349, 0.9349, 0.956, ...]
    },
    "growth": {
      "B1": [0.0008829, 0.0009933, ...],
      "B2": [0.0602785, 0.0816995, ...]
    },
    "crown": {
      "BCR1": [5.63, 6.0, 7.84, ...],
      "BCR2": [0.0047, 0.0053, ...],
      "BCR3": [3.523, 0.431, ...],
      "BCR4": [-0.0689, -0.0012, ...]
    },
    "height_diameter": {
      "SNALL": [...]
    },
    "height_growth": {
      "...": [...]
    },
    "site_index": {
      "SDICON": [...],
      "SICOEF": [...]
    },
    "species_definitions": {
      "JSP": ["BF ", "TA ", "WS ", ...],
      "FIAJSP": ["012", "071", "094", ...],
      "PLNJSP": ["ABBA  ", "LALA  ", ...]
    },
    "other": {
      "...": [...]
    }
  }
}
```

## Key Features

### Fortran Format Support

The script correctly handles:

- **Fortran repetition syntax**: `4*.9324` expands to `[0.9324, 0.9324, 0.9324, 0.9324]`
- **Multi-line continuations**: Lines ending with `&` are automatically joined
- **Comment handling**: `!` comments are stripped before parsing
- **Mixed data types**: Both numeric and character/string arrays
- **Various numeric formats**: Integer, float, scientific notation

### Array Categorization

Arrays are automatically categorized based on naming conventions:
- `BKRAT` → bark_ratio
- `BCR1-4` → crown
- `B1, B2, B3` → growth
- `SNALL` → height_diameter
- `JSP, FIAJSP, PLNJSP, NSP, JTYPE` → species_definitions

## Extraction Accuracy

The script has been validated against the source files and correctly:

- Expands Fortran repetition syntax
- Matches MAXSP parameter for species-specific arrays
- Preserves floating-point precision
- Parses quoted species codes and names
- Joins continued data statements

## Files Generated

- **Config Directory**: `/home/aweiskittel/Documents/Claude/fvs-modern/config/`
- **Individual Files**: 25 JSON files (one per variant)
- **Script Location**: `/home/aweiskittel/Documents/Claude/fvs-modern/modernization/extract_parameters.py`
- **Total Output Size**: ~900 KB

## Usage

To re-run extraction:

```bash
cd /home/aweiskittel/Documents/Claude/fvs-modern/modernization
python3 extract_parameters.py
```

