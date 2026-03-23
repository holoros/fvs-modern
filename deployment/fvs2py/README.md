# fvs2py (fvs-modern edition)

A Python wrapper for the Forest Vegetation Simulator (FVS), building from fvs-modern's modernized Fortran 90 source.

Based on [Vibrant Planet's fvs2py](https://github.com/Vibrant-Planet-Open-Science/fvs2py) (CC-BY-NC-SA-4.0), extended with all 24 regional variants (22 US + BC + ON) and the Acadian (ACD) variant.

## Usage

```python
from fvs2py import FVS

# Load a variant shared library
fvs = FVS("/usr/local/lib/FVSne.so")

# Load a keyword file and run
fvs.load_keyfile("my_stand.key")
fvs.run()

# Access results
print(fvs.summary)
print(fvs.species)
```

## Docker

```bash
docker build -t fvs-modern-fvs2py -f deployment/fvs2py/Dockerfile .
docker run -it fvs-modern-fvs2py python3
```

## Attribution

Original fvs2py by David Diaz at Vibrant Planet (CC-BY-NC-SA-4.0).
FVS modernization by Aaron Weiskittel, University of Maine.
