# Comprehensive Assessment of the USDA Forest Vegetation Simulator (FVS)

**Assessment Date:** March 22, 2026
**Prepared by:** Aaron Weiskittel, University of Maine
**Repositories Assessed:**
- ForestVegetationSimulator (Model Code)
- ForestVegetationSimulator-Interface (fvsOL, FVSPrjBldr, rFVS, FVSDataConvert)

---

## 1. Executive Summary

The Forest Vegetation Simulator is one of the most important tools in North American forest management, with a codebase spanning over five decades of continuous development. This assessment examines both the Fortran/C simulation engine and the R/Shiny web interface ecosystem. The model code is scientifically robust with strong regional variant architecture, but carries substantial technical debt from its Fortran 77 origins. The FVS Online interface (fvsOL) represents a significant modernization achievement built on R Shiny, though it has deployment portability issues and security limitations that must be addressed before broader public hosting. Key recommendations include modernizing the Fortran codebase toward free form standards with Fortran modules, refactoring the monolithic server.R in fvsOL, externalizing all hardcoded paths into configuration files, and producing a dedicated FVS Online hosting guide for new server deployments.

---

## 2. Model Code Repository Assessment

### 2.1 Repository Overview

The model code repository contains approximately 2,054 source files totaling roughly 6.3 million lines of code. The language distribution is approximately 98% Fortran (fixed form Fortran 77 with some modern extensions) and 2% C/C++ (SQLite integration, FOFEM fire model, and API wrappers). The build system uses CMake, supporting Visual Studio, MinGW, and Unix/GCC toolchains. Each regional variant compiles into a separate shared library plus a main executable.

### 2.2 Architecture and Module Organization

The FVS model code follows a well conceived variant based architecture that is conceptually sound. A shared base engine (150+ Fortran files in `base/`) provides core simulation logic including the main growth cycle, tree compression algorithms, expression evaluation, error handling, and API bindings. Each of 22 US and 2 Canadian regional variants overrides species specific routines (crown ratio, diameter growth, mortality, height diameter relationships, and site index) while inheriting all shared functionality.

Shared state is managed through over 40 COMMON block include files in `common/`, with key files including ARRAYS.F77 (tree record arrays for DBH, height, crown, probability), CONTRL.F77 (control parameters and growth switches), PLOT.F77 (plot level density metrics), COEFFS.F77 (species specific growth and mortality coefficients), and OUTCOM.F77 (output summary arrays).

Extension modules include a comprehensive fire effects system (`fire/`) with FOFEM integration, a regeneration/establishment subsystem (`estb/`, 31 files), volume calculations (`volume/`, 9 files), economic analysis (`econ/`), mistletoe dynamics (`mistoe/`), Organon growth model integration (`organon/`), a climate change module (`clim/`, currently minimal), and database backends in both legacy BM format (`dbs/`, 36 files) and modern SQLite (`dbsqlite/`).

This variant based polymorphism is an effective design pattern for regional model customization. It allows species specific calibration while maintaining a single code path for the simulation engine itself.

### 2.3 Line Level Code Quality Assessment

**Positive findings:**

IMPLICIT NONE is used broadly across the base module, indicating a mature awareness of type safety. Header comment blocks on routines are generally thorough, documenting purpose, inputs, outputs, and revision history. Research citations appear throughout (e.g., Teck & Hilt references in NE variant, NC-125 TWIGS documentation in crown routines). The API layer (`apisubs.f`) uses modern `iso_c_binding` for C interoperability with proper DLL export attributes for Windows. Species parameter tables in `blkdat.f` files are well documented with FIA species codes and scientific names.

**Areas of concern:**

*Fixed form Fortran exclusively.* The entire codebase uses fixed form Fortran 77 column dependent formatting (labels in columns 1 through 5, continuation in column 6, code in columns 7 through 72). The CMake build explicitly sets `CMAKE_Fortran_Format FIXED`. No free form `.f90` or `.f95` files exist anywhere in the repository. This makes the code harder to read, harder to maintain, and imposes the 72 character line limit that forces awkward line continuations in longer expressions.

*COMMON blocks rather than Fortran modules.* All shared state passes through COMMON blocks via include files rather than Fortran 90+ modules. COMMON blocks provide no encapsulation, no type checking across compilation units, and are inherently hostile to parallelization. The 40+ include files create a complex web of global state that makes reasoning about data flow extremely difficult.

*Extensive GOTO usage.* Computed GOTOs are used throughout for error dispatch (e.g., `errgro.f` with 60+ error codes), loop control, and conditional branching. While functional, these obscure control flow and make the code harder to follow. Modern structured constructs (SELECT CASE, DO WHILE, EXIT/CYCLE) could replace most GOTO patterns.

*Hardcoded magic numbers.* Array dimensions rely on parameters like MAXTRE, MAXSP, MAXPLT, and MAXCYC defined in `PRGPRM.F77`, which is appropriate. However, many routines contain hardcoded numeric constants embedded in DATA statements and inline expressions without named constants or explanatory comments. For example, crown ratio routines contain Weibull distribution coefficients loaded as bare numeric arrays indexed by species number.

*Variable naming conventions.* The codebase relies heavily on cryptic 3 to 4 letter abbreviations (WK1 through WK15 for work arrays, ITRN for tree index counters, DBH/BAL/CCF for forestry variables that at least have standard domain meanings). The work array pattern (temporary arrays reused across routines through COMMON blocks) saves memory but makes tracing data flow difficult.

*Large monolithic files.* Several files exceed reasonable sizes: `apisubs.f` (49,909 bytes), `comprs.f` (33,782 bytes), `errgro.f` (25,866 bytes), `estab.f` (47,197 bytes). These files handle multiple distinct responsibilities and would benefit from decomposition.

*Custom expression system.* The algebraic expression compiler (`algcmp.f`) and evaluator (`algevl.f`) implement a complete infix to reverse Polish notation parser for keyword expressions. While functional and impressively complete, this is a fully homegrown solution with manual stack management that represents a maintenance burden.

### 2.4 Testing Infrastructure

The test framework uses a Python script (`test.py`) that performs simple file comparison between expected and actual output. The orchestration `makefile` runs tests across all 24 variant directories. While comprehensive in variant coverage (all variants have test directories), the testing approach has significant limitations. There are no unit tests for individual routines, no automated regression testing framework (no GoogleTest, CppUnit, pFUnit, or similar), no continuous integration configuration, and the test approach is purely output comparison rather than behavioral verification. This makes it difficult to refactor confidently or detect subtle numerical regressions.

### 2.5 Specific Code Refinement Opportunities

**Priority 1: Modernize Fortran to free form.** A systematic conversion from fixed form to free form Fortran would be the single highest impact modernization. Tools like `fprettify` or `findent` can automate much of this conversion. This would remove the 72 character limit, improve readability, and prepare the codebase for further modernization.

**Priority 2: Replace COMMON blocks with Fortran modules.** Begin with the most heavily used COMMON blocks (ARRAYS, CONTRL, PLOT) and refactor them into Fortran 90 modules with explicit public/private interfaces. This is prerequisite for any future parallelization work and dramatically improves compile time type checking.

**Priority 3: Eliminate computed GOTOs.** Replace computed GOTOs with SELECT CASE statements. Replace loop control GOTOs with DO/EXIT/CYCLE constructs. The error dispatch in `errgro.f` is a prime candidate.

**Priority 4: Name magic numbers.** Extract hardcoded coefficients into named parameters or data structures with descriptive names. Even within the existing Fortran 77 framework, PARAMETER statements would improve clarity.

**Priority 5: Decompose large files.** Split `apisubs.f`, `estab.f`, and similar files into logical subunits. Each ENTRY point in `apisubs.f` could become its own file.

**Priority 6: Adopt a proper testing framework.** pFUnit (Fortran unit testing framework) would enable testing of individual subroutines. This should be accompanied by continuous integration configuration (GitHub Actions) to run tests on every commit.

---

## 3. FVS Interface Repository Assessment

### 3.1 Component Overview

The interface repository contains four components: fvsOL (FVS Online, the primary web application), FVSPrjBldr (Project Builder for initializing new online projects), rFVS (R package providing the Fortran bridge), and FVSDataConvert (database format conversion utility). All four components are built in R, with fvsOL and FVSPrjBldr using the Shiny web framework.

### 3.2 fvsOL (FVS Online): Deep Assessment

**Technology stack.** fvsOL is a comprehensive R Shiny application (package version 2025.09.30) requiring R 4.0+, Shiny 1.6.0+, RSQLite 2.2.4+, ggplot2, rgl (3D graphics), leaflet (mapping), rhandsontable (spreadsheet editing), and the R parallel package for multi core execution.

**Scale.** The application spans approximately 16,761 lines across 13 R source files, with the main `server.R` file alone containing 9,160 lines. The `inst/extdata/` directory holds configuration files, default databases, spatial reference data, help documentation, and custom run scripts for specific regional growth and yield models (e.g., Acadian, Adirondack).

**Functionality.** fvsOL provides a complete web based simulation environment: project management (create, load, save, duplicate, delete runs), inventory data management from SQLite databases, a component editor for building complex simulation configurations with conditional logic (IF/THEN/ENDIF), FVS keyword file generation, parallel execution across multiple CPUs, real time visualization with ggplot2, 3D tree position visualization with rgl, interactive leaflet maps, and data export in multiple formats.

**FVS integration.** The application dynamically discovers available FVS variants from a binary directory, loads them as shared libraries through the rFVS package, and executes simulations with support for stop points that allow interactive R code execution at specific stages of the FVS cycle. This is a powerful feature for advanced users who need custom processing between growth cycles.

**Code quality.** The R code is generally well structured with clear separation of concerns across utility files (fvsRunUtilities.R, writeKeyFile.R, mkInputElements.R, etc.). The use of Shiny reactive expressions for UI updates follows standard patterns. However, the 9,160 line server.R represents a significant maintainability challenge. This file handles project management, run configuration, visualization, data editing, output processing, and UI event handling in a single file. Breaking it into modular Shiny modules would greatly improve maintainability.

**Hardcoded paths and configuration.** This is the most pressing issue for portability. The codebase contains hardcoded references to `/home/shiny/FVSwork/` for project storage, `/home/shiny/FVS/bin/` for FVS binaries, and the Virginia Tech server URL `charcoal2.cnre.vt.edu`. The email delivery system assumes `ssmtp` is available. These paths are scattered throughout server.R and FVSPrjBldr/server.R rather than centralized in a configuration file.

**Security considerations.** The current deployment model has several security limitations. There is no user authentication beyond email address lookup. Projects are accessed via UUID in the URL with no password protection. Email addresses are stored in plain text. There is no rate limiting on project creation. SQL queries are built dynamically with potential injection vectors. The 60 day auto deletion policy provides some data lifecycle management, but this is insufficient for a publicly accessible server.

### 3.3 FVSPrjBldr (Project Builder)

The Project Builder is a lightweight Shiny application (server.R: 2,635 lines, ui.R: 2,979 lines) that creates new fvsOL projects. It generates unique UUIDs, creates project directories, sends email confirmations with project links, and can retrieve existing projects by email. The code is clean and well commented. The primary concern is the same hardcoded path issue, as it writes paths like `/home/shiny/FVSwork/{uuid}` into generated `app.R` files.

### 3.4 rFVS (R Package)

rFVS provides the critical bridge between R and the Fortran FVS shared libraries. It exposes 23 functions covering library loading, execution control, tree/species attribute access, activity scheduling (48+ activity codes), and interactive run capability. The package follows CRAN conventions with proper DESCRIPTION, NAMESPACE, and Roxygen documentation. The `.Fortran()` interface is used correctly with platform aware library extension handling. Code quality is good though some functions have minimal error handling. For example, `fvsLoad()` should provide more informative error messages when shared library loading fails, particularly for users on new server deployments who may have library path issues.

### 3.5 FVSDataConvert

The data conversion utility (server.R: 212 lines) converts Access (.accdb/.mdb) and Excel (.xlsx) databases into SQLite format. It depends on the external `mdb-tools` utility and contains a hardcoded Windows path (`C:/FVS/mdbtools/mdb-schema`). Error handling is minimal, and there is no transactional safety for the conversion process. On a positive note, it validates column names against a schema definition file (`databaseDescription.xlsx`), which helps maintain data integrity.

---

## 4. Documentation Gaps

### 4.1 Model Code Documentation

The model code has reasonable inline documentation (function headers, parameter descriptions, research citations) but lacks several important documentation artifacts. There is no architecture overview document explaining the variant system, the growth cycle state machine, or the module interaction patterns. There is no developer onboarding guide for new contributors. The build instructions assume familiarity with CMake and Fortran compilers. There is no API documentation for the C bindings in `apisubs.f` beyond inline comments. The extension module interfaces (fire, economics, climate) have no integration guides.

### 4.2 Interface Documentation

The fvsOL application includes built in help (`fvsOnlineHelp.html`) and a database schema description (`databaseDescription.xlsx`), which are useful. However, there is no deployment guide for hosting on a new server, which is the most critical missing document. There is no system administrator guide covering backup procedures, monitoring, log management, or user management. The rFVS package has function level documentation but no vignettes demonstrating common workflows. There is no troubleshooting guide for common deployment issues (shared library loading failures, database permission errors, email configuration).

### 4.3 Critical Gap: FVS Online Hosting Guide

The absence of a hosting guide is the single most important documentation gap, particularly given that colleagues wish to deploy FVS Online on new public servers. The following section provides an outline for what this document should contain.

---

## 5. FVS Online Public Server Hosting Guide (Recommended Outline)

### 5.1 Server Requirements

**Hardware.** Minimum 4 CPU cores (FVS uses R parallel for multi core execution), 8 GB RAM (more for concurrent users), 100 GB storage for project databases and FVS output files. SSD recommended for SQLite performance.

**Operating system.** Linux recommended (Ubuntu 22.04 LTS or RHEL 8+). The codebase supports Windows but the Shiny Server deployment model is Linux native.

**Software dependencies.**

- R 4.0 or later with the following packages: shiny (>= 1.6.0), RSQLite (>= 2.2.4), ggplot2, rgl, leaflet, rhandsontable, openxlsx, zip, dplyr, plyr, parallel
- Shiny Server Open Source or Shiny Server Pro (for authentication features)
- SQLite3 command line tools
- mdb-tools (for Access database conversion support)
- ssmtp or alternative SMTP relay (for email notifications)
- gfortran or compatible Fortran compiler (for building FVS binaries)
- CMake 3.0+ (for building FVS from source)

### 5.2 Building FVS Shared Libraries

Document the process of compiling FVS model code from source into shared libraries (.so files on Linux). Each variant produces a separate shared library. Cover CMake configuration, compiler flags, and verification steps. Document the expected output directory structure (one library per variant).

### 5.3 Configuration Externalization

Before deployment, all hardcoded paths must be externalized to a configuration file. The following paths need to be configurable:

- FVS binary directory (currently `/home/shiny/FVS/bin/`)
- Project work directory (currently `/home/shiny/FVSwork/`)
- Server base URL (currently `charcoal2.cnre.vt.edu`)
- SMTP configuration (currently hardcoded ssmtp)
- Project retention period (currently 60 days)
- Maximum upload size (currently 10 GB)
- Log file location

A recommended approach is an `fvsol_config.yml` file read at application startup, with environment variable overrides for containerized deployments.

### 5.4 Shiny Server Configuration

Document Shiny Server installation, the `shiny-server.conf` configuration for hosting fvsOL, reverse proxy setup (nginx or Apache) for HTTPS termination, process management and resource limits, and log rotation configuration.

### 5.5 Security Hardening

For a public server, the following security measures should be implemented:

- User authentication via Shiny Server Pro or a reverse proxy authentication layer
- HTTPS enforcement (Let's Encrypt or institutional certificates)
- Rate limiting on project creation endpoints
- Input validation and SQL parameterization to prevent injection
- Project access controls beyond UUID secrecy
- File upload scanning and size enforcement
- Network firewall configuration restricting access to necessary ports

### 5.6 Database Setup

Document initial database setup including the default training database (`FVS_Data.db.default`), spatial reference data (`SpatialData.RData.default`), and the empty template database. Cover backup procedures for project databases.

### 5.7 Monitoring and Maintenance

Document log monitoring (`FVSOnline.log`), project cleanup automation (the 60 day deletion policy), disk space monitoring, R package update procedures, and FVS binary update procedures when new model versions are released.

### 5.8 Containerization Option

A Docker based deployment would address many portability issues. Document a Dockerfile that packages R, all dependencies, Shiny Server, FVS binaries, and fvsOL into a single deployable container. This is the recommended path for new deployments as it eliminates environment configuration issues.

---

## 6. Summary of Recommendations

### 6.1 Model Code Recommendations

1. Convert all Fortran source to free form format (highest impact, lowest risk)
2. Begin systematic replacement of COMMON blocks with Fortran 90 modules, starting with ARRAYS and CONTRL
3. Replace computed GOTOs with SELECT CASE and structured loop constructs
4. Extract hardcoded coefficients into named PARAMETER constants
5. Decompose files exceeding 500 lines into logical subunits
6. Adopt pFUnit for unit testing and configure GitHub Actions CI
7. Write an architecture overview document and developer onboarding guide
8. Retire the legacy BM database format (`dbs/`) in favor of SQLite only
9. Expand the climate module (`clim/`) given increasing demand for climate aware projections

### 6.2 Interface Recommendations

1. **Create a comprehensive FVS Online hosting guide** (outlined in Section 5 above)
2. Externalize all hardcoded paths to a configuration file (`fvsol_config.yml`)
3. Refactor `server.R` (9,160 lines) into Shiny modules by functional area (project management, run configuration, visualization, data editing, output)
4. Add user authentication for public deployments
5. Parameterize SQL queries to prevent injection vulnerabilities
6. Create a Docker deployment option for portability
7. Add error handling and informative messages to rFVS `fvsLoad()` for troubleshooting shared library issues
8. Write rFVS vignettes covering common workflows (single stand simulation, batch processing, interactive runs)
9. Add input validation throughout fvsOL (file uploads, form inputs, database queries)
10. Establish automated testing for the Shiny interface (shinytest2 package)

### 6.3 Cross Cutting Recommendations

1. Create a unified documentation portal (GitHub Pages or similar) covering both model code and interface
2. Establish coding standards documents for both the Fortran codebase and the R interface code
3. Implement version tagging and release notes for coordinated model/interface releases
4. Consider a formal governance process for accepting community contributions (the repository has Contributing.md but the contribution workflow could be more detailed)

---

## 7. Conclusion

FVS remains an indispensable tool for forest management in North America. The scientific foundation encoded in the model code is sound, with well calibrated regional variants grounded in decades of FIA data and published research. The fvsOL web interface makes this power accessible to a broader audience. However, the technical debt accumulated over 50+ years of development creates real barriers to modernization, community contribution, and deployment flexibility. The recommendations in this assessment provide a prioritized roadmap for reducing that debt while preserving the scientific integrity that makes FVS valuable. The most immediately actionable item is the FVS Online hosting guide, which would enable colleagues to deploy FVS Online on new public servers without reverse engineering the current Virginia Tech deployment.
