# Security Policy

## Reporting vulnerabilities

If you discover a security issue (e.g., in the deployment scripts, web interface, or CI workflows), please email aaron.weiskittel@maine.edu rather than opening a public issue.

## Scope

This repository contains scientific simulation code, not production web services. However, the deployment infrastructure (Docker, Shiny server, nginx configs) may be used in server contexts, so security reports for those components are welcome.

## Credentials

Never commit credentials, API tokens, or passwords to this repository. The `.gitignore` is configured to exclude common credential files, but please double check before committing.
