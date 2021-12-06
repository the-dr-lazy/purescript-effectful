#!/usr/bin/env bash
set -euo pipefail

[[ "${CI-false}" == "false" ]] && pre-commit install
