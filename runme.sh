#!/bin/sh

if [ -L "${0}" ]; then
  SCRIPT_PATH="$(readlink "${0}")"
else
  SCRIPT_PATH="${0}"
fi
SCRIPT_DIR="$(dirname "${SCRIPT_PATH}")"
cd "${SCRIPT_DIR}"

WORK_DIR="$(pwd)"

echo "Installing Git hooks"
ln -s "$WORK_DIR/git-hooks/pre-commit" "$WORK_DIR/.git/hooks/pre-commit"
