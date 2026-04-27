#!/usr/bin/env sh
# scripts/setup-dev.sh
#
# Local-development helper for cl-mcp's attach mode.  Ensures
# slynk-client (which is not on Quicklisp) is checked out beside
# cl-mcp under $LISP_WORKSPACE (or in the parent directory of this
# repo when LISP_WORKSPACE is unset), so ASDF's package-inferred
# source registry can resolve `:depends-on ("slynk-client")` in
# cl-mcp.asd.
#
# Idempotent: a no-op when slynk-client is already present.  Pins the
# upstream tip recorded by PR-A's smoke run; the SHA below tracks the
# revision the cl-mcp test suite has been verified against.
#
# This script is for local development and CI bootstrap only -- it is
# not invoked at run-time and not packaged with the system.

set -eu

# Pinned slynk-client commit (tip of fade/slynk-client master at the
# time PR-A's smoke confirmation landed -- see 01-STATE.md).  Bump
# only when the cl-mcp suite has been re-verified against a newer
# revision.
SLYNK_CLIENT_REPO="${SLYNK_CLIENT_REPO:-git@github.com:fade/slynk-client.git}"
SLYNK_CLIENT_COMMIT="${SLYNK_CLIENT_COMMIT:-f232d4dbbed03ff62ef8419eea580d2dfb9999de}"

# Where to put it.  Order of preference:
#   1. $LISP_WORKSPACE if set (developer-controlled workspace root).
#   2. The parent directory of this repo, mirroring the layout we
#      already use locally (~/SourceCode/lisp/cl-mcp and
#      ~/SourceCode/lisp/slynk-client).
script_dir="$(cd "$(dirname "$0")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"
target_root="${LISP_WORKSPACE:-$(dirname "$repo_root")}"
target_dir="$target_root/slynk-client"

if [ -d "$target_dir/.git" ]; then
  echo "slynk-client already present at $target_dir"
  exit 0
fi

echo "Cloning slynk-client into $target_dir"
mkdir -p "$target_root"
git clone "$SLYNK_CLIENT_REPO" "$target_dir"
git -C "$target_dir" checkout "$SLYNK_CLIENT_COMMIT"

echo
echo "slynk-client is checked out at $target_dir."
echo "Ensure $target_root (or $LISP_WORKSPACE) is on CL_SOURCE_REGISTRY"
echo "so ASDF can find it; the cl-mcp Nix devShell already does this."
