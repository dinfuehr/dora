#!/usr/bin/env bash

set -euo pipefail

ARCHIVE=${1-}
TMPDIR_DL=""
TMPDIR=""

cleanup() {
    [[ -n "$TMPDIR" ]] && rm -rf "$TMPDIR"
    [[ -n "$TMPDIR_DL" ]] && rm -rf "$TMPDIR_DL"
}

detect_archive_name() {
    local os arch
    os=$(uname -s)
    arch=$(uname -m)

    case "$arch" in
        x86_64|amd64)
            arch="x86_64"
            ;;
        arm64|aarch64)
            arch="aarch64"
            ;;
        *)
            echo "Unsupported architecture: $arch" >&2
            exit 1
            ;;
    esac

    case "$os" in
        Darwin)
            if [[ "$arch" != "aarch64" ]]; then
                echo "Only aarch64 macOS builds are supported." >&2
                exit 1
            fi
            echo "dora-aarch64-apple-darwin.tar.gz"
            ;;
        Linux)
            echo "dora-${arch}-unknown-linux-musl.tar.gz"
            ;;
        *)
            echo "Unsupported OS: $os" >&2
            exit 1
            ;;
    esac
}

if [[ -z "$ARCHIVE" ]]; then
    ARCHIVE=$(detect_archive_name)
fi

if [[ ! -f "$ARCHIVE" ]]; then
    TMPDIR_DL=$(mktemp -d -t dora-download-XXXXXX)
    DEST="$TMPDIR_DL/$ARCHIVE"
    URL="https://github.com/dinfuehr/dora/releases/latest/download/$ARCHIVE"

    if command -v curl >/dev/null 2>&1; then
        echo "Downloading $ARCHIVE via curl from $URL..."
        if ! curl -fL "$URL" -o "$DEST"; then
            echo "Failed to download $ARCHIVE with curl." >&2
            exit 1
        fi
    elif command -v wget >/dev/null 2>&1; then
        echo "Downloading $ARCHIVE via wget from $URL..."
        if ! wget -O "$DEST" "$URL"; then
            echo "Failed to download $ARCHIVE with wget." >&2
            exit 1
        fi
    else
        echo "Archive not found and neither curl nor wget is available to download it." >&2
        exit 1
    fi

    ARCHIVE="$DEST"
fi

if [[ ! -f "$ARCHIVE" ]]; then
    echo "Archive not found: $ARCHIVE" >&2
    exit 1
fi

TMPDIR=$(mktemp -d -t dora-install-XXXXXX)
trap cleanup EXIT

tar -xzf "$ARCHIVE" -C "$TMPDIR"

STAGING="$TMPDIR/dora"
if [[ ! -d "$STAGING" ]]; then
    STAGING=$(find "$TMPDIR" -mindepth 1 -maxdepth 1 -type d | head -n 1)
fi

if [[ -z "${STAGING:-}" || ! -d "$STAGING" ]]; then
    echo "Could not find extracted directory in $TMPDIR" >&2
    exit 1
fi

BIN_SRC="$STAGING/bin"
SHARE_SRC="$STAGING/share"

if [[ ! -d "$BIN_SRC" || ! -d "$SHARE_SRC" ]]; then
    echo "Archive is missing bin/ or share directories." >&2
    exit 1
fi

BIN_DEST="$HOME/.local/bin"
SHARE_DEST="$HOME/.local/share/dora"

mkdir -p "$BIN_DEST"
cp "$BIN_SRC"/* "$BIN_DEST"/
echo "Installed binaries into $BIN_DEST"

rm -rf "$SHARE_DEST"
mkdir -p "$SHARE_DEST"
cp -R "$SHARE_SRC"/* "$SHARE_DEST"/
echo "Installed share contents into $SHARE_DEST"
