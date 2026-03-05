#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$PROJECT_ROOT/build-dmg"
DMG_NAME="CoopApp"
VERSION="${1:-dev}"

# Ensure xcodebuild uses Xcode.app (not Command Line Tools)
if [ -d "/Applications/Xcode.app" ]; then
  export DEVELOPER_DIR="/Applications/Xcode.app/Contents/Developer"
fi

echo "==> Building coop binary..."
cd "$PROJECT_ROOT"
cabal build exe:coop
COOP_BIN="$(cabal list-bin coop)"

echo "==> Checking coop dependencies..."
otool -L "$COOP_BIN" | grep -v /usr/lib/ | grep -v /System/ | grep -v "$COOP_BIN" || echo "    No non-system dependencies found (good)"

echo "==> Building CoopApp.app..."
cd "$PROJECT_ROOT/CoopApp"
xcodebuild -project CoopApp.xcodeproj \
  -scheme CoopApp \
  -configuration Release \
  -derivedDataPath "$BUILD_DIR/derived" \
  -arch arm64 \
  CODE_SIGN_IDENTITY="-" \
  CODE_SIGNING_ALLOWED=YES \
  ONLY_ACTIVE_ARCH=NO \
  clean build

APP_PATH="$BUILD_DIR/derived/Build/Products/Release/CoopApp.app"

if [ ! -d "$APP_PATH" ]; then
  echo "ERROR: CoopApp.app not found at $APP_PATH"
  exit 1
fi

echo "==> Copying coop binary into CoopApp.app bundle..."
cp "$COOP_BIN" "$APP_PATH/Contents/MacOS/coop"

echo "==> Ad-hoc signing coop binary..."
codesign --force --sign - "$APP_PATH/Contents/MacOS/coop"

echo "==> Ad-hoc signing CoopApp.app..."
codesign --force --deep --sign - "$APP_PATH"

echo "==> Verifying signature..."
codesign --verify --deep --strict "$APP_PATH" && echo "    Signature OK" || echo "    WARNING: Signature verification failed"

echo "==> Creating DMG..."
STAGING_DIR="$BUILD_DIR/dmg-staging"
rm -rf "$STAGING_DIR"
mkdir -p "$STAGING_DIR"
cp -R "$APP_PATH" "$STAGING_DIR/"
ln -s /Applications "$STAGING_DIR/Applications"

DMG_OUTPUT="$PROJECT_ROOT/$DMG_NAME-$VERSION.dmg"
rm -f "$DMG_OUTPUT"

hdiutil create \
  -volname "$DMG_NAME" \
  -srcfolder "$STAGING_DIR" \
  -ov \
  -format UDZO \
  "$DMG_OUTPUT"

echo "==> Cleaning up..."
rm -rf "$BUILD_DIR"

echo ""
echo "DMG created: $DMG_OUTPUT"
echo ""
echo "To install:"
echo "  1. Open the DMG"
echo "  2. Drag CoopApp to Applications"
echo "  3. Right-click CoopApp → Open (first launch, to bypass Gatekeeper)"
