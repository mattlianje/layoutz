#!/bin/bash
GITHUB_USER="mattlianje"
ARTIFACT_NAME="layoutz"
SCALA_VERSION="3"
VERSION="0.1.0"
BUNDLE_DIR="$HOME/maven-bundle"

# Derived variables
setup() {
  FULL_ARTIFACT_NAME="${ARTIFACT_NAME}_${SCALA_VERSION}"
  SOURCE_DIR="$HOME/.ivy2/local/xyz.matthieucourt/${FULL_ARTIFACT_NAME}/${VERSION}"
  TARGET_PATH="xyz/matthieucourt/${FULL_ARTIFACT_NAME}/${VERSION}"
  TEMP_DIR="$BUNDLE_DIR/temp-${FULL_ARTIFACT_NAME}-${VERSION}"
  BUNDLE_FILE="$BUNDLE_DIR/bundle-${FULL_ARTIFACT_NAME}-${VERSION}.zip"
}

check_bundle() {
  if [ -f "$BUNDLE_FILE" ]; then
    echo "Bundle already exists at $BUNDLE_FILE"
    exit 0
  fi
}

prepare_dirs() {
  mkdir -p "$TEMP_DIR/$TARGET_PATH"
}

copy_files() {
  cd "$TEMP_DIR/$TARGET_PATH"
  
  # Main JAR + metadata
  cp "$SOURCE_DIR/jars/${FULL_ARTIFACT_NAME}.jar" "./${FULL_ARTIFACT_NAME}-${VERSION}.jar" 2>/dev/null
  cp "$SOURCE_DIR/jars/${FULL_ARTIFACT_NAME}.jar.asc" "./${FULL_ARTIFACT_NAME}-${VERSION}.jar.asc" 2>/dev/null
  cp "$SOURCE_DIR/jars/${FULL_ARTIFACT_NAME}.jar.md5" "./${FULL_ARTIFACT_NAME}-${VERSION}.jar.md5" 2>/dev/null
  cp "$SOURCE_DIR/jars/${FULL_ARTIFACT_NAME}.jar.sha1" "./${FULL_ARTIFACT_NAME}-${VERSION}.jar.sha1" 2>/dev/null
  
  # Sources JAR + metadata
  cp "$SOURCE_DIR/srcs/${FULL_ARTIFACT_NAME}-sources.jar" "./${FULL_ARTIFACT_NAME}-${VERSION}-sources.jar" 2>/dev/null
  cp "$SOURCE_DIR/srcs/${FULL_ARTIFACT_NAME}-sources.jar.asc" "./${FULL_ARTIFACT_NAME}-${VERSION}-sources.jar.asc" 2>/dev/null
  cp "$SOURCE_DIR/srcs/${FULL_ARTIFACT_NAME}-sources.jar.md5" "./${FULL_ARTIFACT_NAME}-${VERSION}-sources.jar.md5" 2>/dev/null
  cp "$SOURCE_DIR/srcs/${FULL_ARTIFACT_NAME}-sources.jar.sha1" "./${FULL_ARTIFACT_NAME}-${VERSION}-sources.jar.sha1" 2>/dev/null
  
  # Javadoc JAR + metadata
  cp "$SOURCE_DIR/docs/${FULL_ARTIFACT_NAME}-javadoc.jar" "./${FULL_ARTIFACT_NAME}-${VERSION}-javadoc.jar" 2>/dev/null
  cp "$SOURCE_DIR/docs/${FULL_ARTIFACT_NAME}-javadoc.jar.asc" "./${FULL_ARTIFACT_NAME}-${VERSION}-javadoc.jar.asc" 2>/dev/null
  cp "$SOURCE_DIR/docs/${FULL_ARTIFACT_NAME}-javadoc.jar.md5" "./${FULL_ARTIFACT_NAME}-${VERSION}-javadoc.jar.md5" 2>/dev/null
  cp "$SOURCE_DIR/docs/${FULL_ARTIFACT_NAME}-javadoc.jar.sha1" "./${FULL_ARTIFACT_NAME}-${VERSION}-javadoc.jar.sha1" 2>/dev/null
  
  # POM and its metadata
  cp "$SOURCE_DIR/poms/${FULL_ARTIFACT_NAME}.pom" "./${FULL_ARTIFACT_NAME}-${VERSION}.pom" 2>/dev/null
  cp "$SOURCE_DIR/poms/${FULL_ARTIFACT_NAME}.pom.asc" "./${FULL_ARTIFACT_NAME}-${VERSION}.pom.asc" 2>/dev/null
  cp "$SOURCE_DIR/poms/${FULL_ARTIFACT_NAME}.pom.md5" "./${FULL_ARTIFACT_NAME}-${VERSION}.pom.md5" 2>/dev/null
  cp "$SOURCE_DIR/poms/${FULL_ARTIFACT_NAME}.pom.sha1" "./${FULL_ARTIFACT_NAME}-${VERSION}.pom.sha1" 2>/dev/null
}

create_zip() {
  mkdir -p "$BUNDLE_DIR"
  cd "$TEMP_DIR"
  zip -q -r "$BUNDLE_FILE" xyz
}

cleanup() {
  rm -rf "$TEMP_DIR"
}

main() {
  setup
  check_bundle
  prepare_dirs
  copy_files
  create_zip
  cleanup
  echo "Bundle created at $BUNDLE_FILE"
}

main
