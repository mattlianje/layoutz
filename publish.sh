#!/bin/bash

GITHUB_USER="mattlianje"
ARTIFACT_NAME="layoutz"
SCALA_VERSIONS=("2.12" "2.13" "3")
VERSION="0.5.0"
BUNDLE_DIR="$HOME/maven-bundle"

process_scala_version() {
  local SCALA_VERSION=$1
  local FULL_ARTIFACT_NAME="${ARTIFACT_NAME}_${SCALA_VERSION}"
  local SOURCE_DIR="$HOME/.ivy2/local/xyz.matthieucourt/${FULL_ARTIFACT_NAME}/${VERSION}"
  local TARGET_PATH="xyz/matthieucourt/${FULL_ARTIFACT_NAME}/${VERSION}"
  local TEMP_DIR="$BUNDLE_DIR/temp-${FULL_ARTIFACT_NAME}-${VERSION}"
  local BUNDLE_FILE="$BUNDLE_DIR/bundle-${FULL_ARTIFACT_NAME}-${VERSION}.zip"
  
  echo "Processing Scala ${SCALA_VERSION}..."
  
  # Remove existing bundle and temp directory if they exist
  [ -f "$BUNDLE_FILE" ] && rm "$BUNDLE_FILE"
  [ -d "$TEMP_DIR" ] && rm -rf "$TEMP_DIR"
  
  # Check if source directory exists
  if [ ! -d "$SOURCE_DIR" ]; then
    echo "  Warning: Source directory not found: $SOURCE_DIR"
    echo "  Skipping Scala ${SCALA_VERSION}"
    return 1
  fi
  
  # Prepare directories
  mkdir -p "$TEMP_DIR/$TARGET_PATH"
  
  # Copy files
  cd "$TEMP_DIR/$TARGET_PATH"
  
  echo "  Copying artifacts..."
  
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
  
  echo "  Creating bundle..."
  mkdir -p "$BUNDLE_DIR"
  cd "$TEMP_DIR"
  zip -q -r "$BUNDLE_FILE" xyz
  
  # Clean up temp directory
  rm -rf "$TEMP_DIR"
  
  echo "  Bundle created: $BUNDLE_FILE"
  return 0
}

main() {
  echo "Creating Maven bundles for all Scala versions..."
  echo "Artifact: $ARTIFACT_NAME"
  echo "Version: $VERSION"
  echo "Scala versions: ${SCALA_VERSIONS[*]}"
  echo ""
  
  # Ensure bundle directory exists
  mkdir -p "$BUNDLE_DIR"
  
  local success_count=0
  local total_count=${#SCALA_VERSIONS[@]}
  
  for scala_version in "${SCALA_VERSIONS[@]}"; do
    if process_scala_version "$scala_version"; then
      ((success_count++))
    fi
    echo ""
  done
  
  echo "Completed: $success_count/$total_count bundles created successfully"
  
  if [ $success_count -eq $total_count ]; then
    echo "All bundles ready for Maven Central"
  else
    echo "Some bundles failed"
    exit 1
  fi
}

main
