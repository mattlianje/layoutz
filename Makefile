.PHONY: compile test test-jvm test-js test-native publish-local bundle clean fmt fmt-check repl repl2

VERSION := 0.7.0
BUNDLE_DIR := bundles
GPG_KEY := F36FE8EEBD829E6CF1A5ADB6246482D1268EDC6E

# Compile all versions (JVM + JS + Native)
compile:
	./mill layoutz.__.compile

repl:
	./mill -i layoutz.jvm[3.3.7].console

repl2:
	./mill -i layoutz.jvm[2.13.11].console

test: test-jvm test-js test-native

test-jvm:
	./mill layoutz.jvm.__.test

test-js:
	./mill layoutz.js.__.test

test-native:
	./mill layoutz.native.__.test

publish-local:
	./mill layoutz.__.publishLocal

fmt:
	./mill mill.scalalib.scalafmt/

fmt-check:
	./mill mill.scalalib.scalafmt/ --check

clean:
	./mill clean
	rm -rf $(BUNDLE_DIR)

bundle:
	@echo "Building publish artifacts..."
	@./mill show layoutz.__.publishArtifacts > /dev/null
	@rm -rf $(BUNDLE_DIR) && mkdir -p $(BUNDLE_DIR)/xyz/matthieucourt
	@for platform in jvm js native; do \
		for scalaVer in 2.12.18 2.13.11 3.3.7; do \
			artifactId=$$(./mill show layoutz.$$platform[$$scalaVer].artifactId 2>/dev/null | tr -d '"'); \
			dir=$(BUNDLE_DIR)/xyz/matthieucourt/$$artifactId/$(VERSION); \
			mkdir -p $$dir; \
			cp out/layoutz/$$platform/$$scalaVer/pom.dest/*.pom $$dir/$$artifactId-$(VERSION).pom; \
			cp out/layoutz/$$platform/$$scalaVer/jar.dest/out.jar $$dir/$$artifactId-$(VERSION).jar; \
			cp out/layoutz/$$platform/$$scalaVer/sourceJar.dest/out.jar $$dir/$$artifactId-$(VERSION)-sources.jar; \
			cp out/layoutz/$$platform/$$scalaVer/docJar.dest/out.jar $$dir/$$artifactId-$(VERSION)-javadoc.jar; \
			for f in $$dir/*; do \
				md5sum $$f | cut -d' ' -f1 > $$f.md5; \
				sha1sum $$f | cut -d' ' -f1 > $$f.sha1; \
				gpg --batch --yes -ab -u $(GPG_KEY) $$f; \
			done; \
			echo "Packaged $$artifactId"; \
		done; \
	done
	@cd $(BUNDLE_DIR) && zip -r layoutz-$(VERSION)-bundle.zip xyz
	@rm -rf $(BUNDLE_DIR)/xyz
	@echo "\nBundle ready: $(BUNDLE_DIR)/layoutz-$(VERSION)-bundle.zip"
