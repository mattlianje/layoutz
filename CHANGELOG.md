# Changelog

All notable changes to layoutz will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [0.8.0] - 2026-07-12

### Added
- Inline raster images via the kitty graphics protocol
- One-shot `Ask.*` prompts that collapse to a single line once answered
- `loader` for wrapping `Iterable`s and `Iterator`s with a live progress bar or spinner, with chainable styles
- Clipboard commands: `Cmd.clipboard.read`, `Cmd.clipboard.write`.
- Terminal commands: `Cmd.setTitle`, `Cmd.showCursor`, `Cmd.hideCursor`.
