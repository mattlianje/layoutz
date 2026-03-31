# Changelog

All notable changes to layoutz-hs will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [0.3.4.0] - 2026-03-31

### Added
- `renderText :: Element a => a -> Text` for rendering directly to `Data.Text.Text`.
- Unit tests for `renderText`.

### Changed
- Added `text` as an explicit dep on boot (stil technically zero dep by GHC standards)
- But easy to comment out `renderText` for porting to MicroHs

## [0.3.3.0] - 2026-03-14

### Added
- `runAppFinal`: like `runApp` but returns the final application state.
- `runAppWithFinal`: like `runAppWith` but returns the final application state.
- `runInline`: renders app inline without alt-screen, for embedded spinners/progress bars.
- New `InlineLoadingDemo` example.

## [0.3.2.0] - 2026-03-11

### Added
- `CmdExit` command for graceful app shutdown from within `appUpdate`.
- `cmdIsExit` helper to check if a command tree contains an exit.

### Fixed
- Haddock markup errors.
- `SpinnerDemo.hs` example.

## [0.3.1.0] - 2026-03-01

### Changed
- Documentation tweaks and Haddock polish.

## [0.3.0.0] - 2026-03-01

### Added
- TUI runtime (`LayoutzApp`, Elm Architecture style event loop).
- Keyboard input handling (`Key`, `readKey`).
- Commands (`Cmd`, `cmdFire`, `cmdTask`, `cmdAfterMs`).
- Subscriptions (`Sub`, `subKeyPress`, `subEveryMs`).
- `AppOptions` and `AppAlignment` for layout customization.
- Spinner animations (`SpinnerDots`, `SpinnerLine`, `SpinnerClock`, `SpinnerBounce`).
- Visualization primitives: `plotSparkline`, `plotLine`, `plotPie`, `plotBar`, `plotStackedBar`, `plotHeatmap`.
- Braille-based line and pie chart rendering.
- 256-color and RGB true-color support (`ColorFull`, `ColorTrue`).
- Text styles with `Semigroup` combining (`StyleBold <> StyleItalic`).
- `tightRow` for gapless horizontal layouts.
- `wrap` for word-boundary text wrapping.
- Ordered lists (`ol`) with nested numbering (arabic, alpha, roman).

## [0.1.0.0] - 2026-02-27

### Added
- Initial Haskell port of layoutz.
- Core DSL: `text`, `layout`, `box`, `row`, `center`, `ul`, `table`, `kv`, `tree`.
- Border styles: normal, double, thick, round, ASCII, block, dashed, dotted, half-block, markdown, custom.
- `withBorder`, `withColor`, `withStyle` combinators.
- ANSI-aware width calculation (`charWidth`, `visibleLength`).
- `hr`, `vr`, `pad`, `margin`, `chart`, `section`, `alignLeft`/`alignRight`/`alignCenter`/`justify`.
- `OverloadedStrings` support for `L`.
