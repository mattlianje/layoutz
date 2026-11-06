#!/usr/bin/env python3
"""
Render README screenshots of layoutz demos.

    render.py Demo.scala out.png                  # vim buffer | program output
    render.py --output-only Charts.scala out.png  # program output only

Needs scala-cli, rsvg-convert and the JetBrains Mono NL font.
"""
import argparse
import os
import re
import subprocess
import sys
import unicodedata
from html import escape

FONT = 'JetBrains Mono NL'
FONT_SIZE = 14
CELL_W = FONT_SIZE * 0.6
LINE_H = round(FONT_SIZE * 1.3, 2)
PAD_X, PAD_Y = 14, 12
CLOCK = '12:21 05-Nov-26'

# Catppuccin Mocha
BASE = '#1e1e2e'
MANTLE = '#181825'
SURFACE0 = '#313244'
SURFACE1 = '#45475a'
OVERLAY1 = '#7f849c'
OVERLAY2 = '#9399b2'
TEXT = '#cdd6f4'
BLUE = '#89b4fa'
GREEN = '#a6e3a1'
MAUVE = '#cba6f7'
PEACH = '#fab387'
YELLOW = '#f9e2af'
SKY = '#89dceb'
MAROON = '#eba0ac'
ANSI = [
    '#45475a', '#f38ba8', '#a6e3a1', '#f9e2af', '#89b4fa', '#f5c2e7', '#94e2d5', '#bac2de',
    '#585b70', '#f38ba8', '#a6e3a1', '#f9e2af', '#89b4fa', '#f5c2e7', '#94e2d5', '#a6adc8',
]

SGR = re.compile(r'\x1b\[([0-9;]*)m')
RESET = '\x1b[0m'


def char_width(ch):
    if unicodedata.combining(ch):
        return 0
    if unicodedata.east_asian_width(ch) in 'WF' or ord(ch) >= 0x1F300:
        return 2
    return 1


def width(s):
    return sum(char_width(c) for c in SGR.sub('', s))


def params(match):
    return [int(p) if p else 0 for p in match.group(1).split(';')] if match.group(1) else [0]


def esc(fg=None, bg=None, bold=False, italic=False):
    def rgb(c):
        if isinstance(c, tuple):
            return ';'.join(map(str, c))
        return ';'.join(str(int(c[i:i + 2], 16)) for i in (1, 3, 5))

    codes = ['0']
    if bold:
        codes.append('1')
    if italic:
        codes.append('3')
    if fg:
        codes.append('38;2;' + rgb(fg))
    if bg:
        codes.append('48;2;' + rgb(bg))
    return f'\x1b[{";".join(codes)}m'


def pad(s, n, bg=None):
    return s + esc(bg=bg) + ' ' * (n - width(s)) + RESET


def xterm256(n):
    if n < 16:
        return ANSI[n]
    if n < 232:
        n -= 16
        levels = [0, 95, 135, 175, 215, 255]
        return (levels[n // 36], levels[n // 6 % 6], levels[n % 6])
    g = 8 + (n - 232) * 10
    return (g, g, g)


def recolor(line):
    """Map the program's terminal colours onto the palette."""
    st = dict(fg=None, bg=None, bold=False, rev=False)

    def sub(match):
        ps = params(match)
        i = 0
        while i < len(ps):
            p = ps[i]
            if p == 0:
                st.update(fg=None, bg=None, bold=False, rev=False)
            elif p == 1:
                st['bold'] = True
            elif p == 7:
                st['rev'] = True
            elif 30 <= p <= 37:
                st['fg'] = ANSI[p - 30]
            elif 90 <= p <= 97:
                st['fg'] = ANSI[p - 82]
            elif 40 <= p <= 47:
                st['bg'] = ANSI[p - 40]
            elif 100 <= p <= 107:
                st['bg'] = ANSI[p - 92]
            elif p == 39:
                st['fg'] = None
            elif p == 49:
                st['bg'] = None
            elif p in (38, 48):
                key = 'fg' if p == 38 else 'bg'
                if ps[i + 1] == 5:
                    st[key] = xterm256(ps[i + 2])
                    i += 2
                elif ps[i + 1] == 2:
                    st[key] = tuple(ps[i + 2:i + 5])
                    i += 4
            i += 1
        if st['rev']:
            return esc(st['bg'] or BASE, st['fg'] or TEXT, st['bold'])
        return esc(st['fg'], st['bg'], st['bold'])

    return SGR.sub(sub, line) + RESET


STYLES = {
    'kw': (MAUVE, False), 'str': (GREEN, False), 'com': (OVERLAY1, True),
    'num': (PEACH, False), 'const': (PEACH, False), 'fn': (BLUE, False),
    'typ': (YELLOW, False), 'op': (SKY, False), 'punc': (OVERLAY2, False),
    'param': (MAROON, False), 'plain': (TEXT, False),
}
KEYWORDS = {'val', 'if', 'else', 'import', 'by', 'to', 'def', 'new'}
TOKEN = re.compile(
    r'(?P<com>/\*.*?\*/|//.*)'
    r'|(?P<str>"[^"]*")'
    r'|(?P<num>\b\d+(\.\d+)?\b)'
    r'|(?P<id>[A-Za-z_]\w*)'
    r'|(?P<op>->|=>|\+\+|[=+*\-<>])'
    r'|(?P<punc>[()\[\]{},.;:])'
    r'|(?P<plain>.)'
)


def highlight(line, bg):
    out, prev = [], ''
    for m in TOKEN.finditer(line):
        kind, s = m.lastgroup, m.group(0)
        if kind == 'id':
            if s in KEYWORDS:
                kind = 'kw'
            elif s[0].isupper():
                kind = 'const' if prev == '.' else 'typ'
            elif line[m.end():].startswith('('):
                kind = 'fn'
            elif s in ('i', 'r', 'g', 'b'):
                kind = 'param'
            else:
                kind = 'plain'
        if s.strip():
            prev = s
        color, italic = STYLES[kind]
        out.append(esc(color, bg, italic=italic) + s)
    return ''.join(out)


def displayed_source(src):
    lines = [l for l in src.splitlines() if not l.startswith('//>')]
    lines = [re.sub(r'^@main def \w+\s*=\s*', '', l) for l in lines]
    while lines and not lines[0].strip():
        lines.pop(0)
    return lines


def prompt(name):
    return (esc(MAUVE) + '~/layoutz ' + esc(GREEN, bold=True) + '❯ '
            + esc(TEXT) + f'scala-cli run {name}' + RESET)


def output_screen(output, name):
    return ['', '  ' + prompt(name), ''] + ['  ' + recolor(l.rstrip()) + '  ' for l in output]


def split_screen(code, output, name):
    left = [''] + ['   ' + highlight(l, MANTLE) for l in code] + ['']
    right = ['', '  ' + prompt(name), ''] + ['  ' + recolor(l.rstrip()) for l in output]
    half = max(max(width(l) for l in code) + 6, max(width(l) for l in right) + 4)
    rows = max(len(left) + 1, len(right))
    left += [''] * (rows - 1 - len(left))
    left = [pad(esc(bg=MANTLE) + l, half, MANTLE) for l in left]

    mode = esc(BASE, BLUE, True) + ' NORMAL ' + esc(TEXT, SURFACE0) + f' {name} '
    pos = esc(OVERLAY2, SURFACE0) + ' scala ' + esc(BASE, BLUE, True) + f' {len(code)}:1 '
    gap = half - width(mode) - width(pos)
    left.append(mode + esc(bg=MANTLE) + ' ' * gap + pos + RESET)

    divider = esc(SURFACE1) + '│' + RESET
    right += [''] * (rows - len(right))
    lines = [l + divider + pad(r, half) for l, r in zip(left, right)]

    session, windows, clock = ' layoutz ', ' 1:nvim*  2:zsh ', f' {CLOCK} '
    gap = max(width(l) for l in lines) - len(session) - len(windows) - len(clock)
    lines.append(esc(BASE, GREEN, True) + session + esc(TEXT, MANTLE) + windows
                 + ' ' * gap + esc(BASE, GREEN) + clock + RESET)
    return lines


def cells(line):
    st = dict(fg=None, bg=None, bold=False, italic=False)
    out, pos = [], 0
    for m in list(SGR.finditer(line)) + [None]:
        end = m.start() if m else len(line)
        for ch in line[pos:end]:
            out.append((ch, char_width(ch), st['fg'], st['bg'], st['bold'], st['italic']))
        if not m:
            break
        pos = m.end()
        ps = params(m)
        i = 0
        while i < len(ps):
            p = ps[i]
            if p == 0:
                st.update(fg=None, bg=None, bold=False, italic=False)
            elif p == 1:
                st['bold'] = True
            elif p == 3:
                st['italic'] = True
            elif p in (38, 48) and ps[i + 1] == 2:
                st['fg' if p == 38 else 'bg'] = '#%02x%02x%02x' % tuple(ps[i + 2:i + 5])
                i += 4
            i += 1
    return out


BRAILLE_DOTS = [(0, 0, 1), (0, 1, 2), (0, 2, 4), (1, 0, 8), (1, 1, 16), (1, 2, 32), (0, 3, 64), (1, 3, 128)]


def is_block(c):
    return 0x2581 <= ord(c) <= 0x2588


def is_braille(c):
    return 0x2800 <= ord(c) <= 0x28FF


def svg(screen):
    rows = [cells(l) for l in screen]
    cols = max(sum(c[1] for c in row) for row in rows)
    w, h = PAD_X * 2 + cols * CELL_W, PAD_Y * 2 + len(rows) * LINE_H
    out = []
    for n, row in enumerate(rows):
        y = PAD_Y + n * LINE_H
        runs, col = [], 0
        for ch, cw, fg, bg, bold, italic in row:
            if cw == 0:
                continue
            style = (fg, bg, bold, italic)
            if runs and runs[-1][0] == style:
                runs[-1][2].append((ch, col))
            else:
                runs.append([style, col, [(ch, col)]])
            col += cw

        for (fg, bg, bold, italic), start, chars in runs:
            fill = fg or TEXT
            if bg:
                last, last_col = chars[-1]
                span = last_col + char_width(last) - start
                out.append(f'<rect x="{PAD_X + start * CELL_W:.2f}" y="{y:.2f}" '
                           f'width="{span * CELL_W + 0.5:.2f}" height="{LINE_H + 0.5:.2f}" fill="{bg}"/>')
            # Draw block and braille glyphs as shapes so bars and plots are crisp.
            for c, k in chars:
                x = PAD_X + k * CELL_W
                if is_block(c):
                    bh = LINE_H * (ord(c) - 0x2580) / 8
                    out.append(f'<rect x="{x:.2f}" y="{y + LINE_H - bh:.2f}" '
                               f'width="{CELL_W + 0.4:.2f}" height="{bh + 0.4:.2f}" fill="{fill}"/>')
                elif is_braille(c) and c != '⠀':
                    bits = ord(c) - 0x2800
                    r = min(CELL_W * 0.5, LINE_H * 0.25) * 0.36
                    for dx, dy, bit in BRAILLE_DOTS:
                        if bits & bit:
                            out.append(f'<circle cx="{x + CELL_W * (0.27 + 0.46 * dx):.2f}" '
                                       f'cy="{y + LINE_H * (0.125 + 0.25 * dy):.2f}" r="{r:.2f}" fill="{fill}"/>')
            text = [(c, k) for c, k in chars if c != ' ' and not is_block(c) and not is_braille(c)]
            if not text:
                continue
            attrs = f'fill="{fill}"'
            if bold:
                attrs += ' font-weight="bold"'
            if italic:
                attrs += ' font-style="italic"'
            ty = f'{y + LINE_H * 0.76:.2f}'
            glyphs = ''.join(f'<text x="{PAD_X + k * CELL_W:.2f}" y="{ty}">{escape(c)}</text>' for c, k in text)
            out.append(f'<g {attrs}>{glyphs}</g>')

    body = '\n'.join(out)
    return (f'<svg xmlns="http://www.w3.org/2000/svg" width="{w:.0f}" height="{h:.0f}" '
            f'viewBox="0 0 {w:.2f} {h:.2f}">\n'
            f'<rect width="{w:.2f}" height="{h:.2f}" fill="{BASE}"/>\n'
            f'<g font-family="{FONT}" font-size="{FONT_SIZE}" xml:space="preserve">\n{body}\n</g></svg>\n')


def main():
    ap = argparse.ArgumentParser(description='Render a layoutz demo to PNG.')
    ap.add_argument('--output-only', action='store_true', help='skip the vim pane and tmux bar')
    ap.add_argument('source')
    ap.add_argument('png')
    args = ap.parse_args()

    run = subprocess.run(['scala-cli', 'run', '-q', args.source], capture_output=True, text=True)
    if run.returncode != 0:
        sys.exit(run.stderr or run.stdout)
    output = run.stdout.rstrip('\n').splitlines()
    name = os.path.basename(args.source)

    if args.output_only:
        screen = output_screen(output, name)
    else:
        with open(args.source) as f:
            screen = split_screen(displayed_source(f.read()), output, name)

    subprocess.run(['rsvg-convert', '-z', '2', '-o', args.png], input=svg(screen).encode(), check=True)
    print(f'wrote {args.png}')


if __name__ == '__main__':
    main()
