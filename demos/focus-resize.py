#!/usr/bin/env python3
"""Run a program in a pty whose width can be slid around mid-recording.

VHS fixes the terminal size at the top of a tape, so this sits between VHS's
terminal and the program...

Usage: focus-resize.py PROGRAM [ARGS...]
"""

import fcntl, os, pty, select, signal, struct, sys, termios, time, tty

NARROW = int(os.environ.get("NARROW", 76))  # columns after Alt+n
DURATION = float(os.environ.get("DURATION", 0.9))  # seconds per slide
FPS = int(os.environ.get("FPS", 40))             # resizes per second while sliding
EDGE = "\x1b[38;2;69;71;90m"  # Catppuccin Mocha surface1

CPR_ASK = b"\x1b[6n"
FRAME_END = b"\x1b[?2026l"     # layoutz wraps every frame in a synchronized update


def set_size(fd, rows, cols):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


def main():
    full, rows = os.get_terminal_size(0)
    full -= 1                    # leave a column for the edge
    width = full

    pid, fd = pty.fork()
    if pid == 0:
        os.execvp(sys.argv[1], sys.argv[1:])
    set_size(fd, rows, width)

    saved = termios.tcgetattr(0)
    tty.setraw(0)
    out = sys.stdout.buffer

    def edge(w):
        # the program can't clear past its own width, so wipe what it left there
        line = "".join(f"\x1b[{r};{w + 1}H│\x1b[K" for r in range(1, rows + 1))
        return f"\x1b7{EDGE}{line}\x1b[0m\x1b8".encode()

    slide = None                 # (start time, from width, to width)
    pending = b""                # child output past the last whole frame
    told = width                 # the width the program last asked for and got
    try:
        while True:
            timeout = 1 / FPS if slide else None
            ready, _, _ = select.select([0, fd], [], [], timeout)

            if 0 in ready:
                data = os.read(0, 1024)
                for key, target in ((b"\x1bn", NARROW), (b"\x1bw", full)):
                    if key in data:
                        data = data.replace(key, b"")
                        slide = (time.monotonic(), width, target)
                if data:
                    os.write(fd, data)

            if slide:
                start, a, b = slide
                t = min(1.0, (time.monotonic() - start) / DURATION)
                eased = t * t * (3 - 2 * t)            # smoothstep
                new = round(a + (b - a) * eased)
                if new != width:
                    width = new
                    set_size(fd, rows, width)          # child gets SIGWINCH
                if t >= 1.0:
                    slide = None

            if fd in ready:
                try:
                    data = os.read(fd, 65536)
                except OSError:
                    break
                if not data:
                    break
                data = pending + data
                pending = b""
                # frames already here were drawn for the width the program was
                # last told, not the one the pty has slid to since: an edge at
                # the new width would wipe the right of a wider frame
                drawn = told
                if CPR_ASK in data:
                    data = data.replace(CPR_ASK, b"")
                    os.write(fd, f"\x1b[{rows};{width}R".encode())
                    told = width
                # pass on whole frames only: the recording terminal doesn't
                # honour synchronized updates, so a frame split across two
                # writes can get painted half-drawn
                cut = data.rfind(FRAME_END)
                if cut == -1:
                    pending = data
                    continue
                cut += len(FRAME_END)
                data, pending = data[:cut], data[cut:]
                out.write(data.replace(FRAME_END, FRAME_END + edge(drawn)))
                out.flush()
    finally:
        out.write(pending)
        out.flush()
        termios.tcsetattr(0, termios.TCSADRAIN, saved)
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        os.waitpid(pid, 0)


if __name__ == "__main__":
    main()
