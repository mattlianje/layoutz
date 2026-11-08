#include <pty.h>
#include <stdlib.h>
#include <unistd.h>

/* forkpty + exec argv[0] on a rows x cols terminal. Returns the pid (or -1)
   and puts the master fd in *master. Done in C so the GHC runtime never runs
   in the forked child. */
int layoutz_bench_spawn(char *const argv[], int rows, int cols, int *master)
{
    struct winsize size = { .ws_row = rows, .ws_col = cols };
    pid_t pid = forkpty(master, NULL, NULL, &size);
    if (pid == 0) {
        /* same TERM for everyone */
        setenv("TERM", "xterm-256color", 1);
        unsetenv("COLORTERM");
        execv(argv[0], argv);
        _exit(127);
    }
    return pid;
}

long layoutz_bench_clock_ticks(void)
{
    return sysconf(_SC_CLK_TCK);
}
