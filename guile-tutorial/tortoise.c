/* Simple backend for a Logo like tortoise drawer.  */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

static const int WIDTH = 10;
static const int HEIGHT = 10;

static FILE*
start_gnuplot ()
{
    FILE* output;
    int pipes[2];
    pid_t pid;

    pipe (pipes);
    pid = fork ();

    if (!pid)
    {
        dup2 (pipes[0], STDIN_FILENO);
        execlp ("gnuplot", "gnuplot", NULL);
        return 0; /* Not reached.  */
    }

    output = fdopen (pipes[1], "w");

    fprintf (output, "set multiplot\n");
    fprintf (output, "set parametric\n");
    fprintf (output, "set xrange [-%d:%d]\n", WIDTH, WIDTH);
    fprintf (output, "set yrange [-%d:%d]\n", HEIGHT, HEIGHT);
    fprintf (output, "set size ratio -1\n");
    fprintf (output, "unset xtics\n");
    fprintf (output, "unset ytics\n");
    fflush (output);

    return output;
}

static FILE* global_output;

int
main (int argc, char* argv[])
{
    global_output = start_gnuplot ();

    return EXIT_SUCCESS;
}
