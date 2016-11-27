/* Code sample: Using libdwarf for getting the address of a function
** from DWARF in an ELF executable.
** Not much error-handling or resource-freeing is done here...
**
** Eli Bendersky (http://eli.thegreenplace.net)
** This code is in the public domain.
*/
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <libdwarf/dwarf.h>
#include <libdwarf/libdwarf.h>


void die(char* fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    exit(EXIT_FAILURE);
}

void list_eh_frame_entries(Dwarf_Debug dbg)
{
}


int main(int argc, char** argv)
{
    Dwarf_Debug dbg = 0;
    Dwarf_Error err;
    const char* progname;
    int fd = -1;

    if (argc > 1) {
        progname = argv[1];
    } else {
        progname = "../../target/debug/dora";
    }

    if ((fd = open(progname, O_RDONLY)) < 0) {
        perror("open");
        return 1;
    }

    if (dwarf_init(fd, DW_DLC_READ, 0, 0, &dbg, &err) != DW_DLV_OK) {
        fprintf(stderr, "Failed DWARF initialization\n");
        return 1;
    }

    list_eh_frame_entries(dbg);

    if (dwarf_finish(dbg, &err) != DW_DLV_OK) {
        fprintf(stderr, "Failed DWARF finalization\n");
        return 1;
    }

    close(fd);
    return 0;
}


