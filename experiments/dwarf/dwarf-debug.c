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

void list_eh_frame_entries(Dwarf_Debug dbg, Dwarf_Addr mypcval)
{
    Dwarf_Signed count = 0;
    Dwarf_Cie *cie_data = 0;
    Dwarf_Signed cie_count = 0;
    Dwarf_Fde *fde_data = 0;
    Dwarf_Signed fde_count = 0;
    Dwarf_Error error = 0;
    int fres = 0;

    fres = dwarf_get_fde_list_eh (dbg, &cie_data, &cie_count,
                                  &fde_data, &fde_count, &error);

    if (fres == DW_DLV_OK) {
        Dwarf_Fde myfde = 0;
        Dwarf_Addr low_pc = 0;
        Dwarf_Addr high_pc = 0;

        fres = dwarf_get_fde_at_pc (fde_data, mypcval, &myfde, &low_pc, &high_pc,
                                    &error);

        if (fres == DW_DLV_OK) {
            Dwarf_Cie mycie = 0;

            fres = dwarf_get_cie_of_fde (myfde, &mycie, &error);

            if (fres == DW_DLV_OK) {
                printf("found cie\n");

                Dwarf_Unsigned bytes_in_cie;
                Dwarf_Small version;
                char *augmenter;
                Dwarf_Unsigned code_alignment_factor;
                Dwarf_Signed data_alignment_factor;
                Dwarf_Half return_address_register_rule;
                Dwarf_Ptr initial_instructions;
                Dwarf_Unsigned initial_instructions_length;

                fres = dwarf_get_cie_info (mycie,
                                           &bytes_in_cie,
                                           &version,
                                           &augmenter,
                                           &code_alignment_factor,
                                           &data_alignment_factor,
                                           &return_address_register_rule,
                                           &initial_instructions,
                                           &initial_instructions_length,
                                           &error);

                if (fres == DW_DLV_OK) {
                    printf("bytes_in_cie = %u\n", bytes_in_cie);
                    printf("version = %d\n", version);
                    printf("augmenter = %s\n", augmenter);
                    printf("code_alignment_factor = %u\n", code_alignment_factor);
                    printf("data_alignment_factor = %d\n", data_alignment_factor);
                    printf("return_address_register_rule = %d\n", return_address_register_rule);
                    printf("initial_instructions = %p\n", initial_instructions);
                    printf("initial_instructions_length = %u\n", initial_instructions_length);

                    unsigned char *instr = initial_instructions;

                    for (int i=0; i<initial_instructions_length; i++) {
                        printf("%02x ", instr[i]);
                    }

                    printf("\n");
                }
            }

            {
                Dwarf_Small value_type;
                Dwarf_Signed offset_relevant;
                Dwarf_Signed register_num;
                Dwarf_Signed offset_or_block_len;
                Dwarf_Ptr block_ptr;
                Dwarf_Addr row_pc;

                fres = dwarf_get_fde_info_for_cfa_reg3 (myfde,
                                                    mypcval,
                                                    &value_type,
                                                    &offset_relevant,
                                                    &register_num,
                                                    &offset_or_block_len,
                                                    &block_ptr,
                                                    &row_pc,
                                                    &error);

                if (fres == DW_DLV_OK) {
                    printf("cfa\n");
                    printf("\tvalue_type = %d\n", (Dwarf_Signed) value_type);
                    printf("\toffset_relevant = %d\n", offset_relevant);
                    printf("\tregister_num = %d\n", register_num);
                    printf("\toffset_or_block_len = %d\n", offset_or_block_len);
                    printf("\tblock_ptr = %p\n", block_ptr);
                }
            }

            Dwarf_Half table_column = 1;

            for (table_column = 0; table_column <= 16; table_column++) {
                Dwarf_Small value_type;
                Dwarf_Signed offset_relevant;
                Dwarf_Signed register_num;
                Dwarf_Signed offset_or_block_len;
                Dwarf_Ptr block_ptr;
                Dwarf_Addr row_pc;

                fres = dwarf_get_fde_info_for_reg3 (myfde,
                                                    table_column,
                                                    mypcval,
                                                    &value_type,
                                                    &offset_relevant,
                                                    &register_num,
                                                    &offset_or_block_len,
                                                    &block_ptr,
                                                    &row_pc,
                                                    &error);

                if (fres == DW_DLV_OK) {
                    printf("register %d\n", table_column);
                    printf("\tvalue_type = %d\n", (Dwarf_Signed) value_type);
                    printf("\toffset_relevant = %d\n", offset_relevant);
                    printf("\tregister_num = %d\n", register_num);
                    printf("\toffset_or_block_len = %d\n", offset_or_block_len);
                    printf("\tblock_ptr = %p\n", block_ptr);
                }
            }
        }

        dwarf_fde_cie_list_dealloc (dbg, cie_data, cie_count, fde_data, fde_count);
    }

    /* ERROR or NO ENTRY. Do something */
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
        progname = "dorabin";
    }

    if ((fd = open(progname, O_RDONLY)) < 0) {
        perror("open");
        return 1;
    }

    if (dwarf_init(fd, DW_DLC_READ, 0, 0, &dbg, &err) != DW_DLV_OK) {
        fprintf(stderr, "Failed DWARF initialization\n");
        return 1;
    }

    list_eh_frame_entries(dbg, 0x375f96);

    if (dwarf_finish(dbg, &err) != DW_DLV_OK) {
        fprintf(stderr, "Failed DWARF finalization\n");
        return 1;
    }

    close(fd);
    return 0;
}


