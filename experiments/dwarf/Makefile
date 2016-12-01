build: dwarf-addr dwarf-debug

dwarf-addr: dwarf-addr.c
	gcc dwarf-addr.c -o dwarf-addr -g -ldwarf -lelf -I/usr/include/libdwarf

dwarf-debug: dwarf-debug.c
	gcc dwarf-debug.c -o dwarf-debug -g -ldwarf -lelf -I/usr/include/libdwarf
