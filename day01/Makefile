# Default: build with debug symbols
# Usage: make program_name (debug) or make release program_name (no debug)

ifdef RELEASE
NASM_FLAGS = -f elf64
else
NASM_FLAGS = -f elf64 -g -F dwarf
endif

%.o: %.asm
	nasm $(NASM_FLAGS) $< -o $@

%: %.o
	@[ -f $< ] || { echo "Error: $< not found"; exit 1; }
	ld -o $@ $<

.SUFFIXES:
.SUFFIXES: .asm .o

.PHONY: release
release:
	@$(MAKE) RELEASE=1 $(filter-out release,$(MAKECMDGOALS))
