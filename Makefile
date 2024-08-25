default:
	@fpm run

gdb:
	@MALLOC_CHECK_=2 fpm run --flag   -g \
	                         --c-flag -g

valgrind:
	@valgrind --trace-children=yes --leak-check=full fpm run --flag   -g \
	                                                         --c-flag -g

release:
	@fpm run --flag   -fuse-ld=mold --flag   -O3 --flag   -march=native --flag   -mtune=native \
	         --c-flag -fuse-ld=mold --c-flag -O3 --c-flag -march=native --c-flag -mtune=native

.PHONY: test
test:
	@fpm test

.PHONY: test_gdb
test_gdb:
	@MALLOC_CHECK_=2 fpm test --flag   -g \
	                          --c-flag -g
.PHONY: test_valgrind
test_valgrind:
	@valgrind --trace-children=yes --leak-check=full fpm test --flag   -g \
	                                                          --c-flag -g

#! BUILD COMMANDS.
.PHONY: build
build:
	@fpm build
	@./scripts/copy_built_file_debug.sh
	
.PHONY: r_build
r_build:
	@fpm build --flag   -fuse-ld=mold --flag   -O3 --flag   -march=native --flag   -mtune=native \
	           --c-flag -fuse-ld=mold --c-flag -O3 --c-flag -march=native --c-flag -mtune=native
	@./scripts/copy_built_file_release.sh


#! CLEANING COMMANDS.
	
# Use this if the vscode extension gives up.
clean:
	@./scripts/clear_mod_files.sh
	@./scripts/remove_build_folder.sh
	@./scripts/remove_out_folder.sh


#? Leaving this in for when polymorphic types are implemented.
# --compiler flang-new 
