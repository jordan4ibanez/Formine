default:
	@fpm run

clean:
	@./scripts/clear_mod_files.sh
	@./scripts/remove_build_folder.sh