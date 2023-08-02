DRONES_DIR = $(shell git config "borg.drones-directory" || echo "site-lisp")
USER-EMACS-DIR = ~/.emacs.d/
CONFIG_DIR = core lisp lib
CONFIG_CLEAN_ARG = --eval "(when (fboundp 'comp-el-to-eln-filename)\
  (dolist (dir '(\"~/.emacs.d/core/\"\
				 \"~/.emacs.d/lisp/\"\
				 \"~/.emacs.d/lib/\"))\
	(dolist (el (directory-files dir t \"\\\\.el\\\\'\"))\
	  (delete-file (comp-el-to-eln-filename el)))))"
EMACS_EXTRA ?= --load "/usr/local/share/emacs/29.1/lisp/emacs-lisp/cl-lib.el.gz" \
-L ~/.emacs.d/site-lisp/once/ -L ~/.emacs.d/site-lisp/once/once-setup/ \
-L ~/.emacs.d/site-lisp/setup/ -L ~/.emacs.d/site-lisp/epkg/ \
--load ~/.emacs.d/core/core-incremental-loading.el \
--load ~/.emacs.d/core/core-setup.el

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
		@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url git@github.com:emacscollective/borg.git
		@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
		@cd $(DRONES_DIR)/borg; git reset --hard HEAD

update: 
	git submodule update --init --recursive

rebuild: clean native config-clean config-native

config-clean:
ifeq "$(BORG_CLEAN_ELN)" "true"
	@$(EMACS) $(EMACS_ARGUMENTS) $(CONFIG_CLEAN_ARG)
	-find $(addprefix $(USER-EMACS-DIR), $(CONFIG_DIR)) -name "*.elc" -type f -delete
else
	-find $(addprefix $(USER-EMACS-DIR), $(CONFIG_DIR)) -name "*.elc" -type f -delete
endif

config-native:
	@$(EMACS) $(EMACS_ARGUMENTS) --load ~/.emacs.d/script/config-recompile.el 2>&1

.PHONY: config-clean config-native rebuild update
