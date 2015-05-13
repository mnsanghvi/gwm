## Process this file with Automake to create Makefile.in

gwm_data = *.scm *.scm.template *.x[pb]m */*

install-data-am:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	$(TAR) cf - $(gwm_data) | (cd $(DESTDIR)$(pkgdatadir); $(TAR) xof -)
#	$(INSTALL) gwm-menu-method /etc/menu-methods/
#	update-menus

uninstall-am:
	@$(NORMAL_UNINSTALL)
	rm -fR $(DESTDIR)$(pkgdatadir)/*

EXTRA_DIST = $(gwm_data) gwm-menu-method

