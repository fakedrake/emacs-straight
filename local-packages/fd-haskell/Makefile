VERSION:=0.0.2
PACKAGE_NAME:=fd-haskell-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r ../fd-haskell/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/demo-multifile-package-template.el > $@/demo-multifile-pkg.el

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)

