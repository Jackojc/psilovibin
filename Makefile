# psilovibin
.POSIX:

include config.mk

all: options pv

config:
	@mkdir -p build/

options:
	@printf "cxx \033[32m$(CXX)\033[0m | "
	@printf "release \033[32m$(release)\033[0m\n"

pv: config options
	@$(CXX) $(PV_CXXFLAGS) src/pv.cpp -o build/pv $(PV_LDFLAGS)

clean:
	rm -rf build/pv pv-$(VERSION).tar.gz

dist: clean pv
	mkdir -p pv-$(VERSION)
	cp -R LICENSE Makefile README.md config.mk src/ doc/ modules/ pv-$(VERSION)
	tar -cf - pv-$(VERSION) | gzip > pv-$(VERSION).tar.gz
	rm -rf pv-$(VERSION)

install: pv
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp -f build/pv $(DESTDIR)$(PREFIX)/bin
	chmod 755 $(DESTDIR)$(PREFIX)/bin/pv
	mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	sed "s/VERSION/$(VERSION)/g" < doc/pv.1 > $(DESTDIR)$(MANPREFIX)/man1/pv.1
	chmod 644 $(DESTDIR)$(MANPREFIX)/man1/pv.1

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/pv
	rm -f $(DESTDIR)$(MANPREFIX)/man1/pv.1

.PHONY: all config options clean dist install uninstall

