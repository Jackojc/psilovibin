#include <cstddef>
#include <cstdint>

#include <unordered_set>
#include <iostream>

extern "C" {
	#include <jack/jack.h>
	#include <jack/midiport.h>
	#include <jack/ringbuffer.h>
}

#include <util.hpp>
#include <view.hpp>
#include <pv.hpp>

#include <conflict/conflict.hpp>

using namespace pv;

int main(int, const char*[]) {
	std::string buf;  // Keep outside of try/catch to keep buffer alive.

	try {
		SymbolTable syms;

		while (std::getline(std::cin, buf)) {
			Lexer lx { View { buf.data(), buf.data() + buf.size() }};
			Symbol sym = take(lx, syms);  // Prepare the lexer.

			while (lx.peek.kind != SymbolKind::TERM)
				std::cout << take(lx, syms) << '\n';
		}
	}

	catch (Report x) {
		report_handler(std::cerr, x);
	}

	return 0;
}
