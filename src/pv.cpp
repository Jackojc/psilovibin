#include <cstddef>
#include <cstdint>

#include <unordered_set>
#include <vector>
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
	Context ctx;

	while (std::getline(std::cin, buf)) {
		try {
			Lexer lx { View { buf.data(), buf.data() + buf.size() }};
			Symbol sym = take(lx, ctx.syms);  // Prepare the lexer.

			// while (lx.peek.kind != SymbolKind::TERM)
				// std::cout << take(lx, ctx.syms) << '\n';
			std::vector<Symbol> syms = program(ctx, lx);

			for (Symbol s: syms)
				println(std::cout, s);
		}

		catch (Report x) {
			report_handler(std::cerr, x);
		}
	}

	return 0;
}
