#include <cstddef>
#include <cstdint>

#include <unordered_set>
#include <vector>
#include <iostream>

// JACK Audio
extern "C" {
	#include <jack/jack.h>
	#include <jack/midiport.h>
	#include <jack/ringbuffer.h>
}

// Core
#include <util.hpp>
#include <view.hpp>
#include <pv.hpp>

// Passes
#include <printer.hpp>

#include <conflict/conflict.hpp>

using namespace pv;

int main(int, const char*[]) {
	std::string buf;  // Keep outside of try/catch to keep buffer alive.
	Context ctx;

	while (std::getline(std::cin, buf)) {
		try {
			Lexer lx { View { buf.data(), buf.data() + buf.size() }};
			Symbol sym = take(lx, ctx.syms);  // Prepare the lexer.

			std::vector<Symbol> syms = program(ctx, lx);

			PV_DBG_RUN([&] {  // Print flat AST.
				for (Symbol s: syms)
					PV_LOG(LogLevel::INF, s);
			} ());

			// Passes.
			printer(ctx, syms);

			// Everything went okay.
			println(std::cout, "[" PV_OK "ok" PV_RESET "]");
		}

		catch (Report x) {
			report_handler(std::cerr, x);
		}
	}

	return 0;
}
