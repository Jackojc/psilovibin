#include <cstddef>
#include <cstdint>

#include <type_traits>
#include <algorithm>
#include <unordered_set>
#include <vector>
#include <array>
#include <string_view>
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

// Debug passes
#include <printer.hpp>
#include <dot.hpp>

// Passes
// ...

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

			// Debug passes.
			PV_DBG_RUN(printer(ctx, syms));
			PV_DBG_RUN(dot(ctx, syms));

			// Passes
			// ...

			// Everything went okay.
			println(std::cout, "[" PV_OK "ok" PV_RESET "]");
		}

		catch (Report x) {
			report_handler(std::cerr, x);
		}
	}

	return 0;
}
