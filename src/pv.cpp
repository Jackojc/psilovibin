#include <cstddef>
#include <cstdint>

#include <type_traits>
#include <algorithm>
#include <unordered_set>
#include <vector>
#include <array>
#include <list>
#include <string_view>
#include <iostream>
#include <chrono>

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

using timer = std::chrono::steady_clock;
using unit = std::chrono::milliseconds;
namespace chrono = std::chrono;

constexpr std::string_view unit_suffix = "ms";

int main(int, const char*[]) {
	std::string buf;  // Keep outside of try/catch to keep buffer alive.
	std::list<std::string> history;

	std::vector<Symbol> tree;  // Persistent AST that is appended to.

	Context ctx;

	while (std::getline(std::cin, buf)) {
		std::string& current = *history.insert(history.end(), buf);

		try {
			auto compile_t1 = timer::now();

				Lexer lx { View { current.data(), current.data() + current.size() }};
				Symbol sym = take(lx, ctx.syms);  // Prepare the lexer.

				std::vector<Symbol> prog = program(ctx, lx);
				tree.insert(tree.end(), prog.begin(), prog.end());

			auto compile_t2 = timer::now();

			PV_LOG(LogLevel::OK, "compilation took ",
				chrono::duration_cast<unit>(compile_t2 - compile_t1).count(), unit_suffix
			);

			// Passes.
			auto passes_t1 = timer::now();

				PV_DBG_RUN(printer(ctx, tree));
				// PV_DBG_RUN(dot(ctx, tree));

			auto passes_t2 = timer::now();

			PV_LOG(LogLevel::OK, "passes took ",
				chrono::duration_cast<unit>(passes_t2 - passes_t1).count(), unit_suffix
			);

			// Everything went okay.
			println(std::cout, "[" PV_OK "ok" PV_RESET "]");
		}

		catch (Report x) {
			report_handler(std::cerr, x);
		}
	}

	return 0;
}
