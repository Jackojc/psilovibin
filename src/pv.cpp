#include <cstddef>
#include <cstdint>

#include <type_traits>
#include <algorithm>
#include <unordered_map>
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
// #include <printer.hpp>
// #include <dot.hpp>

// Passes
// #include <canonicalise.hpp>
// #include <semantic.hpp>
// #include <fold.hpp>
// #include <timeline.hpp>

// Argument parsing
#include <conflict/conflict.hpp>

// Namespaces
using namespace pv;

using timer = std::chrono::steady_clock;
using unit = std::chrono::milliseconds;
namespace chrono = std::chrono;

/// Constants
constexpr std::string_view unit_suffix = "ms";

enum: uint64_t {
	OPT_HELP  = 1 << 0,
	OPT_DEBUG = 1 << 1,
};

int main(int argc, const char* argv[]) {
	// Argument parser.
	uint64_t flags;

	auto parser = conflict::parser {
		conflict::option {{ 'h', "help", "show help" }, flags, OPT_HELP }
	};

	parser.apply_defaults();
	auto status = parser.parse(argc - 1, argv + 1);

	// Handle argument parsing errors.
	switch (status.err) {
		case conflict::error::invalid_option: {
			println(std::cerr, PV_ERR "invalid option: " PV_RESET, status.what1);
			return 1;
		} break;

		case conflict::error::invalid_argument: {
			println(std::cerr, PV_ERR "invalid argument: " PV_RESET, status.what1, " for ", status.what2);
			return 1;
		} break;

		case conflict::error::missing_argument: {
			println(std::cerr, PV_ERR "missing argument: " PV_RESET, status.what1);
			return 1;
		} break;

		case::conflict::error::ok: break;
	}

	if (flags & OPT_HELP) {
		parser.print_help();
		return 0;
	}

	// REPL
	std::string buf;  // Keep outside of try/catch to keep buffer alive.
	std::list<std::string> history;

	Tree tree;  // Persistent AST that is appended to.

	Context ctx;

	while (std::getline(std::cin, buf)) {
		std::string& current = *history.insert(history.end(), buf);

		try {
			auto compile_t1 = timer::now();

				Lexer lx { View { current.data(), current.data() + current.size() }};
				Symbol sym = lx.take();  // Prepare the lexer.

				Tree prog = program(ctx, lx);

			auto compile_t2 = timer::now();

			PV_LOG(LogLevel::OK, "compilation took ",
				chrono::duration_cast<unit>(compile_t2 - compile_t1).count(), unit_suffix
			);

			// Passes.
			auto passes_t1 = timer::now();

				// canonicalise(ctx, prog);
				// semantic(ctx, prog);
				// timeline(ctx, prog);

			auto passes_t2 = timer::now();

			PV_LOG(LogLevel::OK, "passes took ",
				chrono::duration_cast<unit>(passes_t2 - passes_t1).count(), unit_suffix
			);

			// Everything went okay.
			tree.cat(prog);

			println(std::cout, tree);
			println(std::cout, "[" PV_OK "ok" PV_RESET "]");
		}

		catch (Report x) {
			report_handler(std::cerr, x);
		}
	}

	return 0;
}
