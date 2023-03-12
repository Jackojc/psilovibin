#ifndef PV_PASS_PRINTER_HPP
#define PV_PASS_PRINTER_HPP

/*
	HEADERS

	#include <iostream>
	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass pretty-prints the AST with a nested structure.
*/
namespace pv {
	namespace detail {
		inline std::ostream& indent(std::ostream& os, size_t n) {
			while (n--) print(os, "    ");
			return os;
		}
	}

	Tree printer_impl(
		Context& ctx,
		Tree tree,
		Tree::const_iterator current,
		Tree::const_iterator& it,
		size_t spaces = 0
	) {
		auto [sv, kind] = *current;

		constexpr std::array colours {
			PV_BLUE, PV_YELLOW,
		};

		const auto colour = colours[spaces % colours.size()];

		switch (kind) {
			case SymbolKind::NONE:
			case SymbolKind::END: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
			} break;

			case SymbolKind::LEFT:  // Commands with integer arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:
			case SymbolKind::UP:
			case SymbolKind::DOWN: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					return visit(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::SEQUENCE:  // Note patterns.
			case SymbolKind::PARALLEL: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					return visit(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					return visit(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::MIDI:  // Expressions/Statements with no arguments.
			case SymbolKind::SELECT: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					return visit(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::LET: {  // Expressions/Statements with arguments.
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
					return visit(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			default: {
				PV_LOG(LogLevel::WRN, "unhandled symbol: `", kind, "`");
			} break;
		}

		return {};
	}

	[[nodiscard]] inline Tree printer(Context& ctx, Tree tree) {
		PV_LOG(LogLevel::OK);
		return pass(printer_impl, ctx, tree, 0ul);
	}
}

#endif

