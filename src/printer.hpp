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
			while (n--) print(os, PV_BLACK "| " PV_RESET);
			return os;
		};
	}

	bool printer_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		View sv,
		SymbolKind kind,
		std::vector<Symbol>::iterator& it,
		size_t spaces = 0
	) {
		constexpr std::array colours {
			PV_BLUE, PV_YELLOW,
		};

		const auto colour = colours[spaces % colours.size()];

		switch (kind) {
			case SymbolKind::NONE: {
				detail::indent(std::cout, spaces); println(std::cout, colour, kind, PV_RESET);
			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				detail::indent(std::cout, spaces); println(std::cout, colour, kind, " `", sv, "`", PV_RESET);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:

			case SymbolKind::LET:  // Expressions/Statements.
			case SymbolKind::INSTRUMENT:
			case SymbolKind::CONTROL:
			case SymbolKind::ACTION:
			case SymbolKind::PROGRAM: {  // Top level node.
				detail::indent(std::cout, spaces); println(std::cout, colour, kind, " `", sv, "`", PV_RESET);
					it = visit_block(printer_impl, ctx, tree, it, spaces + 1);
				detail::indent(std::cout, spaces); println(std::cout, colour, SymbolKind::END, PV_RESET);
			} break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator printer(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);
		return visitor(printer_impl, ctx, tree, tree.begin(), 0ul);
	}
}

#endif

