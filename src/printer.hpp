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
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
			} break;

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
					it = visit_block(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::PATTERN: {  // Note patterns.
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					it = visit_block(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					it = visit_block(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::MIDI:  // Expressions/Statements with no arguments.
			case SymbolKind::SELECT:
			case SymbolKind::ACTION: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
					it = visit_block(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			case SymbolKind::LET:  // Expressions/Statements with arguments.
			case SymbolKind::CONTROL: {
				detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
					it = visit_block(printer_impl, ctx, tree, it, spaces + 1);
			} break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator printer(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);
		return pass(printer_impl, ctx, tree, 0ul);
	}
}

#endif

