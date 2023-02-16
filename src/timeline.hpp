#ifndef PV_PASS_TIMELINE_HPP
#define PV_PASS_TIMELINE_HPP

/*
	HEADERS

	#include <iostream>
	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass walks the AST and generates a timeline of events that will then be used
	to generate the MIDI messages we want to send to various devices.
*/
namespace pv {
	bool timelime_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		View sv,
		SymbolKind kind,
		std::vector<Symbol>::iterator& it
	) {
		switch (kind) {
			case SymbolKind::NONE: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
			} break;

			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
			} break;

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
					it = visit_block(timelime_impl, ctx, tree, it);
			} break;

			case SymbolKind::PATTERN: {  // Note patterns.
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
				it = visit_block(timelime_impl, ctx, tree, it);
			} break;

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
				it = visit_block(timelime_impl, ctx, tree, it);
			} break;

			case SymbolKind::MIDI:  // Expressions/Statements with no arguments.
			case SymbolKind::SELECT: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, PV_RESET);
				it = visit_block(timelime_impl, ctx, tree, it);
			} break;

			case SymbolKind::LET:  // Expressions/Statements with arguments.
			case SymbolKind::CONTROL: {
				// detail::indent(std::cerr, spaces); println(std::cerr, colour, kind, " `", sv, "`", PV_RESET);
				it = visit_block(timelime_impl, ctx, tree, it);
			} break;

			default: return false;
		}

		return true;
	}

	inline std::vector<Symbol>::iterator timeline(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);
		return pass(timelime_impl, ctx, tree);
	}
}

#endif

