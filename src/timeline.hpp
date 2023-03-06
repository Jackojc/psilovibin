#ifndef PV_PASS_TIMELINE_HPP
#define PV_PASS_TIMELINE_HPP

/*
	HEADERS

	#include <iostream>
	#include <util.hpp>
	#include <pv.hpp>
*/

/*
	This pass walks the AST and performs stack based code generation. It uses
	the same data structures and types as the AST but it's completely flat.
*/
namespace pv {
	std::vector<Symbol>::iterator pattern_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator it,
		std::vector<Symbol> stack
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::INTEGER: {
				// println(std::cout, SymbolKind::NOTE, " ", sv);
				stack.emplace_back(*current);
			} break;

			case SymbolKind::SEQUENCE: {
				// println(std::cout, SymbolKind::DESCEND);
				it = visit_block(pattern_impl, ctx, tree, it, stack);
				// println(std::cout, SymbolKind::ASCEND);
				println(std::cout, kind);
			} break;

			case SymbolKind::PARALLEL: {
				it = visit_block(pattern_impl, ctx, tree, it, stack);
				println(std::cout, kind);
			} break;

			default: {
				PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");
			} break;
		}

		return it;
	}

	std::vector<Symbol>::iterator timeline_impl(
		Context& ctx,
		std::vector<Symbol>& tree,
		std::vector<Symbol>::iterator current,
		std::vector<Symbol>::iterator it,
		std::vector<Symbol> stack
	) {
		auto [sv, kind] = *current;

		switch (kind) {
			case SymbolKind::STRING:  // Literals.
			case SymbolKind::INTEGER:
			case SymbolKind::IDENTIFIER: {
				stack.emplace_back(*current);
			} break;

			case SymbolKind::SEQUENCE:
			case SymbolKind::PARALLEL: {  // Note patterns.
				it = visit_block(pattern_impl, ctx, tree, it, stack);
			} break;

			case SymbolKind::LEFT:  // Commands with arguments.
			case SymbolKind::RIGHT:
			case SymbolKind::VELOCITY:
			case SymbolKind::BPM:
			case SymbolKind::TIME:

			case SymbolKind::GO:  // Commands with no arguments.
			case SymbolKind::STOP:
			case SymbolKind::CLEAR:
			case SymbolKind::INFO:

			case SymbolKind::MIDI:  // Expressions/Statements with no arguments.
			case SymbolKind::SELECT: {
				it = visit_block(timeline_impl, ctx, tree, it, stack);
				// println(std::cout, kind);
				// println(std::cout, stack.back());
			} break;

			default: {
				PV_LOG(LogLevel::WRN, "unhandled symbol: `", current->kind, "`");
			} break;
		}

		return it;
	}

	inline std::vector<Symbol>::iterator timeline(Context& ctx, std::vector<Symbol>& tree) {
		PV_LOG(LogLevel::OK);

		std::vector<Symbol> stack;
		return pass(timeline_impl, ctx, tree, stack);
	}
}

#endif

